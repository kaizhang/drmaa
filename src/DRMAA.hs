{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module DRMAA where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (bracket_)
import qualified Data.Text                as T
import           Shelly                   hiding (FilePath)

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import qualified Language.C.Inline        as C

C.include "stddef.h"
C.include "stdio.h"
C.include "drmaa.h"

runScriptBulk :: [String] -> DrmaaAttribute -> IO ()
runScriptBulk xs config = do
    _ <- withSGESession $ mapConcurrently (\x -> drmaaScript x config) xs
    return ()

withSGESession :: IO a -> IO a
withSGESession f = bracket_ drmaaInit drmaaExit f

drmaaScript :: String -> DrmaaAttribute -> IO ()
drmaaScript script config = do
    tmp <- shelly $ silently $ run "mktemp" ["ghc_script_tmp.XXXXXXXX.sh"]
    tmpFl <- shelly $ fmap (T.unpack . toTextIgnore) $ absPath $ fromText $
             head $ T.lines tmp
    writeFile tmpFl $ "#!/bin/sh\n" ++ script
    shelly $ run_ "chmod" ["+x", T.pack tmpFl]
    drmaaRun tmpFl [] config
    shelly $ rm $ fromText $ T.pack tmpFl

-- | Initialize a session
drmaaInit :: IO ()
drmaaInit = alloca $ \ptr -> do
    status <- [C.block| int {
        int errnum = 0;
        errnum = drmaa_init (NULL, $(char* ptr), DRMAA_ERROR_STRING_BUFFER);
        if (errnum != DRMAA_ERRNO_SUCCESS) {
            return 1;
        }
        return 0;
        }|]
    case status of
        0 -> putStrLn "DRMAA session started"
        1 -> peekCString ptr >>= error

drmaaExit :: IO ()
drmaaExit = do
    r <- [C.block| int {
        char error[DRMAA_ERROR_STRING_BUFFER];
        int errnum = 0;
        errnum = drmaa_exit (error, DRMAA_ERROR_STRING_BUFFER);
        if (errnum != DRMAA_ERRNO_SUCCESS) {
            fprintf (stderr, "Could not shut down the DRMAA library: %s\n", error);
            return 1;
        }
        return 0;
        }|]
    case r of
        0 -> putStrLn "DRMAA session closed"
        1 -> error "Exit 1"

data DrmaaAttribute = DrmaaAttribute
    { drmaa_wd     :: !FilePath
    , drmaa_tmpDir :: !FilePath   -- temperary directory. Trying access the
                                  -- default temp dir usually causes problem in
                                  -- a cluster
    } deriving (Show, Read)

defaultDrmaaConfig :: DrmaaAttribute
defaultDrmaaConfig = DrmaaAttribute
    { drmaa_wd = "./"
    , drmaa_tmpDir = "./drmaa_tmp"
    }

drmaaRun :: FilePath -> [String] -> DrmaaAttribute -> IO ()
drmaaRun exec args config = do
    wd <- newCString $ drmaa_wd config
    c_exec <- newCString exec

    env <- mapM newCString ["TMPDIR=" ++ drmaa_tmpDir config]
    c_args <- mapM newCString args
    e <- withArray (c_args++[nullPtr]) $ \aptr ->
        withArray (env ++ [nullPtr]) $ \envPtr -> do
            [C.block| int {
            int exception = 0;
            char error[DRMAA_ERROR_STRING_BUFFER];
            int errnum = 0;
            drmaa_job_template_t *jt = NULL;

            errnum = drmaa_allocate_job_template (&jt, error, DRMAA_ERROR_STRING_BUFFER);

            if (errnum != DRMAA_ERRNO_SUCCESS) {
                fprintf (stderr, "Could not create job template: %s\n", error);
            } else {
                /* set tmp dir */
                /* set work directory */
                errnum = drmaa_set_attribute (jt, DRMAA_WD, $(char* wd),
                                             error, DRMAA_ERROR_STRING_BUFFER);

                errnum = drmaa_set_attribute (jt, DRMAA_REMOTE_COMMAND, $(char* c_exec),
                                             error, DRMAA_ERROR_STRING_BUFFER);
                errnum = drmaa_vector_attribute (jt, DRMAA_V_ENV, $(const char** envPtr),
                                             error, DRMAA_ERROR_STRING_BUFFER);
                if (errnum != DRMAA_ERRNO_SUCCESS) {
                    fprintf (stderr, "Could not set attribute \"%s\": %s\n",
                            DRMAA_REMOTE_COMMAND, error);
                } else {
                    errnum = drmaa_set_vector_attribute (jt, DRMAA_V_ARGV, $(const char** aptr), error,
                                                        DRMAA_ERROR_STRING_BUFFER);
                }

                if (errnum != DRMAA_ERRNO_SUCCESS) {
                    fprintf (stderr, "Could not set attribute \"%s\": %s\n",
                            DRMAA_REMOTE_COMMAND, error);
                } else {
                    char jobid[DRMAA_JOBNAME_BUFFER];
                    char jobid_out[DRMAA_JOBNAME_BUFFER];
                    int status = 0;
                    drmaa_attr_values_t *rusage = NULL;

                    errnum = drmaa_run_job (jobid, DRMAA_JOBNAME_BUFFER, jt, error,
                                           DRMAA_ERROR_STRING_BUFFER);

                    if (errnum != DRMAA_ERRNO_SUCCESS) {
                        fprintf (stderr, "Could not submit job: %s\n", error);
                        exception = 1;
                    } else {
                        printf ("Your job has been submitted with id %s\n", jobid);

                        errnum = drmaa_wait (jobid, jobid_out, DRMAA_JOBNAME_BUFFER, &status,
                                            DRMAA_TIMEOUT_WAIT_FOREVER, &rusage, error,
                                            DRMAA_ERROR_STRING_BUFFER);

                        if (errnum != DRMAA_ERRNO_SUCCESS) {
                            fprintf (stderr, "Could not wait for job: %s\n", error);
                            exception = 1;
                        } else {
                            char usage[DRMAA_ERROR_STRING_BUFFER];
                            int aborted = 0;

                            drmaa_wifaborted(&aborted, status, NULL, 0);

                            if (aborted == 1) {
                                printf("Job %s never ran\n", jobid);
                                exception = 1;
                            } else {
                                int exited = 0;

                                drmaa_wifexited(&exited, status, NULL, 0);

                                if (exited == 1) {
                                    int exit_status = 0;

                                    drmaa_wexitstatus(&exit_status, status, NULL, 0);
                                    printf("Job %s finished regularly with exit status %d\n", jobid, exit_status);
                                    exception = exit_status;
                                } else {
                                    int signaled = 0;

                                    drmaa_wifsignaled(&signaled, status, NULL, 0);

                                    if (signaled == 1) {
                                        char termsig[DRMAA_SIGNAL_BUFFER+1];

                                        drmaa_wtermsig(termsig, DRMAA_SIGNAL_BUFFER, status, NULL, 0);
                                        printf("Job %s finished due to signal %s\n", jobid, termsig);
                                    } else {
                                        printf("Job %s finished with unclear conditions\n", jobid);
                                    }
                                } /* else */
                            } /* else */

                            printf ("Job Usage:\n");

                            while (drmaa_get_next_attr_value (rusage, usage, DRMAA_ERROR_STRING_BUFFER) == DRMAA_ERRNO_SUCCESS) {
                                printf ("  %s\n", usage);
                            }

                            drmaa_release_attr_values (rusage);
                        } /* else */
                    } /* else */
                } /* else */

                errnum = drmaa_delete_job_template (jt, error, DRMAA_ERROR_STRING_BUFFER);

                if (errnum != DRMAA_ERRNO_SUCCESS) {
                    fprintf (stderr, "Could not delete job template: %s\n", error);
                }
            } /* else */

            return exception;
            }|]
    if e /= 0 then error ("status=" ++ show e) else return ()
