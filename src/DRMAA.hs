{-# LANGUAGE RecordWildCards #-}
module DRMAA
    ( withSession
    , JobAttributes(..)
    , JobId(..)
    , JobResult(..)
    , RUsage(..)
    , ExitStatus(..)
    , defaultJobAttributes
    , runJob
    , waitJob
    , version
    ) where

import           Control.Exception     (bracket_, bracket)
import           Control.Monad         (when, unless, forM)
import           System.Directory      (getCurrentDirectory)
import Data.Maybe
import Data.List (break)
import Control.Arrow (second)
import Foreign
import Foreign.C.String

import DRMAA.Bindings
import DRMAA.Types

withSession :: IO a -> IO a 
withSession = bracket_ start quit
  where
    start = allocaBytes 1000 $ \errMsg -> do
        exitCode <- drmaaInit nullPtr errMsg 1000
        unless (exitCode == 0) $ peekCString errMsg >>= error
    quit = allocaBytes 1000 $ \errMsg -> do
        exitCode <- drmaaExit errMsg 1000
        unless (exitCode == 0) $ peekCString errMsg >>= error

data JobAttributes = JobAttributes
    { _wd :: Maybe FilePath  -- ^ specifies the directory name where the job will be executed.
    , _job_name :: String  -- ^ A job name SHALL contain only alphanumeric and '_' characters.
    , _native_specification :: Maybe String
    , _env :: [(String, String)]
    } deriving (Show)

defaultJobAttributes :: JobAttributes
defaultJobAttributes = JobAttributes
    { _wd = Nothing
    , _job_name = "drmaa"
    , _native_specification = Nothing
    , _env = [] } 

setJobAttributes :: JobAttributes -> JobTemplate -> IO ()
setJobAttributes JobAttributes{..} jt = do
    setAttribute jt "drmaa_job_name" _job_name
    setAttribute jt "drmaa_wd" =<< get_wd _wd
    when (isJust _native_specification) $
        setAttribute jt "drmaa_native_specification" $ fromJust _native_specification
    unless (null _env) $
        setVectorAttribute jt "drmaa_v_env" $ map (\(a,b) -> a ++ "=" ++ b) _env
{-# INLINE setJobAttributes #-}

setAttribute :: JobTemplate -> String -> String -> IO ()
setAttribute jt name value = allocaBytes 200 $ \errMsg -> do
    exitCode <- drmaaSetAttribute jt name value errMsg 200
    unless (exitCode == 0) $ peekCString errMsg >>= error
{-# INLINE setAttribute #-}

setVectorAttribute :: JobTemplate -> String -> [String] -> IO ()
setVectorAttribute jt name value = allocaBytes 200 $ \errMsg ->
    bracket (mapM newCString value) (mapM_ free) $ \strings ->
        withArray0 nullPtr strings $ \vec -> do
            exitCode <- drmaaSetVectorAttribute jt name vec errMsg 200
            unless (exitCode == 0) $ peekCString errMsg >>= error
{-# INLINE setVectorAttribute #-}

withJobAttributes :: JobAttributes -> (JobTemplate -> IO a) -> IO a
withJobAttributes ja fun = allocaJobTemplate $ \jt -> do
    setJobAttributes ja jt
    fun jt
{-# INLINE withJobAttributes #-}

newtype JobId = JobId { getJobId :: String }

runJob :: FilePath
       -> [String]
       -> JobAttributes
       -> IO (Either String JobId)
runJob cmd args ja = withJobAttributes ja $ \jt -> allocaBytes 100 $ \jobId ->
    allocaBytes 200 $ \errMsg -> do
        setAttribute jt "drmaa_remote_command" cmd
        setVectorAttribute jt "drmaa_v_argv" args
        exitCode <- drmaaRunJob jobId 100 jt errMsg 200
        if exitCode == 0
            then Right . JobId <$> peekCString jobId
            else Left <$> peekCString errMsg
{-# INLINE runJob #-}

data RUsage = RUsage [(String, String)] deriving (Show)

waitJob :: JobId -> IO (JobResult, RUsage)
waitJob (JobId jid) = allocaBytes 100 $ \finished -> alloca $ \status ->
    alloca $ \attr -> allocaBytes 100 $ \errMsg -> do
        exitCode <- drmaaWait jid finished 100 status (-1) attr errMsg 100
        case getExitStatus exitCode of
            Success -> do
                result <- peek status >>= getJobResult
                usage <- consumeAttrValues =<< peek attr
                return (result, RUsage $ map (second tail . break (=='=')) usage)
            e -> error $ "Could not wait job: " ++ show e
  where
    getJobResult st = alloca $ \tmp -> do
        let aborted next = drmaaWifaborted tmp st nullPtr 0 >> peek tmp >>=
                (\x -> if x == 1 then return Aborted else next)
            exited next = drmaaWifexited tmp st nullPtr 0 >> peek tmp >>= ( \x ->
                if x == 1
                    then drmaaWexitstatus tmp st nullPtr 0 >>
                        peek tmp >>= return . Exit . getExitStatus
                    else next )
            signaled next = drmaaWifsignaled tmp st nullPtr 0 >> peek tmp >>= ( \x ->
                if x == 1
                    then allocaBytes 100 $ \buf ->
                        drmaaWtermsig buf 100 st nullPtr 0 >>
                        peekCString buf >>= return . Signaled
                    else next )
        aborted $ exited $ signaled $ error "Job finished with unclear conditions"
{-# INLINE waitJob #-}

consumeAttrValues :: Ptr AttrValues -> IO [String]
consumeAttrValues ptr = allocaBytes 1024 $ \tmp -> do
    result <- loop tmp
    drmaaReleaseAttrValues ptr
    return result
  where
    loop tmp = do
        exitCode <- drmaaGetNextAttrValue ptr tmp 1024
        if (exitCode == 0)
            then do
                x <- peekCString tmp
                xs <- loop tmp
                return $ x : xs
            else return []
{-# INLINE consumeAttrValues #-}

get_wd :: Maybe FilePath -> IO FilePath
get_wd Nothing  = getCurrentDirectory
get_wd (Just x) = return x
{-# INLINE get_wd #-}

version :: IO String
version = alloca $ \major -> alloca $ \minor -> allocaBytes 100 $ \impl ->
    allocaBytes 200 $ \errMsg -> do
        exitCode <- drmaaVersion major minor errMsg 200
        v <- if (exitCode == 0)
            then do
                x <- peek major
                y <- peek minor
                return $ show x ++"." ++ show y
            else error =<< peekCString errMsg

        exitCode' <- drmaaGetDRMAAImplementation impl 100 errMsg 200
        i <- if (exitCode' == 0)
            then peekCString impl
            else error =<< peekCString errMsg
        return $ "DRMAA v" ++ v ++ "; Implementation: " ++ i