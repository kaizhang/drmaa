
#include "stddef.h"

#include "stdio.h"

#include "drmaa.h"

int inline_c_DRMAA_0_54e600eaaa1f15b2d247a7799eeb2c2626774aa9(char * ptr_inline_c_0) {

        int errnum = 0;
        errnum = drmaa_init (NULL, ptr_inline_c_0, DRMAA_ERROR_STRING_BUFFER);
        if (errnum != DRMAA_ERRNO_SUCCESS) {
            return 1;
        }
        return 0;
        
}


int inline_c_DRMAA_1_df791815daa185d543e0513c42f96dbbd8b8b214() {

        char error[DRMAA_ERROR_STRING_BUFFER];
        int errnum = 0;
        errnum = drmaa_exit (error, DRMAA_ERROR_STRING_BUFFER);
        if (errnum != DRMAA_ERRNO_SUCCESS) {
            fprintf (stderr, "Could not shut down the DRMAA library: %s\n", error);
            return 1;
        }
        return 0;
        
}


int inline_c_DRMAA_2_8569c20fd54d45f727dd1560458a1febedbc42e1(char * wd_inline_c_0, const char ** envPtr_inline_c_1, char * native_inline_c_2, char * c_exec_inline_c_3, const char ** aptr_inline_c_4) {

            int exception = 0;
            char error[DRMAA_ERROR_STRING_BUFFER];
            int errnum = 0;
            drmaa_job_template_t *jt = NULL;

            errnum = drmaa_allocate_job_template (&jt, error, DRMAA_ERROR_STRING_BUFFER);

            if (errnum != DRMAA_ERRNO_SUCCESS) {
                fprintf (stderr, "Could not create job template: %s\n", error);
            } else {
                /* set options */
                errnum = drmaa_set_attribute (jt, DRMAA_WD, wd_inline_c_0,
                                             error, DRMAA_ERROR_STRING_BUFFER);
                errnum = drmaa_set_vector_attribute (jt, DRMAA_V_ENV, envPtr_inline_c_1,
                                             error, DRMAA_ERROR_STRING_BUFFER);
                errnum = drmaa_set_attribute (jt, DRMAA_NATIVE_SPECIFICATION, native_inline_c_2,
                                             error, DRMAA_ERROR_STRING_BUFFER);

                errnum = drmaa_set_attribute (jt, DRMAA_REMOTE_COMMAND, c_exec_inline_c_3,
                                             error, DRMAA_ERROR_STRING_BUFFER);
                if (errnum != DRMAA_ERRNO_SUCCESS) {
                    fprintf (stderr, "Could not set attribute \"%s\": %s\n",
                            DRMAA_REMOTE_COMMAND, error);
                } else {
                    errnum = drmaa_set_vector_attribute (jt, DRMAA_V_ARGV, aptr_inline_c_4, error,
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
            
}

