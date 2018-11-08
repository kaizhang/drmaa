{-# LANGUAGE ForeignFunctionInterface #-}

module DRMAA.Bindings
    ( -- * Opaque data types
      JobTemplate
    , AttrValues

      -- * String Vector Helper Functions
    , drmaaGetNextAttrValue
    , drmaaGetNumAttrValues
    , drmaaReleaseAttrValues

      -- * Session management
    , drmaaInit
    , drmaaExit


      -- * Job template
    , allocaJobTemplate
    , drmaaSetAttribute
    , drmaaSetVectorAttribute
    , drmaaGetAttribute

      -- * Job submission
    , drmaaRunJob

      -- * Job status and control
    , drmaaJobPs
    , drmaaWait
    , drmaaWifexited
    , drmaaWexitstatus
    , drmaaWifsignaled
    , drmaaWtermsig
    , drmaaWcoredump
    , drmaaWifaborted

    , drmaaVersion
    , drmaaGetDRMAAImplementation
    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception (bracket_)

#include "drmaa.h"

-------------------------------------------------------------------------------
-- Opaque Data Types
-------------------------------------------------------------------------------

newtype JobTemplate = JobTemplate (Ptr JobTemplate)

unJobTempate :: JobTemplate -> Ptr a
unJobTempate (JobTemplate ptr) = castPtr ptr

data AttrValues

-------------------------------------------------------------------------------
-- String Vector Helper Functions
-------------------------------------------------------------------------------

{#fun drmaa_get_next_attr_value as ^
    { castPtr `Ptr AttrValues'
    , `CString'
    , `CInt'
    } -> `CInt' #}

{#fun drmaa_get_num_attr_values as ^
    { castPtr `Ptr AttrValues'
    , castPtr `Ptr CULong'
    } -> `CInt' #}

{#fun drmaa_release_attr_values as ^
    { castPtr `Ptr AttrValues'
    } -> `()' #}


-------------------------------------------------------------------------------
-- Session Management
-------------------------------------------------------------------------------

{#fun drmaa_init as ^
    { `CString'  -- ^ A string indicating to which DRMS the DRMAA
                 -- session should bind during initialization.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer. 
    } -> `CInt' #}

{#fun drmaa_exit as ^
    { `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer. 
    } -> `CInt' #}

-------------------------------------------------------------------------------
-- Job template
-------------------------------------------------------------------------------

allocaJobTemplate :: (JobTemplate -> IO a) -> IO a
allocaJobTemplate fun = alloca $ \jt_ptr -> bracket_
    (drmaaAllocateJobTemplate jt_ptr nullPtr 0)
    ( do p <- peek jt_ptr
         drmaaDeleteJobTemplate p nullPtr 0 )
    (peek jt_ptr >>= fun . JobTemplate . castPtr)

{#fun drmaa_allocate_job_template as ^
    { id `Ptr (Ptr ())', `CString', `CInt'
    } -> `CInt' #}

{#fun drmaa_delete_job_template as ^
    { id `Ptr ()', `CString', `CInt'
    } -> `CInt' #}

{#fun drmaa_set_attribute as ^
    { unJobTempate `JobTemplate'  -- ^ The job template in which the
                                  -- attribute is to be set.
    , `String'   -- ^ The name of the attribute to set.
    , `String'   -- ^ The value to which to set the attribute.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer. 
    } -> `CInt' #}

{#fun drmaa_get_attribute as ^
    { unJobTempate `JobTemplate'  -- ^ The job template from which to get the
                                  -- attribute value.
    , `String'    -- ^ The name of the attribute whose value will be retrieved.
    , `CString'   -- ^ A buffer into which the attribute's value will be written.
    , `CInt'      -- ^ The size in characters of the attribute value buffer.
    , `CString'   -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'      -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_set_vector_attribute as ^
    { unJobTempate `JobTemplate'  -- ^ The job template in which the
                                  -- attribute is to be set.
    , `String'   -- ^ The name of the attribute to set.
    , id `Ptr CString'   -- ^ The value array to which to set the attribute.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer. 
    } -> `CInt' #}

{-
int drmaa_get_vector_attribute(drmaa_job_template_t *jt,
 const char *name,
 drmaa_attr_values_t **values,
 char *error_diagnosis,
 size_t error_diag_len);
int drmaa_get_attribute_names(drmaa_attr_names_t **values,
 char *error_diagnosis,
 size_t error_diag_len);
int drmaa_get_vector_attribute_names(drmaa_attr_names_t **values,
 char *error_diagnosis,
 size_t error_diag_len); 
 -}


-------------------------------------------------------------------------------
-- Job Submission
-------------------------------------------------------------------------------

{#fun drmaa_run_job as ^
    { `CString'  -- ^ A buffer into which the submitted
                                     -- job's job identifier will be written.
    , `CInt'     -- ^ The size in characters of the job identifier buffer.
    , unJobTempate `JobTemplate'  -- ^ The job template in which the
                                  -- attribute is to be set.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}


-------------------------------------------------------------------------------
-- Job Status and Control
-------------------------------------------------------------------------------

{#fun drmaa_job_ps as ^
    { `String'   -- ^ The job identifier of the job to be queried.
    , id `Ptr CInt' -- ^ Space to write the job's status information.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wait as ^
    { `String'  -- ^ The job identifier of the job for which to wait.
    , `CString' -- ^ A buffer into which the finished job identifier will be written.
    , `CULong'  -- ^ The size in characters of the job identifier buffer.
    , id `Ptr CInt'  -- ^ Space to write the status code of the finished job.
    , `CLong'   -- ^ The number of seconds to remain blocked waiting for a result
    , castPtr `Ptr (Ptr AttrValues)' -- ^ Space to write the resouce usage information of the finished job.
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wifexited as ^
    { id `Ptr CInt' -- ^ Space to write whether the job has an exit status available.
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wexitstatus as ^
    { id `Ptr CInt' -- ^ Space to write job's exit status.
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wifsignaled as ^
    { id `Ptr CInt' -- ^ Space to write whether the job terminated on a signal.
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wtermsig as ^
    { `CString' -- ^ A buffer in which to write the name of the signal.
    , `CULong'    -- ^ The size in characters of the signal buffer.
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wcoredump as ^
    { id `Ptr CInt' -- ^ Space in which to write whether the job produce a core image.
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_wifaborted as ^
    { id `Ptr CInt' -- ^ Space in which to write whether the job aborted before running. 
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}


{#fun drmaa_version as ^
    { id `Ptr CUInt'
    , id `Ptr CUInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}

{#fun drmaa_get_DRMAA_implementation as ^
    { `CString'
    , `CInt'
    , `CString'  -- ^ A buffer into which error diagnosis information will be written.
    , `CInt'     -- ^ The size in characters of the error diagnosis string buffer.
    } -> `CInt' #}