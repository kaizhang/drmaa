module DRMAA.Types
    ( ExitStatus(..)
    , JobResult(..)
    , getExitStatus
    ) where

import Foreign.C.Types

data JobResult = Exit ExitStatus
               | Aborted
               | Signaled String
               deriving (Show, Eq)

data ExitStatus = Success
                | InternalError
                | DrmCommunicationFailure
                | AuthFailure
                | InValidArgument
                | NoActiveSession
                | NoMemory
                | InvalidContactString
                | DefaultContactStringError
                | DrmsInitFailed
                | AlreadyActiveSession
                | DrmsExitError
                | InvalidAttribureFormat
                | InvalidAttribureValue
                | ConflictingAttributeValues
                | TryLater
                | DeniedByDrm
                | InvalidJob
                | ResumeInconsistentState
                | SuspendInconsistentState
                | HoldInconsistentState
                | ReleaseInconsistentState
                | ExitTimeout
                | NoRusage
                deriving (Show, Eq)

getExitStatus :: CInt -> ExitStatus
getExitStatus code = case code of
    0 -> Success
    1 -> InternalError
    2 -> DrmCommunicationFailure
    3 -> AuthFailure
    4 -> InValidArgument
    5 -> NoActiveSession
    6 -> NoMemory
    7 -> InvalidContactString
    8 -> DefaultContactStringError
    9 -> DrmsInitFailed
    10 -> AlreadyActiveSession
    11 -> DrmsExitError
    12 -> InvalidAttribureFormat
    13 -> InvalidAttribureValue
    14 -> ConflictingAttributeValues
    15 -> TryLater
    16 -> DeniedByDrm
    17 -> InvalidJob
    18 -> ResumeInconsistentState
    19 -> SuspendInconsistentState
    20 -> HoldInconsistentState
    21 -> ReleaseInconsistentState
    22 -> ExitTimeout
    23 -> NoRusage
    x -> error $ "Unknown status code: " <> show x