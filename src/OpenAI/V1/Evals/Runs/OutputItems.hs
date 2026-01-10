-- | @\/v1\/evals\/{eval_id}\/runs\/{run_id}\/output_items@
--
-- Output items represent individual test results from an evaluation run,
-- containing the input data, model sample, and grader results.
module OpenAI.V1.Evals.Runs.OutputItems
    ( -- * Main types
      OutputItemID(..)
    , OutputItemObject(..)
      -- * Sample types
    , Sample(..)
    , SampleInput(..)
    , SampleOutput(..)
    , SampleUsage(..)
      -- * Grader results
    , GraderResult(..)
      -- * Enums
    , OutputItemStatus(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Error (Error)
import OpenAI.V1.Evals (EvalID)
import OpenAI.V1.Evals.Runs (EvalRunID)
import OpenAI.V1.ListOf (ListOf)

-- | Output item ID
newtype OutputItemID = OutputItemID{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Status of an output item
data OutputItemStatus = Pass | Fail
    deriving stock (Eq, Generic, Show)

instance FromJSON OutputItemStatus where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON OutputItemStatus where
    toJSON = genericToJSON aesonOptions

instance ToHttpApiData OutputItemStatus where
    toUrlPiece Pass = "pass"
    toUrlPiece Fail = "fail"

-- | Token usage details for a sample
data SampleUsage = SampleUsage
    { cached_tokens :: Natural
    , completion_tokens :: Natural
    , prompt_tokens :: Natural
    , total_tokens :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An input message in the sample
data SampleInput = SampleInput
    { content :: Text
    , role :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An output message in the sample
data SampleOutput = SampleOutput
    { content :: Maybe Text
    , role :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A sample containing the input and output of the evaluation run
data Sample = Sample
    { error :: Maybe Error
      -- ^ Present when the sample failed, null otherwise
    , finish_reason :: Text
    , input :: Vector SampleInput
    , max_completion_tokens :: Natural
    , model :: Text
    , output :: Vector SampleOutput
    , seed :: Natural
    , temperature :: Double
    , top_p :: Double
    , usage :: SampleUsage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Result from a grader for an output item
data GraderResult = GraderResult
    { name :: Text
    , passed :: Bool
    , score :: Double
    , sample :: Maybe Value
    , type_ :: Maybe Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON GraderResult where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON GraderResult where
    toJSON = genericToJSON aesonOptions

-- | An output item from an evaluation run
data OutputItemObject = OutputItemObject
    { id :: OutputItemID
    , object :: Text
    , created_at :: POSIXTime
    , datasource_item :: Value
    , datasource_item_id :: Natural
    , eval_id :: Text
    , results :: Vector GraderResult
    , run_id :: Text
    , sample :: Sample
    , status :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API for @\/v1\/evals\/{eval_id}\/runs\/{run_id}\/output_items@
type API =
        "evals"
    :>  Capture "eval_id" EvalID
    :>  "runs"
    :>  Capture "run_id" EvalRunID
    :>  "output_items"
    :>  (         QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Text
              :>  QueryParam "status" OutputItemStatus
              :>  Get '[JSON] (ListOf OutputItemObject)
        :<|>      Capture "output_item_id" OutputItemID
              :>  Get '[JSON] OutputItemObject
        )
