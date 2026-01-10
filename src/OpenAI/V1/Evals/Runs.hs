-- | @\/v1\/evals\/{eval_id}\/runs@
--
-- Evaluation runs execute an eval against a model with specific data.
-- Each run produces output items containing grader results.
module OpenAI.V1.Evals.Runs
    ( -- * Main types
      EvalRunID(..)
    , CreateEvalRun(..)
    , _CreateEvalRun
    , EvalRunObject(..)
    , EvalRunDeleteResponse(..)
      -- * Data source types
    , RunDataSource(..)
    , JSONLSource(..)
    , ResponsesSource(..)
    , CompletionsSource(..)
    , SamplingParams(..)
    , InputMessages(..)
    , TemplateMessage(..)
      -- * Result types
    , ResultCounts(..)
    , PerModelUsage(..)
    , PerTestingCriteriaResult(..)
      -- * Enums
    , RunStatus(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Error (Error)
import OpenAI.V1.Evals (EvalID)
import OpenAI.V1.ListOf (ListOf)

-- | Eval Run ID
newtype EvalRunID = EvalRunID{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Status of an evaluation run
data RunStatus
    = Queued
    | In_Progress
    | Completed
    | Canceled
    | Failed
    deriving stock (Eq, Generic, Show)

instance FromJSON RunStatus where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON RunStatus where
    toJSON = genericToJSON aesonOptions

instance ToHttpApiData RunStatus where
    toUrlPiece Queued = "queued"
    toUrlPiece In_Progress = "in_progress"
    toUrlPiece Completed = "completed"
    toUrlPiece Canceled = "canceled"
    toUrlPiece Failed = "failed"

-- | Counters summarizing the outcomes of the evaluation run
data ResultCounts = ResultCounts
    { errored :: Natural
    , failed :: Natural
    , passed :: Natural
    , total :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Usage statistics for each model during the evaluation run
data PerModelUsage = PerModelUsage
    { cached_tokens :: Natural
    , completion_tokens :: Natural
    , invocation_count :: Natural
    , model_name :: Text
    , prompt_tokens :: Natural
    , total_tokens :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Results breakdown per testing criteria
data PerTestingCriteriaResult = PerTestingCriteriaResult
    { failed :: Natural
    , passed :: Natural
    , testing_criteria :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A template message for input_messages configuration
data TemplateMessage = TemplateMessage
    { content :: Text
    , role :: Text
    , type_ :: Maybe Text
      -- ^ The type of message, always "message" when present
    } deriving stock (Eq, Generic, Show)

instance FromJSON TemplateMessage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON TemplateMessage where
    toJSON = genericToJSON aesonOptions

-- | Input messages configuration: either a template or a reference
data InputMessages
    = InputMessages_Template
        { template :: Vector TemplateMessage
        }
    | InputMessages_ItemReference
        { item_reference :: Text
        }
    deriving stock (Eq, Generic, Show)

inputMessagesOptions :: Options
inputMessagesOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "InputMessages_" camelToSnake
    }

instance FromJSON InputMessages where
    parseJSON = genericParseJSON inputMessagesOptions

instance ToJSON InputMessages where
    toJSON = genericToJSON inputMessagesOptions

-- | Sampling parameters for model generation
--
-- Note: The Evals API uses @max_completions_tokens@ (plural), unlike the rest
-- of the OpenAI API which uses @max_completion_tokens@ (singular).
data SamplingParams = SamplingParams
    { max_completions_tokens :: Maybe Natural
    , seed :: Maybe Natural
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , reasoning_effort :: Maybe Text
      -- ^ Reasoning effort level for o-series models (e.g., "low", "medium", "high")
    , response_format :: Maybe Value
      -- ^ Response format configuration (JSON schema, JSON object, or text)
    , tools :: Maybe (Vector Value)
      -- ^ Tools available for the model to call
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Source for JSONL data
data JSONLSource
    = JSONLSource_FileContent
        { file_content_content :: Vector Value
        }
    | JSONLSource_FileID
        { file_content_id :: Text
        }
    deriving stock (Eq, Generic, Show)

jsonlSourceOptions :: Options
jsonlSourceOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "JSONLSource_" camelToSnake
    , fieldLabelModifier = stripPrefix "file_content_"
    }

instance FromJSON JSONLSource where
    parseJSON = genericParseJSON jsonlSourceOptions

instance ToJSON JSONLSource where
    toJSON = genericToJSON jsonlSourceOptions

-- | Source for responses data
data ResponsesSource
    = ResponsesSource_FileContent
        { responses_content :: Vector Value
        }
    | ResponsesSource_FileID
        { responses_id :: Text
        }
    | ResponsesSource_Responses
        { responses_created_after :: Maybe POSIXTime
        , responses_created_before :: Maybe POSIXTime
        , responses_instructions_search :: Maybe Text
        , responses_metadata :: Maybe (Map Text Text)
        , responses_model :: Maybe Text
        , responses_reasoning_effort :: Maybe Text
        , responses_temperature :: Maybe Double
        , responses_tools :: Maybe (Vector Text)
        , responses_top_p :: Maybe Double
        , responses_users :: Maybe (Vector Text)
        }
    deriving stock (Eq, Generic, Show)

responsesSourceOptions :: Options
responsesSourceOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "ResponsesSource_" camelToSnake
    , fieldLabelModifier = stripPrefix "responses_"
    }

instance FromJSON ResponsesSource where
    parseJSON = genericParseJSON responsesSourceOptions

instance ToJSON ResponsesSource where
    toJSON = genericToJSON responsesSourceOptions

-- | Source for completions data
data CompletionsSource
    = CompletionsSource_FileContent
        { completions_content :: Vector Value
        }
    | CompletionsSource_FileID
        { completions_id :: Text
        }
    | CompletionsSource_StoredCompletions
        { completions_created_after :: Maybe POSIXTime
        , completions_created_before :: Maybe POSIXTime
        , completions_limit :: Maybe Natural
        , completions_metadata :: Maybe (Map Text Text)
        , completions_model :: Maybe Text
        }
    deriving stock (Eq, Generic, Show)

completionsSourceOptions :: Options
completionsSourceOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "CompletionsSource_" camelToSnake
    , fieldLabelModifier = stripPrefix "completions_"
    }

instance FromJSON CompletionsSource where
    parseJSON = genericParseJSON completionsSourceOptions

instance ToJSON CompletionsSource where
    toJSON = genericToJSON completionsSourceOptions

-- | Data source for an evaluation run
data RunDataSource
    = RunDataSource_JSONL
        { jsonl_source :: JSONLSource
        }
    | RunDataSource_Completions
        { completions_source :: CompletionsSource
        , completions_input_messages :: Maybe InputMessages
        , completions_model :: Maybe Text
        , completions_sampling_params :: Maybe SamplingParams
        }
    | RunDataSource_Responses
        { responses_source :: ResponsesSource
        , responses_input_messages :: Maybe InputMessages
        , responses_model :: Maybe Text
        , responses_sampling_params :: Maybe SamplingParams
        }
    deriving stock (Eq, Generic, Show)

runDataSourceOptions :: Options
runDataSourceOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "RunDataSource_" camelToSnake
    , fieldLabelModifier = stripAnyPrefix
        [ "jsonl_"
        , "completions_"
        , "responses_"
        ]
    }

instance FromJSON RunDataSource where
    parseJSON = genericParseJSON runDataSourceOptions

instance ToJSON RunDataSource where
    toJSON = genericToJSON runDataSourceOptions

-- | Request body for creating an evaluation run
data CreateEvalRun = CreateEvalRun
    { data_source :: RunDataSource
    , metadata :: Maybe (Map Text Text)
    , name :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateEvalRun`
_CreateEvalRun :: CreateEvalRun
_CreateEvalRun = CreateEvalRun
    { metadata = Nothing
    , name = Nothing
    }

-- | Response for deleting an evaluation run
data EvalRunDeleteResponse = EvalRunDeleteResponse
    { deleted :: Bool
    , object :: Text
    , run_id :: EvalRunID
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An evaluation run object
data EvalRunObject = EvalRunObject
    { id :: EvalRunID
    , object :: Text
    , created_at :: POSIXTime
    , data_source :: RunDataSource
    , error :: Maybe Error
    , eval_id :: Text
    , metadata :: Maybe (Map Text Text)
    , model :: Maybe Text
    , name :: Text
    , per_model_usage :: Maybe (Vector PerModelUsage)
    , per_testing_criteria_results :: Maybe (Vector PerTestingCriteriaResult)
    , report_url :: Maybe Text
    , result_counts :: Maybe ResultCounts
    , status :: RunStatus
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API for @\/v1\/evals\/{eval_id}\/runs@
--
-- Note: The create endpoint returns 201 Created, not 200 OK.
type API =
        "evals"
    :>  Capture "eval_id" EvalID
    :>  "runs"
    :>  (         ReqBody '[JSON] CreateEvalRun
              :>  Verb 'POST 201 '[JSON] EvalRunObject
        :<|>      QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Text
              :>  QueryParam "status" RunStatus
              :>  Get '[JSON] (ListOf EvalRunObject)
        :<|>      Capture "run_id" EvalRunID
              :>  Get '[JSON] EvalRunObject
        :<|>      Capture "run_id" EvalRunID
              :>  Delete '[JSON] EvalRunDeleteResponse
        :<|>      Capture "run_id" EvalRunID
              :>  "cancel"
              :>  Post '[JSON] EvalRunObject
        )
