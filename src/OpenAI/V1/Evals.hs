-- | @\/v1\/evals@
--
-- The Evals API enables programmatic evaluation of LLM outputs. An evaluation
-- defines testing criteria (graders) and a data source schema. Runs execute
-- the eval against a model to produce output items with grader results.
module OpenAI.V1.Evals
    ( -- * Main types
      EvalID(..)
    , CreateEval(..)
    , _CreateEval
    , ModifyEval(..)
    , _ModifyEval
    , EvalObject(..)
    , EvalDeleteResponse(..)
      -- * Data source config types (request)
    , CreateDataSourceConfig(..)
      -- * Data source config types (response)
    , DataSourceConfig(..)
      -- * Testing criteria (graders)
    , TestingCriterion(..)
    , GraderInput(..)
    , StringCheckOperation(..)
    , TextSimilarityMetric(..)
    , GraderSamplingParams(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.ListOf (ListOf)

-- | Eval ID
newtype EvalID = EvalID{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Operation for string check grader
data StringCheckOperation
    = Eq
    | Ne
    | Like
    | Ilike
    deriving stock (Eq, Generic, Show)

instance FromJSON StringCheckOperation where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON StringCheckOperation where
    toJSON = genericToJSON aesonOptions

-- | Evaluation metric for text similarity grader
data TextSimilarityMetric
    = Cosine
    | Fuzzy_Match
    | Bleu
    | Gleu
    | Meteor
    | Rouge_1
    | Rouge_2
    | Rouge_3
    | Rouge_4
    | Rouge_5
    | Rouge_L
    deriving stock (Eq, Generic, Show)

instance FromJSON TextSimilarityMetric where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON TextSimilarityMetric where
    toJSON = genericToJSON aesonOptions

-- | Sampling parameters for model graders (label_model, score_model)
data GraderSamplingParams = GraderSamplingParams
    { max_completions_tokens :: Maybe Natural
    , reasoning_effort :: Maybe Text
    , seed :: Maybe Natural
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Input for model graders (label_model, score_model)
--
-- Each element represents a message with role and content, where content
-- can reference variables using @{{item.field}}@ or @{{sample.output_text}}@
-- notation.
--
-- Note: This is a simplified representation of the API's @EvalItem@ type.
-- The full API supports complex content types (input_text, output_text,
-- input_image, input_audio) as either single items or arrays. We use 'Value'
-- for the @content@ field to handle all forms without the complexity of
-- modeling the full sum type, since most use cases involve simple text
-- templates.
data GraderInput = GraderInput
    { role :: Text
    , content :: Value
      -- ^ Either a simple string or a structured object (e.g., @{"type": "input_text", "text": "..."}@)
    , type_ :: Text
      -- ^ The type of message, e.g., "message"
    } deriving stock (Eq, Generic, Show)

instance FromJSON GraderInput where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON GraderInput where
    toJSON = genericToJSON aesonOptions

-- | Testing criteria define how to grade model outputs.
--
-- The grader types available are:
--
-- * @string_check@: Exact string matching with operations like eq, ne, like
-- * @label_model@: Uses an LLM to classify output into labels
-- * @score_model@: Uses an LLM to assign a numeric score
-- * @text_similarity@: Compares text similarity using metrics like cosine
-- * @python@: Executes custom Python code to grade output
-- * @multi@: Combines multiple graders to produce a single score
--
-- Note: The @id@, @grdr_id@, and @inactive_at@ fields are output-only
-- (returned by the API but not needed when creating).
data TestingCriterion
    = Grader_StringCheck
        { string_check_id :: Maybe Text
        , string_check_grdr_id :: Maybe Text
        , string_check_inactive_at :: Maybe POSIXTime
        , string_check_name :: Text
        , string_check_input :: Text
        , string_check_reference :: Text
        , string_check_operation :: StringCheckOperation
        }
    | Grader_LabelModel
        { label_model_id :: Maybe Text
        , label_model_grdr_id :: Maybe Text
        , label_model_inactive_at :: Maybe POSIXTime
        , label_model_name :: Text
        , label_model_model :: Text
        , label_model_input :: Vector GraderInput
        , label_model_labels :: Vector Text
        , label_model_passing_labels :: Vector Text
        }
    | Grader_ScoreModel
        { score_model_id :: Maybe Text
        , score_model_grdr_id :: Maybe Text
        , score_model_inactive_at :: Maybe POSIXTime
        , score_model_name :: Text
        , score_model_model :: Text
        , score_model_input :: Vector GraderInput
        , score_model_range :: Maybe (Vector Double)
        , score_model_pass_threshold :: Maybe Double
        , score_model_sampling_params :: Maybe GraderSamplingParams
        }
    | Grader_TextSimilarity
        { text_similarity_id :: Maybe Text
        , text_similarity_grdr_id :: Maybe Text
        , text_similarity_inactive_at :: Maybe POSIXTime
        , text_similarity_name :: Text
        , text_similarity_input :: Text
        , text_similarity_reference :: Text
        , text_similarity_pass_threshold :: Maybe Double
        , text_similarity_evaluation_metric :: TextSimilarityMetric
        }
    | Grader_Python
        { python_id :: Maybe Text
        , python_grdr_id :: Maybe Text
        , python_inactive_at :: Maybe POSIXTime
        , python_name :: Text
        , python_source :: Text
        , python_pass_threshold :: Maybe Double
        , python_image_tag :: Maybe Text
        }
    | Grader_Multi
        { multi_id :: Maybe Text
        , multi_grdr_id :: Maybe Text
        , multi_inactive_at :: Maybe POSIXTime
        , multi_name :: Text
        , multi_graders :: Vector Value
          -- ^ Nested graders - using Value since they can be any grader type
        , multi_calculate_output :: Text
          -- ^ Formula to calculate the output based on grader results
        }
    deriving stock (Eq, Generic, Show)

testingCriterionOptions :: Options
testingCriterionOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "Grader_" camelToSnake
    , fieldLabelModifier = stripAnyPrefix
        [ "string_check_"
        , "label_model_"
        , "score_model_"
        , "text_similarity_"
        , "python_"
        , "multi_"
        ]
    }

instance FromJSON TestingCriterion where
    parseJSON = genericParseJSON testingCriterionOptions

instance ToJSON TestingCriterion where
    toJSON = genericToJSON testingCriterionOptions

-- | Configuration for the data source when creating an evaluation.
--
-- The @item_schema@ defines what variables are available throughout the eval
-- via the @{{item.field}}@ notation. Setting @include_sample_schema@ to True
-- makes @{{sample.output_text}}@ available.
data CreateDataSourceConfig
    = CreateDataSourceConfig_Custom
        { create_custom_item_schema :: Value
          -- ^ JSON schema for each row in the data source (required)
        , create_custom_include_sample_schema :: Maybe Bool
          -- ^ Whether the eval should expect you to populate the sample namespace
        }
    | CreateDataSourceConfig_Logs
        { create_logs_metadata :: Maybe (Map Text Text)
          -- ^ Metadata filters for the logs data source
        }
    | CreateDataSourceConfig_StoredCompletions
        { create_stored_completions_metadata :: Maybe (Map Text Text)
          -- ^ Metadata filters for the stored completions data source
        }
    deriving stock (Eq, Generic, Show)

createDataSourceConfigOptions :: Options
createDataSourceConfigOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "CreateDataSourceConfig_" camelToSnake
    , fieldLabelModifier = stripAnyPrefix
        [ "create_custom_"
        , "create_logs_"
        , "create_stored_completions_"
        ]
    }

instance FromJSON CreateDataSourceConfig where
    parseJSON = genericParseJSON createDataSourceConfigOptions

instance ToJSON CreateDataSourceConfig where
    toJSON = genericToJSON createDataSourceConfigOptions

-- | Configuration for the data source returned in evaluation responses.
--
-- The @schema@ contains the full JSON schema for the data source items,
-- including both @item@ and @sample@ namespaces.
data DataSourceConfig
    = DataSourceConfig_Custom
        { custom_schema :: Value
          -- ^ Full JSON schema for the run data source items
        }
    | DataSourceConfig_Logs
        { logs_schema :: Value
          -- ^ Full JSON schema for the run data source items
        , logs_metadata :: Maybe (Map Text Text)
          -- ^ Metadata filters for the logs data source
        }
    | DataSourceConfig_StoredCompletions
        { stored_completions_schema :: Value
          -- ^ Full JSON schema for the run data source items
        , stored_completions_metadata :: Maybe (Map Text Text)
          -- ^ Metadata filters for the stored completions data source
        }
    deriving stock (Eq, Generic, Show)

dataSourceConfigOptions :: Options
dataSourceConfigOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefixThen "DataSourceConfig_" camelToSnake
    , fieldLabelModifier = stripAnyPrefix
        [ "custom_"
        , "logs_"
        , "stored_completions_"
        ]
    }

instance FromJSON DataSourceConfig where
    parseJSON = genericParseJSON dataSourceConfigOptions

instance ToJSON DataSourceConfig where
    toJSON = genericToJSON dataSourceConfigOptions

-- | Request body for creating an evaluation
data CreateEval = CreateEval
    { data_source_config :: CreateDataSourceConfig
    , testing_criteria :: Vector TestingCriterion
    , metadata :: Maybe (Map Text Text)
    , name :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateEval`
_CreateEval :: CreateEval
_CreateEval = CreateEval
    { metadata = Nothing
    , name = Nothing
    }

-- | Request body for modifying an evaluation
data ModifyEval = ModifyEval
    { metadata :: Maybe (Map Text Text)
    , name :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `ModifyEval`
_ModifyEval :: ModifyEval
_ModifyEval = ModifyEval
    { metadata = Nothing
    , name = Nothing
    }

-- | Response for deleting an evaluation
data EvalDeleteResponse = EvalDeleteResponse
    { deleted :: Bool
    , eval_id :: EvalID
    , object :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | An evaluation object
data EvalObject = EvalObject
    { id :: EvalID
    , object :: Text
    , created_at :: POSIXTime
    , data_source_config :: DataSourceConfig
    , metadata :: Maybe (Map Text Text)
      -- ^ Can be null per the Metadata schema
    , name :: Text
    , testing_criteria :: Vector TestingCriterion
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API for @\/v1\/evals@
--
-- Note: The create endpoint returns 201 Created, not 200 OK.
type API =
        "evals"
    :>  (         ReqBody '[JSON] CreateEval
              :>  Verb 'POST 201 '[JSON] EvalObject
        :<|>      QueryParam "after" Text
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Text
              :>  QueryParam "order_by" Text
              :>  Get '[JSON] (ListOf EvalObject)
        :<|>      Capture "eval_id" EvalID
              :>  Get '[JSON] EvalObject
        :<|>      Capture "eval_id" EvalID
              :>  ReqBody '[JSON] ModifyEval
              :>  Post '[JSON] EvalObject
        :<|>      Capture "eval_id" EvalID
              :>  Delete '[JSON] EvalDeleteResponse
        )
