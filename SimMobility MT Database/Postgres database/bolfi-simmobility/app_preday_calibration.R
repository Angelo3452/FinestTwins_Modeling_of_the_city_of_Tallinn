shared.env <<- new.env()
BOLFI_HOME <- Sys.getenv("BOLFI_HOME", unset = NA)
if(is.na(BOLFI_HOME)) BOLFI_HOME <- getwd()

DEFAULT_CONFIG_FILENAME <- paste( c(BOLFI_HOME,"config/defaults.yml"), collapse = .Platform$file.sep ) #defaults.yml
DEFAULT_LOGGING_FILENAME <- paste( c(BOLFI_HOME,"config/logger.yml"), collapse = .Platform$file.sep ) 
UPDATE_DEF <- F

suppressMessages(library(tidyverse))
suppressMessages(library(reshape2))
suppressMessages(library(purrr))
suppressMessages(library(optparse))
suppressMessages(library(yaml))
suppressMessages(library(log4r))
suppressMessages(library(R.utils))

### ----------------- Utility functions -------------------------- ###
load_library <- function(library, library_path){
  if( ! file.exists(paste( c(library_path,"__init__.R"), collapse = .Platform$file.sep )) ){
    return(
      list( code = 1, message = paste0("The library ",library," was not properly setup. No __init__.R file found.Thus, it is not loaded.") )
    )
  }
  source( paste( c(library_path,"__init__.R"), collapse = .Platform$file.sep ) )
  
  if( ! file.exists(paste( c(library_path,"config/defaults.yml"), collapse = .Platform$file.sep )) ){
    return( list( code = 1, message = paste0("The library ",library," is initialized without default settings.")  ) )
  } 
  return( 
    list( 
      data = modifyList( list(), read_yaml( paste( c(library_path,"config/defaults.yml"), collapse = .Platform$file.sep )  ) ), 
      code = 0
    )
  )
}

load_logger <- function(config_path, run_id, level = "INFO"){
  tmp_logger <- read_yaml(config_path)
  if( !dir.exists( tmp_logger$LOG_PATH ) ){
    dir.create( tmp_logger$LOG_PATH, recursive = TRUE )
  }
  tmp_logger$FILE = paste0(tmp_logger$LOG_PATH,"/log_",run_id,".txt")
  tmp_logger$APPENDERS <- list(
    console_appender(layout = default_log_layout()),
    file_appender(tmp_logger$FILE, append = TRUE, 
                  layout = default_log_layout())
  )
  tmp_logger$LOGGER <- log4r::logger(threshold = level, 
                                     appenders= tmp_logger$APPENDERS)
  
  return( tmp_logger )
}

option_list <- list(
  make_option(c("-f", "--fingerprint"), type="character", default=NA, 
              help="Run fingerprint", metavar="character"),
  make_option(c("-m", "--surrogate_model"), type="character", default=NA, 
              help="Surrogate model", metavar="character"),
  make_option(c("-u", "--uitility_function"), type="character", default=NA, 
              help="Utility function", metavar="character"),
  make_option(c("-s", "--param_change_share"), type="numeric", default=NA, 
              help="Share of parameters to be changed at each iteration", metavar="character"),
  make_option(c("-v", "--param_change_variation"), type="numeric", default=NA, 
              help="Variation of parameter change at each iteration", metavar="character"),
  make_option(c("-C", "--config_file"), type="character", default="config/preday/config_local.yml", 
              help="Config file to load", metavar="character"),
  make_option(c("-P", "--param_definition_file"), type="character", default=NA,#"BO_CALIB/preday_params.csv", 
              help="Parameters file to load", metavar="character"),
  make_option(c("-E", "--param_exclusion_file"), type="character", default=NA,#"BO_CALIB/preday_params_omitted_0_local_declaration.csv", 
              help="File with exluded parameters to load", metavar="character"),
  make_option(c("-A", "--city_actual_stats"), type="character", default=NA, 
              help="File with true city statistics", metavar="character"),
  make_option(c("-O", "--object_storage_path"), type="character", default=NA, 
              help="Path where output objects will be stored", metavar="character"),
  make_option(c("-X", "--sh_scripts_path"), type="character", default=NA, 
              help="Path of shell scripts for executing commands over the simulator", metavar="character"),
  make_option(c("-H", "--container_home"), type="character", default=NA, 
              help="Home path of container runtime and configuration", metavar="character"),
  make_option(c("-c", "--containerized"), type="logical", default=NA, 
              help="Flag for executing simulation in container (Docker)", metavar="character"),
  make_option(c("-i", "--container_image"), type="character", default=NA, 
              help="Container image name", metavar="character"),
  make_option(c("-b", "--initialization_only"), type="logical", default=F, 
              help="Indicator to indicate only initialization of random seed and initial sample set", metavar="character")
  
); 
### =============================================================== ###

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

## Setup running and configuration fingerprint settings
settings <<- list()
settings$SCOPE <- "preday"

if(is.na(opt$fingerprint)){
  settings$FINGERPRINT <- round(as.numeric(Sys.time())*1000, digits=0)  
} else {
  settings$FINGERPRINT <- opt$fingerprint
}


## Load logger
logger <- load_logger(DEFAULT_LOGGING_FILENAME, settings$FINGERPRINT, level = "INFO")
shared.env$logger <- logger

## Load default configuration
settings$DEFAULTS <- read_yaml(DEFAULT_CONFIG_FILENAME)
settings <- modifyList( settings, settings$DEFAULTS )

if(hasName(settings, "PRELOADED_LIBRARIES") && !is.na(settings$PRELOADED_LIBRARIES)){
  for(i in 1:length(settings$PRELOADED_LIBRARIES)){
    lib_config <- load_library( settings$PRELOADED_LIBRARIES[i], paste( c(settings$LIB_PATH,settings$PRELOADED_LIBRARIES[i]), collapse = .Platform$file.sep ) )
    
    if( "data" %in% names(lib_config) ){
      settings <- modifyList( 
        settings, 
        lib_config$data
      )
    } else {
      if( lib_config$code == 1 && ("message" %in% names(lib_config$message)) ) log4r::warn(logger$LOGGER, lib_config$message)
      if( lib_config$code == 2 && ("message" %in% names(lib_config$message)) ) log4r::error(logger$LOGGER, lib_config$message)
    }
  }
}

## Load provided config file (if provided)
CONFIG_FILE <- opt$config_file
if(!is.na(CONFIG_FILE)){
  settings$PROVIDED_CONFIGURATION <- read_yaml( CONFIG_FILE )
  
  if(!is.na(settings$PROVIDED_CONFIGURATION$LIBRARIES) && length(settings$PROVIDED_CONFIGURATION$LIBRARIES) > 0 ){
    for(i in 1:length(settings$PROVIDED_CONFIGURATION$LIBRARIES)){
      lib_config <- load_library( settings$PROVIDED_CONFIGURATION$LIBRARIES[i], paste( c(BOLFI_HOME,settings$LIB_PATH,settings$PROVIDED_CONFIGURATION$LIBRARIES[i]), collapse = .Platform$file.sep ) )
      
      if( "data" %in% names(lib_config) ){
        settings <- modifyList( 
          settings, 
          lib_config$data
        )
      } else {
        if( lib_config$code == 1 && ("message" %in% names(lib_config$message)) ) log4r::warn(logger$LOGGER, lib_config$message)
        if( lib_config$code == 2 && ("message" %in% names(lib_config$message)) ) log4r::error(logger$LOGGER, lib_config$message)
      }
    }
  }
  settings <- modifyList( settings, settings$PROVIDED_CONFIGURATION )
}

## Load parameters provided inline
if(!is.na(opt$surrogate_model)) settings$SURROGATE_MODEL_STR <- opt$surrogate_model
if(!is.na(opt$uitility_function)) settings$UTILITY_FUNC_STR <- opt$uitility_function
if(!is.na(opt$object_storage_path)) settings$OBJECT_STORAGE_PATH <- opt$object_storage_path
if(!is.na(opt$param_definition_file)) settings$INITIAL_PARAMS_PATH <- opt$param_definition_file  #"BO_CALIB/preday_params.csv"
if(!is.na(opt$param_exclusion_file)) settings$OMITTED_PARAMS_PATH <- opt$param_exclusion_file  #"BO_CALIB/preday_params_omitted_0.csv"
if(!is.na(opt$city_actual_stats)) settings$OBSERVED_STATS_PATH <- opt$city_actual_stats #"BO_CALIB/individual_10_stats.csv"
if(!is.na(opt$container_home)) settings$CONTAINER_HOME_PATH <- opt$container_home
if(!is.na(opt$sh_scripts_path)) settings$SH_SCRIPTS <- opt$sh_scripts_path
if(!is.na(opt$param_change_share)) settings$CHANGE_SHARE_ATTRIBUTES <- opt$param_change_share
if(!is.na(opt$param_change_variation)) settings$VARIATION_SD <- opt$param_change_variation
if(!is.na(opt$containerized)) settings$CONTAINERIZED <- opt$containerized
if(!is.na(opt$container_image)) settings$IMAGE_NAME <- opt$container_image
if(!is.na(opt$initialization_only)) settings$INIT_ONLY <- opt$initialization_only

## Confugure the rest of necessary parameters/settings
settings$PARAM_FILE_PATH <- paste(c("scripts","lua","mid","behavior_vc"),collapse = .Platform$file.sep)
settings$SH_UPDATE_PARAM <- paste(c(settings$SH_SCRIPTS,"update_param.sh"),collapse = .Platform$file.sep) ## remove "local_"
settings$SH_EXEC_SIMULATION <- paste(c(settings$SH_SCRIPTS,"exec_simulation.sh"),collapse = .Platform$file.sep)
settings$SH_PUSH_PARAM <- paste(c(settings$SH_SCRIPTS,"push_param.sh"),collapse = .Platform$file.sep)
settings$SH_SETUP_WORKER <- paste(c(settings$SH_SCRIPTS,"create_worker.sh"),collapse = .Platform$file.sep)
settings$SH_REMOVE_WORKER <- paste(c(settings$SH_SCRIPTS,"remove_worker.sh"),collapse = .Platform$file.sep)
settings$CONTAINER_RUNTIME_PATH <- paste(c(settings$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep )
settings$OBJECT_PATH <- getAbsolutePath(paste(c(".",settings$OBJECT_STORAGE_PATH,paste0("output_",settings$FINGERPRINT) ), collapse = .Platform$file.sep))
settings$RANDOM_SEED_FILE <- paste(settings$OBJECT_PATH, paste0("bolfi_",settings$SCOPE,"_random_seed_object.Rds"), sep = .Platform$file.sep )
settings$PARAM_SPACE_FILE <- paste(settings$OBJECT_PATH, paste0("bolfi_",settings$SCOPE,"_param_space_object.Rds"), sep = .Platform$file.sep )
settings$OUTPUT_FILE <- paste(settings$OBJECT_PATH, paste0("bolfi_",settings$SCOPE,"_output_object.Rds"), sep = .Platform$file.sep )
settings$CONFIGURATION_OUTPUT_FILE <- paste(settings$OBJECT_PATH, paste0("bolfi_",settings$SCOPE,"_configuration_object.Rds"), sep = .Platform$file.sep )

## Adapt the local file system and necessary file loadings
if(!dir.exists(settings$OBJECT_PATH)){
  dir.create(settings$OBJECT_PATH)
}
if(!dir.exists(settings$CONTAINER_RUNTIME_PATH)){
  dir.create(settings$CONTAINER_RUNTIME_PATH)
}
# settings$CITY_STATS <- read_csv(settings$OBSERVED_STATS_PATH) %>% 
#   filter(! variable %in% c("pop_ratio")) %>% 
#   unite("stats",scope,type,variable,sep="_") %>% 
#   arrange(stats)

settings$CITY_STATS <- list()
settings$CITY_STATS$value <- as.matrix( read.csv(settings$OBSERVED_STATS_PATH, row.names = 1, encoding="UTF-8") ) * ifelse(hasName(settings,"PARTIAL_POPULATION"),settings$PARTIAL_POPULATION,1)
## ---------- add small amount to 0's as we are dividing with them later -------
# settings$CITY_STATS$value <- settings$CITY_STATS$value  + 0.00001
##------------------------------------------------------------------------------
settings$CITY_STATS$total_trips <- sum(settings$CITY_STATS$value, na.rm = T)
if(hasName(settings,"RELATIVE_OD") && settings$RELATIVE_OD) settings$CITY_STATS$value <- settings$CITY_STATS$value/sum(settings$CITY_STATS$value)
settings$CITY_STATS$district_map <- read_csv(settings$DISTRICT_MAP_PATH, locale = locale(encoding = "UTF-8"))
if(hasName(settings, "WEIGHT_MAP_PATH")) settings$CITY_STATS$weights <- as.matrix( read.csv(settings$WEIGHT_MAP_PATH, row.names = 1, encoding="UTF-8") )
if(hasName(settings, "MODE_BALANCE_PATH")) settings$CITY_STATS$balance <- read_csv(settings$MODE_BALANCE_PATH, locale = locale(encoding = "UTF-8"))
if(hasName(settings, "WORKERS_POPULATION_PATH")) settings$CITY_STATS$workers <- read_csv(settings$WORKERS_POPULATION_PATH, locale = locale(encoding = "UTF-8"))
OD_cells <- rownames(settings$CITY_STATS$value)
settings$CITY_STATS$emptyOD <- matrix(rep(0, length(OD_cells)^2 ), nrow = length(OD_cells))
rownames(settings$CITY_STATS$emptyOD) <- OD_cells
colnames(settings$CITY_STATS$emptyOD) <- OD_cells

## If omit file provided, load it
settings$OMIT_LIST <- c()
if( ("OMITTED_PARAMS_PATH" %in% names(settings)) && !is.na(settings$OMITTED_PARAMS_PATH)){
  settings$OMIT_LIST <- read_csv(settings$OMITTED_PARAMS_PATH)$parameter
}

## If external method is used, adapt the directories for input and output to be the same as the current run home directory
if(hasName(settings,"EXTERNAL_METHOD")){
  settings$METHOD_INPUT_PATH <- paste(getAbsolutePath(settings$OBJECT_PATH),"/",sep=.Platform$file.sep)
  settings$METHOD_OUTPUT_PATH <- paste(getAbsolutePath(settings$OBJECT_PATH),"/",sep=.Platform$file.sep) #getAbsolutePath(paste(c(".",settings$OBJECT_STORAGE_PATH,"output"),collapse = .Platform$file.sep))
  
  settings$CONTAINER_HOME_PATH <- getAbsolutePath(settings$CONTAINER_HOME_PATH)
  settings$CONTAINER_RUNTIME_PATH <- getAbsolutePath(settings$CONTAINER_RUNTIME_PATH)
  
  if(hasName(settings,"EXTERNAL_FEEDBACK_SCRIPT")) file.copy(settings$EXTERNAL_FEEDBACK_SCRIPT, settings$OBJECT_PATH)
}

## Copy and load existing files, if they are provided and they are required (READ_MEMORY)
if(settings$READ_MEMORY && 
   file.exists( paste(".",settings$OBJECT_STORAGE_PATH, paste0("bolfi_",settings$SCOPE,"_param_space_object.Rds"), sep = .Platform$file.sep )  ) &&
   !file.exists( settings$PARAM_SPACE_FILE  ) ){
  file.copy( paste(".",settings$OBJECT_STORAGE_PATH, paste0("bolfi_",settings$SCOPE,"_param_space_object.Rds"), sep = .Platform$file.sep ), settings$PARAM_SPACE_FILE )
}
if(settings$READ_MEMORY && 
   file.exists( paste(".",settings$OBJECT_STORAGE_PATH, paste0("bolfi_",settings$SCOPE,"_random_seed_object.Rds"), sep = .Platform$file.sep )  ) && 
   !file.exists( settings$RANDOM_SEED_FILE  ) ){
  file.copy( paste(".",settings$OBJECT_STORAGE_PATH, paste0("bolfi_",settings$SCOPE,"_random_seed_object.Rds"), sep = .Platform$file.sep ), settings$RANDOM_SEED_FILE )
}

settings$SURROGATE_MODEL_FN <- paste0("fn_fit_",settings$SURROGATE_MODEL_STR)
settings$SURROGATE_PREDICT_FN <- paste0("fn_predict_",settings$SURROGATE_MODEL_STR)
settings$SURROGATE_UTILITY_FN <- paste0("fn_utility_",settings$UTILITY_FUNC_STR)
settings$HEADER <- names(settings)

verbose_output <- function(msg, ...){
  message_params <- list(...)
  message_params_str <- paste( lapply(names(message_params), function(x){ paste(x,message_params[x], sep=": ") } ), collapse = ", " )
  log4r::info(shared.env$logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: ",msg, " |=> ", message_params_str ))
}
save_object <- function(obj,filename = "space", ...) {
  object_params <- list(...)
  object_params_str <- paste( lapply(names(object_params), function(x){ paste(x,object_params[x], sep="_") } ), collapse = "_" )
  obj %>% saveRDS( paste(settings$OBJECT_PATH, paste0(filename,"_",object_params_str,".Rds"), sep = .Platform$file.sep ) )
}
## ============================================================================

## --------------------------- Script -----------------------------------------
start <- Sys.time()

## Setting up random seed
if(settings$READ_MEMORY && file.exists( settings$RANDOM_SEED_FILE  ) ){
  settings$RANDOM.SEEDS <- readRDS( settings$RANDOM_SEED_FILE )
  
  verbose_output("Random seed configuration has been loaded from a file.")
} else {
  settings$RANDOM.SEEDS <- list()
  current_time <- as.numeric(Sys.time())
  settings$RANDOM.SEEDS$seed <- as.integer(current_time %% (floor(current_time/10000) * 10000)) 
  settings$RANDOM.SEEDS$set <- sample(-1000:1000,100, replace = F) + settings$RANDOM.SEEDS$seed
  
  settings$RANDOM.SEEDS %>% saveRDS( settings$RANDOM_SEED_FILE )
  
  verbose_output("Random seed configuration has been generated and saved into a file.")
}
set.seed(settings$RANDOM.SEEDS$seed)

## If containerized, setup the worker and adapt scripts
if(settings$CONTAINERIZED){
  # Setup worker
  fn_worker_setup(settings)
  
  # Modify shell script to be called
  settings$SH_UPDATE_PARAM <- paste(c(settings$CONTAINER_HOME_PATH,"runtime",settings$FINGERPRINT,"local_update_param.sh"),collapse = .Platform$file.sep)
  settings$SH_EXEC_SIMULATION <- paste(c(settings$CONTAINER_HOME_PATH,"runtime",settings$FINGERPRINT,"docker_exec_simulation.sh"),collapse = .Platform$file.sep)
  settings$SH_PUSH_PARAM <- paste(c(settings$CONTAINER_HOME_PATH,"runtime",settings$FINGERPRINT,"docker_push_param.sh"),collapse = .Platform$file.sep)
  if( ! file.exists(settings$SH_PUSH_PARAM) ) settings$SH_PUSH_PARAM <- NA
  
  # Modify activity file reference to workers home
  settings$ACTIVITY_FILE <- paste(c(settings$CONTAINER_HOME_PATH,"runtime",settings$FINGERPRINT,"activity_schedule"), collapse = .Platform$file.sep )
}

BOLFI_INPUT <- list()
# Use single file for many runs (if required)
if(settings$READ_MEMORY && file.exists( settings$PARAM_SPACE_FILE ) ){
  BOLFI_INPUT <- readRDS( settings$PARAM_SPACE_FILE )
  
  ## Update the definition
  if(UPDATE_DEF){
    BOLFI_INPUT$definition <- fn_load_params_space_definition( settings$INITIAL_PARAMS_PATH, omit = settings$OMIT_LIST )$definition  
  }
  
  verbose_output("Initial parameter space has been loaded from a file.")
} else {
  BOLFI_INPUT <- fn_load_params_space_definition( settings$INITIAL_PARAMS_PATH, omit = settings$OMIT_LIST )
  BOLFI_INPUT %>% saveRDS( settings$PARAM_SPACE_FILE )
  
  verbose_output("Initial parameter space has been generated and saved into a file.")
}
if(!hasName(settings,"PARAM_SPACE_DIM")) settings$PARAM_SPACE_DIM <- nrow(BOLFI_INPUT$definition)

## Target name
settings$target_col = "inadequacy"

## Specify epsilon
settings$EPSILON$tours$focused_sampling <- 0.001
settings$EPSILON$tours$spread_sampling <- 0.1
settings$EPSILON$mode$focused_sampling <- 0.05
settings$EPSILON$mode$spread_sampling <- 1.0
settings$EPSILON$od$focused_sampling <- 100 # not used atm
settings$EPSILON$od$spread_sampling <- 10000 # not used atm

## Save settings initially
saveRDS(settings, file=settings$CONFIGURATION_OUTPUT_FILE )

## Broadcast the shared environment
shared.env$settings <- settings
shared.env$param_space <- BOLFI_INPUT

BOLFI_OUTPUT <- #tryCatch({
  fn_bayes_optimisiation(
    param_init = BOLFI_INPUT$space,
    param_def = BOLFI_INPUT$definition,
    param_addons = BOLFI_INPUT[c("residual_pool")],
    simulation_fn = fn_perform_simulation,
    termination = "fn_termination_max_iterations", 
    max_iterations = settings$MAX_ITERS,
    verbose_output_fn = verbose_output,
    save_object_fn = save_object,
    target = settings$target_col, 
    surrogate_fit_fn = settings$SURROGATE_MODEL_FN,
    surrogate_predict_fn = settings$SURROGATE_PREDICT_FN,
    surrogate_utility_fn = settings$SURROGATE_UTILITY_FN,
    init_sampling_fn = settings$INIT_SAMPLING_FUNC_STR,
    sampling_fn = settings$SAMPLING_FUNC_STR, #"fn_optimal_acquisition", #"fn_acquisition_random_complete",#"fn_acquisition_v2_normal_around_optimal",
    inner_optimisation_fn = settings$INNER_OPTIMIZATION_FUNC_STR,
    selecton_fn = "fn_pull_optimal_selection",
    init_sample_size = settings$INIT_SAMPLE_SIZE, 
    sample_size = settings$ITER_SAMPLE_SIZE,
    potential_size = settings$POTENTIAL_SAMPLE_SIZE,
    selection_size = settings$OPTIMAL_SAMPLE_SIZE,
    objective_fn = min,
    randomized_selection = settings$SELECT_RANDOMIZATION,
    variation.rate = settings$CHANGE_SHARE_ATTRIBUTES,
    sampling_sd = function(mu) { return( abs(mu) * settings$VARIATION_SD );  },
    setting_obj = settings,
    method_params = settings$METHOD_PARAMS,
    enforce_bounds = T,
    initialize_only = settings$INIT_ONLY,
    opt_cores = settings$OPTIMISATION_CORES,
    cv_cores = settings$OPTIMISATION_CORES,
    opt_store_intermediate = T,
    is_normalization_required = (settings$SURROGATE_MODEL_FN %in% c("fn_fit_gp","fn_fit_mlegp")),
    eval_complement = F
  )
# }, 
# error = function(er) { 
#   throw(er)
#   NULL  
# })
  

## If containerized, remove the worker from the cluster
if(settings$CONTAINERIZED){
  fn_worker_remove(settings)
}
  
end <- Sys.time()
log4r::info(logger$LOGGER, paste0("Script elapsed time: ", (end - start) ))

settings$runtime <- list(
  start = as.numeric(start),
  end = as.numeric(end),
  ellapsed = as.numeric(difftime(end, start, units = "mins"))
)

if(!is.null(BOLFI_OUTPUT)) {
  saveRDS(BOLFI_OUTPUT, file=settings$OUTPUT_FILE )
}
saveRDS(settings, file=settings$CONFIGURATION_OUTPUT_FILE )

print("--------------------")
print((end - start))
print("--------------------")
## ============================================================================

