#! /usr/bin/Rscript
args = commandArgs(trailingOnly=TRUE)
if(length(args)<3) stop("Not enough arguments supplied. Two are required: 'home_dir', 'filepath' to the new parameter values (csv), 'filepath' to the output file (csv)")
start <- Sys.time()

suppressMessages(library(R.utils))
suppressMessages(library(readr))
suppressMessages(library(log4r))
source(getAbsolutePath("libs/preday/__init__.R"))

setwd(args[1]) ## Set working directory to the provided path
shared.env <<- new.env()

## Load settings object
settings <- readRDS(getAbsolutePath(paste(".",paste0("bolfi_","preday","_configuration_object.Rds"), sep = .Platform$file.sep )))
shared.env$settings <- settings

## Load logger
if( hasName(settings,"ENABLE_EXTERNAL_LOGGING") && settings$ENABLE_EXTERNAL_LOGGING ){
  load_local_logger <- function(log_path, run_id, level = "INFO"){
    if( !dir.exists( log_path ) ){
      dir.create( log_path, recursive = TRUE )
    }
    log_file_path = paste0(log_path,"/log_",run_id,".txt")
    log_appenders <- list(
      file_appender(log_file_path, append = TRUE, 
                    layout = default_log_layout())
    )
    return( list(LOGGER = log4r::logger(threshold = level, 
                                       appenders= log_appenders) ))
  }
  
  local_logger <- load_local_logger(getAbsolutePath("./"), settings$FINGERPRINT, level = "INFO")
  shared.env$logger <- logger
}

## Load initial param space (inc. param def object)
initial_ps <- readRDS(getAbsolutePath(paste(".",paste0("bolfi_","preday","_param_space_object.Rds"), sep = .Platform$file.sep)))

## Load new values for the parameters to be simulated
generated_ps <- read_csv(args[2],col_names = F,show_col_types = F)

# Simulate
simulated_output <- apply(generated_ps, 1, function(x){
  param_iter <- initial_ps$definition %>% mutate(value = x)
  param_iter_run <- fn_simulation(param_iter)
  param_iter_run$inadequacy
})

## Save intermediate results
interm_ps <- initial_ps
if(file.exists(getAbsolutePath("./output_object_stage_partial.Rds"))){
  interm_ps <- readRDS(getAbsolutePath("./output_object_stage_partial.Rds"))
}
generated_ps <- cbind(generated_ps,simulated_output)
colnames(generated_ps) <- c(interm_ps$definition$parameter,"inadequacy")
interm_ps$space <- bind_rows(interm_ps$space,generated_ps)
saveRDS(interm_ps,file = getAbsolutePath("./output_object_stage_partial.Rds"))

# Return outcome/inadequacy
write.table(simulated_output, file=args[3], row.names=FALSE, col.names=FALSE, sep=",")
end <- Sys.time()

if(exists("local_logger")) log4r::info(local_logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: Simulation elapsed time: ", (end - start), ". Currently simulated: ",nrow(generated_ps),". Totally simulated samples: ", nrow(interm_ps$space) ))

