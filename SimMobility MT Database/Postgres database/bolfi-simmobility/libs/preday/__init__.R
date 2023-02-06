suppressMessages(library(tidyverse))
suppressMessages(library(reshape2))
suppressMessages(library(purrr))

## --------------------------- Functions ----------------------------------

## Function that execute system call to replace necessary line with param value
# Params:
# - model: model name (filename without extension)
# - param_name: name of the parameter
# - declaration: type of declaration used in the code (local or as property of an object)
# - value: value to be set
fn_param_update <- function(model,param_name,declaration,param_name_left_hand,value){
  ## Get the global setting object
  setting_obj <- shared.env$settings
  
  if(setting_obj$MOCKING_MODE) return(0) ## Mocking mode is useful for local testing, not to run actual simulation!
  
  sys_call_param <- paste(
    "sh",
    setting_obj$SH_UPDATE_PARAM, 
    paste(setting_obj$PARAM_FILE_PATH,paste0(model,".",setting_obj$PARAM_FILE_EXTENSION),sep=.Platform$file.sep), 
    paste0("'", param_name_left_hand, "='" ),
    paste0("'", declaration, param_name, " = ", value, "'" ) 
  )
  
  if(setting_obj$CONTAINERIZED){
    sys_call_param <- paste(sys_call_param, paste0(model,".",setting_obj$PARAM_FILE_EXTENSION), paste(c(setting_obj$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep ), setting_obj$FINGERPRINT)
  }
  output <- system( 
    sys_call_param,
    intern = T,
    wait = T
  )
  
  #if(length(output) == 0) return(0)
  if( is.null( attr(output,"status") )) return(0)
  
  return(-1)
}

fn_param_push <- function(){
  ## Get the global setting object
  setting_obj <- shared.env$settings
  
  if(setting_obj$MOCKING_MODE) return(0) ## Mocking mode is useful for local testing, not to run actual simulation!
  
  sys_call_param <- paste(
    "sh",
    setting_obj$SH_PUSH_PARAM,
    paste(c(setting_obj$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep ), 
    setting_obj$FINGERPRINT
  )
  
  output <- system( 
    sys_call_param,
    intern = T,
    wait = T
  )
  
  if( is.null( attr(output,"status") )) return(0)
  
  return(-1)
}

## Function that will try to update all parameters based on a flag that signaling a change (flag "changed" T/F)
## NOTE: DUring first run, if there are not existent model files, then "changed" flag need to be set to T for all parameters, otherwise part of them won't be set at all
# Params:
# - params: dataframe (tidy) containing necessary columns
fn_simulation_config <- function(params){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  #updated_params <- params %>% 
  #  filter(changed)
  
  status <- apply(params, 1, function(x){
    fn_param_update(x["model"],x["param_name"],x["declaration"],x["param_name_left_hand"],x["value"])
  })
  
  if(setting_obj$CONTAINERIZED && !is.na(setting_obj$SH_PUSH_PARAM)){
    push_status <- fn_param_push()
    
    if(push_status != 0) {
      status <- rep(push_status,length(status))
    }
  }
  
  #updated_params %>% bind_cols(list(status = status))
}

## Function that will setup a docker worker
fn_worker_setup <- function(setting_obj){
  output <- system( 
    paste(
      "sh",
      setting_obj$SH_SETUP_WORKER,
      setting_obj$CONTAINER_HOME_PATH,
      paste(c(setting_obj$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep ),
      setting_obj$FINGERPRINT,
      setting_obj$IMAGE_NAME
    ),
    intern = T,
    wait = T
  )
  
  if( is.null( attr(output,"status") )) return(0)
  
  stop(output)
}

## Function that will remove a docker worker
fn_worker_remove <- function(setting_obj){
  output <- system( 
    paste(
      "sh",
      setting_obj$SH_REMOVE_WORKER,
      paste(c(setting_obj$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep ),
      setting_obj$FINGERPRINT
    ),
    intern = T,
    wait = T
  )
  if( is.null( attr(output,"status") )) return(0)
  
  stop(output)
}

## Function that execute system call to run the simulation with SimMobility
# Params:
# - /
fn_simulation_call <- function(){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  if(setting_obj$MOCKING_MODE) return(0) ## Mocking mode is useful for local testing, not to run actual simulation!
  
  if(file.exists(setting_obj$ACTIVITY_FILE)) file.remove(setting_obj$ACTIVITY_FILE)
  
  sys_call_param <- paste(
    "sh",
    setting_obj$SH_EXEC_SIMULATION
  )
  
  if(setting_obj$CONTAINERIZED){
    sys_call_param <- paste(sys_call_param, paste(c(setting_obj$CONTAINER_HOME_PATH,"runtime"), collapse = .Platform$file.sep ), setting_obj$FINGERPRINT)
  }
  
  output <- system( 
    sys_call_param,
    intern = T,
    wait = T
  )
  
  if(file.exists(setting_obj$ACTIVITY_FILE)) return(0)
  
  stop(output)
}

## Function that calculates output statistics
# Params:
# - activity: activity_schedule table (dataframe,tible)
# - pop_size: total population size [currently not in use as no ration of population is considered]
fn_output_stat <- function(activity,pop_size = 0){ ##TALLINN
  if(nrow(activity) == 0){
    return( NULL )
  }
  
  pop_travelled <- length(unique(activity$person_id))
  
  activity <- activity %>% 
    mutate(od = paste0(prev_stop_location,"_",stop_location))
  
  stat_total <- activity %>% 
    group_by(person_id) %>% 
    summarise(
      tours = length(unique(tour_no)),
      trips = n()
    ) %>% 
    ungroup() %>% 
    select(-person_id) %>% 
    summarise_all( sum ) %>% 
    mutate(avg_trips_tour = round(trips/tours,3) )
  
  stat_purpose <- activity %>% 
    group_by(person_id,tourType) %>% 
    summarise(
      tours = length(unique(tour_no)),
      trips = n()
    ) %>% 
    group_by(tourType) %>% 
    summarise_at( vars(c(tours,trips)), .funs = sum, na.rm = T ) %>% 
    mutate(avg_trips_tour = round(trips/tours,3) )
  
  stat_mode <- activity %>% 
    group_by(person_id,stop_mode) %>% 
    summarise(
      tours = length(unique(tour_no)),
      trips = n()
    ) %>% 
    group_by(stop_mode) %>% 
    summarise_at( vars(c(tours,trips)), .funs = sum, na.rm = T ) %>% 
    mutate(avg_trips_tour = round(trips/tours,3) )
  
  bind_rows(
    stat_total %>% melt() %>% mutate(type = "general",scope = "global"),
    stat_purpose %>% melt(id.vars = "tourType") %>% mutate(scope = "purpose") %>% rename(type = tourType),
    stat_mode %>% melt(id.vars = "stop_mode") %>% mutate(scope = "mode") %>% rename(type = stop_mode)
  ) %>% select(scope,type,variable,value)
}

fn_output_stat_virtualcity <- function(activity,pop_size = 0){
  if(nrow(activity) == 0){
    return( NULL )
  }
  
  pop_travelled <- length(unique(activity$person_id))
  
  activity <- activity %>% 
    mutate(od = paste0(prev_stop_location,"_",stop_location))
  
  stat_total <- activity %>% 
    group_by(person_id) %>% 
    summarise(
      tours = length(unique(tour_no)),
      stops = length(unique(od)),
      trips = n()
    ) %>% 
    select(-person_id) %>% 
    summarise_all( sum ) #%>% 
  #mutate(pop_ratio = length(unique(activity %>% pull(person_id)))/pop_size)
  
  stat_purpose <- activity %>% 
    group_by(person_id,tourType) %>% 
    summarise(
      tours = length(unique(tour_no)),
      stops = length(unique(od)),
      trips = n()
    ) %>% 
    group_by(tourType) %>% 
    summarise_at( vars(c(tours,stops,trips)), .funs = sum, na.rm = T )
  
  
  stat_purpose <- left_join(
    stat_purpose,
    activity %>% 
      group_by(tourType) %>% 
      summarise(
        travel_ratio = length(unique(person_id))/pop_travelled #,
        #pop_ratio = length(unique(person_id))/pop_size
      ),
    by="tourType"
  )
  
  
  stat_mode <- activity %>% 
    group_by(person_id,stop_mode) %>% 
    summarise(
      tours = length(unique(tour_no)),
      stops = length(unique(od)),
      trips = n()
    ) %>% 
    group_by(stop_mode) %>% 
    summarise_at( vars(c(tours,stops,trips)), .funs = sum, na.rm = T )
  
  stat_mode <- left_join(
    stat_mode,
    activity %>% 
      group_by(stop_mode) %>% 
      summarise(
        travel_ratio = length(unique(person_id))/pop_travelled #,
        #pop_ratio = length(unique(person_id))/pop_size
      ),
    by="stop_mode"
  )
  
  bind_rows(
    stat_total %>% melt() %>% mutate(type = "general",scope = "global"),
    stat_purpose %>% melt(id.vars = "tourType") %>% mutate(scope = "purpose") %>% rename(type = tourType),
    stat_mode %>% melt(id.vars = "stop_mode") %>% mutate(scope = "mode") %>% rename(type = stop_mode)
  ) %>% select(scope,type,variable,value)
}

## Function that calculates output statistics
# Params:
# - activity: activity_schedule table (dataframe,tible)
# - setting_obj: object with overall settings
fn_output_od <- function(activity){ ## O-D MATRIX
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  OD_cells <- rownames(setting_obj$CITY_STATS$value)
  
  if(nrow(activity) == 0){
    return( NULL )
  }
  
  OD_m <- matrix(rep(0, length(OD_cells)^2 ), nrow = length(OD_cells))
  rownames(OD_m) <- OD_cells
  colnames(OD_m) <- OD_cells
  
  as <- activity %>% 
    left_join(setting_obj$CITY_STATS$district_map %>% select(origin_station_code = code,prev_stop_location = newTAZ), by="prev_stop_location") %>% 
    left_join(setting_obj$CITY_STATS$district_map %>% select(destination_station_code = code,stop_location = newTAZ), by="stop_location")
  
  for( id_sr in 1:nrow(as) ){
    travel_subroute <- as[id_sr,]
    origin_n <- travel_subroute$origin_station_code
    destination_n <- travel_subroute$destination_station_code
    OD_m[origin_n,destination_n] <- OD_m[origin_n,destination_n] + 1
  }
  
  return( OD_m )
}

## Function that calculates output statistics (OD and balance between transportation modes)
# Params:
# - activity: activity_schedule table (dataframe,tible)
# - setting_obj: object with overall settings
fn_output_od_mode_balance <- function(activity){ ## O-D MATRIX + transportation mode balance
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  OD_m <- setting_obj$CITY_STATS$emptyOD
  output <- list(
    total_legs = nrow(activity),
    od = OD_m,
    balance = settings$CITY_STATS$balance %>% mutate(estimated_share = 0) %>% select(mode_category,estimated_share, everything()),
    workers_work = 1
  )
  
  if(nrow(activity) == 0){
    return( output )
  }
  
  as <- activity %>% 
    left_join(setting_obj$CITY_STATS$district_map %>% select(origin_station_code = code,prev_stop_location = newTAZ), by="prev_stop_location") %>% 
    left_join(setting_obj$CITY_STATS$district_map %>% select(destination_station_code = code,stop_location = newTAZ), by="stop_location") %>% 
    select(origin_station_code,destination_station_code)
  
  for( id_sr in 1:nrow(as) ){
    travel_subroute <- as[id_sr,]
    origin_n <- travel_subroute$origin_station_code
    destination_n <- travel_subroute$destination_station_code
    OD_m[origin_n,destination_n] <- OD_m[origin_n,destination_n] + 1
  }
  
  output$od = OD_m #OD_m/sum(OD_m) ## IF OPERATES IN RELATIVE MODE
  
  ## Transportation mode balance
  if(hasName(settings$CITY_STATS,"balance")){
    total_legs <- nrow(activity)
    mode_stats <- activity %>% 
      mutate(
        mode_category = if_else(stop_mode %in% c("BusTravel","SMS"), "public", if_else(stop_mode %in% c("Car","Car Sharing 2", "Car Sharing 3"), "car", if_else(stop_mode %in% c("PrivateBus","Motorcycle","Taxi"),"other", tolower(stop_mode) ))) 
      ) %>% 
      group_by(mode_category) %>% 
      summarise(
        estimated_share = n()/total_legs
      ) %>% 
      ungroup() %>% 
      right_join(settings$CITY_STATS$balance, by="mode_category") %>% 
      mutate(
        estimated_share = if_else(is.na(estimated_share),0,estimated_share)
      )
      
    output$balance <- mode_stats
  }
  
  
  ## Workers share scheduled to take a trip
  if(hasName(settings$CITY_STATS,"workers")){
    assigned_workers <- activity %>% 
      filter(tourType == "Work", primary_stop == T) %>% 
      mutate(person_id = as.character(person_id)) %>% 
      separate(person_id,c("id","family_mem_id"), sep = "-") %>% 
      mutate(id = as.numeric(id)) %>% 
      filter(id %in% settings$CITY_STATS$workers$id) %>% 
      pull(id) %>% unique(.)
    
    output$workers_work <- (1 - (length(assigned_workers)/nrow(settings$CITY_STATS$workers)))
  }
  
  return( output )
}

## Function that compare two vectors in terms of distance
fn_quantify_vector <- function(vector_1,vector_2, ...){
  return( sqrt(sum((vector_1 - vector_2) ^ 2)) ) # Euclidean
}
fn_quantify_vector_correlation <- function(vector_1,vector_2, ...){
  return( as.numeric(cor(vector_1,vector_2)) ) # Pearson correlation
}

## Function that compare two matrix
fn_quantify_matrix <- function(matrix_1,matrix_2, mat_weights = NULL, squared = F, absolute = F){
  if( is.null(mat_weights) ){
    mat_dim <- dim(matrix_1)
    mat_weights <- matrix(rep(1, mat_dim[1]*mat_dim[2]), nrow = mat_dim[1])
  }
  mat_difference <- (matrix_1 - matrix_2)
  if(squared) mat_difference <- mat_difference^2
  if(absolute) mat_difference <- abs(mat_difference)
  
  return( 
    #fn_matrix_max_cell_loss( mat_difference * mat_weights, observed_mat = matrix_2) 
    fn_matrix_rmse( mat_difference )
  )
}
fn_matrix_norm <- function(mat,...){ # Frobenius matrix norm
  return( norm( mat, type = "F") ) 
}
fn_matrix_max_cell_loss <- function(mat, observed_mat, ...){ ## max relative loss/error (loss per cell) - rounded at 4 decimals
  
  rel_mat <- mat/observed_mat
  rel_non_tol <- length(which(rel_mat > 0.1))/nrow(rel_mat)^2 ## rel number of cells with non-tollerable error
  
  #return( max(round(mat/observed_mat,4)) ) 
  return( round(max(rel_mat)+rel_non_tol,4) )
}
fn_matrix_rmse <- function(mat, ...){
  return( round(sqrt(mean(mat^2)), 4) )
}

## Function that compare two vectors in terms of similarity
fn_quantify <- function(vals_1,vals_2,vals_weights = NULL){
  fn_quantify_matrix(vals_1,vals_2,vals_weights)
}

## Function that quantify inadequacy or discrepancy
# Params:
# - iter_stats: output stats from iteration
# - data_stats: output stats calculated from data
fn_quantify_inadequacy_stats <- function(iter_stats){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  ## If empty activity schedule is generated (i.e., no trips at all), return max distance
  if(is.null(iter_stats)){ 
    return( fn_quantify( rep(0,length(setting_obj$CITY_STATS$value)), setting_obj$CITY_STATS$value ))
  }
  
  ## Otherwise calculate proper distance
  joined_stats <- left_join( 
    iter_stats %>% unite("stats",scope,type,variable,sep="_"), 
    setting_obj$CITY_STATS %>% rename(observed_value = value), 
    by="stats" 
  )
  
  ## If certain stats are not available in observed stats, then remove it from comparison
  joined_stats <- joined_stats %>% filter( ! is.na(observed_value))
  
  return( fn_quantify( joined_stats$value, joined_stats$observed_value ) )
}

## Function that quantify inadequacy or discrepancy
# Params:
# - iter_stats: output stats from iteration
# - data_stats: output stats calculated from data
fn_quantify_inadequacy_od <- function(iter_stats){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  mat_weights <- NULL
  if(hasName(setting_obj$CITY_STATS, "weights")) {
    mat_weights <- setting_obj$CITY_STATS$weights
  }
  
  ## If empty activity schedule is generated (i.e., no trips at all), return max distance
  if(is.null(iter_stats)){ 
    od_dim <- dim(setting_obj$CITY_STATS$value)
    return( fn_quantify( matrix(rep(0, od_dim[1]*od_dim[2]), nrow = od_dim[1]), setting_obj$CITY_STATS$value, mat_weights ))
  }
  
  return( fn_quantify( iter_stats, setting_obj$CITY_STATS$value, mat_weights ) )
}

## Function that quantify inadequacy or discrepancy
# Params:
# - iter_stats: output stats from iteration
# - data_stats: output stats calculated from data
fn_quantify_inadequacy_BCKUP <- function(iter_stats){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  od_discrepancy <- fn_quantify_inadequacy_od(iter_stats$od)
  balance_discrepancy <- 1
  
  if(hasName(iter_stats,"balance")){
    balance_discrepancy <- 1-fn_quantify_vector_correlation(iter_stats$balance[,2],iter_stats$balance[,3])
  }
  
  #return( list(value = (od_discrepancy ^ (1+balance_discrepancy))/iter_stats$total_legs, base = c(od=od_discrepancy,tours=iter_stats$total_legs,mode = balance_discrepancy) ) ) #tours=(iter_stats$total_legs - setting_obj$CITY_STATS$total_trips)
  return( list(value = balance_discrepancy*abs(iter_stats$total_legs-setting_obj$CITY_STATS$total_trips), base = c(od=od_discrepancy,tours=iter_stats$total_legs,mode = balance_discrepancy) ) ) #tours=(iter_stats$total_legs - setting_obj$CITY_STATS$total_trips)
}
fn_quantify_inadequacy <- function(iter_stats){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  od_discrepancy <- fn_quantify_inadequacy_od(iter_stats$od)
  balance_discrepancy <- 0

  if(hasName(iter_stats,"balance")){
    balance_discrepancy <- fn_quantify_vector(iter_stats$balance[,2],iter_stats$balance[,3])
  }
  
  workers_work_error <- 0
  if(hasName(iter_stats,"workers_work")){
    workers_work_error <- iter_stats$workers_work
  }
  
  # ## ----------- Error value ----------------
  # simulated_num_legs <- iter_stats$total_legs
  # observed_num_legs <- setting_obj$CITY_STATS$total_trips
  # 
  # error_val <- (0.00001 + abs(simulated_num_legs-observed_num_legs)/observed_num_legs) * 
  #              (0.00001 + od_discrepancy) *
  #              (1 + ((abs(simulated_num_legs-observed_num_legs)/observed_num_legs) - od_discrepancy)^2) ## DESC: [ NUM_LEGS_ERROR * OD_ERROR * DIFFERENCE_BETWEEN_THEM^2 ] both sides have equal multiplicative contribution to the loss/error. Minimum (optimum) is close to 0.
  # 
  # error_base <- c()#list(tours_residual = abs(simulated_num_legs-observed_num_legs)/observed_num_legs, od_residual = od_discrepancy) # tours=iter_stats$total_legs
  # 
  # if(exists("logger", shared.env)){
  #   log4r::info(shared.env$logger$LOGGER, paste0("ERROR-TERM: [Sim/Obs]Legs: ", simulated_num_legs, "/", observed_num_legs, " (ABS-DIF: ", abs(simulated_num_legs-observed_num_legs)/observed_num_legs,"); OD_DISCREPANCY: ", od_discrepancy, "; OVERALL-ERROR: ", error_val))
  # }
  # 
  # ## ----------------------------------------
  
  ## ----------- Error value ----------------
  simulated_num_legs <- iter_stats$total_legs
  observed_num_legs <- setting_obj$CITY_STATS$total_trips
  error_val <- od_discrepancy/100 * (1 + balance_discrepancy) * (1 + workers_work_error)
  error_base <- c()
  
  if(exists("logger", shared.env) && hasName(shared.env$logger, "LOGGER")){
    log4r::info(shared.env$logger$LOGGER, paste0("ERROR-TERM: TOTAL-LEGS: ", simulated_num_legs, "/", observed_num_legs, "; OD: ", od_discrepancy, "; BALANCE: ", balance_discrepancy,"; OVERALL-ERROR: ", error_val))
  }
  
  ## ----------------------------------------
  
  #error_val_experimental <- error_val*(1 + ((abs(simulated_num_legs-observed_num_legs)/observed_num_legs) - od_discrepancy)^2)
  
  #return( list(value = (od_discrepancy ^ (1+balance_discrepancy))/iter_stats$total_legs, base = c(od=od_discrepancy,tours=iter_stats$total_legs,mode = balance_discrepancy) ) ) #tours=(iter_stats$total_legs - setting_obj$CITY_STATS$total_trips)
  return( list(value = error_val, base = error_base ) )
}


## Function that take activity_schedule at input and return calculated output statistics
# Params:
# - /
fn_process_activities <- function(){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  # Load activities
  activity_schedule <- read_csv(setting_obj$ACTIVITY_FILE,
                                col_names = c("person_id","tour_no","tourType","stop_no","stop_type","stop_location","stopZone","stop_mode","primary_stop","arrival_time","departure_time","prev_stop_location","prev_stopZone","prev_stop_departure_time","pid"),
                                show_col_types = F)
  
  # TEMPORAL: save file
  activity_schedule %>% write_csv(paste(setting_obj$OBJECT_PATH, paste0(setting_obj$FINGERPRINT,"_activity_schedule_",as.numeric(Sys.time()),".csv"), sep = .Platform$file.sep ))
  
  # Calculate outcomes (number of tours, trips, stops, VKT?, PKT / general + per mode + per activity)
  #output_stats <- fn_output_stat(activity_schedule)
  #output_stats <- fn_output_od(activity_schedule,setting_obj)
  output_stats <- fn_output_od_mode_balance(activity_schedule)
  
  return(output_stats)
}

## Simulation function updates config file for simulation, runs the simmobility simulation (preday), reads the result and calculate measure of adequacy (similarity) of the result
# Params:
# - parameter dataframe (including name of parameter, model specification and declaration type (local or property))
# - 
fn_simulation <- function(params){ 
  # Update files with parameters
  param_config_output <- fn_simulation_config(params)
  
  # Execute simulation
  fn_simulation_call()
  
  # Read and process activity schedules
  outcome <- fn_process_activities()
  
  # Quantify (in)adequacy (discrepancy) of output statistics to the real world data (stats)
  inadequacy <- fn_quantify_inadequacy(outcome)
  if(shared.env$settings$MOCKING_MODE) inadequacy$value <- inadequacy$value + rnorm(1, 0, 3)
  
  return(list(config_output = param_config_output, inadequacy = inadequacy$value, base_residuals = inadequacy$base ) )
}

fn_perform_simulation <- function(value, param_def){
  # Transform param row into corresponding params tibble
  param_def[,"value"] <- value ## THIS IS TEMP UNLIST SOLUTION, OTHERWISE NEED TO BE FIXED IN PREDAY_SAMPLING not to return list but vector/matrix
  
  # Simulate
  param_iter_run <- fn_simulation(param_def)
  
  # Warnings for particular param not being applied
  # ... TODO
  
  # Update the value pool 
  fn_update_value_pools(value,param_iter_run$base_residuals,param_def)
  
  # Return outcome/inadequacy
  return(param_iter_run$inadequacy)
}
fn_perform_simulation_only <- function(value, param_def){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  # Transform param row into corresponding params tibble
  param_def[,"value"] <- value
  
  # Update files with parameters
  param_config_output <- fn_simulation_config(param_def)
  
  # Execute simulation
  fn_simulation_call()
  
  # Load activities
  activity_schedule <- read_csv(setting_obj$ACTIVITY_FILE,
                                col_names = c("person_id","tour_no","tourType","stop_no","stop_type","stop_location","stopZone","stop_mode","primary_stop","arrival_time","departure_time","prev_stop_location","prev_stopZone","prev_stop_departure_time","pid"),
                                show_col_types = F)
  
  # Save file
  activity_schedule %>% write_csv(paste(setting_obj$OBJECT_PATH, paste0(setting_obj$FINGERPRINT,"_activity_schedule_",as.numeric(Sys.time()),".csv"), sep = .Platform$file.sep ))
  
  # Return activity_schedule
  return(activity_schedule)
}

## Scaling up function
fn_scaleup_standard <- function(l_sample, p_def){
  apply(matrix(1:ncol(l_sample), nrow = 1),2, function(c_i, sample_mat, limit_mat){
    sample_mat[,c_i]*(limit_mat[c_i,2] - limit_mat[c_i,1]) + limit_mat[c_i,1]
  }, sample_mat = l_sample,limit_mat = as.matrix(p_def[,c("lower_limit","upper_limit")]))
}

## Custom sampling function
fn_preday_sampling <- function(vals=NULL, param_space, sample_size = 1, ...){
  is_initial <- ifelse( (is.null(vals) || nrow(param_space$space) == 0) ,T,F)
  
  enabled_idx <- 1:nrow(param_space$definition)
  if(c("enabled") %in% names(param_space$definition)) enabled_idx <- which(param_space$definition[,"enabled"] == T)
  dim_names <- param_space$definition$parameter
  sample_dim <- length(enabled_idx)     #nrow(param_space$definition)      #[which(param_space$definition[,"include"] == T),]
  
  ## Default generated sample
  genereted_sample <- matrix(rep(unlist(param_space$definition[,"initial"], use.names = F),sample_size), nrow = sample_size, byrow = T)
  
  if(is_initial){
    #prior_sample <- matrix(rep(unlist(param_space$definition[,"initial"], use.names = F), sample_size), nrow = sample_size, byrow = T)
    genereted_sample[,enabled_idx] <- fn_scaleup_standard(lhs::improvedLHS(sample_size, sample_dim), param_space$definition[enabled_idx,])
    colnames(genereted_sample) <- dim_names
    
    ## Add the initial param values
    genereted_sample <- rbind(unlist(param_space$definition[,"initial"], use.names = F),genereted_sample)
    
    return(genereted_sample)
  }
  
  ## Collect couple of optimal values
  if(hasName(shared.env$settings,"SAMPLING_OPTIMAL_TOLERANCE") && shared.env$settings$SAMPLING_OPTIMAL_TOLERANCE > 0){
    min_inadequacy <- min(param_space$space[,shared.env$settings$target_col], na.rm = T)
    vals <- param_space$space[which(param_space$space[,shared.env$settings$target_col] < (min_inadequacy + shared.env$settings$SAMPLING_OPTIMAL_TOLERANCE) ), dim_names ]
    nrow_vals <- nrow(vals)
    
    if(nrow_vals == 1){
      vals <- matrix(unlist(vals, use.names = F), nrow = nrow_vals)
    } else {
      vals <- as.matrix(vals)
    }
  }
  
  if(nrow(vals) == 1){
    vals <- matrix(vals[,enabled_idx], nrow = nrow_vals)
  } else {
    vals <- vals[,enabled_idx]
  }
  
  sampling_intensities <- list(
    tours = c(0.6,0.6),  ## high and low intensity
    mode = c(0.5,0.5),
    od = c(0.5,0.5),
    none = c(0.2,0.2)
  )
  
  ## Compare the value pool of residuals in order to assign sampling intensity
  sampling_intensity <- lapply(names(sampling_intensities), function(s_intensity, s_intensities, r_pool){
    
    if(is.null(r_pool[[s_intensity]]) || length(r_pool[[s_intensity]]) < 2) return( s_intensities[[s_intensity]][1] )
    
    tail_diff <- abs(tail(diff(r_pool[[s_intensity]],1),3))
    thrs <- shared.env$settings$EPSILON[s_intensity]
    if(!any(tail_diff > thrs$focused_sampling)) return( s_intensities[[s_intensity]][1] ) ## Error diff is too low (means stucked in local optima) and need reset with spread sampling
    if(!any(tail_diff > thrs$spread_sampling)) return( s_intensities[[s_intensity]][2] ) ## Error diff is relativly low (means detected all optimal regions) and need to focus on more smaller search region
    return( s_intensities[[s_intensity]][1] )
  }, s_intensities = sampling_intensities, r_pool = shared.env$param_space$residual_pool)
  names(sampling_intensity) <- names(sampling_intensities)
  
  if(hasName(shared.env$settings, "OPTIMISATION_CORES") && shared.env$settings$OPTIMISATION_CORES > 1){
    genereted_subspace_sample <- mclapply(matrix(1:sample_dim, nrow = 1), function(c_i, p_defs, s_size, s_intensity, o_values){
      p_def <- p_defs[c_i,]
      p_intensity <- s_intensity[[p_def[["direct_influence"]]]]
      p_replaced_size <- ceiling(s_size*p_intensity)
      p_current <- o_values[sample(1:nrow(o_values),1),c_i]  ## In case currently most optimal sets contains more than one sample set, then sample randomly one of them.
      
      p_lower_bound <- p_def[["lower_limit"]]
      p_upper_bound <- p_def[["upper_limit"]]
      p_tunnel_bound <- 2
      if("tunnel_width" %in% names(p_def)) {
        p_tunnel_bound <- p_def[["tunnel_width"]]
        p_lower_bound <- p_current - p_tunnel_bound
        p_upper_bound <- p_current + p_tunnel_bound
      }
      
      p_sample <- rep(p_current, s_size)
      p_sample_replace <- sample(1:s_size, p_replaced_size, replace = F)
      
      #p_sample[p_sample_replace] <- runif(p_replaced_size,min = p_lower_bound,max = p_upper_bound) ## UNIFORMLY - SAMPLING
      p_sample[p_sample_replace] <- rnorm(p_replaced_size, mean = p_current, sd = p_tunnel_bound) ## NORMAL AROUND THE OPTIMAL - SAMPLING
      
      return(p_sample)
    }, p_defs = param_space$definition[enabled_idx,], s_size = sample_size, s_intensity = sampling_intensity, o_values = vals, mc.cores = shared.env$settings$OPTIMISATION_CORES)
    genereted_subspace_sample <- Reduce('cbind',genereted_subspace_sample)
  } else {
    
    genereted_subspace_sample <- apply(matrix(1:sample_dim, nrow = 1), 2, function(c_i, p_defs, s_size, s_intensity, o_values){
      p_def <- p_defs[c_i,]
      p_intensity <- s_intensity[[p_def[["direct_influence"]]]]
      p_replaced_size <- ceiling(s_size*p_intensity)
      p_current <- o_values[sample(1:nrow(o_values),1),c_i]  ## In case currently most optimal sets contains more than one sample set, then sample randomly one of them.
      
      p_lower_bound <- p_def[["lower_limit"]]
      p_upper_bound <- p_def[["upper_limit"]]
      p_tunnel_bound <- 2
      if("tunnel_width" %in% names(p_def)) {
        p_tunnel_bound <- p_def[["tunnel_width"]]
        p_lower_bound <- p_current - p_tunnel_bound
        p_upper_bound <- p_current + p_tunnel_bound
      }
      
      p_sample <- rep(p_current, s_size)
      p_sample_replace <- sample(1:s_size, p_replaced_size, replace = F)
      
      #p_sample[p_sample_replace] <- runif(p_replaced_size,min = p_lower_bound,max = p_upper_bound) ## UNIFORMLY - SAMPLING
      p_sample[p_sample_replace] <- rnorm(p_replaced_size, mean = p_current, sd = p_tunnel_bound) ## NORMAL AROUND THE OPTIMAL - SAMPLING
      
      return(p_sample)
    }, p_defs = param_space$definition[enabled_idx,], s_size = sample_size, s_intensity = sampling_intensity, o_values = vals)
  }
  genereted_sample[,enabled_idx] <- genereted_subspace_sample
  colnames(genereted_sample) <- dim_names
  
  if(!hasName(shared.env$settings,"IMPUTED_INITIAL_VALUE_SET")){
    if( sum(as.numeric(param_space$space[1, 1:nrow(param_space$definition) ]) - unlist(param_space$definition[,"initial"], use.names = F)) != 0 ){
      init_param_values <- param_space$definition %>% pull(initial)
      genereted_sample <- rbind(init_param_values,genereted_sample)
    }
    shared.env$settings$IMPUTED_INITIAL_VALUE_SET <- T
  }
  
  return(genereted_sample)
}

## Function to update the value pool that will be used in sampling
fn_update_value_pools <- function(params, base_residuals, param_def){
  ## Get global settings object
  setting_obj <- shared.env$settings
  
  if(setting_obj$MOCKING_MODE) {
    base_residuals <- base_residuals + rnorm(length(base_residuals))
  }
  
  l_residual_object <- shared.env$param_space$residual_pool
  l_residual_components <- setdiff(names(l_residual_object),c("none"))
  for(l_comp in l_residual_components){
    shared.env$param_space$residual_pool[[l_comp]] <- c(as.numeric(l_residual_object[[l_comp]]),as.numeric(base_residuals[l_comp]))
  }
}

## Function that loads definition of param data.frame with provided values (if any)
fn_load_params_space_definition <- function(filepath, omit = list(), sys_modules = c("preday"), tunnel_constraint = T, update_initial = F){
  no_tours_models <- c("dpb","dps","dpt","nte","ntw","nto","nts","isg")
  mode_balance_models <- c("tme","tmw","stmd")
  low_priority_models <- c("tws","sttd","ttde","ttdw","ttdo","itd")
  
  file_content <- read_csv(filepath) %>% 
    filter( module %in% sys_modules, include) %>% 
    separate(param,c("param_name","value"), sep="=", remove = F) %>% 
    mutate(
      declaration = str_extract(param,".+?(?=beta|cons)"),
      param_name_left_hand = param_name,
      param_name = trimws(str_replace(param_name,"(local |bundled_variables.)","")),
      parameter = paste(model, param_name, sep="_"),
      raw_value = trimws(value),
      value = as.numeric(gsub(" ","", value)),
      direct_influence = if_else(model %in% no_tours_models, "tours", if_else(model %in% mode_balance_models,"mode","none")),
      enabled = (lower_limit != upper_limit), ## This will allow to disable those betas that have been assigned with a fixed value
      # initial = if_else(enabled, value, lower_limit)
    ) %>% 
    select(-raw_value)
  
  if(update_initial){
    file_content <- file_content %>% 
      mutate(
        initial = if_else(enabled, value, lower_limit)
      )
  }
  
  
  if(!is.list(omit) && length(omit) == 1){
    omit_col_name <- omit
    omit <- file_content %>% filter( !! rlang::sym(omit) ) %>% pull(parameter)
    file_content <- file_content %>% select(- !!rlang::sym(omit_col_name))
  }
  
  ## Filter out not changing parameters (make things faster)
  file_content <- file_content %>% filter( ! parameter %in% omit ) 
  omit <- c()
  column_header <- file_content %>% pull(parameter)
  residual_pool_header <- c("tours","mode","od")
  
  ## If enabled, make lower and upper bounds around current value +/-t_width; t_width = 2 (default)
  if(tunnel_constraint){ 
    t_width <- 2.0
    t_restricted_width <- 0.05
    file_content <- file_content %>% mutate(
      lower_limit = if_else(!enabled, initial, if_else(str_detect(parameter,"logsum"), (initial - t_restricted_width), (initial - t_width))),
      upper_limit = if_else(!enabled, initial, if_else(str_detect(parameter,"logsum"), (initial + t_restricted_width), (initial + t_width))),
      tunnel_width = t_width
    ) 
  }
  
  return(list(
    definition = file_content %>% select(-value) %>% mutate(changed = if_else(parameter %in% omit, F, T), init_lower_limit = lower_limit, init_upper_limit = upper_limit ),
    space = head(setNames(file_content %>% select(c(value)) %>% t() %>% as_tibble(), column_header),0),
    value_pool = setNames(lapply( column_header, function(x){ return( c()) }), column_header),
    residual_pool = setNames(lapply( residual_pool_header, function(x){ return( c() ) }), residual_pool_header)
  ))
}


