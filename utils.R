get_session_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trial_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- do.call(rbind, trail_list)
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

get_session_summary_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trial_list <- list()
  for (trail_id in 1:n_trail){
    trial_tibble <- get_trial_summary(session_id,trail_id)
    trial_list[[trail_id]] <- trial_tibble
  }
  session_tibble <- do.call(rbind, trial_list)
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

get_trial_summary <- function(session_id, trial_id){
  tmp <- get_trial_data(session_id, trial_id) %>% arrange(desc(region_mean_spike))
  trial_summary <- tmp[1,] 
  trial_summary <- trial_summary %>% add_column("main_region" = tmp$brain_area[1]) %>% add_column("strength_main_region" = tmp$region_sum_spike[1]/sum(tmp$region_sum_spike))
  trial_summary
}


## First we will create a function that allows us to select a trial within a session and sort the data by brain region, sum of spikes within that region, total number of neurons within the region. Also included as variables are the mean number of spikes, the trial id, contrast values, and feedback type which indicates whether the trial was successful or not.
get_trial_data <- function(session_id, trial_id){
  spikes <- session[[session_id]]$spks[[trial_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trial_tibble <- tibble("neuron_spike" = rowSums(spikes))  %>%  add_column("brain_area" = session[[session_id]]$brain_area ) %>% group_by(brain_area) %>% summarize( region_sum_spike = sum(neuron_spike), region_count = n(),region_mean_spike = mean(neuron_spike)) 
  trial_tibble  = trial_tibble%>% add_column("trial_id" = trial_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trial_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trial_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trial_id])
  trial_tibble
}


get_session_functional_data <- function(session_id){
  n_trial <- length(session[[session_id]]$spks)
  trial_list <- list()
  for (trial_id in 1:n_trial){
    trial_tibble <- get_trial_functional_data(session_id,trial_id)
    trial_list[[trial_id]] <- trial_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trial_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

get_test_functional_data <- function(session_id){
  n_trial <- length(trt[[session_id]]$spks)
  trial_list <- list()
  for (trial_id in 1:n_trial){
    trial_tibble <- get_trials_functional_data(session_id,trial_id)
    trial_list[[trial_id]] <- trial_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trial_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = trt[[session_id]]$mouse_name) %>% add_column("date_exp" = trt[[session_id]]$date_exp) %>% add_column("test_id" = session_id) 
  session_tibble
}

get_trial_functional_data <- function(session_id, trial_id){
  bin_num <- paste0("bin", as.character(1:40))
  spikes <- session[[session_id]]$spks[[trial_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  
  trial_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trial_bin_average) <- bin_num
  trial_tibble  = as_tibble(trial_bin_average)%>% add_column("trial_id" = trial_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trial_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trial_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trial_id])
  
  trial_tibble
}

get_trials_functional_data <- function(session_id, trial_id){
  bin_num <- paste0("bin", as.character(1:40))
  spikes <- trt[[session_id]]$spks[[trial_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  
  trial_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trial_bin_average) <- bin_num
  trial_tibble  = as_tibble(trial_bin_average)%>% add_column("trial_id" = trial_id) %>% add_column("contrast_left"= trt[[session_id]]$contrast_left[trial_id]) %>% add_column("contrast_right"= trt[[session_id]]$contrast_right[trial_id]) %>% add_column("feedback_type"= trt[[session_id]]$feedback_type[trial_id])
  
  trial_tibble
}