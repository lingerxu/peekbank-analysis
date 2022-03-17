# dev tmp scripts


# demo function for running validator
demo_resample <- function() {
  # check for xy_data, trials, aoa_coordinates

  devtools::install_github("langcog/peekds")
  devtools::install_github("langcog/peekbankr")

  library(peekds)
  library(peekbankr)
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(ggthemes)

  dir_datasets <- "testdataset" # local datasets dir
  lab_dataset_id <- "pomper_salientme"
  dir_data <- file.path(dir_datasets, lab_dataset_id, "resample_debug_tmp.rds")
  ds_fulldata <- readRDS(dir_data)

  ds_resample_input <- select(ds_fulldata, "trial_id", "administration_id", "t", "aoi", "point_of_disambiguation")
  tictoc::tic()
  ds_resample_out <- resample_times(ds_resample_input, table_type = "aoi_timepoints")
  tictoc::toc()

  tictoc::tic()
  ds_resample_order <- arrange(ds_resample_out, administration_id, trial_id)
  tictoc::toc()

  df <- data.frame(name=c("a","b","b","c","c","c"), count=c(1,2,3,1,2,3))

  tictoc::tic()
  ds_resample_out %>%
    group_by(administration_id, trial_id) %>%
    nest()
  tictoc::toc()

  # dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
  # dir.create(file.path(dir_datasets, lab_dataset_id))
  # file_ext <- '.csv'
  # get_processed_data(lab_dataset_id, path = dir_csv, osf_address = "pr6wu") # if you dont have the most updated version of processed_data
  #
  # table_type <- "xy_timepoints"
  # file_csv <- file.path(dir_csv, paste0(table_type, '.csv'))
  # df_table <- utils::read.csv(file_csv)
  #
  # sum(is.na(df_table$aoi))
  # df_resampled <- resample_times(df_table, table_type)
  # sum(is.na(df_resampled$aoi))
}

# demo function for running validator
demo_validator <- function() {
  # check for xy_data, trials, aoa_coordinates

  detach("package::peekds")
  remove.packages("peekds")
  devtools::install_github("langcog/peekds")
  # devtools::install_github("langcog/peekbankr")

  library(peekds)
  library(dplyr)
  # library(peekbankr)
  # library(tidyverse)
  # library(ggplot2)
  # library(ggthemes)
  datasets <- get_datasets()
  dir_datasets <- "testdataset" # local datasets dir
  lab_dataset_id <- "reflook_socword"
  dir.create(file.path(dir_datasets, lab_dataset_id))
  dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
  file_ext <- '.csv'
  get_processed_data(lab_dataset_id, path = dir_csv, osf_address = "pr6wu") # if you dont have the most updated version of processed_data

  msg_error_all <- validate_for_db_import(dir_csv)

  visualize_for_db_import(dir_csv, is_save = TRUE)

  # table level debugging
  # "subjects"        "trial_types"     "trials"          "stimuli"
  table_type <- "subjects"

  file_csv <- file.path(dir_csv, paste0(table_type, file_ext))
  df_table <- utils::read.csv(file_csv)
  validate_table(df_table, table_type)

  cat(crayon::red("hi"))
}



# demo function for running visualization check
demo_vis <- function() {
  # rm(list = ls())
  # setwd("C:/Dropbox/_codes/peek/peekds")
  dir_datasets <- "testdataset" # local datasets dir
  datasets <- get_datasets()
  dataset_list <- datasets$dataset_name
  file_ext <- '.csv'

  lab_dataset_id <- "potter_canine"
  dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")



  # get in all the tables
  df_dataset <- data.frame(
    dataset_id = 0,
    lab_dataset_id = lab_dataset_id,
    dataset_name <- lab_dataset_id)

    df_aoi <- utils::read.csv(file.path(dir_csv, paste0("aoi_timepoints_resampled", file_ext)))
    df_stimuli <- utils::read.csv(file.path(dir_csv, paste0("stimuli", file_ext)))
    df_trials <- utils::read.csv(file.path(dir_csv, paste0("trials", file_ext)))
    df_admin <- utils::read.csv(file.path(dir_csv, paste0("administrations", file_ext)))

    t_range <- c(-1000,3000)
    aoi_data_joined <- df_aoi %>%
      right_join(df_admin) %>%
      right_join(df_trials) %>%
      right_join(df_dataset) %>%
      mutate(stimulus_id = target_id) %>%
      right_join(df_stimuli) %>%
      filter(t_norm > t_range[1],
             t_norm < t_range[2])

    # get subject info
    subinfo <- aoi_data_joined %>%
      group_by(subject_id, dataset_id, lab_dataset_id, age) %>%
      summarise(trials = length(unique(trial_id)))

    subinfo %>%
      ggplot(aes(x = age, fill = lab_dataset_id)) +
      geom_histogram(binwidth = 3) +
      scale_fill_solarized(name = "Dataset") +
      xlab("Age (months)")

    # time series
    means <- aoi_data_joined %>%
      filter(age > 12, age <= 60) %>%
      mutate(age_binned = cut(age, seq(0,60,12))) %>%
      group_by(t_norm, dataset_name, age_binned, stimulus_novelty) %>%
      summarise(n = sum(aoi %in% c("target","distractor"), na.rm = TRUE),
                p = sum(aoi == "target", na.rm = TRUE),
                prop_looking = p / n,
                ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
                ci_upper = binom::binom.confint(p, n, method = "bayes")$upper)

    g1 <- ggplot(means,
                 aes(x = t_norm, y = prop_looking)) +
      geom_rect(xmin = t_range[1],
                xmax = t_range[2],
                ymin = 0,
                ymax = 1, fill = "gray", alpha = .1) +
      geom_line(aes(col = dataset_name)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper,
                      fill = dataset_name), alpha = .5) +
      facet_grid(age_binned~stimulus_novelty) +
      geom_hline(yintercept = .5, lty = 2) +
      geom_vline(xintercept = 0, lty = 2) +
      ylab("Proportion Target Looking") +
      xlab("Time (msec)") +
      theme_classic() +
      scale_color_solarized() +
      scale_fill_solarized()

    plot_name <- paste0(lab_dataset_id, "_profile.png")
    ggsave(plot_name)
}
