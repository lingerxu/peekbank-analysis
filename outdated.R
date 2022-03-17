#' check the peekbank_column_info csv files against database schema
#'
#' @param path_csv the directory of csv file "peekbank_column_info.csv"
#'
#' @return TRUE only if all the csv files have valid columns
#'
#' @examples
#' \dontrun{
#' is_valid = validate_column_info(path_csv = ".")
#' }
#'
#' @export
validate_column_info <- function(path_csv) {
  # get json file from github
  peekjson <- get_peekjson() # TODO: unused?
  # fetch the table list
  table_list <- list_ds_tables()
  # admin table is not required
  table_list <- table_list[table_list != "admin"];
  # read in the csv columns info file
  file_name <- "peekbank_column_info.csv"
  file_csv <- file.path(path_csv, file_name)

  if (file.exists(file_csv)) {
    # read in csv file and check if the data is valid
    info_csv <- utils::read.csv(file_csv) # TODO: unused?
  } else {
    is_all_valid <- FALSE
    stop(.msg("Cannot find file: {file_csv}"))
  }

  is_all_valid <- TRUE

  for (table_type in table_list) {
    fields_json <- get_json_fields(table_type)
    column_info <- NULL  # TODO: added because column_info wasn't defined
    rows_sel <- column_info[column_info$table == table_type, ]

    fieldnames_json <- fields_json$field_name # TODO: unused?
    fieldnames_csv <- rows_sel[, "field_name"] # TODO: unused?
  }
  return(is_all_valid)
}

#' Generate time course plots after all the processed tables are validated
#'
#' @param dir_csv the folder directory containing all the csv files,
#'                the path should end in "processed_data"
#' @param lab_dataset_id TODO
#' @param is_save TODO
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' visualize_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
visualize_for_db_import <- function(dir_csv, lab_dataset_id, is_save = FALSE) {
  # first read in all the csv files
  aoi_data <- utils::read.csv(file.path(dir_csv, "aoi_timepoints.csv"))
  trials_data <- utils::read.csv(file.path(dir_csv, "trials.csv"))
  trial_types_data <- utils::read.csv(file.path(dir_csv, "trial_types.csv"))
  stimuli_data <- utils::read.csv(file.path(dir_csv, "stimuli.csv"))
  # aoi_data <- utils::read.csv(fs::path(dir_csv, "aoi_timepoints.csv"))
  # trials_data <- utils::read.csv(fs::path(dir_csv, "trials.csv"))
  # trial_types_data <- utils::read.csv(fs::path(dir_csv, "trial_types.csv"))
  # stimuli_data <- utils::read.csv(fs::path(dir_csv, "stimuli.csv"))

  # rename columns for distractor
  distractor_stimuli_data <- stimuli_data
  colnames(distractor_stimuli_data) <- paste("distractor_",
                                             colnames(stimuli_data), sep = "")

  #join to full dataset
  full_data <- aoi_data %>%
    dplyr::left_join(trials_data) %>%
    dplyr::left_join(trial_types_data) %>%
    dplyr::left_join(stimuli_data, by = c("target_id" = "stimulus_id",
                                          "dataset_id")) %>%
    dplyr::left_join(distractor_stimuli_data %>%
                       dplyr::select(-.data$distractor_dataset_id),
                     by = c("distractor_id" = "distractor_stimulus_id"))

  #dplyr::mutate aoi
  full_data <- full_data %>%
    dplyr::mutate(aoi_new = dplyr::case_when(
      aoi == "target" ~ 1,
      aoi == "distractor"~0,
      aoi == "missing"~ NaN
    )) %>%
    dplyr::mutate(aoi_new = ifelse(is.nan(.data$aoi_new), NA, .data$aoi_new))

  ##### summarize by subject (really: administrations) ####
  summarize_by_subj <- full_data %>%
    dplyr::group_by(.data$administration_id, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$aoi_new)),
                     mean_accuracy = mean(.data$aoi_new, na.rm = TRUE))

  #### summarize across subjects ####
  summarize_across_subj <- summarize_by_subj %>%
    dplyr::group_by(.data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$mean_accuracy)),
                     accuracy = mean(.data$mean_accuracy, na.rm = TRUE),
                     sd_accuracy = stats::sd(.data$mean_accuracy, na.rm = TRUE))

  # plot (remove data points where not a lot of subjects contributed, to avoid
  # discontinuities in the slope)
  g1 <- summarize_across_subj %>%
    dplyr::filter(.data$N > length(unique(full_data$administration_id)) / 3) %>%
    ggplot2::ggplot(ggplot2::aes(.data$t_norm, .data$accuracy)) +
    ggplot2::geom_line(data = dplyr::filter(summarize_by_subj, .data$N > 10),
                       ggplot2::aes(y = .data$mean_accuracy,
                                    color = as.factor(.data$administration_id),
                                    group = as.factor(.data$administration_id)),
                       alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "gam", se = FALSE) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_vline(xintercept = 300, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggplot2::theme(legend.position = "none")
  print(g1)

  #### by condition plotting (only if applicable) ####

  ##### summarize by subject by condition ####
  summarize_by_subj_by_condition <- full_data %>%
    dplyr::group_by(.data$administration_id, .data$condition, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$aoi_new)),
                     mean_accuracy = mean(.data$aoi_new, na.rm = TRUE))

  #### summarize across subjects ####
  summarize_across_subj_by_condition <- summarize_by_subj_by_condition %>%
    dplyr::group_by(.data$condition, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$mean_accuracy)),
                     accuracy = mean(.data$mean_accuracy, na.rm = TRUE),
                     sd_accuracy = stats::sd(.data$mean_accuracy, na.rm = TRUE))

  g2 <- summarize_across_subj_by_condition %>%
    dplyr::filter(.data$N > length(unique(full_data$administration_id)) / 3) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$t_norm, y = .data$accuracy,
                                 color = .data$condition,
                                 group = .data$condition)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "gam", se = FALSE) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_vline(xintercept = 300, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed")
  # print(g2)

  if (is_save) {
    plot_name <- paste0(lab_dataset_id, "_profile.png")
    ggplot2::ggsave(plot_name)
  }
}




#' supporting function for saving processed table as csv files in
#' processed_data
#'
#' @param df_procd processed data frame
#' @param table_type type of table to be saved, such as "aoi_timepoints", "trials", etc
#'
#' @export
save_table <- function(df_procd, table_type) {
  # because of the standardized data structure, processed csvs need to be under
  # "[dataset_name]/processed_data"
  dir_procd <- "../processed_data"

  # save processed data table in csv file
  # create dir for saving processed data if this directory does not exist
  if (!file.exists(dir_procd)) {
    dir.create(dir_procd)
  }
  utils::write.csv(df_procd, file.path(
    dir_procd, paste(table_type, ".csv", sep = "")), row.names = FALSE)
}

# filter_eyetracking
# average
# universal function for converting orgin coordinates

#' Process directory of raw data to xy data
#'
#' @param format One of "tobii", "smi", "eyelink"
#' @param dir Directory with raw data
#'
#' @return TODO
#'
#' @export
process_to_xy <- function(format, dir) {
  # can be changed to process_smi_xy or for any dataset that needs to average
  # xy data in order to xy_data table
  # 1. average
  # 2. filter out of range data

  readers <- list(
    "tobii" = process_tobii,
    "smi" = process_smi
    # "eyelink" = process_eyelink
  )
  stopifnot(format %in% names(readers))
  reader <- readers[format]
  reader(dir)
}

#' Create empty table
#'
#' @param table_type character TODO
#'
#' @return TODO
#' @export
create_emtpy_table <- function(table_type) {
  # # fetch the required columns from json file
  # colnames_json <- get_json_colnames(table_type)
  # # create emtpy df
  # df_table <- utils::read.csv(text = paste0(colnames_json, collapse = ","))
  # # add NA values
  # df_table[1,] = c(0, rep(NA, length(colnames_json)-1))
  # return(df_table)
}



#' Process tobii raw data
#' dataset specific: datasets
#'
#' @param dir_raw Directory with raw data
#' @param dataset_name TODO
#' @param dataset_type TODO
#'
#' @export
process_tobii <- function(dir_raw, dataset_name = "sample_data",
                          dataset_type = "automated") {
  raw_format <- "tobii"
  # list all the files under the directory
  file_raw <- list.files(path = dir_raw,
                         pattern = "*data*.tsv",
                         all.files = FALSE)

  # read in raw data frame from file oi
  raw_data <- utils::read.table(file = file.path(dir_raw, file_raw),
                                sep = "\t",
                                header = TRUE)

  # Start processing table by table type
  ######## DATASETS ########
  table_type <- "datasets"
  if (is_table_required(table_type, dataset_type)) {
    ## [monitor_size_x], [monitor_size_y]
    # get the unique monitor sizes
    monitor_size_str <- raw_data[["RecordingResolution"]] %>%
      unique()  %>%
      stats::na.omit() %>%
      as.vector()
    # when multiple monitor size, just pick one
    if (length(monitor_size_str) > 1) {
      monitor_size_str <- monitor_size_str[1]
      warning(.msg("Dataset {dataset_name} has multiple monitor sizes, only
                   {monitor_size_str} will be saved."))
    }
    # extract numeric values from list
    monitor_size <- regmatches(monitor_size_str,
                               gregexpr("[[:digit:]]+", monitor_size_str))
    monitor_size <- monitor_size[[1]]
    monitor_size_x <- as.numeric(monitor_size[1])
    monitor_size_y <- as.numeric(monitor_size[2])

    ## [sample_rate]
    sample_timestamp <- raw_data[["RecordingTimestamp"]] %>%
      as.vector()
    sample_steps <- sample_timestamp[-1] -
      sample_timestamp[-length(sample_timestamp)]
    sample_rate <- 1000 / mean(sample_steps)

    df_datasets <- data.frame(dataset_id = 0,
                              monitor_size_x = monitor_size_x,
                              monitor_size_y = monitor_size_y,
                              sample_rate = sample_rate,
                              tracker = "tobii",
                              lab_dataset_id = dataset_name)

    # validate against json, if valid, then save csv
    if (validate_table(df_datasets, table_type)) {
      save_table(df_datasets, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## AOI_REGIONS ########
  table_type <- "aoi_regions"
  if (is_table_required(table_type, dataset_type)) {
    # find the roi region file under dir_raw
    file_aoi <- list.files(path = dir_raw,
                           pattern = "*aoi*",
                           all.files = FALSE)

    if (length(file_aoi) != 0) {
      file_aoi <- file.path(dir_raw, file_aoi)
      has_aoi_info <- file.exists(file_aoi)
    } else {
      has_aoi_info <- FALSE
    }
    if (has_aoi_info) {
      df_aoi <- utils::read.table(file_aoi, header = TRUE, sep = "")
      aoi_id <- seq(0, (nrow(df_aoi) - 1))
      df_aoi[["aoi_region_id"]] <- c(aoi_id)
    } else {
      stop(.msg("Cannot find aoi_regions info file {file_aoi}."))
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_aoi, table_type)) {
      save_table(df_aoi, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## TRIALS ########
  table_type <- "trials"
  if (is_table_required(table_type, dataset_type)) {
    # find the trial file under dir_raw
    file_trials <- list.files(path = dir_raw,
                              pattern = "*trial*",
                              all.files = FALSE)
    if (length(file_trials) != 0) {
      file_trials <- file.path(dir_raw, file_trials)
      has_trial_info <- file.exists(file_trials)
    } else {
      has_trial_info <- FALSE
    }
    if (has_trial_info) {
      df_trials <- utils::read.table(file_trials, header = TRUE, sep = "")
      trial_id <- seq(0, (nrow(df_trials) - 1))
      df_trials[["trial_id"]] <- c(trial_id)
    } else {
      stop(.msg("Cannot find trial_info file {file_trials}."))
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_trials, table_type)) {
      save_table(df_trials, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## TRIAL TYPES ########
  table_type <- "trial_types"
  if (is_table_required(table_type, dataset_type)) {
    # find the trial file under dir_raw
    file_trial_types <- list.files(path = dir_raw,
                                   pattern = "*trial_type*",
                                   all.files = FALSE)
    if (length(file_trials) != 0) {
      file_trials <- file.path(dir_raw, file_trial_types)
      has_trial_type_info <- file.exists(file_trial_types)
    } else {
      has_trial_type_info <- FALSE
    }
    if (has_trial_type_info) {
      df_trials <- utils::read.table(file_trial_types, header = TRUE, sep = "")
      trial_id <- seq(0, (nrow(df_trials) - 1))
      df_trials[["trial_id"]] <- c(trial_id)
    } else {
      stop(.msg("Cannot find trial_type file {file_trial_types}."))
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_trials, table_type)) {
      save_table(df_trials, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## SUBJECTS ########
  table_type <- "subjects"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_subjects <- map_columns(raw_data, raw_format, table_type)

    # get the unique subjects in the raw table
    # + generate primary key for subjects table
    df_subjects <- dplyr::distinct(df_subjects)
    subject_id <- seq(0, (nrow(df_subjects) - 1))
    df_subjects[["subject_id"]] <- c(subject_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_subjects, table_type)) {
      save_table(df_subjects, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## XY_DATA ########
  table_type <- "xy_data"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_xy <- map_columns(raw_data, raw_format, table_type)

    # replace 'subject_id', 'trial_id' to unique integer keys
    xy_data_id <- seq(0, (nrow(df_xy) - 1))
    df_xy[["xy_data_id"]] <- c(xy_data_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_xy, table_type)) {
      save_table(df_xy, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## AOI_DATA ########
  table_type <- "aoi_data"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_aoi <- map_columns(raw_data, raw_format, table_type)

    # replace 'subject_id', 'trial_id' to unique integer keys
    aoi_data_id <- seq(0, (nrow(df_aoi) - 1))
    df_aoi[["aoi_data_id"]] <- c(aoi_data_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_aoi, table_type)) {
      save_table(df_aoi, table_type)
    } # no else error message, because that will be handled by validator.R
  }
}


# Process eyelink raw data
#
# @param dir Directory with raw data
#
# @export
# process_eyelink <- function(dir) {
#
# }



#' Process smi raw data file
#'
#' @param dir Directory with raw data
#' @param file_ext eyetracking file type, default value is ".txt"
#'
#' @export
process_smi <- function(dir, file_ext = ".txt") {

  # list files in directory
  all_files <- list.files(path = dir,
                          pattern = paste0("*", file_ext),
                          all.files = FALSE)

  # process individual smi files
  all_data <- lapply(all_files, process_smi_file, dir = dir)

  # extract specific datasets from the processed data
  xy_data <- all_data %>%
    purrr::map("xy") %>%
    dplyr::bind_rows()

  dataset_data <- all_data %>%
    purrr::map("dataset") %>%
    dplyr::bind_rows()

  # save data
  save_table(xy_data, table_type = "xy_data")
  save_table(dataset_data, table_type = "dataset")
}

#' Process an individual smi raw data file
#'
#' @param x character, filename
#' @param dir character, directory
#' @param stims_to_remove_chars character, e.g. ".avi"
#' @param stims_to_keep_chars character, e.g. "_"
#' @param possible_delims character vector
#' @param stimulus_coding character
#'
#' @return a list containing the processed xy eyetracking data
#' @export
process_smi_file <- function(x, dir, stims_to_remove_chars = c(".avi"),
                             stims_to_keep_chars = c("_"),
                             possible_delims = c("\t", ","),
                             stimulus_coding = "stim_column") {

  # general parameters
  max_lines_search <- 40 # comments from initial header of smi eyetracking file
  subid_name <- "Subject"
  monitor_size <- "Calibration Area"
  sample_rate <- "Sample Rate"
  left_x_col_name <- "L POR X [px]" # TODO: unused?
  right_x_col_name <- "R POR X [px]" # TODO: unused?
  left_y_col_name <- "L POR Y [px]" # TODO: unused?
  right_y_col_name <- "R POR Y [px]" # TODO: unused?

  # create file path
  file_path <- paste0(dir, "/", x)

  # guess delimiter
  sep <- reader::get.delim(file_path, comment = "#", delims = possible_delims,
                           skip = max_lines_search)

  # extract information about subject, monitor size, and sample rate from header
  subject_id <- readr::read_lines(file_path, n_max = max_lines_search) %>%
    stringr::str_subset(subid_name) %>%
    stringr::str_extract(paste("(?<=", subid_name, ":\\t).*", sep = "")) %>%
    trimws()

  monitor_size <- readr::read_lines(file_path, n_max = max_lines_search) %>%
    stringr::str_subset(monitor_size) %>%
    stringr::str_extract(paste("(?<=", monitor_size, ":\\t).*", sep = "")) %>%
    trimws() %>%
    stringr::str_replace("\t", "x")

  sample_rate <- readr::read_lines(file_path, n_max = max_lines_search) %>%
    stringr::str_subset(sample_rate) %>%
    stringr::str_extract(paste("(?<=", sample_rate, ":\\t).*", sep = "")) %>%
    trimws()

  # get maximum x-y coordinates on screen
  screen_xy <- stringr::str_split(monitor_size, "x") %>%
    unlist()
  x_max <- as.numeric(as.character(screen_xy[1]))
  y_max <- as.numeric(as.character(screen_xy[2]))

  # read in data
  data <-
    readr::read_delim(
      file_path,
      comment = "##",
      delim = sep
    )

  # select and rename columns
  data <-  data %>%
    dplyr::rename(
      raw_t = "Time",
      lx = .data$left_x_col_name,
      rx = .data$right_x_col_name,
      ly = .data$left_y_col_name,
      ry = .data$right_y_col_name
    )

  # select rows for xy file
  data <- data %>%
    dplyr::filter(
      # remove anything that isn't actually collecting ET data
      .data$Type == "SMP",
      # remove calibration
      .data$Stimulus != "-",
      # remove anything that isn't actually a trial based on specific characters
      !grepl(paste(stims_to_remove_chars, collapse = "|"), .data$Stimulus),
      # from here, keep only trials fitting desired patters
      grepl(paste(stims_to_keep_chars, collapse = "|"), .data$Stimulus))

  # add sub_id column (extracted from data file)
  data <- data %>%
    dplyr::mutate(subject_id = .data$subject_id)

  #### General eyetracking data processing

  ## Remove out of range looks
  data <- data %>%
    dplyr::mutate(
      rx = dplyr::if_else(.data$rx <= 0 | .data$rx >= x_max,
                          NA_real_, .data$rx),
      lx = dplyr::if_else(.data$lx <= 0 | .data$lx >= x_max,
                          NA_real_, .data$lx),
      ry = dplyr::if_else(.data$ry <= 0 | .data$ry >= y_max,
                          NA_real_, .data$ry),
      ly = dplyr::if_else(.data$ly <= 0 | .data$ly >= y_max,
                          NA_real_, .data$ly)
    )

  ## Average left-right x-y coordinates
  # Take one eye's measurements if we only have one; otherwise average them
  data <-
    data %>%
    dplyr::mutate(
      x = dplyr::case_when(
        is.na(rx) & !is.na(lx) ~ lx,
        !is.na(rx) & is.na(lx) ~ rx,
        !is.na(rx) & !is.na(lx) ~ (rx + lx) / 2,
        is.na(rx) & is.na(lx) ~ NA_real_
      ),
      y = dplyr::case_when(
        is.na(ry) & !is.na(ly) ~ ly,
        !is.na(ry) & is.na(ly) ~ ry,
        !is.na(ry) & !is.na(ly) ~ (ry + ly) / 2,
        is.na(ry) & is.na(ly) ~ NA_real_
      )
    ) %>%
    dplyr::select(
      -.data$rx, -.data$ry, -.data$lx, -.data$ly
    )

  ## Convert time into ms starting from 0
  data <- data %>%
    dplyr::mutate(
      timestamp = round((.data$raw_t - .data$raw_t[1]) / 1000, 3)
    )

  ## Redefine coordinate origin (0,0)
  # SMI starts from top left
  # Here we convert the origin of the x,y coordinate to be bottom left (by
  # "reversing" y-coordinate origin)
  data <- data %>%
    dplyr::mutate(
      y = y_max - .data$y
    )

  # If trials are identified via a Stimulus column, determine trials and
  # redefine time based on trial onsets
  if (stimulus_coding == "stim_column") {

    # Redefine trials based on stimuli rather than SMI output
    # check if previous stimulus value is equal to current value; ifelse, trial
    # test increases by 1
    data <- data %>%
      dplyr::mutate(stim_lag = dplyr::lag(.data$Stimulus),
                    temp = ifelse(.data$Stimulus != .data$stim_lag, 1, 0),
                    temp_id = cumsum(c(0, .data$temp[!is.na(.data$temp)])),
                    trial_id = 1 + .data$temp_id)

    # set time to zero at the beginning of each trial
    data <- data %>%
      dplyr::group_by(.data$trial_id) %>%
      dplyr::mutate(t = .data$timestamp - min(.data$timestamp))
  }

  # extract final columns
  xy_data <- data %>%
    dplyr::select(.data$sub_id, .data$x, .data$y, .data$t, .data$trial_id,
                  .data$Stimulus)

  ## Make dataset table
  dataset_data <- data.frame(id = "refword",
                             tracker = "SMI",
                             monitor_size = monitor_size,
                             sample_rate = sample_rate)

  return(list(xy = xy_data, dataset = dataset_data))
}


#' Round times to our specified sample rate to be consistent across labs
#'
#' @param df df that has subject_id, dataset_id, trial_id and times
#'
#' @return TODO
#' @export
round_times <- function(df) {
  # set sample rates

  df %>%
    dplyr::group_by(.data$administration_id, .data$trial_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = .data$data %>%
        purrr::map(function(df) {
          df_rounded <- df %>%
            dplyr::mutate(
              t_zeroed = round(pkg_globals$SAMPLE_DURATION *
                                 round(.data$t_zeroed /
                                         pkg_globals$SAMPLE_DURATION)))

          t_resampled <- dplyr::tibble(
            t_zeroed = round(seq(min(df_rounded$t_zeroed),
                                 max(df_rounded$t_zeroed),
                                 pkg_globals$SAMPLE_DURATION)))

          dplyr::left_join(t_resampled, df_rounded, by = "t_zeroed") %>%
            dplyr::group_by(.data$t_zeroed)
        })) %>%
    tidyr::unnest(.data$data)
}

#' This function trims the xy_data such that only gaze coordinates that fall
#' within the stimuli space is preserved
#'
#' @param xy_data the pre-trim xy data generated from the import script
#' @param datasets the peekDS formatted datasets.csv that contains screen
#'   dimension info
#' @return TODO
#'
#' @export
xy_trim <- function(xy_data, datasets) {
  datasets <- datasets %>%
    dplyr::mutate(video_size_x = ifelse(.data$monitor_size_x == 1280,
                                        1280, 1200),
                  video_size_y = ifelse(.data$monitor_size_y == 1024,
                                        960, 900))
  x_min <- (datasets$monitor_size_x - datasets$video_size_x) / 2
  y_min <- (datasets$monitor_size_y - datasets$video_size_y) / 2

  xy_data <- xy_data %>%
    dplyr::filter(.data$x > x_min, .data$x < datasets$monitor_size_x - x_min,
                  .data$y > y_min, .data$y < datasets$monitor_size_y - y_min)
}

