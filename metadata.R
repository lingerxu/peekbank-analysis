# analyze peekbank data to start meta analysis
all_aoi_timepoints <- get_aoi_timepoints()
all_stimuli <- get_stimuli()
all_administrations <- get_administrations()
all_subjects <- get_subjects()
all_trial_types <- get_trial_types() 
all_trials <- get_trials() 

aoi_data_joined <- all_aoi_timepoints |>
  right_join(all_administrations) |>
  right_join(all_subjects) |>
  right_join(all_trials) |>
  right_join(all_trial_types) |>
  mutate(stimulus_id = target_id) |>
  right_join(all_stimuli) |>
  select(dataset_name, subject_id, administration_id, trial_id, dataset_id, stimulus_id, t_norm, age, aoi, english_stimulus_label, stimulus_novelty)
# TO DO: looks like a very small number of rows get added in the join, probably a sign of an issue/ ambiguity somewhere.

save(aoi_data_joined, file = here("explorations","data","aoi_data_joined.Rds"))