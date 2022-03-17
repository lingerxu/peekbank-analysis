## libraries
library(papaja)
library(here)
library(tidyverse)
library(peekbankr)
library(xtable)
library(fs)

data_path <- fs::path("brm", "data")

knitr::opts_chunk$set(cache = TRUE, cache.extra = knitr::rand_seed,
                      echo = FALSE, warning = FALSE, message = FALSE,
                      fig.env = "figure*")

theme_set(theme_bw(base_size = 14))
theme_update(panel.grid = ggplot2::element_blank(),
             strip.background = ggplot2::element_blank(),
             legend.key = ggplot2::element_blank(),
             panel.border = ggplot2::element_blank(),
             axis.line = ggplot2::element_line(),
             strip.text = ggplot2::element_text(face = "bold"),
             legend.position = "bottom")

set.seed(42)

dataset_name_mapping <- read_csv(here("brm","data","dataset_name_mapping.csv"))
iso_codes <- read_csv(fs::path(data_path, "iso_language_codes.csv"))

dataset_info <- readRDS(fs::path(data_path, "dataset_info.rds"))


summarize_datasets <- readRDS(fs::path(data_path, "summarize_datasets.rds"))

# too look at a certain dataset

dataset_name <- "attword_processed"
dataset_unique_subj <- dataset_info %>%
  filter(dataset_name == "attword_processed") %>%
  distinct(subject_id,sex)
