# run pipeline
library(targets)
source("other_code/load_libraries.R")

targets::tar_make()
tar_load_everything()
