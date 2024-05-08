# run pipeline
library(targets)
source("other_code/")

targets::tar_make()
tar_load_everything()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
