## code to prepare `DATASET` dataset goes here
hadley <- jpeg::readJPEG('data-raw/hadley.jpg')


usethis::use_data(hadley, overwrite = TRUE)
