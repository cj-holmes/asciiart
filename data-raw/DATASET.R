## code to prepare `DATASET` dataset goes here
hadley <- jpeg::readJPEG('data-raw/hadley.jpg')
rstats <- jpeg::readJPEG('data-raw/rstats.jpeg')

usethis::use_data(hadley, rstats, overwrite = TRUE)
