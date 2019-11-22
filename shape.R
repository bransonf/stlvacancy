# Create a Spatial Object (For Exploring Satelite Imagery)

library(sf);library(mapview);library(dplyr);library(magrittr)
load('data/all.rda')
rmf = ls()
shp = prcl.shp
rm(rmf, list = rmf)

est = read.csv('vacancy_estimate.csv', stringsAsFactors = FALSE)
shp %<>% transmute(Handle = as.numeric(as.character(HANDLE)))

sf = left_join(shp, est) %>%
  filter(!is.na(VacStatus))

mapview(sf, legend = FALSE)
