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

# Save Shapes
VacBldgShp <- filter(sf, VacStatus %in% c('Vacant Building','Possible Building')) %>% select()
VacLotShp <- filter(sf, VacStatus %in% c('Vacant Lot','Possible Vacant Lot')) %>% select()

st_write(VacLotShp, '../stl_rtm/vac_lot/vac_lot.shp')
st_write(VacBldgShp, '../stl_rtm/vac_bldg/vac_bldg.shp')
