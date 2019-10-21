# Combine Original Datasets
library(dplyr); library(magrittr); library(sf); library(stringr)

# load parsed data
load('data/par.rda')
load('data/all.rda')

# Remove Unused Objects.. (Can be brought back later...)
rm(BldgResImp, CxPrclCnBlk10, dbo_vw_boardup_public, PrclAddr,
   PrclAddrLRMS, PrclAttr, PrclImp, PrmWaiver, BldgInsp, BldgSect, LtrOwner)

# Use Current Parcels as master key
master <- data.frame(stringsAsFactors = FALSE,
                     Handle = as.character(prcl.dbf$HANDLE))

# define handle standard (Unclear Standardization...)
handle <- function(block, parcel, condo = 0){ # Warning, Not vectorized!
  block %<>% as.character
  
  if(grepl('\\.', block)){
    block %<>% strsplit('\\.')
    subblock = str_pad(block[[1]][2], 2, 'right', '0')
    block = str_pad(block[[1]][1], 4, 'left', '0')
  }else{
    block = str_pad(block, 4, 'left', '0')
    subblock = "00"
  }
  
  parcel %<>% 
    as.character %>%
    str_pad(3, 'left', '0')
    
  paste0(1,block,subblock,condo,parcel)
}

# Add Handle for Datasets that need it
add_handle <- function(data, block, parcel){
  Handle <- vector('character', nrow(data))
  blck = data[[block]]
  prcl = data[[parcel]]
  for (i in seq(1,nrow(data))) {
    Handle[i] <- handle(blck[i], prcl[i])
  }  
  data$Handle <- Handle
  return(data)
}

BldgCom  %<>% add_handle('CityBlock', 'Parcel')
BldgRes  %<>% add_handle('CityBlock', 'Parcel')
PrclAsmt %<>% add_handle('CityBlock', 'Parcel')
PrclREAR %<>% add_handle('CityBlock', 'Parcel')
PrmDemo  %<>% add_handle('CityBlock', 'Parcel')

# Select Variables from Datasets (Can Always Add more back in later...)
BldgCom %<>% transmute(
  Handle,
  ComCategory = case_when(BldgCategory == 'IN' ~ 'Industrial',
                       BldgCategory == 'LO' ~ 'Lodging',
                       BldgCategory == 'MC' ~ 'Mercantile',
                       BldgCategory == 'OF' ~ 'Office',
                       BldgCategory == 'PB' ~ 'Public Building',
                       BldgCategory == 'PR' ~ 'Processes',
                       BldgCategory == 'PS' ~ 'Prof. Services',
                       BldgCategory == 'RR' ~ 'Restaurant/Rec.',
                       BldgCategory == 'SV' ~ 'Services'
                       ),
  BldgCom = TRUE
)

BldgRes %<>% transmute(
  Handle,
  BldgRes = TRUE
)

Condemn %<>% transmute(
  Handle = as.character(Handle),
  Condemn = TRUE,
  CndStatus = case_when(Status == 'B' ~ 'Boarded Up',
                        Status == 'L' ~ 'Lifted',
                        Status == 'R' ~ 'Rehab',
                        Status == 'W' ~ 'Wrecked'
                        ),
  CndType = case_when(InspectType == 'CB' ~ 'Notice to Board Up',
                      InspectType == 'CD' ~ 'Condemned to be Demolished',
                      InspectType == 'CE' ~ 'Emergency Condemnation',
                      InspectType == 'CF' ~ 'Condmened for Occupancy',
                      InspectType == 'CO' ~ 'Condemned Secured for Year',
                      InspectType == 'CV' ~ 'Condemned to Vacate',
                      InspectType == 'LB' ~ 'Notice to Board Up - LRA',
                      InspectType == 'LD' ~ 'Condemned to be Demolished - LRA',
                      InspectType == 'LE' ~ 'Emergency Condemnation - LRA',
                      InspectType == 'LF' ~ 'Condemned for Occupancy - LRA',
                      InspectType == 'NE' ~ 'Emergency Secure',
                      InspectType == 'SG' ~ 'Sign',
                      InspectType == 'XX' ~ 'Emergency Board Up'
                      )
)

dbo_vw_public_inventory %<>%
  filter(Parcel.Status == "Available") %>%
  transmute(
    Handle = as.character(Handle),
    LRA_Usage = Usage,
    LRA_Inventory = TRUE
  )

`forestry-maintenance-properties.csv` %<>% transmute(
  Handle = as.character(HANDLE),
  ForestryType = as.character(PROPERTYTYPE)
)

VacBldg %<>%
  filter(SurveyYear >= 2017) %>% 
  transmute(
  Handle,
  VacantSurvery = TRUE
)

PrclREAR %<>%  ## Need to test still
  filter(TaxBal > 0) %>%
  transmute(
  Handle,
  TaxBal,
  BillYear
  ) %>%
  group_by(Handle) %>%
  summarise(Txyear = min(BillYear),
            Txbalance = sum(TaxBal)) %>%
  mutate(TaxDelinq = 2019 - Txyear)

PrmDemo %<>% 
  #filter() %>%
  transmute(
  Handle,
  PrmDemo = TRUE
  )

PrmBldg %<>% 
  #filter() %>%
  transmute(
  Handle,
  PrmBldg = TRUE
  )

PrmOcc %<>%
  #filter() %>%
  transmute(
  Handle,
  PrmOcc = TRUE
  )

# Join all of these Objects Based on Handle
master %<>%
  left_join(BldgCom) %>%
  left_join(BldgRes) %>%
  left_join(Condemn) %>%
  left_join(dbo_vw_public_inventory) %>%
  left_join(`forestry-maintenance-properties.csv`) %>%
  left_join(VacBldg) %>%
  left_join()

# Compute Other Fields

# Classify Vacancy According to Criteria

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Vacant Building ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Owned by LRA and Contains a Building
# Building Division Marked as Vacant 2017 or Later
# Forestry Marked as Vacant, Structurally Condemned, 5 Years Tax Deliquent, Boarded Up at Least Once

## FOR ALL: No Demo/Occupancy Permits (Since Jan 1 2016)
# If Demo, Vacant Lot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Possible Vacant Building ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Forestry Marked as Vacant, 5 Year Tax Delinquent
# Boarded Up at Least Once (!)
# Vacant Building Registration Fee or Penalty Applied (!)

## FOR ALL: No Demo/Occupancy Permits (Since Jan 1 2016)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Vacant Lot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Owned by LRA, Does not Contain a Building
# Owned by LRA, has Demo Permit
# Marked vacant by Building Division (2017 or later), had Demo Permit
# Marked vacant by Assessor, Forestry Maintenance (!)
# Marked vacant by Assessor, 3 Years Tax Delinquent
# Parcel Categorized as Trade, No Buildings, More than one Forestry Maintenance (!)

## FOR ALL: No Building Permits (Since Jan 1 2016)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Possible Vacant Lot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Marked as Vacant Lot by Assessor and No Maintenance
# Marked as Vacant Lot by Assessor and Owned by Religious Organization (!)

## FOR ALL: No Building Permits (Since Jan 1 2016)

## Also, Remove all Parks, Greenways, Highway Right of Ways, and Land Use Projects (!)
