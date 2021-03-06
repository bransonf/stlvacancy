# Combine Original Datasets
library(dplyr); library(magrittr); library(sf); library(stringr); library(lubridate)

# load parsed data
load('data/parsed.rda')

# Use Current Parcels as master key
master <- data.frame(stringsAsFactors = FALSE,
                     Handle = as.character(prcl.dbf$HANDLE))

# Handle
# 1---------- Default
# -0000------ City Block
# -----00---- SubBlock
# -------0--- Owner Code
# --------000 Parcel

# Vectorized Handle Function
handle <- function(data){
  data %<>%
    mutate(h.block =
             sprintf("%.2f", CityBlock) %>%
             str_remove('\\.') %>%
             str_pad(6, 'left', '0'),
           h.parcel =
             as.character(Parcel) %>%
             str_pad(3, 'left', '0'),
           Handle = paste0(1, h.block, 0, h.parcel)
    ) %>%
    select(-h.block,-h.parcel)
  
}

# Add Handle to Datasets
BldgCom  %<>% handle
BldgRes  %<>% handle
PrclREAR %<>% handle
PrmDemo  %<>% handle

# Select Variables from Datasets
Prcl %<>% 
  transmute(
    Handle = as.character(Handle),
    BldgVac = VacBldgYear > 2016, # Building Division Survey 2017
    Church = ifelse(AsrClassCode %in% c(19:27, 30:37), TRUE, FALSE),
    AsrVacLot = ifelse(AsrLandUse1 %in% c(1010,9100), TRUE, FALSE),
    AsrVacBld = ifelse(AsrLandUse1 %in% c(9111,9112,9141,9142,9151,9152,9171,9172,9400), TRUE, FALSE),
    AsrTrade = ifelse(AsrLandUse1 %in% c(5000,5990,9140,5900,5800,5700,5600,5400,5300,5200,5190,5100), TRUE, FALSE),
    Rmv = ifelse(AsrLandUse1 %in% c(7600, 7610, 4500) | CDALandUse1 %in% c(7600, 7610, 4500, 6242) ,TRUE, FALSE) # Parks, Highway Right of Way, Green Way(?), Landuse Projects(?)
  ) %>%
  filter(!duplicated(Handle))

BldgCom %<>%
  transmute(
    Handle,
    ComCategory = case_when(
      BldgCategory == 'IN' ~ 'Industrial',
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
  ) %>%
  filter(!duplicated(Handle))

BldgRes %<>% 
  transmute(
    Handle,
    BldgRes = TRUE
  ) %>%
  filter(!duplicated(Handle))

Condemn %<>%
  transmute(
    Handle = as.character(Handle),
    Condemn = TRUE,
    CndStatus = case_when(
      Status == 'B' ~ 'Boarded Up',
      Status == 'L' ~ 'Lifted',
      Status == 'R' ~ 'Rehab',
      Status == 'W' ~ 'Wrecked'
    ),
    CndType = case_when(
      InspectType == 'CB' ~ 'Notice to Board Up',
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
  ) %>%
  filter(!duplicated(Handle))

dbo_vw_public_inventory %<>%
  filter(Parcel.Status == "Available") %>%
  transmute(
    Handle = as.character(Handle),
    LRA_Usage = Usage,
    LRA_Inventory = TRUE
  ) %>%
  filter(!duplicated(Handle))

forestry_maintenance_properties %<>%
  transmute(
    Handle = as.character(HANDLE),
    ForestryType = as.character(PROPERTYTYPE)
  ) %>%
  filter(!duplicated(Handle))

VacBldg %<>%
  filter(SurveyYear >= 2017) %>% 
  transmute(
    Handle = as.character(Handle),
    VacantSurvery = TRUE
  ) %>%
  filter(!duplicated(Handle))

PrclREAR %<>%
  filter(TaxBal > 0) %>%
  select(
    Handle,
    TaxBal,
    BillYear
  ) %>%
  group_by(Handle) %>%
  summarise(
    Txyear = min(BillYear),
    Txbalance = sum(TaxBal)
  ) %>%
  mutate(TaxDelinq = as.integer(format(Sys.Date(), '%Y')) - Txyear)

PrmDemo %<>% 
  mutate(year = year(mdy_hms(as.character(IssueDate)))) %>%
  filter(year >= 2016) %>%
  transmute(
    Handle,
    PrmDemo = TRUE
  ) %>%
  filter(!duplicated(Handle))

PrmBldg %<>% 
  mutate(year = year(mdy_hms(as.character(IssueDate)))) %>%
  filter(year >= 2016) %>%
  transmute(
    Handle = as.character(Handle),
    PrmBldg = TRUE
  ) %>%
  filter(!duplicated(Handle))

PrmOcc %<>%
  mutate(year = year(mdy_hms(as.character(IssueDate)))) %>%
  filter(year >= 2016) %>%
  transmute(
    Handle = as.character(Handle),
    PrmOcc = TRUE
  ) %>%
  filter(!duplicated(Handle))

forestry_services %<>%
  left_join(for_codes, by = c('ServType' = 'ServiceType')) %>%
  mutate(Handle = as.character(Handle)) %>%
  group_by(Handle) %>%
  summarise(
    BoardUp = sum(Descr == 'Building Board Up'),
    ForServCnt = sum(!Descr %in% c('Spraying','Seeding','Balance Cancellation','Interest')),
    VacReg = sum(Descr == 'Vacant Building Registration'),
    VacPnlty = sum(Descr == 'Vacant Building Penalty')
  )

# Join all of these Objects Based on Handle
master %<>%
  left_join(Prcl) %>%
  left_join(BldgCom) %>%
  left_join(BldgRes) %>%
  left_join(Condemn) %>%
  left_join(dbo_vw_public_inventory) %>%
  left_join(forestry_maintenance_properties) %>%
  left_join(VacBldg) %>%
  left_join(PrclREAR) %>%
  left_join(PrmDemo) %>%
  left_join(PrmBldg) %>%
  left_join(PrmOcc)  %>%
  left_join(forestry_services)

# Replace NAs in Logicals
master %<>%
  mutate(
    BldgVac = ifelse(is.na(BldgVac), FALSE, BldgVac),
    Church = ifelse(is.na(Church), FALSE, Church),
    AsrVacLot = ifelse(is.na(AsrVacLot), FALSE, AsrVacLot),
    AsrVacBld = ifelse(is.na(AsrVacBld), FALSE, AsrVacBld),
    AsrTrade = ifelse(is.na(AsrTrade), FALSE, AsrTrade),
    BldgCom = ifelse(is.na(BldgCom), FALSE, BldgCom),
    BldgRes = ifelse(is.na(BldgRes), FALSE, BldgRes),
    Condemn = ifelse(is.na(Condemn), FALSE, Condemn),
    LRA_Inventory = ifelse(is.na(LRA_Inventory), FALSE, LRA_Inventory),
    VacantSurvery = ifelse(is.na(VacantSurvery), FALSE, VacantSurvery),
    PrmDemo = ifelse(is.na(PrmDemo), FALSE, PrmDemo),
    PrmBldg = ifelse(is.na(PrmBldg), FALSE, PrmBldg),
    PrmOcc = ifelse(is.na(PrmOcc), FALSE, PrmOcc),
    BoardUp = ifelse(is.na(BoardUp), 0, BoardUp),
    ForServCnt = ifelse(is.na(ForServCnt), 0, ForServCnt),
    VacReg = ifelse(is.na(VacReg), 0, VacReg),
    VacPnlty = ifelse(is.na(VacPnlty), 0, VacPnlty),
    Rmv = ifelse(is.na(Rmv), FALSE, Rmv)
  )

# Compute Vacancy Status
master %<>%
  mutate(
    VacStatus = case_when(
      !PrmDemo & !PrmOcc & (BldgCom | BldgRes) & LRA_Inventory ~ 'Vacant Building',
      !PrmDemo & !PrmOcc & VacantSurvery ~ 'Vacant Building',
      !PrmDemo & !PrmOcc & ForestryType == 'Vacant Building' & Condemn & TaxDelinq >= 5 & BoardUp > 0 ~ 'Vacant Building' ,
      !PrmDemo & !PrmOcc & ForestryType == 'Vacant Building' & TaxDelinq >= 5 ~ 'Possible Building' ,
      !PrmDemo & !PrmOcc & BoardUp > 0 ~ 'Possible Building',
      !PrmDemo & !PrmOcc & (VacReg > 0 | VacPnlty > 0) ~ 'Possible Building',
      !PrmBldg & LRA_Inventory & !BldgCom & !BldgRes ~ 'Vacant Lot',
      !PrmBldg & LRA_Inventory & PrmDemo ~ 'Vacant Lot',
      !PrmBldg & BldgVac & PrmDemo ~ 'Vacant Lot',
      !PrmBldg & AsrVacLot & ForServCnt > 0 ~ 'Vacant Lot',
      !PrmBldg & AsrVacLot & TaxDelinq >= 3 ~ 'Vacant Lot',
      !PrmBldg & AsrTrade & !BldgCom & !BldgRes & ForServCnt > 0 ~ 'Vacant Lot',
      !PrmBldg & VacantSurvery & PrmDemo ~ 'Vacant Lot',
      !PrmBldg & AsrVacLot & ForServCnt == 0 ~ 'Possible Vacant Lot',
      !PrmBldg & AsrVacLot & Church ~ 'Possible Vacant Lot',
      TRUE ~ 'Not Vacant'
    )
  ) %>%
  filter(!Rmv) %>%
  select(-Rmv)

write.csv(filter(master, VacStatus != 'Not Vacant'), file ='vacancy_estimate.csv', row.names = FALSE)

# Criteria for Classifying Vacancy

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Vacant Building ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Owned by LRA and Contains a Building
# Building Division Marked as Vacant 2017 or Later
# Forestry Marked as Vacant, Structurally Condemned, 5 Years Tax Deliquent, Boarded Up at Least Once

## FOR ALL: No Demo/Occupancy Permits (Since Jan 1 2016)
# If Demo, Vacant Lot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Possible Vacant Building ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Forestry Marked as Vacant, 5 Year Tax Delinquent
# Boarded Up at Least Once
# Vacant Building Registration Fee or Penalty Applied

## FOR ALL: No Demo/Occupancy Permits (Since Jan 1 2016)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Vacant Lot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Owned by LRA, Does not Contain a Building
# Owned by LRA, has Demo Permit
# Marked vacant by Building Division (2017 or later), had Demo Permit
# Marked vacant lot by Assessor, Forestry Maintenance
# Marked vacant lot by Assessor, 3 Years Tax Delinquent
# Parcel Categorized as Trade, No Buildings, More than one Forestry Maintenance

## FOR ALL: No Building Permits (Since Jan 1 2016)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Possible Vacant Lot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Marked as Vacant Lot by Assessor and No Maintenance
# Marked as Vacant Lot by Assessor and Owned by Religious Organization

## FOR ALL: No Building Permits (Since Jan 1 2016)

## Also, Remove all Parks, Greenways, Highway Right of Ways, and Land Use Projects (!)
