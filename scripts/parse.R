# Parse Downloaded Data into R Data Structures
# (This Script will only run on a Mac/*nix OS, but can be adapted to Windows using the RODBC package)
library(Hmisc)   # for .mdb
library(foreign) # for .dbf
library(readxl)  # for .xls*
library(readr)   # for .csv
library(sf)      # for .shp

tmp <- tempdir() # Should be the same as in Download.R

# Unzip all files in temp directory
for (i in list.files(tmp, '\\.zip')) {
  unzip(file.path(tmp, i), exdir = tmp)
}

# Parse All .dbf
for (i in list.files(tmp, '\\.dbf')){
  assign(i, read.dbf(file.path(tmp, i)))
}

# Parse All .shp
for (i in list.files(tmp, '\\.shp')) {
  assign(i, st_read(file.path(tmp, i)))
}

# Parse All .mdb
for (i in list.files(tmp, '\\.mdb')){
  # Parse Each Table Individually
  tables <- mdb.get(file.path(tmp, i), tables = TRUE)
    for (j in tables) {
      assign(j, mdb.get(file.path(tmp, i), tables = j))
    }
}

# Parse All .csv
for (i in list.files(tmp, '\\.csv')) {
  assign(i, read.csv(file.path(tmp, i)))
}
# Rename Forestry Dataset
forestry_maintenance_properties <- `forestry-maintenance-properties.csv`

# Parse All Excel Files
for (i in list.files('data', '\\.xlsx')){
  assign(i, read_xlsx(file.path('data', i)))
}

# Load Encrypted Forestry Data
source('https://bransonf.com/scripts/encryption.R')
forestry_services = decrypt_csv('data/Forestry19.csv.encrypted', Sys.getenv('PW'))
for_codes = readr::read_csv('data/forestry_services.csv')

# Save Parsed Data
save(prcl.dbf, Prcl, BldgCom, BldgRes, Condemn, dbo_vw_public_inventory,
     forestry_maintenance_properties, VacBldg, for_codes, forestry_services,
     PrclREAR, PrmDemo, PrmBldg, PrmOcc,
     file = 'data/parsed.rda')
