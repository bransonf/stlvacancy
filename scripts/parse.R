# Parse Downloaded Data into R Data Structures
# (This Script will only run on a Mac/*nix OS, but can be adapted to Windows using the RODBC package)
library(Hmisc)
library(foreign)
library(readxl)
library(readr)
library(sf)

tmp <- tempdir() # Should be the same as in Download.R

# Unzip all files in temp directory
files <- list.files(tmp, '\\.zip')
for (i in files) {
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
  # Check for multiple tables
  tables <- mdb.get(file.path(tmp, i), tables = TRUE)
  if(length(tables) > 1){
    for (j in tables) {
      assign(j, mdb.get(file.path(tmp, i), tables = j))
    }
  }else{
    assign(i, mdb.get(file.path(tmp, i)))
  }
  
}

# Parse All .csv
for (i in list.files(tmp, '\\.csv')) {
  assign(i, read.csv(file.path(tmp, i)))
}

# Parse All Excel Files
for (i in list.files('data', '\\.xlsx')){
  assign(i, read_xlsx(file.path('data', i)))
}

# Save entire Global Environment to Rdata (After removing some extraneous vars)
rmv <- c(grep('Cd', ls(), value = TRUE), grep('Date', ls(), value = TRUE),
         'files', 'i', 'j','tables','tmp')
rm(rmv, download, list = rmv)

# Save Par.dbf to own file
save(par.dbf, file = 'data/par.rda')
rm(par.dbf)

# Save all else
save(list = ls(), file = 'data/all.rda')
