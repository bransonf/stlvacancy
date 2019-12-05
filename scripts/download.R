# Download Sources of Data

# Download to Temp Function
tmp <- tempdir()
download <- function(url, temp = tmp){
  download.file(url, file.path(temp, basename(url)))
}

# Parcels (Shape, used for Key: HANDLE)
download('https://www.stlouis-mo.gov/data/upload/data-files/prcl_shape.zip')

# Parcels (Information)
# Includes Tax Billing Information
download('https://www.stlouis-mo.gov/data/upload/data-files/prcl.zip')
download('https://www.stlouis-mo.gov/data/upload/data-files/par.zip')

# LRA Inventory
download('https://www.stlouis-mo.gov/data/upload/data-files/lra_public.zip')

# Building Inspections
# Includes Condemnations and Vacant buildings
download('https://www.stlouis-mo.gov/data/upload/data-files/bldginsp.zip')

# Building Permits
# Includes Occupancy and Demolition
download('https://www.stlouis-mo.gov/data/upload/data-files/prmbdo.zip')

# Property Maintenance Billing (Using Forestry Property Maintenance)
download('https://www.stlouis-mo.gov/data/upload/data-files/forestry-maintenance-properties.csv')
