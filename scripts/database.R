# Fit Vacancy Data to Schema
schema <- transmute(master,
            id = as.integer()
        )

# Connect to Database and Insert Data to Vacancy Table
library(DBI);library(RPostgres)
source('https://bransonf.com/scripts/encryption.R')
creds <- decrypt_yaml('creds.yml.encrypted', Sys.getenv('pass'))

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = creds$database,
                      host = creds$host,
                      port = 5432,
                      user = creds$username,
                      password = creds$password)
# CAUTION on Overwrite, TODO Implement Append Instead
dbWriteTable(con, 'vacancy', schema, overwrite = TRUE)

# Send Update Confirmation
## TODO
