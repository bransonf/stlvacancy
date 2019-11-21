# Implement the Final Dataset
library(RPostgreSQL)
library(cyphr); library(yaml)

# Credentials for Admin Access are Encrypted
cyphr::decrypt_file('creds.yaml', )

drvr <- dbDriver('PostgreSQL')
connection <- dbConnect(drvr, dbname = ,
                        host = ,
                        port = ,
                        user =
                        password = )