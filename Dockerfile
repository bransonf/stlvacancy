FROM rocker/geospatial:3.6.1

LABEL maintainer="Branson Fox <bransonf@wustl.edu>"

# Arg for Password
ARG pass=''

# Install Dependencies
RUN apt-get update && apt-get install -y \
    mdb-tools
    
RUN R -e "install.packages(c('sodium','cyphr','getPass'))"

# Add Files (Scripts, Data, Creds)
RUN mkdir /scripts
COPY scripts/* /scripts/
COPY data/forestry_services.csv /scripts/
COPY data/Forestry19.csv.encrypted /scripts/
COPY creds.yml.encrypted /scripts/

WORKDIR /scripts

# Execute
CMD ["R", "-e", "source('full.R')"]
