FROM rocker/geospatial

LABEL maintainer="bransonf@wustl.edu"

# Set Password
ARG PW=""

# Install Dependencies
RUN apt-get update && apt-get install -y \
    mdb-tools \
    cron
    
RUN R -e "install.packages(c('cyphr','sodium','getPass'))"

# Add Files
RUN mkdir stlvacancy
COPY * stlvacancy
WORKDIR stlvacancy

# Execute
CMD ["./scripts/full.sh"]
