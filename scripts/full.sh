#!/bin/bash

Rscript -e "source('download.R')" 
Rscript -e "source('parse.R')"
Rscript -e "source('combine.R')"