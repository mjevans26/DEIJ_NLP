library(dplyr)
library(stringr)
library(tidyr)

dat <- read.csv(file = 'data/LIsteningTourResponses.csv', header = TRUE, stringsAsFactors = FALSE)
source(file ='functions.R')

# 1. TEXT CLEANING

# eliminate/expand contractions
dat$Statement <- sapply(dat$Statement, fix_contractions)

#' function to remove special characters
remove_spChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# remove special characters
dat$Statement <- sapply(dat$Statement, remove_spChars)

# convert everything to lower case
dat$Statement <- sapply(dat$Statement, tolower)

# eliminate by-product double spaces (may need to run twice)
dat$Statement <- str_replace(dat$Statement, "  ", " ")

# we will nedd a row number column to ID individual statements later
dat <- mutate(dat, text = Statement, Statement = row_number())
# data is clean, save the data
saveRDS(dat, file = 'data/data.rds')

