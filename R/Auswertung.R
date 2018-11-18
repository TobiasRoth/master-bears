rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(readxl)
library(arm)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read in data from Excel and change column names / data types
dat <- read_excel("Data/Datasheet_Bear_2.0.xlsx") %>% 
  transmute(day = Collection %>% as.integer,
            rep = Replication %>% as.integer,
            observer = ID,
            treatment = Setup,
            n_fish = `Number Fish`,
            n_0 = as.integer(`Contact`)  + as.integer(`Contact > 10s`),
            n_2 = as.integer(`2m`)  + as.integer(`2m > 10s`),
            n_4 = as.integer(`4m`)  + as.integer(`4m > 10s`),
            n_tot = n_0 + n_2 + n_4,
            resp_0 = as.integer(`Response Contact`),
            resp_2 = as.integer(`Response 2m`),
            resp_4 = as.integer(`Response 4m`))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check plausibility of data ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The number of fishes should always be higher than the number of fishes counted
# in the contact zone?
dat %>% filter(n_fish < n_0)

# The number of response should not be higher than the number of observations in
# the respective circle?
dat %>% filter(n_0 < resp_0)
dat %>% filter(n_2 < resp_2)
dat %>% filter(n_4 < resp_4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# First model ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# We have do decide how we want to measure the reaction of the salmons

# For example the proportion of observations in the contact zone
glmer(cbind(n_0, n_tot-n_0) ~ treatment + (1|day), family = binomial, data = dat) %>% 
  summary



