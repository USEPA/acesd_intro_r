# NE NERRS Analysis

################################################################################
# Purpose: The purpose of this document is to serve as motivating example (R
# and Open data are cool!), but will also serve to structure the rest of this
# workshop in that we will see how to work with and visualize data in R, combine
# code and documentation with R Markdown, and introduce The Tidyverse
# (https://tidyverse.org) which is an opinionated (but effective) way to think
# about organizing and analyzing data in R.  To accomplish this, we will be 
# using data from Wells, Great Bay, Waquoit Bay, and Narragansett Bay National 
# Estuarine Research Reserves.  These data provide a nice water quality relevant
# example and you all should be familiar with it to help sort out any issues we 
# might run into.

################################################################################
# Install packages, if needed: This is fancier than it normally needs to be.  It
# checks to make sure that packages are installed and installs if they aren't
# then loads it up.

pkgs <- c("ggplot2", "dplyr", "readr", "plotly", "SWMPr")
for(i in pkgs){
  if(!i %in% installed.packages()){
    install.packages(i)
  }
}

################################################################################
# Load up packages in R session

library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(lubridate)
library(stringr)
library(tidyr)

################################################################################
# Get Data: The data we need is available from the National Aquatic Resource
# Survey's website. We 

nla_2017_chem <- read_csv("../data/nla_2017_water_chemistry_chla-data.csv", 
                          guess_max = 23000)

################################################################################
# Manipulate Data: Let's tidy up this dataset by making all my column names
# lower case, selecting out just the columns we want, and adding some individual
# columns for day, month, and year.

nla_2017_chem_clean <- nla_2017_chem |>
  rename_all(tolower) |>
  select(uid:visit_no, state, analyte, result, nars_flag) |>
  filter(is.na(nars_flag), visit_no == 1) |>
  select(-nars_flag) |>
  mutate(date_col = dmy(date_col), 
         year = year(date_col), 
         month = month(date_col), 
         day = day(date_col),
         analyte = tolower(analyte),
         result = as.numeric(result)) |>
  select(uid:date_col, year, month, day, state:result)

nla_2017_chem_clean

################################################################################
# Visualize Data: Next step is to visualize the data.  Let's look at state 
# averages of total nitrogen, total phosphorus, and chlorophyll.

nla_tn_tp_chla_gg <- nla_2017_chem_clean |>
  pivot_wider(names_from = analyte, values_from = result) |>
  group_by(state) |>
  summarize(mean_tn = mean(ntl, na.rm = TRUE),
            mean_tp = mean(ptl, na.rm = TRUE),
            mean_chla = mean(chla, na.rm = TRUE)) |>
  ungroup()|> 
  ggplot() +
  aes(x=mean_tn,y=mean_tp, color = mean_chla, size = mean_chla, group = state) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Mean Total Nitrogen", y = "Mean Total Phosphorus", 
       title = "Comparison of Nitrogen,  Phosphorus, and Chlorophyll") +
  scale_color_continuous(low = "springgreen", high = "darkgreen")

  
ggplotly(nla_tn_tp_chla_gg)



