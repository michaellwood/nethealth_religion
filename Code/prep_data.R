
# Header ------------------------------------------------------------------

# Title: Prep_data
# Author: Michael Wood
# Created: 20220908
# Last Edited: 20220908
# Purpose: Prepare survey and ego-network data for (W3) analysis


# Load packages -----------------------------------------------------------

library(here)
library(readr)
library(dplyr)


# Load data ---------------------------------------------------------------

# load ego network survey
df_netsurv <- read_csv(here('data', 'NetWorkSurvey(2-28-20).csv'))

# load basic survey
df_basicsurv <- read_csv(here('data', 'BasicSurvey(3-6-20).csv'))

#load ego-network edge list 
df_egonet_edges <- read.csv(here('data', 'AlterAlterEdgeList(1-26-20).csv'))

# Subset Data -------------------------------------------------------------

# subset the network dataframes to W3
df_netsurv <- df_netsurv |> 
  filter(wave == 'Wave3')

df_egonet_edges <- df_egonet_edges |> 
  filter(wave == 3)

# Calculate Clustering Coefficient ----------------------------------------

df_ego_alter_ties <- df_egonet_edges |> 
  filter(egoid == vertex1)

df_alter_alter_ties <- df_egonet_edges |> 
  filter(egoid != vertex1)

# calculate n (number of alters)
df_egonet_summary <- df_ego_alter_ties |> 
  count(egoid)

# calculate m (number of alter-alter ties)
df_egonet_alt_summary <- df_alter_alter_ties |> 
  count(egoid, name = 'm')

# join summary data frames 
df_egonet_summary <- left_join(df_egonet_summary, df_egonet_alt_summary, by = 'egoid')

# calculate CC
df_egonet_summary <- df_egonet_summary |> 
  mutate(cc = (m/(n*(n-1))))

# Merge data frames -------------------------------------------------------

#create combined file with all three dataframes 
df_w3_comb_data <- left_join(df_basicsurv, df_netsurv, by = 'egoid')
df_w3_comb_data <- left_join(df_w3_comb_data, df_egonet_summary)

# Export and Clean --------------------------------------------------------

# export df as an .rds in data folder
saveRDS(df_w3_comb_data, here('data', 'df_w3_comb_data.RDS'))

# remove all unnecessary objects
rm(list=setdiff(ls(), 'df_w3_comb_data'))
