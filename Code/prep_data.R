
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
  filter(wave == "Wave3")

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
  count(egoid, name = "m")

df_egonet_summary <- left_join(df_egonet_summary, df_egonet_alt_summary, by = "egoid")

# calculate CC
df_egonet_summary <- df_egonet_summary |> 
  mutate(cc = (m/(n*(n-1))))

# calculate 

# Clustering coeff = m/n(n-1), 
# n = number of alters
# m = Number of alter-to-alter edges


# egoid: person who reported the tie 
# vertex ids the two alters of the ego who the ego reported as having a tie.  
# If the first vertex id is the same as the egoid, then this is an ego-alter tie.  
# 
# To DO: Compute for each ego the number of alters (degree) by collapsing over the ego-alter ties to get a count.  
# Then delete the ego-alter ties
# and collapse over alter-alter pairs to count the number of alter-alter ties that ego identified in that wave.  
# Then the Clustering coeff = density of ego network = E/n(n-1), where N is size of ego net and E is the count of the number of alter-alter edges names by an ego in that wave.
# 
