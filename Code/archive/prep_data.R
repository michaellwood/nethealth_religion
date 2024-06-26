
# Header ------------------------------------------------------------------

# Title: Prep_data
# Author: Michael Wood
# Created: 20220908
# Last Edited: 20220915
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

# the previous steps yield NA if there are no alter-alter ties. This fixes that.
df_egonet_summary$m <- df_egonet_summary$m |> 
  tidyr::replace_na(0)

# calculate CC
df_egonet_summary <- df_egonet_summary |> 
  mutate(cc = (m/((n*(n-1))/2)))

# Merge data frames -------------------------------------------------------

#create combined file with all three dataframes 
df_w3_comb_data <- left_join(df_basicsurv, df_netsurv, by = 'egoid')
df_w3_comb_data <- left_join(df_w3_comb_data, df_egonet_summary)


# Create Proportion Measures ----------------------------------------------

# filter out altrelig == NA
df_w3_comb_data <- df_w3_comb_data |> 
  filter(!is.na(altrelig))

# filter out yourelig == NA
df_w3_comb_data <- df_w3_comb_data |> 
  filter(!is.na(yourelig_1))

# create same-gender variable
df_w3_comb_data <- df_w3_comb_data |> 
  mutate(
    same_gender = case_when(
      gender_1 == "Female" & altsex == "Female" ~ 1,
      gender_1 == "Male" & altsex == "Male" ~ 1,
      TRUE ~ 0
    ))

table(df_w3_comb_data$altrelig)

# create same-relig variable
df_w3_comb_data <- df_w3_comb_data |> 
  mutate(
    same_relig = case_when(
      yourelig_1 == "Catholic" & altrelig == "Catholic" ~ 1,
      yourelig_1 == "No Religion" & altrelig == "NoReligion" ~ 1,
      yourelig_1 == "Other Religion" & altrelig == "OtherReligion" ~ 1,
      yourelig_1 == "Protestant" & altrelig == "Protestant" ~ 1,
      TRUE ~ 0
    ))

# subset to on-campus ties
df_w3_comb_data <- df_w3_comb_data %>% 
  filter(altrelucat=="Student")

# count the number of non-religious alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(n_norelig = sum(altrelig == "NoReligion"))
table(df_w3_comb_data$n_norelig)

# count the number of same-gender alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(n_samegender = sum(same_gender == 1))

# count the number of same-relig alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(n_samerelig = sum(same_relig == 1))

#count the degree
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(degree = n())

# count the number of diff-relig alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(n_diffrelig = degree-n_samerelig)

prop.table(table(df_w3_comb_data$yourelig_1, df_w3_comb_data$altrelu),1)
prop.table(table(df_w3_comb_data$yourelig_1, df_w3_comb_data$altrelig),1)
test <- df_w3_comb_data %>% 
  filter(altrelucat=="Student")
prop.table(table(test$yourelig_1, test$altrelig),1)

# calculate the percent of non-religious alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(perc_none = n_norelig/degree)

# calculate the percent of same-gender alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(perc_samegender = n_samegender/degree)

# calculate the EI index
# E-I/(E+I)
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) |> 
  mutate(ei = (n_diffrelig-n_samerelig)/(degree))

(193-525)/718

table(df_w3_comb_data$yourelig_1)

# Export and Clean --------------------------------------------------------

# export df as an .rds in data folder
saveRDS(df_w3_comb_data, here('data', 'df_w3_comb_data.RDS'))

# remove all unnecessary objects
rm(list=setdiff(ls(), c('df_w3_comb_data','df_alter_alter_ties')))
