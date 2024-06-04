# Header ------------------------------------------------------------------

# Title: Prep_data
# Author: Michael Wood
# Created: 20220908
# Last Edited: 20240603
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

# Subset Data -------------------------------------------------------------

# subset the network dataframes to W3
df_netsurv <- df_netsurv |> 
  filter(wave == 'Wave3')

# Merge data frames -------------------------------------------------------

df_w3_comb_data <- left_join(df_basicsurv, df_netsurv, by = 'egoid')

#netsurv_id_list <- unique(df_netsurv$egoid)
#basicsurv_id_list <- unique(df_basicsurv$egoid)
#comb_id_list <- Reduce(intersect,list(netsurv_id_list,basicsurv_id_list))

# Filter/Subset Data -------------------------------------------------------------

# filter out yourelig == NA
df_w3_comb_data <- df_w3_comb_data |> 
  filter(!is.na(yourelig_1))

# subset to on-campus ties
df_w3_comb_data <- df_w3_comb_data %>% 
  filter(altrelucat=="Student")


#Just for testing
#df_w3_comb_data <- df_w3_comb_data %>% 
#  select(egoid, alterid, yourelig_1, altrelig)


# impute missing alter relig in cases where
# 1) the alter was reported by multiple egos
# 2) at least one ego attributed a relig to that alter
# 3) only one other relig was attributed to that alter
# This preserves 108 ties from being deleted
df_w3_comb_data <- df_w3_comb_data %>% 
  mutate(dupe_alter=
           duplicated(df_w3_comb_data["alterid"]))

df_alter_dupes <- df_w3_comb_data %>% 
  filter(dupe_alter == TRUE)

list_alter_dupes <- unique(df_alter_dupes$alterid)

df_alter_dupes <- df_w3_comb_data %>% 
  filter(alterid %in% list_alter_dupes)

df_alter_dupes <- df_alter_dupes %>%
  group_by(alterid) %>%
  mutate(n_rels = n_distinct(altrelig))

df_alter_dupes <- df_alter_dupes %>% 
  filter(n_rels == 2)

list_na_alter <- df_alter_dupes %>% 
  filter(is.na(altrelig))

list_na_alter <- list_na_alter$alterid

df_alter_dupes <- df_alter_dupes %>%
  filter(alterid %in% list_na_alter)

df_alter_dupes <- df_alter_dupes %>% 
  group_by(alterid) %>%
  mutate(altrelig = unique(altrelig[!is.na(altrelig)]))

df_imputed_alts <- df_alter_dupes %>%
  select(alterid, altrelig)

df_imputed_alts <- distinct(df_imputed_alts)

df_imputed_alts <- rename(df_imputed_alts, altrelig_i = altrelig)

df_w3_comb_data <- left_join(df_w3_comb_data, df_imputed_alts, by = "alterid")

df_w3_comb_data$altrelig <- ifelse(is.na(df_w3_comb_data$altrelig), df_w3_comb_data$altrelig_i, df_w3_comb_data$altrelig)

rm(df_imputed_alts, df_alter_dupes, list_alter_dupes, list_na_alter)


# filter out altrelig == NA
df_w3_comb_data <- df_w3_comb_data |> 
  filter(!is.na(altrelig))


# Create Proportion Measures ----------------------------------------------

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

# calculate EI Index
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) |> 
  mutate(ei = (n_diffrelig-n_samerelig)/(degree))


# Calculate Normalized EI Indices -----------------------------------------


#create vars for the denominators
table(df_basicsurv$yourelig_1)
e_cath <- 89+33+71
i_cath <- 525-1
e_none <- 525+33+71
i_none <- 89-1
e_other<- 525+89+71
i_other<- 33-1
e_prot <- 525+89+33
i_prot <- 71-1




df_w3_comb_data <- df_w3_comb_data %>% 
  mutate(
    e_denominator = case_when(
      yourelig_1 == "Catholic" ~ e_cath,
      yourelig_1 == "No Religion" ~ e_none,
      yourelig_1 == "Other Religion" ~ e_other,
      yourelig_1 == "Protestant" ~ e_prot,
      TRUE ~ 0
    ))

df_w3_comb_data <- df_w3_comb_data %>% 
  mutate(
    i_denominator = case_when(
      yourelig_1 == "Catholic" ~ i_cath,
      yourelig_1 == "No Religion" ~ i_none,
      yourelig_1 == "Other Religion" ~ i_other,
      yourelig_1 == "Protestant" ~ i_prot,
      TRUE ~ 0
    ))

# calculate normalized EI Index
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) |> 
  mutate(ei_norm = 
           ((n_diffrelig/e_denominator)-(n_samerelig/i_denominator))/((n_diffrelig/e_denominator)+(n_samerelig/i_denominator)))


# check mean ei_norm by group
# df_w3_comb_data %>%
#   group_by(yourelig_1) %>%
#   summarise_at(vars(ei_norm), list(name = mean))



# Simulation Check --------------------------------------------------------

#Function to create many simulations and aggregate the average EI Indices for each group
get_simulated_ei <- function(data){
  df_simulated <- data
  df_simulated <- df_simulated %>% 
    select(egoid, yourelig_1, alterid, altrelig)
  
  #Set the choices of religious identities
  religions <- c("Catholic", "NoReligion", "OtherReligion", "Protestant")
  
  #Set weights equal to the distribution of religious identities
  weights <- c(0.73119777, 0.12395543, 0.04596100, 0.09888579)
  
  #Set size equal to the length of the df
  size = nrow(df_simulated)
  
  #Replace altrelig with a weighted random sample
  weighted_sample <- sample(religions, size = size, prob = weights, replace = TRUE)
  df_simulated$altrelig <- weighted_sample
  
  df_simulated <- df_simulated %>% 
    mutate(
      same_relig = case_when(
        yourelig_1 == "Catholic" & altrelig == "Catholic" ~ 1,
        yourelig_1 == "No Religion" & altrelig == "NoReligion" ~ 1,
        yourelig_1 == "Other Religion" & altrelig == "OtherReligion" ~ 1,
        yourelig_1 == "Protestant" & altrelig == "Protestant" ~ 1,
        TRUE ~ 0
      ))
  
  # count the number of same-relig alters
  df_simulated <- df_simulated %>% 
    group_by(egoid) %>% 
    mutate(n_samerelig = sum(same_relig == 1))
  
  #count the degree
  df_simulated <- df_simulated %>%  
    group_by(egoid) %>% 
    mutate(degree = n())
  
  # count the number of diff-relig alters
  df_simulated <- df_simulated %>%  
    group_by(egoid) %>% 
    mutate(n_diffrelig = degree-n_samerelig)
  
  # calculate EI Index
  df_simulated <- df_simulated |> 
    group_by(egoid) |> 
    mutate(ei = (n_diffrelig-n_samerelig)/(degree))
  
  #subset and return
  df_simulated_sub <- df_simulated |> 
    select(egoid, yourelig_1, ei)
  df_simulated_sub <- distinct(df_simulated_sub)
  
  
  df_means <- df_simulated_sub %>%
    group_by(yourelig_1) %>%
    summarise_at(vars(ei), list(name = mean))
  return(df_means[,2])
}

test_df <- get_simulated_ei(df_w3_comb_data)


for (i in 1:92){
  test_df <- cbind(test_df, get_simulated_ei(df_w3_comb_data))
}



simulated_avg_ei <- test_df
simulated_avg_ei <- simulated_avg_ei[,-3]
# export df as an .rds in data folder
saveRDS(simulated_avg_ei, here('data', 'simulated_avg_ei_100.RDS'))


library(tidyr)
simulated_avg_ei <- gather(simulated_avg_ei, key = sim_num, value = percent, -yourelig_1)

# Basic density plot in ggplot2
library(ggplot2)
ggplot(simulated_avg_ei, aes(x = percent, colour = yourelig_1, fill = yourelig_1)) +
  geom_density(alpha = .5)

ggplot(simulated_avg_ei, aes(x=yourelig_1, y=percent, fill=yourelig_1)) + 
  geom_boxplot()

ggplot(simulated_avg_ei, aes(x=yourelig_1, y=percent, fill=yourelig_1)) + 
  geom_violin(alpha=.8)+
  geom_point(data=df_observed_avg_ei)+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  xlab("Ego Religion")+
  ylab("Average EI Index")+
  theme_minimal()+
  scale_y_continuous(breaks = seq(-1, 1, by = .1))

df_avg_sim_ei <- simulated_avg_ei %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(percent), list(avg_percentage = mean))

df_avg_ei_comb <- left_join(df_avg_sim_ei, df_observed_avg_ei)
df_avg_ei_comb$diff <- abs(abs(df_avg_ei_comb$avg_percentage) - abs(df_avg_ei_comb$percent))

#Copy the observed df and subset. We copy to preserve the degree, then we
#randomize the alter's religious identity
df_simulated <- df_w3_comb_data
df_simulated <- df_simulated %>% 
  select(egoid, yourelig_1, alterid, altrelig, e_denominator, i_denominator)

#Set the choices of religious identities
table(df_simulated$altrelig)
religions <- c("Catholic", "NoReligion", "OtherReligion", "Protestant")

#Set weights equal to the distribution of religious identities
prop.table(table(df_basicsurv$yourelig_1))
weights <- c(0.73119777, 0.12395543, 0.04596100, 0.09888579)

#Set size equal to the length of the df
size = nrow(df_simulated)

#Replace altrelig with a weighted random sample
weighted_sample <- sample(religions, size = size, prob = weights, replace = TRUE)
df_simulated$altrelig <- weighted_sample

df_simulated <- df_simulated %>% 
  mutate(
    same_relig = case_when(
      yourelig_1 == "Catholic" & altrelig == "Catholic" ~ 1,
      yourelig_1 == "No Religion" & altrelig == "NoReligion" ~ 1,
      yourelig_1 == "Other Religion" & altrelig == "OtherReligion" ~ 1,
      yourelig_1 == "Protestant" & altrelig == "Protestant" ~ 1,
      TRUE ~ 0
    ))

# count the number of same-relig alters
df_simulated <- df_simulated %>% 
  group_by(egoid) %>% 
  mutate(n_samerelig = sum(same_relig == 1))

#count the degree
df_simulated <- df_simulated %>%  
  group_by(egoid) %>% 
  mutate(degree = n())

# count the number of diff-relig alters
df_simulated <- df_simulated %>%  
  group_by(egoid) %>% 
  mutate(n_diffrelig = degree-n_samerelig)

# calculate EI Index
df_simulated <- df_simulated |> 
  group_by(egoid) |> 
  mutate(ei = (n_diffrelig-n_samerelig)/(degree))



# calculate normalized ei
df_simulated <- df_simulated %>% 
  group_by(egoid) %>% 
  mutate(ei_norm = 
           ((n_diffrelig/e_denominator)-(n_samerelig/i_denominator))/((n_diffrelig/e_denominator)+(n_samerelig/i_denominator)))

df_simulated_sub <- df_simulated |> 
  select(egoid, yourelig_1, degree, ei_norm, ei, n_samerelig)

df_simulated_sub <- distinct(df_simulated_sub)  

df_simulated_sub %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei_norm), list(name = mean))

df_simulated_sub %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei), list(name = mean))

model <- lm(ei_norm ~ yourelig_1, df_simulated_sub)
summary(model)







# check mean ei by group
df_simulated_sub %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei), list(name = mean))

df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei_norm), list(name = mean))

#compare altrelig distribution for each group
df_w3_sub <- df_w3_comb_data %>% 
  select(egoid, alterid, yourelig_1, altrelig)

df_catholic <- df_w3_sub %>%
  filter(yourelig_1 == "Catholic")
prop.table(table(df_catholic$altrelig))
df_cath_alters_perc <- as.data.frame(prop.table(table(df_catholic$altrelig)))
df_cath_alters_perc <- rename(df_cath_alters_perc, altrelig = Var1, cath_perc = Freq)

df_norelig <- df_w3_sub %>% 
  filter(yourelig_1 == "No Religion")
prop.table(table(df_norelig$altrelig))
df_norelig_alters_perc <- as.data.frame(prop.table(table(df_norelig$altrelig)))
df_norelig_alters_perc <- rename(df_norelig_alters_perc, altrelig = Var1, norelig_perc = Freq)

df_othrelig <- df_w3_sub %>% 
  filter(yourelig_1 == "Other Religion")
prop.table(table(df_othrelig$altrelig))
df_othrelig_alters_perc <- as.data.frame(prop.table(table(df_othrelig$altrelig)))
df_othrelig_alters_perc <- rename(df_othrelig_alters_perc, altrelig = Var1, othrelig_perc = Freq)


df_prot <- df_w3_sub %>% 
  filter(yourelig_1 == "Protestant")
prop.table(table(df_prot$altrelig))
df_prot_alters_perc <- as.data.frame(prop.table(table(df_prot$altrelig)))
df_prot_alters_perc <- rename(df_prot_alters_perc, altrelig = Var1, prot_perc = Freq)

prop.table(table(df_simulated$altrelig))
prop.table(table(df_w3_comb_data$altrelig))

rm(baseline_df)
baseline_df <- as.data.frame(prop.table(table(df_basicsurv$yourelig_1)))
baseline_df <- rename(baseline_df, altrelig = Var1, baseline_perc = Freq)
library(plyr)

baseline_df$altrelig <- revalue(baseline_df$altrelig, c("No Religion" = "NoReligion", "Other Religion" = "OtherReligion"))

df_alters_relig_perc <- left_join(baseline_df, df_cath_alters_perc)
df_alters_relig_perc <- left_join(df_alters_relig_perc, df_norelig_alters_perc)
df_alters_relig_perc <- left_join(df_alters_relig_perc, df_othrelig_alters_perc)
df_alters_relig_perc <- left_join(df_alters_relig_perc, df_prot_alters_perc)

df_alters_relig_perc$cath_diff <- df_alters_relig_perc$cath_perc - df_alters_relig_perc$baseline_perc
df_alters_relig_perc$none_diff <- df_alters_relig_perc$norelig_perc - df_alters_relig_perc$baseline_perc
df_alters_relig_perc$other_diff <- df_alters_relig_perc$othrelig_perc - df_alters_relig_perc$baseline_perc
df_alters_relig_perc$prot_diff <- df_alters_relig_perc$prot_perc - df_alters_relig_perc$baseline_perc

df_sub <- df_alters_relig_perc %>% 
  select(altrelig, baseline_perc, cath_diff, none_diff, other_diff, prot_diff)

library(ggplot2)

ggplot(df_sub, aes(x = altrelig, y = cath_diff)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) + # Remove the legend
  xlab("Group") +
  ylab("Value")+
  coord_flip()

df_sub_long <- df_sub
df_sub_long <- df_sub_long %>% 
  select(-baseline_perc)
df_sub_long <- gather(df_sub_long,key = "ego_relig", value = "difference", -altrelig)

df_sub_long$ego_relig <- as.factor(df_sub_long$ego_relig)
df_sub_long$ego_relig <- revalue(df_sub_long$ego_relig, c("cath_diff" = "Catholic Egos",
                                                            "none_diff" = "No Relig Egos",
                                                            "other_diff"= "Other Relig Egos",
                                                            "prot_diff" = "Protestant Egos"))

p <- ggplot(df_sub_long, aes(x = altrelig, y = difference))+
  geom_bar(stat = "identity",
           show.legend = FALSE)+
  coord_flip()+
  xlab("Alter's Religion")+
  ylab("Preference toward group beyond expected")
p + facet_grid(rows = vars(ego_relig))


# Checking stuff ----------------------------------------------------------

df_norelig <- df_w3_comb_data %>% 
  filter(yourelig_1 == "No Religion")

df_norelig <- df_norelig %>% 
  select(egoid, yourelig_1, altrelig, ei, ei_norm, degree, n_samerelig, n_diffrelig)


df_catholic <- df_w3_comb_data %>% 
  filter(yourelig_1 == "Catholic")

df_catholic <- df_catholic %>% 
  select(egoid, yourelig_1, altrelig, ei, ei_norm, degree, n_samerelig, n_diffrelig)

# calculate the percent of non-religious alters
df_w3_comb_data <- df_w3_comb_data |> 
  group_by(egoid) %>% 
  mutate(perc_same_relig = n_samerelig/degree)


# check mean ei_norm by group
df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei), list(name = mean))


# # Create baseline EI Indices ----------------------------------------------
# 
# table(df_basicsurv$yourelig_1)
# 
# base_ei_cath <- ((89+33+71)-525)/718
# base_ei_none <- ((525+33+71)-89)/718
# base_ei_other <- ((525+89+71)-33)/718
# base_ei_prot <- ((525+89+33)-71)/718  
# 
# 
# # Create adjusted EI indices ----------------------------------------------
# 
# df_w3_comb_data <- df_w3_comb_data %>% 
#   mutate(
#     baseline_ei = case_when(
#       yourelig_1 == "Catholic" ~ base_ei_cath,
#       yourelig_1 == "No Religion" ~ base_ei_none,
#       yourelig_1 == "Other Religion" ~ base_ei_other,
#       yourelig_1 == "Protestant" ~ base_ei_prot,
#       TRUE ~ 0
#       ))
# 
# #This math isn't right
# df_w3_comb_data <- df_w3_comb_data %>% 
#   mutate(adjusted_ei = ei - baseline_ei)
# 
# 
# 
# 

# Models ------------------------------------------------------------------


df_w3_model_data <- df_w3_comb_data |> 
  select(egoid, yourelig_1, degree, ei, ei_norm, n_samerelig)

df_w3_model_data <- distinct(df_w3_model_data)  

model <- lm(ei_norm ~ yourelig_1 + degree, df_w3_model_data)
summary(model)

plot(df_w3_model_data$ei, df_w3_model_data$ei_norm)

table(df_w3_model_data$yourelig_1)

cor.test(df_w3_model_data$ei, df_w3_model_data$ei_norm)

df_w3_model_data <- df_w3_model_data %>% 
  mutate(
    all_ingroup =
      case_when(ei_norm == -1 ~ 1,
      TRUE ~ 0))

df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(all_ingroup), list(name = mean))

df_observed_avg_ei <- df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei), list(name = mean))
df_observed_avg_ei <-  rename(df_observed_avg_ei, percent = name)

df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei_norm), list(name = mean))

df_w3_model_data %>%
  group_by(yourelig_1) %>%
  summarise_at(vars(ei), list(name = mean))



model <- lm(all_ingroup ~ yourelig_1, df_w3_model_data)
summary(model)

hist(df_w3_model_data$ei_norm)

table(df_catholic$altrelig)
prop.table(table(df_catholic$altrelig))
prop.table(table(df_w3_model_data$yourelig_1))


# I have observed ties and simulated ties, and I want to see whether the observed ties are more clustered
# based on what we would expect by chance. I also want to see whether some groups are more clustered than others

