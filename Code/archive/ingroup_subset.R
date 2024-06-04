
# Header ------------------------------------------------------------------


# Subset to Ingroup Ties Only ---------------------------------------------

df_w3_comb_sub <- df_w3_comb_data |> 
  filter(same_relig==1)

df_w3_comb_sub <- df_w3_comb_sub |> 
  add_count(egoid, name = "n_in")

df_w3_comb_sub <- df_w3_comb_sub |> 
  add_count(egoid, name = "m_in")

df_w3_alter_relig <- df_w3_comb_data |> 
  select(egoid, alterid, same_relig)

df_w3_alter_relig <- df_w3_alter_relig |> 
  filter(same_relig==1)

df_w3_comb_relig <- df_w3_comb_data |> 
  select(egoid, alterid, yourelig_1, altrelig)

library(tidyr)

df_ego_relig <- df_w3_comb_relig |> 
  select(egoid, yourelig_1) |> 
  distinct()

df_alt_relig <- df_w3_comb_relig |> 
  select(alterid, altrelig) |> 
  distinct()


df_alter_alter_relig <- left_join(df_w3_alter_relig, df_alter_alter_ties,
                                  by = c('egoid'='egoid',
                                         'alterid'='vertex1'))

df_alter_alter_relig <- left_join(df_w3_alter_relig, df_alter_alter_relig,
                                  by = c('egoid'='egoid',
                                         'alterid'='vertex2'))

df_alter_alter_relig <- drop_na(df_alter_alter_relig)

#need a df that is egoid, same-relig-alter

df_w3_alter_relig_comb <- left_join(df_w3_alter_relig, df_alter_alter_ties,
                                    by = c('egoid' = 'egoid', 'alterid' = 'vertex1'))



df_m_in <- df_alter_alter_relig |> 
  count(egoid, name = "m_in")

df_w3_comb_sub <- left_join(df_w3_comb_sub, df_m_in, by = 'egoid')

# the previous steps yield NA if there are no alter-alter ties. This fixes that.
df_w3_comb_sub$m_in <- df_w3_comb_sub$m_in |> 
  tidyr::replace_na(0)

df_w3_comb_sub <- df_w3_comb_sub |> 
  mutate(cc_in = (m_in/((n_in*(n_in-1))/2)))


df_w3_comb_sub <- df_w3_comb_sub |> 
  select(egoid, yourelig_1, degree, n_norelig, perc_none, gender_1, 
         perc_samegender, cc_in, ei)

df_w3_comb_sub <- distinct(df_w3_comb_sub)  


# df_w3_comb_data <- df_w3_comb_data |> 
#   filter(!is.na(cc))

model <- lm(cc_in ~ yourelig_1 + degree, df_w3_comb_sub)
summary(model)

model_glm <- glm(cc_in ~ yourelig_1 + degree,
                 data = df_w3_comb_sub,
                 family = binomial)
summary(model_glm)

library(lmtest)
library(sandwich)
install.packages("frm")

se_glm_robust = coeftest(model_glm, vcov = vcovHC(model_glm, type="HC"))
se_glm_robust


