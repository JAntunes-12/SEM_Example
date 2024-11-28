#########################
#### 0 - Preliminary ####
#########################

# Install the package
install.packages("seminr")

# Load the package
library(seminr)

# Load data
df <- corp_rep_data
View(df) #show the data

##############################
#### 1 - Model Definition ####
##############################

##### 1.1. - Measurement Model #####

simple_mm <- constructs(composite("COMP", multi_items("comp_", 1:3)), composite("LIKE", multi_items("like_", 1:3)), composite("CUSA", single_item("cusa")), composite("CUSL", multi_items("cusl_", 1:3)))

##### 1.2. - Structural Model #####

simple_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

############################
#### 2 - Model Analysis ####
############################
  
##### 2.1. - Estimate the model #####

corp_rep_simple_model <- estimate_pls(data = df,
                                      measurement_model = simple_mm,
                                      structural_model = simple_sm,
                                      missing_value = "-99")

summary_simple_corp_rep <- summary(corp_rep_simple_model)

###### 2.1.1. - Measurement Model Analysis ######
# Indicator reliability (item loadings)
summary_simple_corp_rep$loadings

# Internal Consistency (Cronbach alpha, rohC, rohA) & Convergent Validity (AVE)
summary_simple_corp_rep$reliability

# Discriminant Validity (HTMT)
summary_simple_corp_rep$validity$htmt

###### 2.1.2. - Structural Model Analysis ######
# Collinearity Assessment
summary_simple_corp_rep$vif_antecedents


##### 2.2. - Bootstrap the model #####

boot_simple_corp_rep <- bootstrap_model(seminr_model = df,
                                        nboot = 1000,
                                        cores = NULL,
                                        seed = 123)

sum_boot_simple_corp_rep <- summary(boot_simple_corp_rep)

###### 2.2.1. - Measurement Model Analysis ######
# Indicator reliability (item loadings)
sum_boot_simple_corp_rep$bootstrapped_loadings

# Discriminant Validity (HTMT)
sum_boot_simple_corp_rep$bootstrapped_HTMT

###### 2.2.2. - Structural Model Analysis ######
# Path Analysis
sum_boot_simple_corp_rep$bootstrapped_paths

##### 2.3. - Represent the model #####
# Plot the model
plot(boot_simple_corp_rep)
