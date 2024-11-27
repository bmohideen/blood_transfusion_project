# Team project script

# Install required packages
library(readxl)
library(dplyr)
library(tableone)
library(glmnet)

#########################################
##### Loading the Data #####
#########################################


# load original data
data_orig <- read_excel("transfusiondata.xlsx")

# rename data 
data <- data_orig %>% 
  rename(
       study_id = "STUDY ID #",
       tx_db_id = "TX DB ID", 
       or_date = "OR Date", 
       gender_male = "Gender (male)",
       aat_deficiency = "alpha1-Antitrypsin Deficiency",
       cys_fib = "Cystic Fibrosis",
       ipah = "Idiopathic Pulmonary Hypertension",
       ild = "Interstitial Lung Disease",
       pulm_other = "Pulm_Other",
       cad = "Coronary Artery Disease",
       t1d = "Diabetes (insulin)",
       t2d = "Diabetes (diet/OHGs)",
       gerd_pud = "GERD/PUD",
       renal_fail = "Renal Failure",
       stroke = "Stroke/CVA",
       liver_disease = "Liver Disease",
       thyroid_disease = "Thyroid Disease",
       first_transplant = "First Lung Transplant",
       redo_transplant = "Redo Lung Transplant", 
       dcd_dbd = "DCD vs DBD",
       evlp = "ExVIVO Lung Perfusion",
       preop_ecls = "Preoperative ECLS",
       las = "LAS score",
       intraop_ecls = "Intraoperative ECLS",
       protamine_yes = "Protamine (Y=1 N=0)", 
       intra_albumin_5perc_mL = "Intra_Albumin 5% (mL)", 
       intra_crystalloid_ml = "Intra_Crystalloid (mL)",
       intra_cell_saver_returned_ml = "Intra_Cell Saver returned (mL)",
       intra_plasma = "Intra_Fresh Frozen Plasma",
       intra_packed_cells = "Intra_Packed Cells",
       intra_PCC_octaplex = "Intra_PCC/Octaplex",
       icu_stay = "Duration of ICU Stay (days)",
       rbc_0_24 = "RBC 0-24hrs",
       rbc_24_48 = "RBC 24-48hrs",
       rbc_48_72 = "RBC 48-72hrs",
       rbc_72_tot = "RBC 72hr Total",
       ffp_0_24 = "FFP 0-24hrs",
       ffp_24_48 = "FFP 24-48hrs",
       ffp_48_72 = "FFP 48-72hrs",
       ffp_72_tot = "FFP 72hr Total",
       plt_0_24 = "Plt 0-24hrs",
       plt_24_48 = "Plt 24-48hrs",
       plt_48_72 = "Plt 48-72hrs",
       plt_72_tot = "Plt 72hr Total",
       cryo_0_24 = "Cryo 0-24hrs",
       cryo_24_48 = "Cryo 24-48hrs",
       cryo_48_72 = "Cryo 48-72hrs",
       cryo_72_tot = "Cryo 72hr Total",
       tot_24_rbc = "Total 24hr RBC",
       massive_transfusion = "Massive Transfusion"
       )

# Select the relevant data
data_use <- data %>%
  select(Type, study_id, tx_db_id, or_date, gender_male, aat_deficiency, cys_fib, ipah, 
         ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud, renal_fail, stroke, 
         liver_disease, thyroid_disease, first_transplant, redo_transplant, evlp, preop_ecls,
         las, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Fibrinogen, Pre_Creatinine,
         intraop_ecls, ECLS_ECMO, ECLS_CPB, intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
         icu_stay, DEATH_DATE, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS,
         rbc_0_24, rbc_24_48, rbc_48_72, rbc_72_tot, ffp_0_24, ffp_24_48, ffp_48_72, ffp_72_tot,
         plt_0_24, plt_24_48, plt_48_72, plt_72_tot, cryo_0_24, cryo_24_48, cryo_48_72, cryo_72_tot,
         tot_24_rbc, massive_transfusion, Age, BMI) %>%
  mutate(type = if_else(Type == "Bilateral", "Double", "Single"))

# Converting to Proper Binary structure (TRUE/Y = 1, FALSE/N = 0) 

# Identify columns with binary "TRUE"/"FALSE" values stored as character
binary_chr_cols <- sapply(data_use, function(col) all(col %in% c("TRUE", "FALSE")))

# Convert these columns to numeric (0/1)
data_use[, binary_chr_cols] <- lapply(data_use[, binary_chr_cols], function(col) as.numeric(col == "TRUE"))

# Identify columns with "Y"/"N" values stored as character
yn_cols <- sapply(data_use, function(col) all(col %in% c("Y", "N", NA)))

# Convert into numeric binary (1 for "Y", 0 for "N")
data_use[, yn_cols] <- lapply(data_use[, yn_cols], function(col) as.numeric(col == "Y"))


#####################################
##### Exploratory Data Analysis #####
#####################################

# Specify the categorical variables
categorical_vars <- c("type", "gender_male", "aat_deficiency", "cys_fib", "ipah", "ild", "pulm_other",
                      "cad", "Hypertension", "t1d", "t2d", "gerd_pud", "renal_fail", "stroke",
                      "liver_disease", "thyroid_disease", "first_transplant", "redo_transplant",
                      "evlp", "preop_ecls", "intraop_ecls", "ECLS_ECMO", "ECLS_CPB", "ALIVE_30DAYS_YN",
                      "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "massive_transfusion")

# Specify the continuous variables
continuous_vars <- c("las", "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR",
                     "Pre_PTT", "Pre_Fibrinogen", "Pre_Creatinine", "intra_plasma",
                     "intra_packed_cells", "Intra_Platelets", "Intra_Cryoprecipitate",
                     "icu_stay", "ICU_LOS", "HOSPITAL_LOS", "rbc_72_tot", "ffp_72_tot",
                     "plt_72_tot", "cryo_72_tot", "tot_24_rbc", "Age", "BMI")

# Converting categorical variables to factors
data_use[categorical_vars] <- lapply(data_use[categorical_vars], as.factor)

# Create a vector for all the variables in the dataset
all_vars <- c(categorical_vars, continuous_vars)

# Create a table of the summary statistics using the TableOne package
table_one <- CreateTableOne(vars = all_vars, data = data_use, factorVars = categorical_vars)

# View the summary statistics
summary(table_one, digits =2)

##### QUESTION 1 STUFF #######
####################################
#####     Some stuff     #####
####################################
# In conjunction with the above, what is the impact of transfusion on patient outcomes, including mortality? #

# get rid of pateints who did not get transfusion
# 109 did and 83 didnt 
data_use <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
                     rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, 0, 1
  ))


#####  Report the Median Time Until Death #####
# create a death variable in data_use 
data_with_dead <- data_use %>%
  mutate(has_value = if_else(!is.na(data_use$DEATH_DATE), "1", "0"))
# convert to numberic
data_with_dead$has_value <- as.numeric(data_with_dead$has_value)

data_with_dead <- data_with_dead %>%
  mutate(
    DEATH_DATE = as.POSIXct(DEATH_DATE, format = "%d-%b-%Y"),
    or_date = as.POSIXct(or_date, format = "%Y-%m-%d %H:%M:%S"),  # Adjust if needed
    time_death = as.numeric(difftime(DEATH_DATE, or_date, units = "days"))
  )


# Use full data set (data_use) 
library(survival)
sf1 <- survfit(Surv(time_death, has_value==1)~1, data=data_with_dead)

# add a plot
plot(sf1, xlab = "Time (days)", ylab="Survival", conf.int = 0.95) ## Add a confidence interval 

# see plot for where 0.5 is or something if we want to include this graph we can 

# Kaplan-Meier Curve
plot(sf1,xscale = 365.25, xlab = "Time (days)", ylab="Survival Probability") 
# add a legend with col to distinguish levels

# do more
# Make a Cox PH model
coxmod <- coxph(Surv(time_death, has_value==1) ~ 1+intraop_ecls+intra_plasma+intra_packed_cells+Intra_Platelets+Intra_Cryoprecipitate+icu_stay, data=data_with_dead)

# Create a summary 
summary(coxmod)

##### Stratify by if they got a transfusion #####
# potential limitation is that the amount of people who got the transfusion may be rather small...
table(data_with_dead$transfusion)
nrow(data_with_dead)
67/192 #0.3489583 -> small percentage 
125/192

sf3 <- survfit(Surv(time_death, has_value==1)~transfusion, data=data_with_dead)
# add a plot
plot(sf3, xlab = "Time (days)", ylab="Survival", conf.int = 0.95) ## Add a confidence interval 

# see plot for where 0.5 is or something if we want to include this graph we can 

# Kaplan-Meier Curve
plot(sf3,xscale = 365.25, xlab = "Time (days)", ylab="Survival Probability", col=1:2) 
legend("topright",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:3) 






# potential thing we could stratify by if we felt like it 
data_icu <- data_with_dead %>%
  mutate(
    icu = case_when(
      icu_stay <= 3 ~ "short",             # Short stay: 3 days or less
      icu_stay > 3 & icu_stay <= 9 ~ "medium",  # Medium stay: 4 to 9 days
      icu_stay > 10 ~ "long",               # Long stay: more than 10 days
      TRUE ~ NA_character_                 # leave missing/invalid values
    )
  )
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4122095/#:~:text=A%20prolonged%20ICU%20stay%20of,the%20patients%20who%20were%20female.
# https://ccforum.biomedcentral.com/articles/10.1186/s13054-024-04812-7 
## I didnt really read these 

# plot for stratifying with data_icu
sf2 <- survfit(Surv(time_death, has_value==1)~icu, data=data_icu)
# add a plot
plot(sf2, xlab = "Time (days)", ylab="Survival", conf.int = 0.95) ## Add a confidence interval 

# see plot for where 0.5 is or something if we want to include this graph we can 

# Kaplan-Meier Curve
plot(sf2,xscale = 365.25, xlab = "Time (days)", ylab="Survival Probability", col=1:3) 
legend("topright",legend = c("Short", "Medium", "Long"),lty = 1, col = 1:3) 



##### QUESTION 1 STUFF #######
####################################
#####     Lasso Regression     #####
####################################

# With all variables
## Did not include Pre_Fibrinogen bc there were many missing values
## Did not include Pre_PT BC Pre_INR is the standardized ver of it
data_use_lasso_all <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate))) == 0, 0, 1
  )
  ) %>% 
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Hct, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion)

# Check for missing values
colSums(is.na(data_use_lasso_all))

# Removing NA row from data set (complete case analysis)
data_use_lasso_all_v1 <- na.omit(data_use_lasso)

# cv for Lasso
set.seed(789)

train_indices_all <- sample(nrow(data_use_lasso_all_v1), round(nrow(data_use_lasso_all_v1) / 2))

x_all_all <- model.matrix(transfusion ~ ., data_use_lasso_all_v1)

x_train_all <- x_all_all[train_indices_all, -1]
x_validation_all <- x_all_all[-train_indices_all, -1]

y_train_all <- data_use_lasso_all_v1$transfusion[train_indices_all]
y_validation_all <- data_use_lasso_all_v1$transfusion[-train_indices_all]

lasso_mod_all <- glmnet(x_train_all, y_train_all, alpha = 1, family = "binomial")

cv_lasso_all <- cv.glmnet(x_train_all, y_train_all, alpha = 1, family = "binomial", 
                          type.measure = "auc", nfolds = 5)

plot(cv_lasso_all)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min_all <- cv_lasso_all$lambda.min
coef(cv_lasso_all, s = "lambda.min")









# Create a column for transfusion indicator
data_use_lasso <- data_use %>%
  mutate(transfusion = if_else(
      rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate))) == 0, 0, 1
    )
  ) %>% 
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Hct, Pre_Platelets, 
    Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, preop_ecls, intraop_ecls, transfusion)

# Removing NA row from data set (complete case analysis)
data_use_lasso_v1 <- na.omit(data_use_lasso)

# cv for Lasso
set.seed(789)

train_indices <- sample(nrow(data_use_lasso_v1), round(nrow(data_use_lasso_v1) / 2))


x_all <- model.matrix(transfusion ~ ., data_use_lasso_v1)

x_train <- x_all[train_indices, -1]
x_validation <- x_all[-train_indices, -1]

y_train <- data_use_lasso_v1$transfusion[train_indices]
y_validation <- data_use_lasso_v1$transfusion[-train_indices]


lasso_mod <- glmnet(x_train, y_train, alpha = 1, family = "binomial")

cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial", type.measure = "auc", nfolds = 5)

plot(cv_lasso)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min <- cv_lasso$lambda.min
coef(cv_lasso, s = "lambda.min")







