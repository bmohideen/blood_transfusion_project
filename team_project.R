# Team project script

# Install required packages
library(readxl)
library(dplyr)
library(tableone)
library(glmnet)

#########################################
##### Loading and Cleaning the Data #####
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

# structure
str(data_use)


# Converting to Proper Binary structure (TRUE/Y = 1, FALSE/N = 0) 

# Identify columns with binary "TRUE"/"FALSE" values stored as character
binary_chr_cols <- sapply(data_use, function(col) all(col %in% c("TRUE", "FALSE")))

# Convert these columns to numeric (0/1)
data_use[, binary_chr_cols] <- lapply(data_use[, binary_chr_cols], function(col) as.numeric(col == "TRUE"))

# Identify columns with "Y"/"N" values stored as character
yn_cols <- sapply(data_use, function(col) all(col %in% c("Y", "N", NA)))

# Convert into numeric binary (1 for "Y", 0 for "N")
data_use[, yn_cols] <- lapply(data_use[, yn_cols], function(col) as.numeric(col == "Y"))

# Check the structure of the updated dataset
str(data_use)

# Check the summary of the updated dataset
summary(data_use)

# number of observations
nrow(data_use)
# 192

# number of variables
ncol(data_use)
# 63

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

###### Prepare the tables so that they look pretty for the report #####
# Install gridExtra
library(gridExtra)

# get categorical table 
# quote = F removes quotes around the text
# noSpaces = T removes spaces
# printToggle = F makes the output not be printed
cat_table2 <- print(table_one$CatTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
# save as pdf
pdf("cat_table2.pdf", width = 10, height = 8)
grid.table(cat_table2) # proper format for saving 
dev.off() # closes PDF

# repreat for continous
num_table2 <- print(table_one$ContTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
pdf("num_table2.pdf", width = 10, height = 8)
grid.table(num_table2)
dev.off()

# Boxplot of catogrical variables

# Load necessary libraries
library(ggplot2)

# Loop through categorical variables
# for (cat_var in categorical_vars) {
  # Create a bar plot
  # p <- ggplot(data_use, aes_string(x = cat_var)) +
    # geom_bar(fill = "lightblue", color = "black") +
    # labs(
      # title = paste("Frequency Bar Plot of", cat_var),
      # x = cat_var,
      # y = "Frequency"
    # ) +
    # theme_minimal() +
    # theme(
      # axis.text.x = element_text(angle = 45, hjust = 1),
      # plot.title = element_text(hjust = 0.5, size = 14)
    # )
  
  # Print the plot
  # print(p)
# }


#### Plots for categorical variables ####

# Gender (Male)
ggplot(data_use, aes(x = factor(gender_male))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Gender (Male)", x = "Gender (Male: 1 = Yes, 0 = No)", y = "Count")

# Alpha-1 antitrypsin deficiency
ggplot(data_use, aes(x = factor(aat_deficiency))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Alpha-1 Antitrypsin Deficiency", x = "AAT Deficiency (1 = Yes, 0 = No)", y = "Count")

# Cystic fibrosis
ggplot(data_use, aes(x = factor(cys_fib))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Cystic Fibrosis", x = "Cystic Fibrosis (1 = Yes, 0 = No)", y = "Count")

# Idiopathic pulmonary arterial hypertension (IPAH)
ggplot(data_use, aes(x = factor(ipah))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Idiopathic Pulmonary Arterial Hypertension (IPAH)", x = "IPAH (1 = Yes, 0 = No)", y = "Count")

# Interstitial lung disease (ILD)
ggplot(data_use, aes(x = factor(ild))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Interstitial Lung Disease (ILD)", x = "ILD (1 = Yes, 0 = No)", y = "Count")

# Other pulmonary diseases
ggplot(data_use, aes(x = factor(pulm_other))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Other Pulmonary Diseases", x = "Other Pulmonary Diseases (1 = Yes, 0 = No)", y = "Count")

# Coronary artery disease (CAD)
ggplot(data_use, aes(x = factor(cad))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Coronary Artery Disease (CAD)", x = "CAD (1 = Yes, 0 = No)", y = "Count")

# Hypertension
ggplot(data_use, aes(x = factor(Hypertension))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Hypertension", x = "Hypertension (1 = Yes, 0 = No)", y = "Count")

# Type 1 diabetes (T1D)
ggplot(data_use, aes(x = factor(t1d))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Type 1 Diabetes (T1D)", x = "T1D (1 = Yes, 0 = No)", y = "Count")

# Type 2 diabetes (T2D)
ggplot(data_use, aes(x = factor(t2d))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Type 2 Diabetes (T2D)", x = "T2D (1 = Yes, 0 = No)", y = "Count")

# Gastroesophageal reflux disease or peptic ulcer disease (GERD/PUD)
ggplot(data_use, aes(x = factor(gerd_pud))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of GERD/PUD", x = "GERD/PUD (1 = Yes, 0 = No)", y = "Count")

# Renal failure
ggplot(data_use, aes(x = factor(renal_fail))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Renal Failure", x = "Renal Failure (1 = Yes, 0 = No)", y = "Count")

# Stroke
ggplot(data_use, aes(x = factor(stroke))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Stroke", x = "Stroke (1 = Yes, 0 = No)", y = "Count")

# Liver disease
ggplot(data_use, aes(x = factor(liver_disease))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Liver Disease", x = "Liver Disease (1 = Yes, 0 = No)", y = "Count")

# Thyroid disease
ggplot(data_use, aes(x = factor(thyroid_disease))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Thyroid Disease", x = "Thyroid Disease (1 = Yes, 0 = No)", y = "Count")

# First transplant
ggplot(data_use, aes(x = factor(first_transplant))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of First Transplant", x = "First Transplant (1 = Yes, 0 = No)", y = "Count")

# Redo transplant
ggplot(data_use, aes(x = factor(redo_transplant))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Redo Transplant", x = "Redo Transplant (1 = Yes, 0 = No)", y = "Count")

# Extracorporeal lung support (ECLS)
ggplot(data_use, aes(x = factor(preop_ecls))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Preoperative Extracorporeal Lung Support (ECLS)", x = "Preoperative ECLS (1 = Yes, 0 = No)", y = "Count")

# Massive transfusion
ggplot(data_use, aes(x = factor(massive_transfusion))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Massive Transfusion", x = "Massive Transfusion (1 = Yes, 0 = No)", y = "Count")

#### Plots for continuous variables ####

# Plotting a histogram for las score
ggplot(data_use, aes(x = las)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of LAS Score", x = "Las Score", y = "Frequency")

# Plotting a histogram for Preoperative Hemoglobin Level
ggplot(data_use, aes(x = Pre_Hb)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative Hemoglobin Level", x = "Pre Hb Score", y = "Frequency")

# Plotting a histogram for Preoperative Hematocrit Level
ggplot(data_use, aes(x = Pre_Hct)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative Hematocrit Level", x = "Pre Hct Score", y = "Frequency")

# Plotting a histogram for Preoperative Platelets Level
ggplot(data_use, aes(x = Pre_Platelets)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative Platelets Level", x = "Pre Platelets Score", y = "Frequency")

# Plotting a histogram for Preoperative Prothrombin Time
ggplot(data_use, aes(x = Pre_PT)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative Prothrombin Time", x = "Pre PT Score", y = "Frequency")

# Plotting a histogram for Preoperative international normalized ratio (standardized measure of PT)
ggplot(data_use, aes(x = Pre_INR)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative international normalized ratio", x = "Pre INR Score", y = "Frequency")

# Plotting a histogram for Preoperative partial thromboplastin time
ggplot(data_use, aes(x = Pre_PTT)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative partial thromboplastin time", x = "Pre PTT Score", y = "Frequency")

# Plotting a histogram for Preoperative Fibrinogen Level (Too few data)
#ggplot(data_use, aes(x = Pre_Fibrinogen)) + 
  #geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  #labs(title = "Histogram of Preoperative Fibrinogen Level", x = "Pre Fibrinogen Score", y = "Frequency")

# Plotting a histogram for Preoperative Creatinine Level
ggplot(data_use, aes(x = Pre_Creatinine)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Preoperative Creatinine Level", x = "Pre Creatinine Score", y = "Frequency")

# Plotting a histogram for Intraoperative Fresh Frozen Plasma Administered
ggplot(data_use, aes(x = intra_plasma)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Intraoperative Fresh Frozen Plasma Administered", x = "Intra Plasma", y = "Frequency")

# Plotting a histogram for Intraoperative Packed Red Blood Cell Unit Administered
ggplot(data_use, aes(x = intra_packed_cells)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Intraoperative Packed Red Blood Cell Unit Administered", x = "Intra Packed Cells", y = "Frequency")

# Plotting a histogram for Intraoperative Platelets Unit Administered
ggplot(data_use, aes(x = Intra_Platelets)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Intraoperative Platelets Unit Administered", x = "Intra Platelets Cells", y = "Frequency")

# Plotting a histogram for Intraoperative Platelets Unit Administered
ggplot(data_use, aes(x = Intra_Platelets)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Intraoperative Platelets Unit Administered", x = "Intra Platelets Cells", y = "Frequency")

# Plotting a histogram for Intraoperative Cryoprecipitate Unit Administered
ggplot(data_use, aes(x = Intra_Cryoprecipitate)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Intraoperative Cryoprecipitate Unit Administered", x = "Intra Cryoprecipitate Cells", y = "Frequency")

# Plotting a histogram for ICU stay
ggplot(data_use, aes(x = icu_stay)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of ICU Stay in Patients", x = "ICU Stay (Days)", y = "Frequency")

# Plotting a histogram for ICU LOS (Length of Stay) - same as ICU stay, with 1 observation removed as LOS is too high
#ggplot(data_use, aes(x = ICU_LOS)) + 
  #geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  #labs(title = "Histogram of ICU Stay in Patients", x = "ICU Stay (Days)", y = "Frequency")

# Plotting a histogram for Hospital Length of Stay
ggplot(data_use, aes(x = HOSPITAL_LOS)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Hospital Length of Stay", x = "Hospital LOS (Days)", y = "Frequency")

### Flag! Study ID #51

# Plotting a histogram for Total RBC Unit Transfused 72 hrs Post Surgery
ggplot(data_use, aes(x = rbc_72_tot)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total RBC Unit Tranfused 72 hrs Post Surgery", x = "Total RBC Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total Fresh Frozen Plasma Unit Transfused 72 hrs Post Surgery
ggplot(data_use, aes(x = ffp_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Fresh Frozen Plasma Unit Tranfused 72 hrs Post Surgery", x = "Total Fresh Frozen Plasma Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total Platelets Transfused 72 hrs Post Surgery
ggplot(data_use, aes(x = plt_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Platelets Tranfused 72 hrs Post Surgery", x = "Total Platelets Tranfused", y = "Frequency")

# Plotting a histogram for Total Cryoprecipitate Unit Transfused 72 hrs Post Surgery
ggplot(data_use, aes(x = cryo_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Cryoprecipitate Unit Tranfused 72 hrs Post Surgery", x = "Total Cryoprecipitate Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total RBC Unit Transfused during the first 24 Hr of surgery (including intraop and post op)
ggplot(data_use, aes(x = tot_24_rbc)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total RBC Unit Tranfused in the First 24 hrs of Surgery", x = "Total RBC Unit Tranfused", y = "Frequency")

##### QUESTION 1 STUFF #######
####################################
#####     Some stuff     #####
####################################
# In conjunction with the above, what is the impact of transfusion on patient outcomes, including mortality? #

# get rid of pateints who did not get transfusion
# 109 did and 83 didnt 
data_use <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate))) == 0, 0, 1
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







