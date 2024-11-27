# Team project script

# Install required packages
library(readxl)
library(dplyr)
library(tableone)
library(glmnet)
library(pROC)
library(tidyr)

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

# Boxplot of categorical variables

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
data_use_lasso_all_v1 <- na.omit(data_use_lasso_all)

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

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso_all)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")








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

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")

lambda_min <- cv_lasso$lambda.min
coef(cv_lasso, s = "lambda.min")

# Train data
pred_lasso <- as.numeric(
  predict(
    lasso_mod, 
    newx = model.matrix(transfusion ~., data_use_lasso_v1)[-train_indices,-1], 
    s = cv_lasso$lambda.min, type = "response"))

# plotting the ROC curve
myroc <- roc(transfusion ~ pred_lasso, 
             data = data_use_lasso_v1[-train_indices,])

plot(myroc)

# extracting the Area Under the Curve, a measure of discrimination
auc_lasso <- myroc$auc
auc_lasso

#### LASSO Regression: Amount of Transfusion ####

# adding new columns with sum of 24 hour + intraoperative values
data_use_with_sums <- data_use %>%
  mutate(
    tot_24_plasma = ffp_0_24 + intra_plasma,
    tot_24_plt = plt_0_24 + Intra_Platelets,
    tot_24_cryo = cryo_0_24 + Intra_Cryoprecipitate
  )

# Convert NA values to 0 in these columns
# Should be coded as 0, rather than NA
data_use_with_sums_v1 <- data_use_with_sums %>%
  mutate(
    tot_24_plasma = replace_na(tot_24_plasma, 0),
    tot_24_plt = replace_na(tot_24_plt, 0),
    tot_24_cryo = replace_na(tot_24_cryo, 0),
    tot_24_rbc = replace_na(tot_24_rbc, 0)
  )

# includes all variables
data_use_lasso_all_amount <- data_use_with_sums_v1 %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate, 
                     rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, 0, 1
  )
  ) %>% 
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Hct, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_rbc, tot_24_plasma, tot_24_plt, 
         tot_24_cryo, massive_transfusion)

# Filtering only for patients with a transfusion
only_transf_patients <- data_use_lasso_all_amount %>%
  filter(transfusion == 1)

# Check for missing values
colSums(is.na(only_transf_patients))

# Imputing for missing data
imp <- mice(only_transf_patients, seed = 123, m = 20, print = FALSE)
only_transf_patients <- complete(imp, action = 1)

#### RBC ####

# cv for Lasso
set.seed(789)

train_indices_rbc <- sample(nrow(only_transf_patients), round(nrow(only_transf_patients) / 2))

x_all_rbc <- model.matrix(tot_24_rbc ~ ., only_transf_patients)

x_train_rbc <- x_all_rbc[train_indices_rbc, -1]
x_validation_rbc <- x_all_rbc[-train_indices_rbc, -1]

y_train_rbc <- only_transf_patients$tot_24_rbc[train_indices_rbc]
y_validation_rbc <- only_transf_patients$tot_24_rbc[-train_indices_rbc]

lasso_mod_rbc <- glmnet(x_train_rbc, y_train_rbc, alpha = 1, family = "gaussian")

cv_lasso_rbc <- cv.glmnet(x_train_rbc, y_train_rbc, alpha = 1, family = "gaussian", 
                          type.measure = "auc", nfolds = 5)

plot(cv_lasso_rbc)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min_rbc <- cv_lasso_rbc$lambda.min
coef(cv_lasso_rbc, s = "lambda.min")

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso_rbc)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")

# Train data
pred_lasso_rbc <- as.numeric(
  predict(lasso_mod_rbc, 
    newx = model.matrix(tot_24_rbc ~., only_transf_patients)[-train_indices,-1], 
    s = cv_lasso_rbc$lambda.min, type = "response"))

# plotting the ROC curve
myroc_rbc <- roc(tot_24_rbc ~ pred_lasso_rbc, 
             data = only_transf_patients[-train_indices,])

plot(myroc_rbc)

# extracting the Area Under the Curve, a measure of discrimination
auc_lasso_rbc <- myroc_rbc$auc
auc_lasso_rbc

#### FFP ####

# cv for Lasso
set.seed(789)

train_indices_ffp <- sample(nrow(only_transf_patients), round(nrow(only_transf_patients) / 2))

x_all_ffp <- model.matrix(tot_24_plasma ~ ., only_transf_patients)

x_train_ffp <- x_all_ffp[train_indices_ffp, -1]
x_validation_ffp <- x_all_ffp[-train_indices_ffp, -1]

y_train_ffp <- only_transf_patients$tot_24_plasma[train_indices_ffp]
y_validation_ffp <- only_transf_patients$tot_24_plasma[-train_indices_ffp]

lasso_mod_ffp <- glmnet(x_train_ffp, y_train_ffp, alpha = 1, family = "gaussian")

cv_lasso_ffp <- cv.glmnet(x_train_ffp, y_train_ffp, alpha = 1, family = "gaussian", 
                          type.measure = "auc", nfolds = 5)

plot(cv_lasso_ffp)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min_ffp <- cv_lasso_ffp$lambda.min
coef(cv_lasso_ffp, s = "lambda.min")

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso_ffp)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")

# Train data
pred_lasso_ffp <- as.numeric(
  predict(lasso_mod_ffp, 
          newx = model.matrix(tot_24_plasma ~., only_transf_patients)[-train_indices,-1], 
          s = cv_lasso_ffp$lambda.min, type = "response"))

# plotting the ROC curve
myroc_ffp <- roc(tot_24_plasma ~ pred_lasso_ffp, 
                 data = only_transf_patients[-train_indices,])

plot(myroc_ffp)

# extracting the Area Under the Curve, a measure of discrimination
auc_lasso_ffp <- myroc_ffp$auc
auc_lasso_ffp

#### PLT ####

# cv for Lasso
set.seed(789)

train_indices_plt <- sample(nrow(only_transf_patients), round(nrow(only_transf_patients) / 2))

x_all_plt <- model.matrix(tot_24_plt ~ ., only_transf_patients)

x_train_plt <- x_all_plt[train_indices_plt, -1]
x_validation_plt <- x_all_plt[-train_indices_plt, -1]

y_train_plt <- only_transf_patients$tot_24_plt[train_indices_plt]
y_validation_plt <- only_transf_patients$tot_24_plt[-train_indices_plt]

lasso_mod_plt <- glmnet(x_train_plt, y_train_plt, alpha = 1, family = "gaussian")

cv_lasso_plt <- cv.glmnet(x_train_plt, y_train_plt, alpha = 1, family = "gaussian", 
                          type.measure = "auc", nfolds = 5)

plot(cv_lasso_plt)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min_plt <- cv_lasso_plt$lambda.min
coef(cv_lasso_plt, s = "lambda.min")

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso_plt)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")

# Train data
pred_lasso_plt <- as.numeric(
  predict(lasso_mod_plt, 
          newx = model.matrix(tot_24_plt ~., only_transf_patients)[-train_indices,-1], 
          s = cv_lasso_plt$lambda.min, type = "response"))

# plotting the ROC curve
myroc_plt <- roc(tot_24_plt ~ pred_lasso_plt, 
                 data = only_transf_patients[-train_indices,])

plot(myroc_plt)

# extracting the Area Under the Curve, a measure of discrimination
auc_lasso_plt <- myroc_plt$auc
auc_lasso_plt

#### CRYO ####

# cv for Lasso
set.seed(789)

train_indices_cryo <- sample(nrow(only_transf_patients), round(nrow(only_transf_patients) / 2))

x_all_cryo <- model.matrix(tot_24_cryo ~ ., only_transf_patients)

x_train_cryo <- x_all_cryo[train_indices_cryo, -1]
x_validation_cryo <- x_all_cryo[-train_indices_cryo, -1]

y_train_cryo <- only_transf_patients$tot_24_cryo[train_indices_cryo]
y_validation_cryo <- only_transf_patients$tot_24_cryo[-train_indices_cryo]

lasso_mod_cryo <- glmnet(x_train_cryo, y_train_cryo, alpha = 1, family = "gaussian")

cv_lasso_cryo <- cv.glmnet(x_train_cryo, y_train_cryo, alpha = 1, family = "gaussian", 
                           type.measure = "auc", nfolds = 5)

plot(cv_lasso_cryo)
title(main = "AUC Cross-Validation Curve for Lasso Regression", line = 3)

lambda_min_cryo <- cv_lasso_cryo$lambda.min
coef(cv_lasso_cryo, s = "lambda.min")

# AUC plot
par(family = "serif", cex = 1.2, mgp = c(2.5, 1, 0)) 
plot(cv_lasso_cryo)
title(main = "MSE vs. Log(Lambda) with 5-Fold Cross-Validation", line = 2.5)
mtext("Figure: Mean Squared Error (MSE) values across log(lambda) during 5-fold cross-validation. The dotted vertical line represents the optimal lambda minimizing the MSE.", 
      side = 1, line = 4, cex = 1, col = "black")
legend("bottomleft", legend = c("Optimal Lambda (Min MSE)"), 
       col = c("red"), 
       lty = c(2, 2), cex = 0.8,  bty = "n")

# Train data
pred_lasso_cryo <- as.numeric(
  predict(lasso_mod_cryo, 
          newx = model.matrix(tot_24_cryo ~., only_transf_patients)[-train_indices,-1], 
          s = cv_lasso_cryo$lambda.min, type = "response"))

# plotting the ROC curve
myroc_cryo <- roc(tot_24_cryo ~ pred_lasso_cryo, 
                  data = only_transf_patients[-train_indices,])

plot(myroc_cryo)

# extracting the Area Under the Curve, a measure of discrimination
auc_lasso_cryo <- myroc_cryo$auc
auc_lasso_cryo

