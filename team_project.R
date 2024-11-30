# Team project script

# Install required packages
library(readxl)
library(dplyr)
library(tableone)
library(glmnet)
library(pROC)
library(tree)
library(mice)
library(ggpubr)
library(grid)


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


####################################
#####     Lasso Regression     #####
####################################

##############################
##### With all variables #####
##############################

## Did not include Pre_Fibrinogen bc there were many missing values
## Did not include Pre_PT BC Pre_INR is the standardized ver of it
data_use_lasso_all <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(
      c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
        rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, 0, 1
  )
  ) %>% 
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Hct, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion)

# Factor transfusion
data_use_lasso_all$transfusion <- as.factor(data_use_lasso_all$transfusion)

# Check for missing values
colSums(is.na(data_use_lasso_all))

# Imputing for missing data
imp <- mice(data_use_lasso_all, seed = 123, m = 20, print = FALSE)
data_use_lasso_all_v1 <- complete(imp, action = 1)

# Create a sequence of repeat indices from 1 to 5, since the assessment will be completed for 5 repeats
repeats <- seq(from = 1, to = 5, by = 1)
# Create empty numeric vector store AUC values for each repeat
lasso_auc_all <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the AUC for each repeat
lambda_min_all <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_all <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_all <- list()
# Create empty list to store AUC plots for each repeat
auc_plot_all <- list()
# Create empty list to store ROC plots for each repeat
roc_plot_all <- list()


# Create a loop that completes 5 repeats of lasso regression, cross validation, 
# testing on test set, and roc plot
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_all <- sample(nrow(data_use_lasso_all_v1), round(nrow(data_use_lasso_all_v1) / 2))
  # Create a model matrix
  x_all_all <- model.matrix(transfusion ~ ., data_use_lasso_all_v1)
  # Select training set
  x_train_all <- x_all_all[train_indices_all, -1]
  # Select the rest as test set
  x_validation_all <- x_all_all[-train_indices_all, -1]
  # Select response set for training
  y_train_all <- data_use_lasso_all_v1$transfusion[train_indices_all]
  # Select response set for testing
  y_validation_all <- data_use_lasso_all_v1$transfusion[-train_indices_all]
  # Fit the lasso model
  lasso_mod_all <- glmnet(x_train_all, y_train_all, alpha = 1, family = "binomial")
  # Plot and store the lasso coefficient path plot
  par(bg = "#00000000")
  plot(lasso_mod_all, label = T, xvar = "lambda")
  lasso_plot_all[[i]] <- recordPlot()
  
  # Conduct cross validation with AUC as the measure 
  cv_lasso_all <- cv.glmnet(x_train_all, y_train_all, alpha = 1, 
                            family = "binomial", type.measure = "auc", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(bg = "#00000000")
  plot(cv_lasso_all)
  # Store the plot
  auc_plot_all[[i]] <- recordPlot()
  # Extract and store the lambda value that maximizes AUC
  lambda_min_all[i] <- cv_lasso_all$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_all[i] <- coef(cv_lasso_all, s = "lambda.min")
  
  # Testing in the test set. Validation.
  pred_lasso_all <- as.numeric(
    predict(
      lasso_mod_all, 
      newx = x_validation_all, 
      s = cv_lasso_all$lambda.min, type = "response"))
  
  # Generate ROC
  myroc_all <- roc(y_validation_all, pred_lasso_all)
  # Plotting the ROC curve
  par(bg = "#00000000")
  plot(myroc_all)
  # Save the plot
  roc_plot_all[[i]] <- recordPlot()
  
  # extracting the Area Under the Curve, a measure of discrimination
  lasso_auc_all[i] <- myroc_all$auc
}

# Compile AUC's from the lasso regression of literature variables into a dataframe
lasso_auc_df <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("All", 5),
  Iteration = paste0("Iteration", 1:5),  
  AUC = lasso_auc_all  # Actual AUC values
)
lasso_auc_df


# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_all <- lapply(seq_along(coef_all), function(i) {
  mat <- coef_all[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df <- as.data.frame(as.matrix(mat))
  df$Feature <- rownames(df)
  colnames(df) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df <- df[df[[1]] != 0, , drop = FALSE]
  df
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_all)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef[is.na(nonzero_coef)] <- 0
nonzero_coef


##############################################
##### With Literature Relevant variables #####
##############################################

# Create a column for transfusion indicator
data_use_lasso <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(
      across(
        c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
          rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, 0, 1
  )
  ) %>% 
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Hct, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, preop_ecls, intraop_ecls, transfusion)

# Removing NA row from data set (complete case analysis) - only 1 missing for Pre_PTT
data_use_lasso_v1 <- na.omit(data_use_lasso)

# Factor transfusion
data_use_lasso_v1$transfusion <- as.factor(data_use_lasso_v1$transfusion)

# Create empty numeric vector store AUC values for each repeat
lasso_auc <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the AUC for each repeat
lambda_min <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot <- list()
# Create empty list to store AUC plots for each repeat
auc_plot <- list()
# Create empty list to store ROC plots for each repeat
roc_plot <- list()

# Create a loop that completes 5 repeats of lasso regression, cross validation, 
# testing on test set, and roc plot
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices <- sample(nrow(data_use_lasso_v1), round(nrow(data_use_lasso_v1) / 2))
  # Create a model matrix
  x_all <- model.matrix(transfusion ~ ., data_use_lasso_v1)
  # Select training set
  x_train <- x_all[train_indices, -1]
  # Select the rest as test set
  x_validation <- x_all[-train_indices, -1]
  # Select response set for training
  y_train <- data_use_lasso_v1$transfusion[train_indices]
  # Select response set for testing
  y_validation <- data_use_lasso_v1$transfusion[-train_indices]
  # Fit the lasso model
  lasso_mod <- glmnet(x_train, y_train, alpha = 1, family = "binomial")
  # Plot and store the lasso coefficient path plot
  par(bg = "#00000000")
  plot(lasso_mod, label = T, xvar = "lambda")
  lasso_plot[[i]] <- recordPlot()
  
  # Conduct cross validation with AUC as the measure 
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, 
                        family = "binomial", type.measure = "auc", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(bg = "#00000000")
  plot(cv_lasso)
  # Store the plot
  auc_plot[[i]] <- recordPlot()
  # Extract and store the lambda value that maximizes AUC
  lambda_min[i] <- cv_lasso$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef[i] <- coef(cv_lasso, s = "lambda.min")
  
  # Testing in the test set. Validation.
  pred_lasso <- as.numeric(
    predict(
      lasso_mod, 
      newx = x_validation, 
      s = cv_lasso$lambda.min, type = "response"))
  
  # Generate ROC
  myroc <- roc(y_validation, pred_lasso)
  # Plotting the ROC curve
  par(bg = "#00000000")
  plot(myroc)
  # Save the plot
  roc_plot[[i]] <- recordPlot()
  
  # extracting the Area Under the Curve, a measure of discrimination
  lasso_auc[i] <- myroc$auc
}

# Create a data frame with each model's AUCs as rows and iterations as columns
lit_lasso_auc_df <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("Literature", 5),
  Iteration = paste0("Iteration", 1:5),  
  AUC = lasso_auc  # Actual AUC values
)
lit_lasso_auc_df


# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs <- lapply(seq_along(coef), function(i) {
  mat <- coef[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df <- as.data.frame(as.matrix(mat))
  df$Feature <- rownames(df)
  colnames(df) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df <- df[df[[1]] != 0, , drop = FALSE]
  df
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_lit <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_lit[is.na(nonzero_coef_lit)] <- 0
nonzero_coef_lit

#### Lasso Regression Combined Plots (All Variables) ####

# Combine five repetitions for each of the lasso coefficient, auc, and roc plots into their individual plots. Add labels and figure captions
lasso_plots_all <- ggarrange(plotlist = lasso_plot_all,
                             labels = c("A", "B", "C", "D", "E"),
                             widths = c(1, 1, 1),
                             heights = c(4, 4),
                             ncol = 3,
                             nrow = 2,
                             align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 4. Lasso coefficient paths for 5 repeated trials of Lasso regression containing all available variables. Variability in coefficient values as log lambda changes\nis shown for each of the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
lasso_plots_all
ggsave("lasso_plots_all.png", lasso_plots_all, width = 12, height = 8, dpi = 300)


auc_plots_all <- ggarrange(plotlist = auc_plot_all,
                           labels = c("A", "B", "C", "D", "E"),
                           widths = c(1, 1, 1),
                           heights = c(1,1),
                           ncol = 3,
                           nrow = 2,
                           align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 5. AUC for 5 repeated trials of Lasso regression containing all available variables. Change in AUC values as log lambda changes is shown for each of\nthe 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
auc_plots_all
ggsave("auc_plots_all.png", auc_plots_all, width = 12, height = 8, dpi = 300)

roc_plots_all <- ggarrange(plotlist = roc_plot_all,
                           labels = c("A", "B", "C", "D", "E"),
                           widths = c(1, 1, 1),
                           heights = c(4, 4),
                           ncol = 2,
                           nrow = 3) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 6. ROC curves for 5 repeated trials of Lasso regression containing all available variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
roc_plots_all
ggsave("roc_plots_all.png", roc_plots_all, width = 12, height = 8, dpi = 300)


#### Lasso Regression Combined Plots (Lit Review Variables) ####

# Combine five repetitions for each of the lasso coefficient, auc, and roc plots into their individual plots. Add labels and figure captions
lasso_plots <- ggarrange(plotlist = lasso_plot,
                         labels = c("A", "B", "C", "D", "E"),
                         widths = c(1, 1, 1),
                         heights = c(4, 4),
                         ncol = 3,
                         nrow = 2,
                         align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 7. Lasso coefficient paths for 5 repeated trials of Lasso regression containing literature-relevant variables. Variability in coefficient values as log lambda\nchanges is shown for each of the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
lasso_plots
ggsave("lasso_plots.png", lasso_plots, width = 12, height = 8, dpi = 300)


auc_plots <- ggarrange(plotlist = auc_plot,
                       labels = c("A", "B", "C", "D", "E"),
                       widths = c(1, 1, 1),
                       heights = c(1,1),
                       ncol = 3,
                       nrow = 2,
                       align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 8. AUC for 5 repeated trials of Lasso regression containing literature-relevant variables. Change in AUC values as log lambda changes is shown for each\nof the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
auc_plots
ggsave("auc_plots.png", auc_plots, width = 12, height = 8, dpi = 300)

roc_plots <- ggarrange(plotlist = roc_plot,
                       labels = c("A", "B", "C", "D", "E"),
                       widths = c(1, 1, 1),
                       heights = c(4, 4),
                       ncol = 2,
                       nrow = 3) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 9. ROC curves for 5 repeated trials of Lasso regression containing literature-relevant variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
roc_plots
ggsave("roc_plots.png", roc_plots, width = 12, height = 8, dpi = 300)


#######################################
#####     CART Classification     #####
#######################################

##############################
##### With all variables #####
##############################

# Ensure the response variable is a factor
data_use_lasso_all_v1$transfusion <- as.factor(data_use_lasso_all_v1$transfusion)

# Initialize AUC storage and plot list
pruned_cart_aucs <- c()
pruned_cart_plots <- list()
pruned_cart_roc <- list()

# Set seed for reproducibility
set.seed(789)

# Repeat the process 5 times
for (i in 1:5) {
  
  # Split data into training and validation sets (50/50 split)
  train_indices <- sample(nrow(data_use_lasso_all_v1), round(nrow(data_use_lasso_all_v1) / 2))
  
  ## CART
  
  # Train classification tree using the training data
  tree_model <- tree(transfusion ~ ., data = data_use_lasso_all_v1, subset = train_indices)
  
  # Cross-validation for pruning to find the optimal size
  cv_tree <- cv.tree(tree_model, FUN = prune.misclass, K = 5)
  best_size <- ifelse(min(cv_tree$size) == 1, 2, cv_tree$size[which.min(cv_tree$dev)])
  
  # Prune the tree
  pruned_tree <- prune.misclass(tree_model, best = best_size)
  
  # Predictions and AUC from ROC curve for pruned CART on validation set
  pred_probs_pruned <- predict(pruned_tree, newdata = data_use_lasso_all_v1[-train_indices, ], type = "vector")[, 2]
  y_validation <- data_use_lasso_all_v1$transfusion[-train_indices]
  
  # Compute AUC
  roc_pruned <- roc(y_validation, pred_probs_pruned)
  auc_pruned <- roc_pruned$auc
  
  # Add this iteration's AUC to the pruned_cart_aucs
  pruned_cart_aucs <- c(pruned_cart_aucs, auc_pruned)
  
  # Plot and record the pruned tree
  par(bg = "#00000000")
  plot(pruned_tree, main = paste("Pruned CART - Iteration", i))
  text(pruned_tree, pretty = 0)
  
  # Safely record the plot
  pruned_cart_plots[[i]] <- recordPlot()
  
  # Plot and record the roc plot
  par(bg = "#00000000")
  plot(roc_pruned)
  
  # Safely record the plot
  pruned_cart_roc[[i]] <- recordPlot()
}

# Create a data frame with each model's AUCs as rows and iterations as columns
auc_df <- data.frame(
  Model = rep("Pruned CART", 5),
  Vars = rep("All", 5),
  Iteration = paste0("Iteration", 1:5),  
  AUC = pruned_cart_aucs  # Actual AUC values
)
auc_df

# Combine five repetitions for each of the pruned CART and auc plots into their individual plots. Add labels and figure captions
cart_plots1 <- ggarrange(plotlist = pruned_cart_plots,
                         labels = c("A", "B", "C", "D", "E"),
                         widths = c(1, 1, 1),
                         heights = c(4, 4),
                         ncol = 3,
                         nrow = 2
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 10. Pruned CART trees containing all available variables for repeated trials. Relevant characteristics/factors and their threshold/categories are shown for\neach of the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
cart_plots1

ggsave("CART_plots.png", cart_plots1, width = 12, height = 8, dpi = 300)


cart_plots2 <- ggarrange(plotlist = pruned_cart_roc,
                         labels = c("A", "B", "C", "D", "E"),
                         widths = c(1, 1),
                         heights = c(4, 4),
                         ncol = 3,
                         nrow = 2
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 11. ROC curves for 5 repeated trials of CART containing all available variables. Performance of the classifier is shown for each of the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
cart_plots2

ggsave("CART_plots_2.png", cart_plots2, width = 12, height = 8, dpi = 300)

##############################################
##### With Literature Relevant variables #####
##############################################

# Ensure the response variable is a factor
data_use_lasso_v1$transfusion <- as.factor(data_use_lasso_v1$transfusion)

# Initialize AUC storage and plot list
pruned_lit_cart_aucs <- c()
pruned_lit_cart_plots <- list()
pruned_lit_cart_roc <- list()

# Set seed for reproducibility
set.seed(789)

# Repeat the process 5 times
for (i in 1:5) {
  
  # Split data into training and validation sets (50/50 split)
  train_indices <- sample(nrow(data_use_lasso_v1), round(nrow(data_use_lasso_v1) / 2))
  
  ## CART
  
  # Train classification tree using the training data
  tree_model <- tree(transfusion ~ ., data = data_use_lasso_v1, subset = train_indices)
  
  # Cross-validation for pruning to find the optimal size
  cv_tree <- cv.tree(tree_model, FUN = prune.misclass, K = 5)
  best_size <- ifelse(min(cv_tree$size) == 1, 2, cv_tree$size[which.min(cv_tree$dev)])
  
  # Prune the tree
  pruned_tree <- prune.misclass(tree_model, best = best_size)
  
  # Predictions and AUC from ROC curve for pruned CART on validation set
  pred_probs_pruned <- predict(pruned_tree, newdata = data_use_lasso_v1[-train_indices, ], type = "vector")[, 2]
  y_validation <- data_use_lasso_v1$transfusion[-train_indices]
  
  # Compute AUC
  roc_pruned <- roc(y_validation, pred_probs_pruned)
  auc_pruned <- roc_pruned$auc
  
  # Add this iteration's AUC to the pruned_lit_cart_aucs
  pruned_lit_cart_aucs <- c(pruned_lit_cart_aucs, auc_pruned)
  
  # Plot and record the pruned tree
  par(bg = "#00000000")
  plot(pruned_tree, main = paste("Pruned CART - Iteration", i))
  text(pruned_tree, pretty = 0)
  
  # Safely record the plot
  pruned_lit_cart_plots[[i]] <- recordPlot()
  
  # Plot and record the roc plot
  par(bg = "#00000000")
  plot(roc_pruned)
  # Safely record the plot
  pruned_lit_cart_roc[[i]] <- recordPlot()
}

# Create a data frame with each model's AUCs as rows and iterations as columns
lit_auc_df <- data.frame(
  Model = rep("Pruned CART", 5),
  Vars = rep("Literature", 5),
  Iteration = paste0("Iteration", 1:5),  
  AUC = pruned_lit_cart_aucs  # Actual AUC values
)
lit_auc_df

# Combine five repetitions for each of the pruned CART and auc plots into their individual plots. Add labels and figure captions
cart_plots3 <- ggarrange(plotlist = pruned_lit_cart_plots,
                         labels = c("A", "B", "C", "D", "E"),
                         widths = c(1,1, 1),
                         heights = c(4, 4),
                         ncol = 3,
                         nrow = 2
                        
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 12. Pruned CART trees containing literature-relevant variables for repeated trials. Relevant literature characteristics/factors and their threshold/categories\nare shown for each of the 5 repetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
cart_plots3

ggsave("CART_plots_3.png", cart_plots3, width = 12, height = 8, dpi = 300)


cart_plots4 <- ggarrange(plotlist = pruned_lit_cart_roc,
                         labels = c("A", "B", "C", "D", "E"),
                         widths = c(1, 1, 1),
                         heights = c(4, 4),
                         ncol = 3,
                         nrow = 2
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 13. ROC curves for 5 repeated trials of CART containing literature-relevant variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
cart_plots4

ggsave("CART_plots_4.png", cart_plots4, width = 12, height = 8, dpi = 300)

### Convert all AUCs from lasso regression and pruned trees together ###
auc_combined_df <- rbind(lasso_auc_df, lit_lasso_auc_df, auc_df, lit_auc_df)
auc_combined_df

### Calculate the average of AUCs ###
auc_avg_df <- auc_combined_df %>%
  group_by(Model, Vars) %>%
  summarise(
    mean_auc <- mean(AUC)
    ) %>%
  ungroup() %>%
  # Rename column
  rename(
    mean_auc = "mean_auc <- mean(AUC)"
  )

# View the averaged AUC scores for all the models
auc_avg_df

##### QUESTION 2  #######
# Anything that has !!! means it needs review (by group)
# In conjunction with the above, what is the impact of transfusion on patient outcomes, including mortality? #

# Modify data_use to include the transfusion binary indicator (as done prevoiusly for lasso regression)
data_use <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
                     rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, 0, 1
  ))


# create a death variable in data_use and a new data frame for the analysis
# the death variable indicates if DEATH_DATE has a value, where 1 indicates the patinet is known to have died and 0 indicates they are censored
data_with_dead <- data_use %>%
  mutate(has_value = if_else(!is.na(data_use$DEATH_DATE), "1", "0"))
# convert to numeric
data_with_dead$has_value <- as.numeric(data_with_dead$has_value)

# convert both death date and OR date into POSIXct form
data_with_dead <- data_with_dead %>%
  mutate(
    DEATH_DATE = as.POSIXct(DEATH_DATE, format = "%d-%b-%Y"), #convert to POSIX form
    or_date = as.POSIXct(or_date, format = "%Y-%m-%d %H:%M:%S"), #convert to POSIX form
    time_death = as.numeric(difftime(DEATH_DATE, or_date, units = "days"))
  )

# since censoring occurs at one year and some of the pateints die after a year include only those who died 
# during the first year in the analysis 
sup_dwd <- data_with_dead <- data_with_dead %>%
  mutate(

# add the new variables to the EDA -> for results 
# Patient deaths (from has_value)
ggplot(data_with_dead, aes(x = factor(has_value))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Known Patients Deaths", x = "Patient Death (1 = Yes, 0 = No)", y = "Count")

# time to death - only for those who died 
ggplot(data_with_dead, aes(x = time_death)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Patient Time to Death", x = "Time to Death (days)", y = "Frequency")

# time_death has a lot of missingness since DEATH_DATE has a lot of missingness
table(data_with_dead$time_death)

# therefore if there is no DEATH_DATE assume that the patient was alive after a year so change the NA to 365 (since the last recorded follow up in the dataset was after 12 months)
# censored patients given value of 365
data_with_dead <- data_with_dead %>%
  mutate(
    time_death = if_else(is.na(time_death), 365, time_death))

# time to death - including censored at 365 days
ggplot(data_with_dead, aes(x = time_death)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Patient Time to Death", x = "Time to Death (days)", y = "Frequency")


# Ensure all variables that will be included in the data set for the analyses are relevant 
# removed study Id and tx_db_id -> just identifiers
# removed or_date and DEATH_DATE -> represented in time_death
# remove due to high missingness: pre_fibrinogen, rbc_0_24, rbc_24_48, rbc_48_72, ffp_0_24, ffp_24_48, ffp_48_72, plt_0_24, plt_24_48, plt_48_72, cryo_0_24, cryo_24_48, cyro_48_72
# SHOULD I ALSO REMOVE ALIVE 30 DAYS, 90 DAYS and 1 YEAR? !!!
data_with_dead <- data_with_dead %>%
  select(Type, or_date, gender_male, aat_deficiency, cys_fib, ipah, 
         ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud, renal_fail, stroke, 
         liver_disease, thyroid_disease, first_transplant, redo_transplant, evlp, preop_ecls,
         las, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Fibrinogen, Pre_Creatinine,
         intraop_ecls, ECLS_ECMO, ECLS_CPB, intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
         icu_stay, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS,
         rbc_72_tot,ffp_72_tot, plt_72_tot, cryo_72_tot,
         tot_24_rbc, massive_transfusion, Age, BMI, time_death, has_value, transfusion) %>%
  mutate(type = if_else(Type == "Bilateral", "Double", "Single")) # modifying type to single or double transplant

# add the library required for the survival analysis 
library(survival)
# create an unstratified Kaplan-Meier estimate 
sf1 <- survfit(Surv(time_death, has_value==1)~1, data=data_with_dead)

# Kaplan-Meier Curve
plot(sf1,xscale = 365.25, xlab = "Time (years)", ylab="Survival Probability", conf.int = 0.95) 

# create a stratified surivival anlysis - CAN I OR SHOULD I DO THIS !!! probably cause log rank is looking at this right 
sf3 <- survfit(Surv(time_death, has_value==1)~transfusion, data=data_with_dead)

# Kaplan-Meier Curve - UGLY PROBABLY DELETE !!!
plot(sf3,xscale = 365.25, xlab = "Time (years)", ylab="Survival Probability", conf.int = 0.95, col=1:2, fun = "S") 
legend("topright",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:3)

# Preform the log-rank test
# must first check the proportional hazard assumption using function set to "cloglog"
# plot a cloglog plot against log(t)
plot(survfit(Surv(time_death, has_value==1)~transfusion, data=data_with_dead), fun = "cloglog", col=1:2, xlab = "Time (days)", ylab="log[log(Survival probability)]")
# add a legend with col to distinguish levels
legend("topleft",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:2)

# Run the log-rank test # CRYSTAL GOT A P OF 0.5 !!! a p value of 1 is the max !!!
LR_test3 <- survdiff(Surv(time_death, has_value==1) ~ transfusion, data=data_with_dead)
LR_test3

# Run a Cox proportional hazard model including variables related to the patient (orange) and blood transfused into the patients and their 72 hour stats (yellow)
# Should i be including like a LOT of other things !!! -> do i check for multicollinearity?
coxmod3 <- coxph(Surv(time_death, has_value==1) ~ transfusion + gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
                 + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot, data=data_with_dead)
summary(coxmod3)
## SEE LIT -> HYPERTENSION AND ANEMIA 

##### Stratify by if they got a transfusion #####
### MAIN ANALYSIS !!!
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
plot(sf3,xscale = 365.25, xlab = "Time (years)", ylab="Survival Probability", col=1:2) 
legend("topright",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:3) 

# Cox model 
coxmod3 <- coxph(Surv(time_death, has_value==1) ~ gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
                 + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot, data=data_with_dead)
summary(coxmod3)

# do log rank test as well to compare transfusion or not transfusion 
LR_test3 <- survdiff(Surv(time_death, has_value==1) ~ transfusion, data=data_with_dead)
LR_test3

####################################
#####     WILCOXON TEST CAUSE NOT NORMALLY DISTRIBUTED    #####
####################################

# See EDA that hospital LOS and ICU LOS is not normally distributed
# therefore t test and linear regression will not suffice as the assumptions are not met
# since we are writing a report though we could include the linear regression stuff in the appendix but if we dont want to we just have to delete it !!!
attach(data_with_dead)

# ICU stay 
boxplot(icu_stay~transfusion) # this is really ugly !!!
wilcox.test(icu_stay~transfusion)

# hospital stay
boxplot(HOSPITAL_LOS~transfusion) # this is really ugly !!!
wilcox.test(HOSPITAL_LOS~transfusion)

detach(data_with_dead)

####################################
#####     LINEAR REGRESSION    #####
####################################
### WE CANNOT DO THIS THOUGH BECAUSE THE DATA IS NOT NORMALLY DISTRIBUTED !!!
# Testing for the 3 outcomes -> ICU stay, Hospital stay, and time to death 
# Bonferonni needed*** !!!

### ICU stay
# make linear regression model using WHICH PREDICTORS !!!
# ICU STAY HAS ONE MISSING VARIABLE !!! # Literature review -> just do the yellow ones (+ age, BMI, comorbidities)
## did not add has_value, transfusion, or type !!!
# find literature source for this !!!
model_icu <- lm(icu_stay ~ gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
                + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot,
                data = data_with_dead)
# ensure the degrees of freedom for the predictors is okay (currently 10, 7 continuous variables and 1 factor with 4 levels)
pred_num <- nrow(data_with_dead)/15 
pred_num
# gender_male is binary, rest are numeric
# have 11 predictors so it is okay 

# create the model summary
summary(model_icu)

### Hospital stay 
model_hs <- lm(HOSPITAL_LOS ~ gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
               + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot,
               data = data_with_dead)
summary(model_hs)

### Time to death -> small number of obs... is this okay? would mess up the pred_num stuff... maybe not?
# would it make more sense to do logistic for 
# has_value as repsonse 
model_ttd <- glm(has_value ~ gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
                 + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot,
                 data = data_with_dead,family = binomial)
summary(model_ttd)



##### Stratify by length of ICU stay #####
# this seems a little redundant, may not include in analysis !!!
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




