# Team project script

# Install required packages
library(readxl)
library(dplyr)
library(tableone)

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
  select(study_id, tx_db_id, or_date, gender_male, aat_deficiency, cys_fib, ipah, 
         ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud, renal_fail, stroke, 
         liver_disease, thyroid_disease, first_transplant, redo_transplant, evlp, preop_ecls,
         las, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Fibrinogen, Pre_Creatinine,
         intraop_ecls, ECLS_ECMO, ECLS_CPB, intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
         icu_stay, DEATH_DATE, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS,
         rbc_0_24, rbc_24_48, rbc_48_72, rbc_72_tot, ffp_0_24, ffp_24_48, ffp_48_72, ffp_72_tot,
         plt_0_24, plt_24_48, plt_48_72, plt_72_tot, cryo_0_24, cryo_24_48, cryo_48_72, cryo_72_tot,
         tot_24_rbc, massive_transfusion)

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


#####################################
##### Exploratory Data Analysis #####
#####################################

# Specify the categorical variables
categorical_vars <- c("gender_male", "aat_deficiency", "cys_fib", "ipah", "ild", "pulm_other",
                      "cad", "Hypertension", "t1d", "t2d", "gerd_pud", "renal_fail", "stroke",
                      "liver_disease", "thyroid_disease", "first_transplant", "redo_transplant",
                      "evlp", "preop_ecls", "intraop_ecls", "ECLS_ECMO", "ECLS_CPB", "ALIVE_30DAYS_YN",
                      "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "massive_transfusion")

# Specify the conitnous variables
continuous_vars <- c("las", "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR",
                     "Pre_PTT", "Pre_Fibrinogen", "Pre_Creatinine", "intra_plasma",
                     "intra_packed_cells", "Intra_Platelets", "Intra_Cryoprecipitate",
                     "icu_stay", "ICU_LOS", "HOSPITAL_LOS", "rbc_72_tot", "ffp_72_tot",
                     "plt_72_tot", "cryo_72_tot", "tot_24_rbc")

# Create a vector for all the variables in the dataset
all_vars <- c(categorical_vars, continuous_vars)

# Create a table of the summary statistics using the TableOne package
table_one <- CreateTableOne(vars = all_vars, data = data_use, factorVars = categorical_vars)

# View the summary statistics
summary(table_one, digits =2)


# Boxplot of catogrical variables

# Load necessary libraries
library(ggplot2)

# Loop through categorical variables
for (cat_var in categorical_vars) {
  # Create a bar plot
  p <- ggplot(data_use, aes_string(x = cat_var)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(
      title = paste("Frequency Bar Plot of", cat_var),
      x = cat_var,
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  
  # Print the plot
  print(p)
}


# Plots for continuous variables

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

# Plotting a histogram for Total RBC Unit Tranfused 72 hrs Post Surgery
ggplot(data_use, aes(x = rbc_72_tot)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total RBC Unit Tranfused 72 hrs Post Surgery", x = "Total RBC Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total Fresh Frozen Plasma Unit Tranfused 72 hrs Post Surgery
ggplot(data_use, aes(x = ffp_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Fresh Frozen Plasma Unit Tranfused 72 hrs Post Surgery", x = "Total Fresh Frozen Plasma Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total Platelets Tranfused 72 hrs Post Surgery
ggplot(data_use, aes(x = plt_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Platelets Tranfused 72 hrs Post Surgery", x = "Total Platelets Tranfused", y = "Frequency")

# Plotting a histogram for Total Cryoprecipitate Unit Tranfused 72 hrs Post Surgery
ggplot(data_use, aes(x = cryo_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Cryoprecipitate Unit Tranfused 72 hrs Post Surgery", x = "Total Cryoprecipitate Unit Tranfused", y = "Frequency")

# Plotting a histogram for Total RBC Unit Tranfused during the first 24 Hr of surgery (including intraop and post op)
ggplot(data_use, aes(x = tot_24_rbc)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total RBC Unit Tranfused in the First 24 hrs of Surgery", x = "Total RBC Unit Tranfused", y = "Frequency")

