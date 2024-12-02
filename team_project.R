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
library(gridGraphics)
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
  select(Type, study_id, tx_db_id, or_date, gender_male, COPD, aat_deficiency, cys_fib, ipah, 
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
categorical_vars <- c("type", "gender_male", "COPD", "aat_deficiency", "cys_fib", "ipah", "ild", "pulm_other",
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

# Create summary table for categorical variables
baseline_summ <- data_use %>%
  select(66, 5, 67, 68, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 26, 27, 28, 29, 30, 31, 33, 24, 21) %>%
  mutate(
    across(
      c(gender_male, COPD, aat_deficiency, cys_fib, ipah, ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud,
        renal_fail, stroke, liver_disease, thyroid_disease, preop_ecls, first_transplant),
      ~ ifelse(. == 1, "Yes", "No")
    )
  ) %>%
  tbl_summary(
    type = list(all_continuous() ~ "continuous",
              all_categorical() ~ "categorical"),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    label = list(
      Age = "Age (years)",
      gender_male = "Gender",
      type = "Transplant Type",
      aat_deficiency = "AAT Deficiency",
      cys_fib = "Cystic Fibrosis",
      ipah = "Idiopathic Pulmonary Hypertension",
      ild = "Interstitial Lung Disease",
      pulm_other = "Other",
      cad = "Coronary Artery Disease",
      t1d = "Type 1 Diabetes",
      t2d = "Type 2 Diabetes",
      gerd_pud = "GERD/PUD",
      renal_fail = "Renal Failure",
      stroke = "Stroke",
      liver_disease = "Liver Disease",
      thyroid_disease = "Thyroid Disease",
      las = "Lung Allocation Score",
      Pre_Hb = "Hemoglobin",
      Pre_Hct = "Hematocrit",
      Pre_Platelets = "Platelets",
      Pre_PT = "Prothrombin",
      Pre_INR = "International Normalized Ratio",
      Pre_PTT = "Partial Thromboplastin Time",
      Pre_Fibrinogen = "Fibrinogen",
      Pre_Creatinine = "Creatinine",
      preop_ecls = "ECLS Use",
      first_transplant = "First Transplant"
      )
    ) %>%
  modify_table_body(
    mutate,
    groupname_col = case_when(variable %in% c("Age", "gender_male", "BMI", "type") ~ "Demographics",
                              variable %in% c("COPD", "aat_deficiency", "cys_fib", "ipah", "ild", "pulm_other") ~ "Primary Diagnoses",
                              variable %in% c("cad", "Hypertension", "t1d", "t2d", 
                              "gerd_pud", "renal_fail", "stroke", "liver_disease", "thyroid_disease") ~ "Comorbidities",
                              variable %in% c("las", "Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Fibrinogen", "Pre_Creatinine", "preop_ecls") ~ "Preoperative Status",
                              variable %in% c("first_transplant") ~ "Transplant History")
    ) %>%
  modify_caption(caption = "**Table 1.** Baseline characteristics of lung transplant patients.")
baseline_summ
gt::gtsave(as_gt(baseline_summ), file = file.path(getwd(), "baseline_summ.png"))

# Create summary table for operative and postoperative outcomes
op_postop_summ <- data_use %>%
  select(23, 34, 35, 36, 37, 38, 39, 40, 43, 44, 45, 41, 47, 65) %>%
  mutate(
    across(
      c(evlp, intraop_ecls, ECLS_ECMO, ECLS_CPB, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN),
      ~ ifelse(. == 1, "Yes", "No")
      ),
    massive_transfusion = ifelse(massive_transfusion == 1, "Yes", "No")
  ) %>%
  tbl_summary(
    type = list(all_continuous() ~ "continuous",
                all_categorical() ~ "categorical"),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)",
    label = list(
      evlp = "Ex Vivo Lung Perfusion",
      intraop_ecls = "Intraoperative ECLS", 
      ECLS_ECMO = "Extracorporeal Membrane Oxygenation", 
      ECLS_CPB = "Cardiopulmonary Bypass",
      intra_plasma = "Plasma",
      intra_packed_cells = "Packed Cells",
      Intra_Platelets = "Platelets",
      Intra_Cryoprecipitate = "Cryoprecipiate",
      icu_stay = "Length of ICU Stay",
      ALIVE_30DAYS_YN = "30-Day",
      ALIVE_90DAYS_YN = "90-Day", 
      ALIVE_12MTHS_YN = "12-Month",
      HOSPITAL_LOS = "Length of Hospital Stay",
      massive_transfusion = "Massive Transfusion"
    )
  ) %>%
  modify_table_body(
    mutate,
    groupname_col = case_when(variable %in% c("evlp", "intraop_ecls", "ECLS_ECMO", "ECLS_CPB") ~ "Operative Factors",
                              variable %in% c("intra_plasma", "intra_packed_cells", "Intra_Platelets", "Intra_Cryoprecipitate", "massive_transfusion") ~ "Blood Products",
                              variable %in% c("ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN") ~ "Survival",
                              variable %in% c("icu_stay", "HOSPITAL_LOS") ~ "Hospital Stay Metrics"
  )
  ) %>%
  modify_caption(caption = "**Table 2.** Operative and Postoperative Outcomes of lung transplant patients.")
op_postop_summ
gt::gtsave(as_gt(op_postop_summ), file = file.path(getwd(), "op_postop_summ.png"))

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

#### Plots for categorical variables ####

# Gender (Male)
gender_male_plot <- ggplot(data_use, aes(x = factor(gender_male))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Gender (Male)", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Type of transplant
type_plot <- ggplot(data_use, aes(x = factor(type))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Type of Transplant", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Alpha-1 antitrypsin deficiency
aat_deficiency_plot <- ggplot(data_use, aes(x = factor(aat_deficiency))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "AAT Deficiency", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Cystic fibrosis
cys_fib_plot <- ggplot(data_use, aes(x = factor(cys_fib))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Cystic Fibrosis", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Idiopathic pulmonary arterial hypertension (IPAH)
ipah_plot <- ggplot(data_use, aes(x = factor(ipah))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "IPAH", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Interstitial lung disease (ILD)
ild_plot <- ggplot(data_use, aes(x = factor(ild))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "ILD", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Other pulmonary diseases
pulm_other_plot <- ggplot(data_use, aes(x = factor(pulm_other))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Other Pulmonary Diseases", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Coronary artery disease (CAD)
cad_plot <- ggplot(data_use, aes(x = factor(cad))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "CAD", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Hypertension
hypertension_plot <- ggplot(data_use, aes(x = factor(Hypertension))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Hypertension", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Type 1 diabetes (T1D)
t1d_plot <- ggplot(data_use, aes(x = factor(t1d))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "T1D", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Type 2 diabetes (T2D)
t2d_plot <- ggplot(data_use, aes(x = factor(t2d))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "T2D", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Gastroesophageal reflux disease or peptic ulcer disease (GERD/PUD)
gerd_pud_plot <- ggplot(data_use, aes(x = factor(gerd_pud))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "GERD/PUD", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Renal failure
renal_fail_plot <- ggplot(data_use, aes(x = factor(renal_fail))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Renal Failure", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Stroke
stroke_plot <- ggplot(data_use, aes(x = factor(stroke))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Stroke", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Liver disease
liver_disease_plot <- ggplot(data_use, aes(x = factor(liver_disease))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Liver Disease", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Thyroid disease
thyroid_disease_plot <- ggplot(data_use, aes(x = factor(thyroid_disease))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Thyroid Disease", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# First transplant
first_transplant_plot <- ggplot(data_use, aes(x = factor(first_transplant))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "First Transplant", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Redo transplant
redo_transplant_plot <- ggplot(data_use, aes(x = factor(redo_transplant))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Redo Transplant", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Extracorporeal lung support (ECLS)
preop_ecls_plot <- ggplot(data_use, aes(x = factor(preop_ecls))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Preoperative ECLS", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Massive transfusion
massive_transfusion_plot <- ggplot(data_use, aes(x = factor(massive_transfusion))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(x = "Massive Transfusion", y = "Count") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

#### Plots for continuous variables ####

# Histogram for LAS score
las_plot <- ggplot(data_use, aes(x = las)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Las Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Hemoglobin Level
pre_hb_plot <- ggplot(data_use, aes(x = Pre_Hb)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre Hb Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Hematocrit Level
pre_hct_plot <- ggplot(data_use, aes(x = Pre_Hct)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre Hct Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Platelets Level
pre_platelets_plot <- ggplot(data_use, aes(x = Pre_Platelets)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre Platelets Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Prothrombin Time
pre_pt_plot <- ggplot(data_use, aes(x = Pre_PT)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre PT Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative International Normalized Ratio
pre_inr_plot <- ggplot(data_use, aes(x = Pre_INR)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre INR Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Partial Thromboplastin Time
pre_ptt_plot <- ggplot(data_use, aes(x = Pre_PTT)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre PTT Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Preoperative Creatinine Level
pre_creatinine_plot <- ggplot(data_use, aes(x = Pre_Creatinine)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Pre Creatinine Score", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Intraoperative Fresh Frozen Plasma Administered
intra_plasma_plot <- ggplot(data_use, aes(x = intra_plasma)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Intra Plasma", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Intraoperative Packed Red Blood Cell Unit Administered
intra_packed_cells_plot <- ggplot(data_use, aes(x = intra_packed_cells)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Intra Packed Cells", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Intraoperative Platelets Unit Administered
intra_platelets_plot <- ggplot(data_use, aes(x = Intra_Platelets)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Intra Platelets Cells", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Intraoperative Cryoprecipitate Unit Administered
intra_cryoprecipitate_plot <- ggplot(data_use, aes(x = Intra_Cryoprecipitate)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Intra Cryoprecipitate Cells", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for ICU Stay
icu_stay_plot <- ggplot(data_use, aes(x = icu_stay)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "ICU Stay (Days)", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Hospital Length of Stay
hospital_los_plot <- ggplot(data_use, aes(x = HOSPITAL_LOS)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Hospital LOS (Days)", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Total RBC Unit Transfused 72 hrs Post Surgery
rbc_72_tot_plot <- ggplot(data_use, aes(x = rbc_72_tot)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(x = "Total RBC Unit Transfused", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Total Fresh Frozen Plasma Unit Transfused 72 hrs Post Surgery
ffp_72_tot_plot <- ggplot(data_use, aes(x = ffp_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Total Fresh Frozen Plasma Unit 
       Transfused", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Total Platelets Transfused 72 hrs Post Surgery
plt_72_tot_plot <- ggplot(data_use, aes(x = plt_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Total Platelets Transfused", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Total Cryoprecipitate Unit Transfused 72 hrs Post Surgery
cryo_72_tot_plot <- ggplot(data_use, aes(x = cryo_72_tot)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Total Cryoprecipitate Unit 
       Transfused", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

# Histogram for Total RBC Unit Transfused during the first 24 Hr of surgery
tot_24_rbc_plot <- ggplot(data_use, aes(x = tot_24_rbc)) + 
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  labs(x = "Total RBC Unit Transfused", y = "Frequency") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_blank()
  )

#### Combined plots for categorical variables ####

# Save all categorical plots in a list
categorical_plots <- list(
  gender_male_plot = gender_male_plot,
  type_plot = type_plot,
  aat_deficiency_plot = aat_deficiency_plot,
  cys_fib_plot = cys_fib_plot,
  ipah_plot = ipah_plot,
  ild_plot = ild_plot,
  pulm_other_plot = pulm_other_plot,
  cad_plot = cad_plot,
  hypertension_plot = hypertension_plot,
  t1d_plot = t1d_plot,
  t2d_plot = t2d_plot,
  gerd_pud_plot = gerd_pud_plot,
  renal_fail_plot = renal_fail_plot,
  stroke_plot = stroke_plot,
  liver_disease_plot = liver_disease_plot,
  thyroid_disease_plot = thyroid_disease_plot,
  first_transplant_plot = first_transplant_plot,
  redo_transplant_plot = redo_transplant_plot,
  preop_ecls_plot = preop_ecls_plot,
  massive_transfusion_plot = massive_transfusion_plot
)

# Generate labels (letters) for each plot
num_cat_plots <- length(categorical_plots)
cat_labels <- LETTERS[1:num_cat_plots]

# Create figure for EDA in report
eda_plots_cat <- ggarrange(
  plotlist = categorical_plots, 
  labels = cat_labels, 
  ncol = 5, 
  nrow = ceiling(num_cat_plots / 5), 
  align = "hv" 
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 1. Exploratory Data Analysis of Categorical Variables. Each subplot shows the distribution of the respective categorical variable in the dataset\n(1 = Yes and 0 = No).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
eda_plots_cat
ggsave("eda_plots_cat.png", eda_plots_cat, width = 12, height = 8, dpi = 300)

#### Combined plots for continuous variables ####

# Save all continuous variable plots in a list
continuous_plots <- list(
  las_plot = las_plot,
  pre_hb_plot = pre_hb_plot,
  pre_hct_plot = pre_hct_plot,
  pre_platelets_plot = pre_platelets_plot,
  pre_pt_plot = pre_pt_plot,
  pre_inr_plot = pre_inr_plot,
  pre_ptt_plot = pre_ptt_plot,
  pre_creatinine_plot = pre_creatinine_plot,
  intra_plasma_plot = intra_plasma_plot,
  intra_packed_cells_plot = intra_packed_cells_plot,
  intra_platelets_plot = intra_platelets_plot,
  intra_cryoprecipitate_plot = intra_cryoprecipitate_plot,
  icu_stay_plot = icu_stay_plot,
  hospital_los_plot = hospital_los_plot,
  rbc_72_tot_plot = rbc_72_tot_plot,
  ffp_72_tot_plot = ffp_72_tot_plot,
  plt_72_tot_plot = plt_72_tot_plot,
  cryo_72_tot_plot = cryo_72_tot_plot,
  tot_24_rbc_plot = tot_24_rbc_plot
)

# Generate labels for each plot
num_con_plots <- length(continuous_plots)
con_labels <- LETTERS[1:num_con_plots]  

# Create figure for EDA in report
eda_plots_con <- ggarrange(
  plotlist = continuous_plots,
  labels = con_labels, 
  ncol = 5, 
  nrow = ceiling(num_con_plots / 5), 
  align = "hv" 
) %>%
  annotate_figure(
    bottom = text_grob(
      "Figure 2. Exploratory Data Analysis of Continuous Variables. Each subplot shows the distribution of the respective continuous 
variable in the dataset.", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
eda_plots_con
ggsave("eda_plots_con.png", eda_plots_con, width = 12, height = 8, dpi = 300)

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
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
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
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Platelets, 
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
      "Figure 3. Lasso coefficient paths for 5 repeated trials of Lasso regression containing all available variables. Variability in coefficient values as log lambda changes\nis shown for each of the 5 repetitions (A-E).", 
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
      "Figure 4. AUC for 5 repeated trials of Lasso regression containing all available variables. Change in AUC values as log lambda changes is shown for each of\nthe 5 repetitions (A-E).", 
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
      "Figure 5. ROC curves for 5 repeated trials of Lasso regression containing all available variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
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
      "Figure 6. Lasso coefficient paths for 5 repeated trials of Lasso regression containing literature-relevant variables. Variability in coefficient values as log lambda\nchanges is shown for each of the 5 repetitions (A-E).", 
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
      "Figure 7. AUC for 5 repeated trials of Lasso regression containing literature-relevant variables. Change in AUC values as log lambda changes is shown for each\nof the 5 repetitions (A-E).", 
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
      "Figure 8. ROC curves for 5 repeated trials of Lasso regression containing literature-relevant variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
roc_plots
ggsave("roc_plots.png", roc_plots, width = 12, height = 8, dpi = 300)

#### Lasso Regression: Amount of Transfusion with All Variables ####

# adding new columns with sum of 24 hour + intraoperative values
data_use_with_sums <- data_use %>%
  mutate(
    tot_24_ffp = ffp_0_24 + intra_plasma,
    tot_24_plt = plt_0_24 + Intra_Platelets,
    tot_24_cryo = cryo_0_24 + Intra_Cryoprecipitate
  )

# Convert NA values to 0 in these columns
# Should be coded as 0, rather than NA
data_use_with_sums_v1 <- data_use_with_sums %>%
  mutate(
    tot_24_ffp = replace_na(tot_24_ffp, 0),
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
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_rbc, tot_24_ffp, tot_24_plt, 
         tot_24_cryo)

# Filtering only for patients with a transfusion
only_transf_patients <- data_use_lasso_all_amount %>%
  filter(transfusion == 1)

# Check for missing values
colSums(is.na(only_transf_patients))

# Imputing for missing data
imp <- mice(only_transf_patients, seed = 123, m = 20, print = FALSE)
only_transf_patients_v1 <- complete(imp, action = 1)

# Create a sequence of repeat indices from 1 to 5, since the assessment will be completed for 5 repeats
repeats <- seq(from = 1, to = 5, by = 1)

#### RBC ####

# Setting variables for RBC Lasso
rbc_amount_data <- only_transf_patients_v1 %>%
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_rbc)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_rbc <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_rbc <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_rbc <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_rbc <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_rbc <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_rbc <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_rbc <- sample(nrow(rbc_amount_data), round(nrow(rbc_amount_data) / 2))
  # Create a model matrix
  x_all_rbc <- model.matrix(tot_24_rbc ~ ., rbc_amount_data)
  # Select training set
  x_train_rbc <- x_all_rbc[train_indices_rbc, -1]
  # Select the rest as test set
  x_validation_rbc <- x_all_rbc[-train_indices_rbc, -1]
  # Select response set for training
  y_train_rbc <- rbc_amount_data$tot_24_rbc[train_indices_rbc]
  # Select response set for testing
  y_validation_rbc <- rbc_amount_data$tot_24_rbc[-train_indices_rbc]
  # Fit the lasso model
  lasso_mod_rbc <- glmnet(x_train_rbc, y_train_rbc, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_rbc, label = T, xvar = "lambda")
  lasso_plot_rbc[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_rbc <- cv.glmnet(x_train_rbc, y_train_rbc, alpha = 1, 
                            type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_rbc)
  # Store the plot
  r2_plot_rbc[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_rbc[i] <- cv_lasso_rbc$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_rbc[i] <- coef(cv_lasso_rbc, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_rbc <- predict(
    lasso_mod_rbc, 
    newx = x_validation_rbc, 
    s = cv_lasso_rbc$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_rbc <- sum((y_validation_rbc - mean(y_validation_rbc))^2)
  ss_residual_rbc <- sum((y_validation_rbc - pred_lasso_rbc)^2)
  r_squared_rbc <- 1 - (ss_residual_rbc / ss_total_rbc)
  lasso_r2_rbc[i] <- r_squared_rbc
  
  # Calculate Mean Squared Error (MSE)
  mse_rbc <- mean((y_validation_rbc - pred_lasso_rbc)^2)
  lasso_mse_rbc[i] <- mse_rbc
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_rbc <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("RBC", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_rbc, 
  MSE = lasso_mse_rbc       
)
lasso_performance_df_rbc

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_rbc <- lapply(seq_along(coef_rbc), function(i) {
  mat <- coef_rbc[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_rbc <- as.data.frame(as.matrix(mat))
  df_rbc$Feature <- rownames(df_rbc)
  colnames(df_rbc) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_rbc <- df_rbc[df_rbc[[1]] != 0, , drop = FALSE]
  df_rbc
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_rbc <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_rbc)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_rbc[is.na(nonzero_coef_rbc)] <- 0
nonzero_coef_rbc

#### FFP ####

# Setting variables for FFP Lasso
ffp_amount_data <- only_transf_patients_v1 %>%
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_ffp)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_ffp <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_ffp <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_ffp <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_ffp <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_ffp <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_ffp <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_ffp <- sample(nrow(ffp_amount_data), round(nrow(ffp_amount_data) / 2))
  # Create a model matrix
  x_all_ffp <- model.matrix(tot_24_ffp ~ ., ffp_amount_data)
  # Select training set
  x_train_ffp <- x_all_ffp[train_indices_ffp, -1]
  # Select the rest as test set
  x_validation_ffp <- x_all_ffp[-train_indices_ffp, -1]
  # Select response set for training
  y_train_ffp <- ffp_amount_data$tot_24_ffp[train_indices_ffp]
  # Select response set for testing
  y_validation_ffp <- ffp_amount_data$tot_24_ffp[-train_indices_ffp]
  # Fit the lasso model
  lasso_mod_ffp <- glmnet(x_train_ffp, y_train_ffp, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_ffp, label = T, xvar = "lambda")
  lasso_plot_ffp[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_ffp <- cv.glmnet(x_train_ffp, y_train_ffp, alpha = 1, 
                            type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_ffp)
  # Store the plot
  r2_plot_ffp[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_ffp[i] <- cv_lasso_ffp$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_ffp[i] <- coef(cv_lasso_ffp, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_ffp <- predict(
    lasso_mod_ffp, 
    newx = x_validation_ffp, 
    s = cv_lasso_ffp$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_ffp <- sum((y_validation_ffp - mean(y_validation_ffp))^2)
  ss_residual_ffp <- sum((y_validation_ffp - pred_lasso_ffp)^2)
  r_squared_ffp <- 1 - (ss_residual_ffp / ss_total_ffp)
  lasso_r2_ffp[i] <- r_squared_ffp
  
  # Calculate Mean Squared Error (MSE)
  mse_ffp <- mean((y_validation_ffp - pred_lasso_ffp)^2)
  lasso_mse_ffp[i] <- mse_ffp
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_ffp <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("FFP", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_ffp, 
  MSE = lasso_mse_ffp       
)
lasso_performance_df_ffp

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_ffp <- lapply(seq_along(coef_ffp), function(i) {
  mat <- coef_ffp[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_ffp <- as.data.frame(as.matrix(mat))
  df_ffp$Feature <- rownames(df_ffp)
  colnames(df_ffp) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_ffp <- df_ffp[df_ffp[[1]] != 0, , drop = FALSE]
  df_ffp
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_ffp <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_ffp)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_ffp[is.na(nonzero_coef_ffp)] <- 0
nonzero_coef_ffp

#### PLT ####

# Setting variables for PLT Lasso
plt_amount_data <- only_transf_patients_v1 %>%
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_plt)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_plt <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_plt <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_plt <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_plt <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_plt <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_plt <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_plt <- sample(nrow(plt_amount_data), round(nrow(plt_amount_data) / 2))
  # Create a model matrix
  x_all_plt <- model.matrix(tot_24_plt ~ ., plt_amount_data)
  # Select training set
  x_train_plt <- x_all_plt[train_indices_plt, -1]
  # Select the rest as test set
  x_validation_plt <- x_all_plt[-train_indices_plt, -1]
  # Select response set for training
  y_train_plt <- plt_amount_data$tot_24_plt[train_indices_plt]
  # Select response set for testing
  y_validation_plt <- plt_amount_data$tot_24_plt[-train_indices_plt]
  # Fit the lasso model
  lasso_mod_plt <- glmnet(x_train_plt, y_train_plt, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_plt, label = T, xvar = "lambda")
  lasso_plot_plt[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_plt <- cv.glmnet(x_train_plt, y_train_plt, alpha = 1, 
                            type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_plt)
  # Store the plot
  r2_plot_plt[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_plt[i] <- cv_lasso_plt$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_plt[i] <- coef(cv_lasso_plt, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_plt <- predict(
    lasso_mod_plt, 
    newx = x_validation_plt, 
    s = cv_lasso_plt$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_plt <- sum((y_validation_plt - mean(y_validation_plt))^2)
  ss_residual_plt <- sum((y_validation_plt - pred_lasso_plt)^2)
  r_squared_plt <- 1 - (ss_residual_plt / ss_total_plt)
  lasso_r2_plt[i] <- r_squared_plt
  
  # Calculate Mean Squared Error (MSE)
  mse_plt <- mean((y_validation_plt - pred_lasso_plt)^2)
  lasso_mse_plt[i] <- mse_plt
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_plt <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("PLT", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_plt, 
  MSE = lasso_mse_plt       
)
lasso_performance_df_plt

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_plt <- lapply(seq_along(coef_plt), function(i) {
  mat <- coef_plt[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_plt <- as.data.frame(as.matrix(mat))
  df_plt$Feature <- rownames(df_plt)
  colnames(df_plt) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_plt <- df_plt[df_plt[[1]] != 0, , drop = FALSE]
  df_plt
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_plt <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_plt)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_plt[is.na(nonzero_coef_plt)] <- 0
nonzero_coef_plt

#### CRYO ####

# Setting variables for CRYO Lasso
cryo_amount_data <- only_transf_patients_v1 %>%
  select(Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
         cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
         thyroid_disease, evlp, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
         preop_ecls, intraop_ecls, las, transfusion, tot_24_cryo)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_cryo <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_cryo <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_cryo <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_cryo <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_cryo <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_cryo <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_cryo <- sample(nrow(cryo_amount_data), round(nrow(cryo_amount_data) / 2))
  # Create a model matrix
  x_all_cryo <- model.matrix(tot_24_cryo ~ ., cryo_amount_data)
  # Select training set
  x_train_cryo <- x_all_cryo[train_indices_cryo, -1]
  # Select the rest as test set
  x_validation_cryo <- x_all_cryo[-train_indices_cryo, -1]
  # Select response set for training
  y_train_cryo <- cryo_amount_data$tot_24_cryo[train_indices_cryo]
  # Select response set for testing
  y_validation_cryo <- cryo_amount_data$tot_24_cryo[-train_indices_cryo]
  # Fit the lasso model
  lasso_mod_cryo <- glmnet(x_train_cryo, y_train_cryo, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_cryo, label = T, xvar = "lambda")
  lasso_plot_cryo[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_cryo <- cv.glmnet(x_train_cryo, y_train_cryo, alpha = 1, 
                             type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_cryo)
  # Store the plot
  r2_plot_cryo[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_cryo[i] <- cv_lasso_cryo$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_cryo[i] <- coef(cv_lasso_cryo, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_cryo <- predict(
    lasso_mod_cryo, 
    newx = x_validation_cryo, 
    s = cv_lasso_cryo$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_cryo <- sum((y_validation_cryo - mean(y_validation_cryo))^2)
  ss_residual_cryo <- sum((y_validation_cryo - pred_lasso_cryo)^2)
  r_squared_cryo <- 1 - (ss_residual_cryo / ss_total_cryo)
  lasso_r2_cryo[i] <- r_squared_cryo
  
  # Calculate Mean Squared Error (MSE)
  mse_cryo <- mean((y_validation_cryo - pred_lasso_cryo)^2)
  lasso_mse_cryo[i] <- mse_cryo
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_cryo <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("CRYO", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_cryo, 
  MSE = lasso_mse_cryo       
)
lasso_performance_df_cryo

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_cryo <- lapply(seq_along(coef_cryo), function(i) {
  mat <- coef_cryo[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_cryo <- as.data.frame(as.matrix(mat))
  df_cryo$Feature <- rownames(df_cryo)
  colnames(df_cryo) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_cryo <- df_cryo[df_cryo[[1]] != 0, , drop = FALSE]
  df_cryo
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_cryo <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_cryo)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_cryo[is.na(nonzero_coef_cryo)] <- 0
nonzero_coef_cryo

#### Lasso Regression: Amount of Transfusion with Lit Review Variables  ####

#### RBC - LIT ####

# Setting variables for RBC Lasso
rbc_amount_data_lit <- only_transf_patients_v1 %>%
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, 
         preop_ecls, intraop_ecls, transfusion, tot_24_rbc)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_rbc_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_rbc_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_rbc_lit <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_rbc_lit <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_rbc_lit <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_rbc_lit <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_rbc_lit <- sample(nrow(rbc_amount_data_lit), round(nrow(rbc_amount_data_lit) / 2))
  # Create a model matrix
  x_all_rbc_lit <- model.matrix(tot_24_rbc ~ ., rbc_amount_data_lit)
  # Select training set
  x_train_rbc_lit <- x_all_rbc_lit[train_indices_rbc_lit, -1]
  # Select the rest as test set
  x_validation_rbc_lit <- x_all_rbc_lit[-train_indices_rbc_lit, -1]
  # Select response set for training
  y_train_rbc_lit <- rbc_amount_data_lit$tot_24_rbc[train_indices_rbc_lit]
  # Select response set for testing
  y_validation_rbc_lit <- rbc_amount_data_lit$tot_24_rbc[-train_indices_rbc_lit]
  # Fit the lasso model
  lasso_mod_rbc_lit <- glmnet(x_train_rbc_lit, y_train_rbc_lit, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_rbc_lit, label = T, xvar = "lambda")
  lasso_plot_rbc_lit[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_rbc_lit <- cv.glmnet(x_train_rbc_lit, y_train_rbc_lit, alpha = 1, 
                                type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_rbc_lit)
  # Store the plot
  r2_plot_rbc_lit[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_rbc_lit[i] <- cv_lasso_rbc_lit$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_rbc_lit[i] <- coef(cv_lasso_rbc_lit, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_rbc_lit <- predict(
    lasso_mod_rbc_lit, 
    newx = x_validation_rbc_lit, 
    s = cv_lasso_rbc_lit$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_rbc_lit <- sum((y_validation_rbc_lit - mean(y_validation_rbc_lit))^2)
  ss_residual_rbc_lit <- sum((y_validation_rbc_lit - pred_lasso_rbc_lit)^2)
  r_squared_rbc_lit <- 1 - (ss_residual_rbc_lit / ss_total_rbc_lit)
  lasso_r2_rbc_lit[i] <- r_squared_rbc_lit
  
  # Calculate Mean Squared Error (MSE)
  mse_rbc_lit <- mean((y_validation_rbc_lit - pred_lasso_rbc_lit)^2)
  lasso_mse_rbc_lit[i] <- mse_rbc_lit
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_rbc_lit <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("RBC", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_rbc_lit, 
  MSE = lasso_mse_rbc_lit       
)
lasso_performance_df_rbc_lit

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_rbc_lit <- lapply(seq_along(coef_rbc_lit), function(i) {
  mat_lit <- coef_rbc_lit[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_rbc_lit <- as.data.frame(as.matrix(mat_lit))
  df_rbc_lit$Feature <- rownames(df_rbc_lit)
  colnames(df_rbc_lit) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_rbc_lit <- df_rbc_lit[df_rbc_lit[[1]] != 0, , drop = FALSE]
  df_rbc_lit
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_rbc_lit <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_rbc_lit)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_rbc_lit[is.na(nonzero_coef_rbc_lit)] <- 0
nonzero_coef_rbc_lit

#### FFP - LIT ####

# Setting variables for FFP Lasso
ffp_amount_data_lit <- only_transf_patients_v1 %>%
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, 
         preop_ecls, intraop_ecls, transfusion, tot_24_ffp)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_ffp_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_ffp_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_ffp_lit <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_ffp_lit <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_ffp_lit <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_ffp_lit <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_ffp_lit <- sample(nrow(ffp_amount_data_lit), round(nrow(ffp_amount_data_lit) / 2))
  # Create a model matrix
  x_all_ffp_lit <- model.matrix(tot_24_ffp ~ ., ffp_amount_data_lit)
  # Select training set
  x_train_ffp_lit <- x_all_ffp_lit[train_indices_ffp_lit, -1]
  # Select the rest as test set
  x_validation_ffp_lit <- x_all_ffp_lit[-train_indices_ffp_lit, -1]
  # Select response set for training
  y_train_ffp_lit <- ffp_amount_data_lit$tot_24_ffp[train_indices_ffp_lit]
  # Select response set for testing
  y_validation_ffp_lit <- ffp_amount_data_lit$tot_24_ffp[-train_indices_ffp_lit]
  # Fit the lasso model
  lasso_mod_ffp_lit <- glmnet(x_train_ffp_lit, y_train_ffp_lit, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_ffp_lit, label = T, xvar = "lambda")
  lasso_plot_ffp_lit[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_ffp_lit <- cv.glmnet(x_train_ffp_lit, y_train_ffp_lit, alpha = 1, 
                                type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_ffp_lit)
  # Store the plot
  r2_plot_ffp_lit[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_ffp_lit[i] <- cv_lasso_ffp_lit$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_ffp_lit[i] <- coef(cv_lasso_ffp_lit, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_ffp_lit <- predict(
    lasso_mod_ffp_lit, 
    newx = x_validation_ffp_lit, 
    s = cv_lasso_ffp_lit$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_ffp_lit <- sum((y_validation_ffp_lit - mean(y_validation_ffp_lit))^2)
  ss_residual_ffp_lit <- sum((y_validation_ffp_lit - pred_lasso_ffp_lit)^2)
  r_squared_ffp_lit <- 1 - (ss_residual_ffp_lit / ss_total_ffp_lit)
  lasso_r2_ffp_lit[i] <- r_squared_ffp_lit
  
  # Calculate Mean Squared Error (MSE)
  mse_ffp_lit <- mean((y_validation_ffp_lit - pred_lasso_ffp_lit)^2)
  lasso_mse_ffp_lit[i] <- mse_ffp_lit
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_ffp_lit <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("FFP", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_ffp_lit, 
  MSE = lasso_mse_ffp_lit       
)
lasso_performance_df_ffp_lit

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_ffp_lit <- lapply(seq_along(coef_ffp_lit), function(i) {
  mat_lit <- coef_ffp_lit[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_ffp_lit <- as.data.frame(as.matrix(mat_lit))
  df_ffp_lit$Feature <- rownames(df_ffp_lit)
  colnames(df_ffp_lit) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_ffp_lit <- df_ffp_lit[df_ffp_lit[[1]] != 0, , drop = FALSE]
  df_ffp_lit
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_ffp_lit <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_ffp_lit)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_ffp_lit[is.na(nonzero_coef_ffp_lit)] <- 0
nonzero_coef_ffp_lit

#### PLT - LIT ####

# Setting variables for PLT Lasso
plt_amount_data_lit <- only_transf_patients_v1 %>%
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, 
         preop_ecls, intraop_ecls, transfusion, tot_24_plt)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_plt_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_plt_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_plt_lit <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_plt_lit <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_plt_lit <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_plt_lit <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_plt_lit <- sample(nrow(plt_amount_data_lit), round(nrow(plt_amount_data_lit) / 2))
  # Create a model matrix
  x_all_plt_lit <- model.matrix(tot_24_plt ~ ., plt_amount_data_lit)
  # Select training set
  x_train_plt_lit <- x_all_plt_lit[train_indices_plt_lit, -1]
  # Select the rest as test set
  x_validation_plt_lit <- x_all_plt_lit[-train_indices_plt_lit, -1]
  # Select response set for training
  y_train_plt_lit <- plt_amount_data_lit$tot_24_plt[train_indices_plt_lit]
  # Select response set for testing
  y_validation_plt_lit <- plt_amount_data_lit$tot_24_plt[-train_indices_plt_lit]
  # Fit the lasso model
  lasso_mod_plt_lit <- glmnet(x_train_plt_lit, y_train_plt_lit, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_plt_lit, label = T, xvar = "lambda")
  lasso_plot_plt_lit[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_plt_lit <- cv.glmnet(x_train_plt_lit, y_train_plt_lit, alpha = 1, 
                                type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_plt_lit)
  # Store the plot
  r2_plot_plt_lit[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_plt_lit[i] <- cv_lasso_plt_lit$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_plt_lit[i] <- coef(cv_lasso_plt_lit, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_plt_lit <- predict(
    lasso_mod_plt_lit, 
    newx = x_validation_plt_lit, 
    s = cv_lasso_plt_lit$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_plt_lit <- sum((y_validation_plt_lit - mean(y_validation_plt_lit))^2)
  ss_residual_plt_lit <- sum((y_validation_plt_lit - pred_lasso_plt_lit)^2)
  r_squared_plt_lit <- 1 - (ss_residual_plt_lit / ss_total_plt_lit)
  lasso_r2_plt_lit[i] <- r_squared_plt_lit
  
  # Calculate Mean Squared Error (MSE)
  mse_plt_lit <- mean((y_validation_plt_lit - pred_lasso_plt_lit)^2)
  lasso_mse_plt_lit[i] <- mse_plt_lit
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_plt_lit <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("PLT", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_plt_lit, 
  MSE = lasso_mse_plt_lit       
)
lasso_performance_df_plt_lit

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_plt_lit <- lapply(seq_along(coef_plt_lit), function(i) {
  mat_lit <- coef_plt_lit[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_plt_lit <- as.data.frame(as.matrix(mat_lit))
  df_plt_lit$Feature <- rownames(df_plt_lit)
  colnames(df_plt_lit) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_plt_lit <- df_plt_lit[df_plt_lit[[1]] != 0, , drop = FALSE]
  df_plt_lit
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_plt_lit <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_plt_lit)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_plt_lit[is.na(nonzero_coef_plt_lit)] <- 0
nonzero_coef_plt_lit

#### CRYO - LIT ####

# Setting variables for CRYO Lasso
cryo_amount_data_lit <- only_transf_patients_v1 %>%
  select(Age, type, ECLS_CPB, ECLS_ECMO, cys_fib, Pre_Hb, Pre_Platelets, 
         Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, Hypertension, 
         preop_ecls, intraop_ecls, transfusion, tot_24_cryo)

# Create empty numeric vector to store R-squared values for each repeat
lasso_r2_cryo_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store MSE values for each repeat
lasso_mse_cryo_lit <- numeric(length = length(repeats))
# Create empty numeric vector to store minimum lambda values that maximize the R-squared for each repeat
lambda_min_cryo_lit <- numeric(length = length(repeats))
# Create empty list to store the coefficients
coef_cryo_lit <- list()
# Create empty list to store lasso coefficient path plots for each repeat
lasso_plot_cryo_lit <- list()
# Create empty list to store AUC plots for each repeat
r2_plot_cryo_lit <- list()

# Create a loop that completes 5 repeats of lasso regression, cross-validation, 
# testing on test set, and plotting R-squared
for (i in repeats) {
  # Set seed for each iteration
  set.seed(i)
  
  # Randomly select row indices for training set and split the data into training and testing sets
  train_indices_cryo_lit <- sample(nrow(cryo_amount_data_lit), round(nrow(cryo_amount_data_lit) / 2))
  # Create a model matrix
  x_all_cryo_lit <- model.matrix(tot_24_cryo ~ ., cryo_amount_data_lit)
  # Select training set
  x_train_cryo_lit <- x_all_cryo_lit[train_indices_cryo_lit, -1]
  # Select the rest as test set
  x_validation_cryo_lit <- x_all_cryo_lit[-train_indices_cryo_lit, -1]
  # Select response set for training
  y_train_cryo_lit <- cryo_amount_data_lit$tot_24_cryo[train_indices_cryo_lit]
  # Select response set for testing
  y_validation_cryo_lit <- cryo_amount_data_lit$tot_24_cryo[-train_indices_cryo_lit]
  # Fit the lasso model
  lasso_mod_cryo_lit <- glmnet(x_train_cryo_lit, y_train_cryo_lit, alpha = 1)
  # Plot and store the lasso coefficient path plot
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(lasso_mod_cryo_lit, label = T, xvar = "lambda")
  lasso_plot_cryo_lit[[i]] <- recordPlot()
  
  # Conduct cross-validation with mean squared error as the measure 
  cv_lasso_cryo_lit <- cv.glmnet(x_train_cryo_lit, y_train_cryo_lit, alpha = 1, 
                                 type.measure = "mse", nfolds = 5)
  # Plot the cross-validation error as a function of lambda
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(cv_lasso_cryo_lit)
  # Store the plot
  r2_plot_cryo_lit[[i]] <- recordPlot()
  # Extract and store the lambda value that minimizes MSE
  lambda_min_cryo_lit[i] <- cv_lasso_cryo_lit$lambda.min
  # Extract and store the relevant coefficients for the associated lambda value
  coef_cryo_lit[i] <- coef(cv_lasso_cryo_lit, s = "lambda.min")
  
  # Testing on the test set
  pred_lasso_cryo_lit <- predict(
    lasso_mod_cryo_lit, 
    newx = x_validation_cryo_lit, 
    s = cv_lasso_cryo_lit$lambda.min)
  
  # Calculate R-squared as a measure of model performance
  ss_total_cryo_lit <- sum((y_validation_cryo_lit - mean(y_validation_cryo_lit))^2)
  ss_residual_cryo_lit <- sum((y_validation_cryo_lit - pred_lasso_cryo_lit)^2)
  r_squared_cryo_lit <- 1 - (ss_residual_cryo_lit / ss_total_cryo_lit)
  lasso_r2_cryo_lit[i] <- r_squared_cryo_lit
  
  # Calculate Mean Squared Error (MSE)
  mse_cryo_lit <- mean((y_validation_cryo_lit - pred_lasso_cryo_lit)^2)
  lasso_mse_cryo_lit[i] <- mse_cryo_lit
}

# Compile R-squared and MSE values into a dataframe
lasso_performance_df_cryo_lit <- data.frame(
  Model = rep("Lasso Regression", 5),
  Vars = rep("CRYO", 5),
  Iteration = paste0("Iteration", 1:5),  
  R_Squared = lasso_r2_cryo_lit, 
  MSE = lasso_mse_cryo_lit       
)
lasso_performance_df_cryo_lit

# Convert each matrix into a dataframe with Feature and Coefficient columns
coef_dfs_cryo_lit <- lapply(seq_along(coef_cryo_lit), function(i) {
  mat_lit <- coef_cryo_lit[[i]]
  
  # Convert matrix to a dataframe and extract non-zero coefficients
  df_cryo_lit <- as.data.frame(as.matrix(mat_lit))
  df_cryo_lit$Feature <- rownames(df_cryo_lit)
  colnames(df_cryo_lit) <- c(paste0("Repetition", i), "Feature")  # Rename coefficient column
  
  # Keep only non-zero coefficients
  df_cryo_lit <- df_cryo_lit[df_cryo_lit[[1]] != 0, , drop = FALSE]
  df_cryo_lit
})

# Merge all dataframes by "Feature", using full_join to align features across repetitions
nonzero_coef_cryo_lit <- Reduce(function(x, y) full_join(x, y, by = "Feature"), coef_dfs_cryo_lit)

# Replace NAs with 0 for features not present in certain repetitions
nonzero_coef_cryo_lit[is.na(nonzero_coef_cryo_lit)] <- 0
nonzero_coef_cryo_lit

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
      "Figure 9. Pruned CART trees containing all available variables for repeated trials. Relevant characteristics/factors and their threshold/categories are shown for\neach of the 5 repetitions (A-E).", 
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
      "Figure 10. ROC curves for 5 repeated trials of CART containing all available variables. Performance of the classifier is shown for each of the 5 repetitions (A-E).", 
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
      "Figure 11. Pruned CART trees containing literature-relevant variables for repeated trials. Relevant literature characteristics/factors and their threshold/categories\nare shown for each of the 5 repetitions (A-E).", 
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
      "Figure 12. ROC curves for 5 repeated trials of CART containing literature-relevant variables. Performance of the classifier is shown for each of the 5\nrepetitions (A-E).", 
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
    mean_auc <- mean(AUC),
    sd_auc <- sd(AUC)
    ) %>%
  ungroup() %>%
  # Rename column
  rename(
    mean_auc = "mean_auc <- mean(AUC)",
    sd_auc = "sd_auc <- sd(AUC)"
  )

# View the averaged AUC scores for all the models
auc_avg_df

##### QUESTION 2  #######

#########################################
##### Loading and Preparing the Data #####
#########################################

# Anything that has !!! means it needs review (by group)
# In conjunction with the above, what is the impact of transfusion on patient outcomes, including mortality? #

# Modify data_use to include the transfusion binary indicator (as done prevoiusly for lasso regression)
data_use <- data_use %>%
  mutate(transfusion = if_else(
    rowSums(across(c(intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
                     rbc_72_tot, ffp_72_tot, plt_72_tot, cryo_72_tot))) == 0, "0", "1"
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
sup_dwd <- data_with_dead  %>%
  mutate(has_value = if_else((data_with_dead$time_death < 365),"1","0"))
         sup_dwd$has_value <- as.numeric(sup_dwd$has_value)
         
# convert NAs to 0 as we know they did not die
sup_dwd <- sup_dwd %>%
mutate(
has_value = if_else(is.na(has_value), 0, has_value))

# duplicate data_with_dead before modification of time to death to account for censoring for creating plot A3
uc_dwd <- data_with_dead
    
# add the new variables to the EDA -> for results 
    # Patient deaths (from has_value)
A1 <- ggplot(data_with_dead, aes(x = factor(has_value))) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(title = "Bar Plot of Known Patients Deaths", x = "Patient Death (1 = Yes, 0 = No)", y = "Count")
table(data_with_dead$has_value)

# Patient deaths (from has_value) in sup_dwd dataset -> this is to show number of patients that died before 1 year
A2 <- ggplot(sup_dwd, aes(x = factor(has_value))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Known Patients Deaths", x = "Patient Death (1 = Yes, 0 = No)", y = "Count")
table(sup_dwd$has_value)
    
# time to death - only for those who died 
A3 <- ggplot(uc_dwd, aes(x = time_death)) + 
      geom_histogram(bins = 10, fill = "lightblue", color = "black") +
      labs(title = "Histogram of Patient Time to Death", x = "Time to Death (days)", y = "Frequency")
    
# time_death has a lot of missingness since DEATH_DATE has a lot of missingness
table(data_with_dead$time_death)
    
# therefore if there is no DEATH_DATE assume that the patient was alive after a year so change the NA to 365 (since the last recorded follow up in the dataset was after 12 months)
# censored patients given value of 365
data_with_dead <- data_with_dead %>%
      mutate(
        time_death = if_else(is.na(time_death), 365, time_death))
# repeat with sup_dwd
sup_dwd <- sup_dwd %>%
  mutate(
    time_death = if_else(is.na(time_death), 365, time_death))
# for sup_dwd also convert anything above 365 to 365 RIGHT !!!
sup_dwd <- sup_dwd %>%
  mutate(
    time_death = if_else((time_death > 365), 365, time_death))
    
# time to death - including censored at 365 days - DEATH_DATE data set 
A4 <- ggplot(data_with_dead, aes(x = time_death)) + 
      geom_histogram(bins = 10, fill = "lightblue", color = "black") +
      labs(title = "Histogram of Patient Time to Death", x = "Time to Death (days)", y = "Frequency")
    
# time to death - including censored at 365 days - sup_dwd dataset 
A5 <- ggplot(sup_dwd, aes(x = time_death)) + 
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Patient Time to Death", x = "Time to Death (days)", y = "Frequency")
    
# Ensure all variables that will be included in the data set for the analyses are relevant 
# removed study Id and tx_db_id -> just identifiers
# removed or_date and DEATH_DATE -> represented in time_death
# removed first transplant since it is the same as redo transplant 
# remove due to high missingness: pre_fibrinogen, rbc_0_24, rbc_24_48, rbc_48_72, ffp_0_24, ffp_24_48, ffp_48_72, plt_0_24, plt_24_48, plt_48_72, cryo_0_24, cryo_24_48, cyro_48_72
# SHOULD I ALSO REMOVE ALIVE 30 DAYS, 90 DAYS and 1 YEAR? !!!
data_with_dead <- data_with_dead %>%
  mutate(type = if_else(Type == "Bilateral", "Double", "Single")) %>% # modifying type to single or double transplant
      select(type, gender_male, aat_deficiency, cys_fib, ipah, 
             ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud, renal_fail, stroke, 
             liver_disease, thyroid_disease, redo_transplant, evlp, preop_ecls,
             las, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Creatinine,
             intraop_ecls, ECLS_ECMO, ECLS_CPB, intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
             icu_stay, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS,
             rbc_72_tot,ffp_72_tot, plt_72_tot, cryo_72_tot,
             tot_24_rbc, massive_transfusion, Age, BMI, time_death, has_value, transfusion)

# same for sup_dwd 
sup_dwd <- sup_dwd %>%
  mutate(type = if_else(Type == "Bilateral", "Double", "Single")) %>% # modifying type to single or double transplant
  select(type, gender_male, aat_deficiency, cys_fib, ipah, 
         ild, pulm_other, cad, Hypertension, t1d, t2d, gerd_pud, renal_fail, stroke, 
         liver_disease, thyroid_disease, redo_transplant, evlp, preop_ecls,
         las, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Creatinine,
         intraop_ecls, ECLS_ECMO, ECLS_CPB, intra_plasma, intra_packed_cells, Intra_Platelets, Intra_Cryoprecipitate,
         icu_stay, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS,
         rbc_72_tot,ffp_72_tot, plt_72_tot, cryo_72_tot,
         tot_24_rbc, massive_transfusion, Age, BMI, time_death, has_value, transfusion)
# included everything from the Lasso model -> plus blood transfusion information essentially 
#Age, type, aat_deficiency, ECLS_CPB, ECLS_ECMO, cys_fib, ipah, ild, pulm_other, 
#cad, Hypertension, t2d, t1d, gerd_pud, renal_fail, stroke, liver_disease, 
#thyroid_disease, evlp, Pre_Hb, Pre_Hct, Pre_Platelets, 
#Pre_INR, Pre_PTT, Pre_Creatinine, redo_transplant, 
#preop_ecls, intraop_ecls, las, transfusion)

### ASK TRINELY !!! - from the model results of the coefficients with all we did not include
# liver_disease, Pre_Hct -> OTHERS CHECK BUT JUST COPY PASTE?? !!! 
### SO SHOULD I KEEP THEM IN HERE OR REMOVE????

#### get the Q2 EDA plots into one figure for simplicity 
# store the plots in a list
q2_eda <- list(A1, A2, A3, A4, A5)
q2eda_plots <- ggarrange(plotlist = q2_eda,
                             labels = c("A", "B", "C", "D", "E"),
                             ncol = 3,
                             nrow = 2,
                             align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure #. Exploratory data analysis for additional variables required for surivival analysis. A) & B) bar plots of patient deaths (X-axis) counts (Y-axis) for pateints that died at any time and before 1 year, respectively. 
      C), D), & E) histograms showing time to death (X-axis) and frequency (Y-axis) for only patients who died, patients who died or were censored at 365 days, and pateints who died before 365 days or were censored at 365 days, respectively", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
q2eda_plots
ggsave("q2eda_plots.png", q2eda_plots, width = 12, height = 8, dpi = 300)

#########################################
##### Primary Survival Analysis #####
#########################################
# done on sup_dwd dataset
# add the library required for the survival analysis 
library(survival)
library(survminer)

##### Kaplan-Meier Survival Curves #####
# create an unstratified Kaplan-Meier estimate 
s1 <- survfit(Surv(time_death, has_value==1)~1, data=sup_dwd)

# Kaplan-Meier Curve with ggplot 
# modify the y axis to only show probabilities above 0.8 since otherwise steps are not visible 
s1plot <- ggsurvplot(
  s1,
  xlab = "Time (days)",
  ylab = "Survival Probability",
  conf.int = TRUE,
  conf.int.style = "step", # dotted for confidence intervals
  legend = "none",         # remove legend since not stratified
  ylim = c(0.8, 1),
  ggtheme = theme_minimal() # minimal theme
)

# create a stratified surivival anlysis - vsiual for log rank test
s3 <- survfit(Surv(time_death, has_value==1)~transfusion, data=sup_dwd)

# Kaplan-Meier Curve for stratified
s3plot <- ggsurvplot(
  s3,
  xlab = "Time (days)",
  ylab = "Survival Probability",
  conf.int = TRUE,
  conf.int.style = "step", # dotted for confidence intervals
  palette = c("#E41A1C", "#377EB8"), # add colors for the stratified groups
  legend.title = "Transfusion",
  ylim = c(0.8, 1),
  legend.labs = c("Yes", "No"), # add legend labels
  ggtheme = theme_minimal()
)

##### get the Kaplan-Meier Curves into one figure for simplicity 
# Extract ggplot objects from ggsurvplot objects
s1plot <- s1plot$plot
s3plot <- s3plot$plot
# store the plots in a list
q2_km <- list(s1plot, s3plot)
q2km_plots <- ggarrange(plotlist = q2_km,
                         labels = c("A", "B"),
                         ncol = 2,
                         nrow = 1,
                         align = "hv") %>%
  annotate_figure(
    bottom = text_grob(
      "Figure #. Kaplan-Meier curves of survival estimate with the X-axis representing the time (in days) and the Y-axis showing the survival probaility beginning at 0.8. 
      A) unstratified. B) stratified by whether or not pateints got blood transfusions.", 
      size = 12, hjust = 0, x = unit(5.5, "pt"), face = "italic"
    )
  )
q2km_plots
ggsave("q2km_plots.png", q2km_plots, width = 12, height = 8, dpi = 300)

##### Log Rank Test #####
# must first check the proportional hazard assumption using function set to "cloglog"
# plot a cloglog plot against log(t)
plot(survfit(Surv(time_death, has_value==1)~transfusion, data=sup_dwd), fun = "cloglog", col=1:2, xlab = "Time (days)", ylab="log[log(Survival probability)]")
# add a legend with col to distinguish levels
legend("topleft",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:2)

# Run the log-rank test # CRYSTAL GOT A P OF 0.5 !!! a p value of 1 is the max !!!
LR_test1 <- survdiff(Surv(time_death, has_value==1) ~ transfusion, data=sup_dwd)
LR_test1

##### Cox Proportional Hazard Model #####
# using the rule of thumb for 10 events per variable
# citation in report (from https://pmc.ncbi.nlm.nih.gov/articles/PMC5045274/#:~:text=The%20choice%20of%20an%20adequate,events%20per%20variable%20(EPV).)
# this will be a significant limitation as there are only 23 events -> would be limited to 2 variables but that seems very small 
## any more may be subject to overfitting or estimates that are highly sensitive to small changes 

# Run a Cox proportional hazard model including variables found to be relevant in Question 1 Lasso regression (all) - see report !!! 
coxmod1 <- coxph(Surv(time_death, has_value==1) ~ transfusion + intraop_ecls + Pre_Hb + Pre_Platelets + redo_transplant,data=sup_dwd)
# warning that suggest redo_transplant has too few observations in the redo category
# therefore remove it
coxmod2 <- coxph(Surv(time_death, has_value==1) ~ transfusion + intraop_ecls + Pre_Hb + Pre_Platelets ,data=sup_dwd)
summary(coxmod2)
# test proportional hazard
cox.zph(coxmod2)


#########################################
##### Secondary Survival Analysis #####
#########################################
# done on data_with_dead dataset -> performed for supplementary methods and interest

# create an unstratified Kaplan-Meier estimate 
sf1 <- survfit(Surv(time_death, has_value==1)~1, data=data_with_dead)
    
# Kaplan-Meier Curve
plot(sf1,xscale = 365.25, xlab = "Time (years)", ylab="Survival Probability", conf.int = 0.95) 
    
# create a stratified surivival anlysis - CAN I OR SHOULD I DO THIS !!! probably cause log rank is looking at this right 
sf3 <- survfit(Surv(time_death, has_value==1)~transfusion, data=data_with_dead)
    
# Kaplan-Meier Curve 
plot(sf3,xscale = 365.25, xlab = "Time (years)", ylab="Survival Probability", conf.int = 0.95, col=1:2, fun = "S") 
legend("topright",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:2)
    
# Preform the log-rank test
# must first check the proportional hazard assumption using function set to "cloglog"
# plot a cloglog plot against log(t)
plot(survfit(Surv(time_death, has_value==1)~transfusion, data=data_with_dead), fun = "cloglog", col=1:2, xlab = "Time (days)", ylab="log[log(Survival probability)]")
# add a legend with col to distinguish levels
legend("topleft",legend = c("Transfusion", "No Transfusion"),lty = 1, col = 1:2)
    
# Run the log-rank test 
LR_test3 <- survdiff(Surv(time_death, has_value==1) ~ transfusion, data=data_with_dead)
LR_test3
    
# Run a Cox proportional hazard model including variables related to the patient (orange) and blood transfused into the patients and their 72 hour stats (yellow)
# Should i be including like a LOT of other things !!! -> do i check for multicollinearity?
coxmod3 <- coxph(Surv(time_death, has_value==1) ~ transfusion + gender_male + Age + BMI + intra_plasma + intra_packed_cells + Intra_Platelets + Intra_Cryoprecipitate
                     + rbc_72_tot + ffp_72_tot + plt_72_tot + cryo_72_tot, data=data_with_dead)
summary(coxmod3)
## SEE LIT -> HYPERTENSION AND ANEMIA !!!
## maybe aslo add relevant predictors from the first quetsion !!!
    
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

#See EDA that hospital LOS and ICU LOS is not normally distributed
# therefore t test and linear regression will not suffice as the assumptions are not met
# since we are writing a report though we could include the linear regression stuff in the appendix but if we dont want to we just have to delete it !!!
attach(sup_dwd)

# ICU stay 
boxplot(icu_stay~transfusion) # this is really ugly !!!
wilcox.test(icu_stay~transfusion)

# hospital stay
boxplot(HOSPITAL_LOS~transfusion) # this is really ugly !!!
wilcox.test(HOSPITAL_LOS~transfusion)

detach(sup_dwd)

##### ANYTHING FROM HERE CAN PROBABLY BE DELETED DEPENDING ON WHAT WE WANT AS SUP MATERIAL #####
    
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
