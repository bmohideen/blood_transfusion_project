# Team project script

# Install required packages
library(readxl)
library(dplyr)

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


