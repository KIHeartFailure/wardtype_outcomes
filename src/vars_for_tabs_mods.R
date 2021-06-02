tabvars <- c(
  "gender", "age", "age_cat", "location",
  "indexYear_cat", "scb.marital.status_cat",
  "scb.education_cat", "scb.dispinc", "scb.dispinc_cat",
  "bmi", "bmi_cat", "durationHF", "nyha",
  "lvef", 
  "bp.sys", "bp.sys_cat", "bp.dia", "heartRate", "heartRate_cat",
  "smoking",
  "d_Atrial_fib_flutter",
  "d_Diabetes",
  "d_Hypertension",
  "com_COPD",
  "com_stroke_tia",
  "d_IHD",
  "d_Valvedisease",
  "com_cancer",
  "com_liver",
  "device_cat",
  "hb", "hb_cat", "GFR", "GFR_cat", "ntProBNP", "ntProBNP_cat",
  "betaBlocker", "ras", "MRA", "diuretics", "digoxin", "statins", "nitrates", 
  "asaTRC", "anticoagulantia",
  "followUp.HF", "followUp.location"
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "lvef",
  # should not be included according to clinical reasons
  "bp.dia", "statins", "nitrates", "asaTRC", "anticoagulantia",
  # should not be included since are included as categorical variables
  "age", "scb.dispinc", "bmi", "bmi_cat", "bp.sys", "heartRate", "hb", "GFR", 
  "ntProBNP"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

# numeric vars for F&G model (modvars with _num_), test performed for vice versa 
# (any _num_ that are not in modvars)
modvarscr <- names(imp$data[grepl("_num_", names(imp$data))])[word(names(imp$data)[grepl("_num_", names(imp$data))], 1, sep = "_num_") %in% modvars]
