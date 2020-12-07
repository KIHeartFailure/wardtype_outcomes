
# Creating variables in imputed dataset -----------------------------------

# keep org imp
imp.org <- imp

# Convert to Long
long <- mice::complete(imp, action = "long", include = TRUE)

# Generate new variable needed only for Forestplot (so org variable imputed)
long <- long %>%
  mutate(
    nyha_cat = factor(case_when(
      is.na(nyha) ~ NA_character_,
      nyha %in% c("NYHA-I", "NYHA-II") ~ "I-II",
      TRUE ~ "III-IV"
    ))
  )


## Create numeric variables needed for comp risk model

crvarFunc(age_cat)
crvarFunc(gender)
crvarFunc(location)
crvarFunc(indexYear_cat)
crvarFunc(scb.marital.status_cat)
crvarFunc(scb.education_cat)
crvarFunc(scb.dispinc_cat)
crvarFunc(durationHF)
crvarFunc(nyha)
crvarFunc(smoking)
crvarFunc(d_Atrial_fib_flutter)
crvarFunc(d_Diabetes)
crvarFunc(d_Hypertension)
crvarFunc(com_COPD)
crvarFunc(com_stroke_tia)
crvarFunc(d_IHD)
crvarFunc(d_Valvedisease)
crvarFunc(com_cancer)
crvarFunc(com_liver)
crvarFunc(device_cat)
crvarFunc(EF)
crvarFunc(bp.sys_cat)
crvarFunc(heartRate_cat)
crvarFunc(hb_cat)
crvarFunc(GFR_cat)
crvarFunc(ntProBNP_cat)
crvarFunc(betaBlocker)
crvarFunc(ras)
crvarFunc(MRA)
crvarFunc(diuretics)
crvarFunc(digoxin)
crvarFunc(followUp.HF)
crvarFunc(followUp.location)

# Convert back to Mids
imput.short <- as.mids(long)
imp <- imput.short
