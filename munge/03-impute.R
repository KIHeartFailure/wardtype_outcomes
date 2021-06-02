

# Impute missing values ---------------------------------------------------

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(timeTodeath, death == "yes") ~ 1, data = wdata, method = "breslow"))
wdata <- left_join(wdata, na, by = c("time_out_hf_hosp" = "time"))

ini <- mice(wdata, maxit = 0, print = F)

# Variables that should not be used in imputation model, clinic is not included in the imputation model.

noimpvars <- c(
  "lvef",
  "indexYear_cat", "bp.dia", "clinic_num_medicine", "clinic",
  "death", "out_hf_hosp", "cvDeath", "time_out_hf_hosp", "timeTodeath", "date",
  "out_hf_hosp_cr",
  "cvdeath_cr",
  "inhospmort",
  "age", "scb.dispinc", "bmi", "bmi_cat",
  "bp.sys", "heartRate", "hb", "GFR", "ntProBNP"
)


pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change mthod used in impuation to prop odds model
meth <- ini$method
meth[c("smoking", "nyha", "scb.education_cat")] <- "polr"
meth[noimpvars] <- ""
meth["bmi_cat"] <- "" # too few non-missing obs to include in models

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imp <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "wdata"),
    .packages = "mice"
  ) %dopar% {
    mice(wdata,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()