crvarFunc <- function(var) {
  var <- enquo(var)
  var2 <- quo_name(var)

  levs <- names(table(long %>% dplyr::select(!!var)))

  namelevs <- gsub(" ", "", levs)
  namelevs <- gsub(">=|>", "over", namelevs)
  namelevs <- gsub("<=|<", "under", namelevs)

  for (i in 2:length(levs)) {
    varname <- paste0(var2, "_num_", gsub(" ", "", namelevs[i]))

    long <<- long %>%
      mutate(!!varname := case_when(
        is.na(!!var) ~ NA_real_,
        !!var == levs[i] ~ 1,
        TRUE ~ 0
      ))
  }
}
