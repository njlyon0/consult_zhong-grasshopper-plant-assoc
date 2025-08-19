#' @title Tidy ANOVA Summary Table
#'
#' @description Accept the output of `stats::aov` and parse it into a tidy table for ease of later inclusion in a scientific paper.
#'
#' @param mod (aov/lm) Model fit object returned by `stats::aov`
#'
#' @return (data.frame) Tidy table of ANOVA results with columns for fixed effect(s), degrees of freedom, F statistic, and P value.
#'
parse_aov <- function(mod = NULL){

  # Errors for 'mod'
  if(is.null(mod) || all(c("aov", "lm") %in% class(mod) != T))
    stop("'mod' must be provided as an object returned by `stats::aov`")

  # Grab the response variable from the model call
  resp_var <- names(mod$model)[1]

  # Get the summary of the model
  mod_smry <- summary(object = mod)

  # Grab the table as a data.frame
  mod_tab <- as.data.frame(mod_smry[[1]])

  # Tidy that up slightly
  mod_tidy <- mod_tab %>%
    # Get terms into a column
    tibble::rownames_to_column(.data = ., var = "fixed_effects") %>%
    # Add the response variable before the fixed effect column
    dplyr::mutate(response_var = resp_var,
                  .before = dplyr::everything()) %>%
    # Drop sums of squares/mean squares
    dplyr::select(-dplyr::ends_with("Sq")) %>%
    # Tidy up remaining column names
    dplyr::rename(df = Df,
                  f_statistic = `F value`,
                  p_value = `Pr(>F)`)

  # Return it
  return(mod_tidy) }

# End ----
