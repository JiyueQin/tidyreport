#' Get cox coefficient summary table
#'
#' This function gets coef summary table from a cox model`
#'
#' @param df A cox fit
#' @param raw if you want to output the raw summary table, default is False
#' @return A summary table in html format
#' @import dplyr
#' @export
#'
#'

cox_summary = function(fit, raw = F){
  estimates = summary(fit)
  hr_summary = estimates$conf.int %>% as.data.frame() %>%
    tibble::add_column(term = rownames(estimates$conf.int),
               p = estimates$coefficients[,5] )%>%
    mutate(p = ifelse(p<0.001, '<0.001', as.character(round(p,3)))) %>%
    mutate_if(is.numeric, ~round(., 3)) %>%
    mutate(CI = paste0('(', `lower .95`, ',' ,`upper .95`, ')' )) %>%
    select(term, HR = `exp(coef)`, CI, p)

  if (raw) hr_summary else hr_summary %>% knitr::kable() %>% kableExtra::kable_styling()

}
