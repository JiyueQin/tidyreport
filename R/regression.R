#####################################################################################
#     Function format_estimates_table - format estimates table for logistic regression
#
######################################################################################

format_estimates_table = function(var, estimates_table, numeric_vars, ref_table){
  if(var %in% numeric_vars){
    estimates_table %>% filter(str_detect(term, paste0('^',var))) %>% mutate(header = 1)
  }else{
    estimates_table %>% filter(str_detect(term, paste0('^',var))) %>%
      mutate(term = str_remove(term, var)) %>%
      mutate(term = paste0(term, ' vs ', ref_table[var,1])) %>%
      mutate(header = 0) %>%
      add_row(term = var, .before =1, header = 1) %>% mutate_at(vars(-outcome),~ifelse(is.na(.x), '', .x))
  }

}


#####################################################################################
#     Function test_linear_hypo - test a linear hypothesis for linear regression
#
######################################################################################

test_linear_hypo = function(fit, hypo){

  test_linear = multcomp::glht(fit, hypo)
  test_out = summary(test_linear, test=multcomp::univariate())
  test_conf = confint(test_linear, calpha = multcomp::univariate_calpha())


  test_conf$confint %>% as.data.frame() %>% rownames_to_column('term') %>%
    select(term, coef = Estimate, low = lwr, up = upr) %>%
    inner_join(test_out$test$pvalues %>% as.data.frame() %>%  setNames(c('p')) %>%
                 rownames_to_column('term'))

}



#' Get a summary of regression results
#'
#' This function summarizes the results for multiple types of regression
#'
#' @param df A dataframe
#' @param outcome A string, the name of the outcome
#' @param predictor_vec A character vector of predictors
#' @param outcome_type A string, can be one of these options:linear, binary, poisson, ordinal, tobit, normal_gee,
#' binary_gee, poisson_gee, ordinal_gee, lme
#' @param format logical, format=T outputs a fomatted table, only supported for logistic regression
#' @param interaction a character vector of interaction terms
#' @param weights a numeric vector of weights
#' @param tobit_upper a number, the upper bound of tobit models
#' @param test_hypo a linear hypothesis, only for linear regression
#' @param aic logical, aic=T also outputs AIC of the model, only for linear regression
#' @param highlight, logical, highlight=T highlights p values, default is F
#' @param highlight_p, a numeric value of the p value cutoff to highlight, default is 0.05
#' @return A dataframe or a formatted html table
#' @import dplyr
#' @export
#'
#'


get_regression_estimates = function(df, outcome, predictor_vec, outcome_type, format = F, interaction = NULL,
                                    weights = NULL, tobit_upper = NULL, test_hypo = NULL, aic = F, highlight=F, highlight_p=0.05){
  formula_char = paste0(outcome, '~', paste(predictor_vec, collapse = '+'))
  if(!is.null(interaction))
  {inter_term = paste(interaction, collapse = '*')
  formula_char = paste0(formula_char, '+',inter_term )}

  formula = as.formula(formula_char)

  if(str_detect(outcome_type, 'gee|lme')){
    if(!'id'%in% colnames(df)){stop('Please make sure there is a column named `id` in your data!')}
    if(any(sort(df$id)!=df$id)){stop("Please make sure data is sorted by ID!")}}

  if(outcome_type == 'linear') {
    fit = lm(formula, data = df, weights = weights)
    CI = confint(fit) %>% as.data.frame() %>%  add_column(term = rownames(.))

    estimates_table = broom::tidy(fit) %>%
      inner_join(CI, by = 'term') %>%
      mutate(low = `2.5 %`,
             up = `97.5 %`) %>%
      select(term, coef = estimate, low, up, p = p.value)
    if(!is.null(test_hypo)){
      tests_table = test_linear_hypo(fit, test_hypo)
      estimates_table = bind_rows(estimates_table, tests_table)
    }
    if(aic){
      estimates_table = bind_rows(estimates_table, tibble(term = 'AIC', coef = AIC(fit)))
    }

  }
  if(outcome_type == 'binary') {
    fit = glm(formula, family=binomial(link='logit'), data = df)
    CI = confint.default(fit) %>% as.data.frame() %>%  add_column(term = rownames(.))

    estimates_table = broom::tidy(fit) %>%
      inner_join(CI, by = 'term') %>%
      mutate(low = exp(`2.5 %`),
             up = exp(`97.5 %`),
             OR = exp(estimate)) %>%
      select(term, coef = OR, low, up, p = p.value)
  }
  if(outcome_type == 'poisson') {
    fit = glm(formula, family=poisson, data = df)
    CI = confint.default(fit) %>% as.data.frame() %>%  add_column(term = rownames(.))

    estimates_table = broom::tidy(fit) %>%
      inner_join(CI, by = 'term') %>%
      mutate(low = exp(`2.5 %`),
             up = exp(`97.5 %`),
             RR = exp(estimate)) %>%
      select(term, coef = RR, low, up, p = p.value)
  }
  if(outcome_type=='ordinal'){
    ordinal_fit = MASS::polr(formula, data = df, Hess = T)
    estimates_table = broom::tidy(ordinal_fit) %>%
      filter(coefficient_type == 'coefficient') %>%
      mutate(p = pnorm(abs(statistic), lower.tail = F)*2) %>%
      mutate(low = exp(estimate-1.96*`std.error`),
             up = exp(estimate+1.96*`std.error`),
             OR = exp(estimate)) %>%
      select(term, coef = OR, low, up, p)

  }
  if(outcome_type == 'tobit'){
    tobit_fit = VGAM::vglm(formula, VGAM::tobit(Upper = tobit_upper), data = df, weights = weights)

    estimates_table = coef(summary(tobit_fit)) %>% as.data.frame() %>%
      add_column(term = rownames(.)) %>%
      mutate(low = Estimate-1.96*`Std. Error`,
             up = Estimate+1.96*`Std. Error`) %>%
      select(term, coef = Estimate, low, up, p = `Pr(>|z|)`)
  }
  if(outcome_type == 'normal_gee'){
    fit = gee::gee(formula, data = df, id = id, corstr = "unstructured")
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      add_column(term = rownames(.)) %>%
      mutate(low = Estimate - 1.96*`Robust S.E.`,
             up = Estimate + 1.96*`Robust S.E.`,
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = Estimate, low, up, p)

  }
  if(outcome_type == 'binary_gee'){
    fit = gee::gee(formula, data = df, family = "binomial", id = id, corstr = "unstructured")
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      add_column(term = rownames(.)) %>%
      mutate(OR = exp(Estimate),
             low = exp(Estimate - 1.96*`Robust S.E.`),
             up = exp(Estimate + 1.96*`Robust S.E.`),
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = OR, low, up, p)

  }
  if(outcome_type == 'poisson_gee'){
    fit = gee::gee(formula, data = df, family = "poisson", id = id, corstr = "unstructured")
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(RR = exp(Estimate),
             low = exp(Estimate - 1.96*`Robust S.E.`),
             up = exp(Estimate + 1.96*`Robust S.E.`),
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = RR, low, up, p)

  }
  if(outcome_type == 'ordinal_gee'){
    fit = multgee::ordLORgee(formula, id = id, data = df)
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      add_column(term = rownames(.)) %>%
      mutate(OR = exp(Estimate),
             low = exp(Estimate - 1.96*san.se),
             up = exp(Estimate + 1.96*san.se)) %>%
      select(term, coef = OR, low, up, p = "Pr(>|san.z|)")
  }

  if(outcome_type == 'lme'){
    fit = nlme::lme(formula, random = ~1|id, na.action = na.omit, data = df)
    CI = nlme::intervals(fit)$fixed %>% as.data.frame() %>% rownames_to_column('term')
    summary_fit = summary(fit)
    estimates_table = summary_fit$tTable %>% as.data.frame() %>% rownames_to_column('term') %>%
      janitor::clean_names() %>%
      inner_join(CI) %>%
      select(term, coef = value, low = lower, up = upper, p=p_value)
  }
  estimates_table = estimates_table %>%
    mutate(outcome = outcome) %>%
    mutate(p = ifelse(p<0.001, '<0.001', as.character(round(p,3))))

  if(outcome_type %in% c('linear', 'tobit', 'normal_gee', 'lme')) {
    estimates_table = estimates_table %>%
      mutate_at(vars(coef, low, up), ~ifelse(round(.,2) != 0, as.character(round(., 2)),
                                             as.character(round(.,3)))) %>%
      mutate(CI = paste0('(', low, ',', up, ')')) %>%
      mutate(CI = ifelse(term == 'AIC', NA, CI))  %>%
      select(outcome, term, estimate=coef, CI, p)}
  else {
    estimates_table = estimates_table %>%
      mutate_at(vars(coef, low, up), ~ifelse(round(.,2) != 1, as.character(round(., 2)),
                                             as.character(round(.,3)))) %>%
      mutate(CI = paste0('(', low, ',', up, ')')) %>%
      mutate(CI = ifelse(term == 'AIC', NA, CI))
    if(outcome_type %in% c('poisson_gee', 'poisson')){
      estimates_table = estimates_table %>%  select(outcome, term, RR=coef, CI, p)
    }else{
      estimates_table = estimates_table %>%  select(outcome, term, OR=coef, CI, p)
    }


  }

  if(format & is.null(interaction) &outcome_type == 'binary'){
    cat_vars = df %>% select(predictor_vec) %>% select_if(is.character) %>% colnames()
    fac_vars = df %>% select(predictor_vec) %>% select_if(is.factor) %>% colnames()
    numeric_vars = df %>%  select(predictor_vec) %>% select_if(is.numeric) %>% colnames()
    cat_ref = map_chr(cat_vars, ~df %>% pull(.x) %>% unique() %>% sort(na.last = T) %>% `[`(1))
    fac_ref = map_chr(fac_vars, ~df %>% pull(.x) %>% levels() %>% `[`(1))
    ref = bind_rows(tibble(var = cat_vars, ref = cat_ref), tibble(var = fac_vars, ref =fac_ref)) %>%
      column_to_rownames('var')

    formatted = map_df(predictor_vec, ~format_estimates_table(.x,  estimates_table, numeric_vars, ref)) %>%
      select(-outcome) %>%
      mutate(outcome = outcome) %>%
      select(outcome, everything())

    indent = formatted %>% rownames_to_column('row') %>% filter(header ==0) %>% pull(row) %>% as.numeric()

    if(highlight){
      formatted =  formatted %>%
        mutate(p = cell_spec(p, background =
                               ifelse((suppressWarnings(as.numeric(p))<highlight_p |
                                         p == '<0.001') & p != '',
                                      "yellow", "white")))
      formatted %>% select(-header) %>% kable(escape = F) %>% kable_styling(full_width = F) %>%
        collapse_rows(columns = 1, valign = "top") %>%
        add_indent(indent)
    }else{
      formatted %>% select(-header) %>% kable() %>% kable_styling(full_width = F) %>%
        collapse_rows(columns = 1, valign = "top") %>%
        add_indent(indent)
    }

  }else{
    estimates_table
  }

}
