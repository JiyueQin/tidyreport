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
#' This function summarizes the results for multiple types of regression.
#' You can specify the model structure by providing the dataset, outcome and predictors.
#' Alternatively, you can directly provide the fitted model object.
#'
#' @param df A dataframe, optional if you provide fit
#' @param outcome A string, the name of the outcome, optional if you provide fit
#' @param predictor_vec A character vector of predictors, optional if you provide fit
#' @param outcome_type A string, can be one of these options:linear, binary, poisson, ordinal, tobit, normal_gee,
#' binary_gee, poisson_gee, ordinal_gee, lme(random intercept), lme2(random intercept and random slope), logistic_glme, cox
#' @param random_slope_var A string, the name of the random slope variable, optional
#' @param lme_method A string, can be 'REML' or "ML", default is "REML"
#' @param time_var A string, the name of the time variable for cox regression
#' @param event_var A string, the name of the event variable for cox regression
#' @param format logical, format=T outputs a formatted table, only supported for logistic regression
#' @param fit A fitted model object, optional if you provide df, outcome and predictor_vec
#' @param interaction a character vector of interaction terms, optional if you provide fit
#' @param weights a numeric vector of weights, optional if you provide fit
#' @param tobit_upper a number, the upper bound of tobit models, optional if you provide fit
#' @param test_hypo a linear hypothesis, only for linear regression
#' @param aic logical, aic=T also outputs AIC of the model, only for linear regression
#' @param highlight, logical, highlight=T highlights p values, default is F
#' @param highlight_p, a numeric value of the p value cutoff to highlight, default is 0.05
#' @return A dataframe or a formatted html table
#' @import dplyr
#' @export
#'
#'


get_regression_estimates = function(df = NULL, outcome = NULL, predictor_vec = NULL,
                                    outcome_type, random_slope_var = NULL,
                                    lme_method = 'REML', time_var = NULL, event_var = NULL, format = F,
                                    interaction = NULL,fit = NULL, weights = NULL,
                                    tobit_upper = NULL, test_hypo = NULL, aic = F,
                                    highlight=F, highlight_p=0.05){
  if(is.null(fit)){
    provide_fit = F
    if(outcome_type == 'cox'){outcome = 'surv_obj'}
    formula_char = paste0(outcome, '~', paste(predictor_vec, collapse = '+'))
    if(!is.null(interaction))
    {inter_term = paste(interaction, collapse = '*')
    formula_char = paste0(formula_char, '+',inter_term )}
    if(outcome_type == 'logistic_glme'){
      formula_char = paste0(formula_char, '+(1|id)')}

    formula = as.formula(formula_char)

    if(str_detect(outcome_type, 'gee|lme')){
      if(!'id'%in% colnames(df)){
        stop('Please make sure there is a column named `id` in your data!')}
      if(any(sort(df$id)!=df$id)){
        stop("Please make sure data is sorted by ID!")}}
  }
  else{provide_fit = T}

  if(outcome_type == 'linear') {
    if(is.null(fit)){
      fit = lm(formula, data = df, weights = weights)}
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
    if(is.null(fit)){
      fit = glm(formula, family=binomial(link='logit'), data = df)}
    CI = confint.default(fit) %>% as.data.frame() %>%  add_column(term = rownames(.))

    estimates_table = broom::tidy(fit) %>%
      inner_join(CI, by = 'term') %>%
      mutate(low = exp(`2.5 %`),
             up = exp(`97.5 %`),
             OR = exp(estimate)) %>%
      select(term, coef = OR, low, up, p = p.value)
  }
  if(outcome_type == 'poisson') {
    if(is.null(fit)){
      fit = glm(formula, family=poisson, data = df)}
    CI = confint.default(fit) %>% as.data.frame() %>%  add_column(term = rownames(.))

    estimates_table = broom::tidy(fit) %>%
      inner_join(CI, by = 'term') %>%
      mutate(low = exp(`2.5 %`),
             up = exp(`97.5 %`),
             RR = exp(estimate)) %>%
      select(term, coef = RR, low, up, p = p.value)
  }
  if(outcome_type=='ordinal'){
    if(is.null(fit)){
      fit = MASS::polr(formula, data = df, Hess = T)}
    estimates_table = broom::tidy(fit) %>%
      filter(coefficient_type == 'coefficient') %>%
      mutate(p = pnorm(abs(statistic), lower.tail = F)*2) %>%
      mutate(low = exp(estimate-1.96*`std.error`),
             up = exp(estimate+1.96*`std.error`),
             OR = exp(estimate)) %>%
      select(term, coef = OR, low, up, p)

  }
  if(outcome_type == 'tobit'){
    if(is.null(fit)){
      fit = VGAM::vglm(formula, VGAM::tobit(Upper = tobit_upper), data = df, weights = weights)}

    estimates_table = VGAM::coef(summary(fit)) %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(low = Estimate-1.96*`Std. Error`,
             up = Estimate+1.96*`Std. Error`) %>%
      select(term, coef = Estimate, low, up, p = `Pr(>|z|)`)
  }
  if(outcome_type == 'normal_gee'){
    if(is.null(fit)){
      fit = gee::gee(formula, data = df, id = id, corstr = "unstructured")}
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(low = Estimate - 1.96*`Robust S.E.`,
             up = Estimate + 1.96*`Robust S.E.`,
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = Estimate, low, up, p)

  }
  if(outcome_type == 'binary_gee'){
    if(is.null(fit)){
      fit = gee::gee(formula, data = df, family = "binomial", id = id, corstr = "unstructured")}
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(OR = exp(Estimate),
             low = exp(Estimate - 1.96*`Robust S.E.`),
             up = exp(Estimate + 1.96*`Robust S.E.`),
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = OR, low, up, p)

  }
  if(outcome_type == 'poisson_gee'){
    if(is.null(fit)){
      fit = gee::gee(formula, data = df, family = "poisson", id = id, corstr = "unstructured")}
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(RR = exp(Estimate),
             low = exp(Estimate - 1.96*`Robust S.E.`),
             up = exp(Estimate + 1.96*`Robust S.E.`),
             p = 2*pnorm(abs(`Robust z`), lower.tail = F)) %>%
      select(term, coef = RR, low, up, p)

  }
  if(outcome_type == 'ordinal_gee'){
    if(is.null(fit)){
      fit = multgee::ordLORgee(formula, id = id, data = df)}
    estimates_table = summary(fit)$coef %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      mutate(OR = exp(Estimate),
             low = exp(Estimate - 1.96*san.se),
             up = exp(Estimate + 1.96*san.se)) %>%
      select(term, coef = OR, low, up, p = "Pr(>|san.z|)")
  }

  if(outcome_type %in% c('lme', 'lme2')){
    if(is.null(fit)){
      if(outcome_type == 'lme'){
        fit = nlme::lme(formula, random = ~1|id, na.action = na.omit, data = df, method = lme_method)
      }
      if(outcome_type == 'lme2'){
        formula_char2 = paste0('~', random_slope_var,'|id')
        formula2 = as.formula(formula_char2)
        fit = nlme::lme(formula, random = formula2, na.action = na.omit, data = df, method = lme_method)
      }
    }
    CI = nlme::intervals(fit)$fixed %>% as.data.frame() %>% rownames_to_column('term')
    summary_fit = summary(fit)
    estimates_table = summary_fit$tTable %>% as.data.frame() %>% rownames_to_column('term') %>%
      janitor::clean_names() %>%
      inner_join(CI) %>%
      select(term, coef = value, low = lower, up = upper, p=p_value)
  }
  if(outcome_type == 'logistic_glme'){
    if(is.null(fit)){
      fit = lme4::glmer(formula, data = df, family = binomial)}

    summary_fit = summary(fit)
    estimates_table = summary_fit$coefficients %>% as.data.frame() %>% rownames_to_column('term') %>%
      janitor::clean_names() %>%
      mutate(OR = exp(estimate),
             low = exp(estimate - 1.96*std_error),
             up = exp(estimate + 1.96*std_error),
             p = 2*pnorm(abs(z_value), lower.tail = F)) %>%
      select(term, coef = OR, low, up, p)
  }
  if(outcome_type == 'cox'){
    outcome = paste('time to', event_var)
    if(is.null(fit)){
      surv_obj = survival::Surv(df %>% pull(time_var), df %>% pull(event_var))
      fit = survival::coxph(formula, data = df)}
    summary_fit = summary(fit)
    estimates_table = summary_fit$conf.int %>% as.data.frame() %>%
      rownames_to_column('term') %>%
      add_column(p = summary_fit$coefficients[,5] ) %>%
      janitor::clean_names() %>%
      select(term, coef = exp_coef , low = lower_95, up = upper_95, p)
  }

  estimates_table = estimates_table %>%
    mutate(outcome = outcome) %>%
    mutate(p = ifelse(p<0.001, '<0.001', as.character(round(p,3))))

  if(outcome_type %in% c('linear', 'tobit', 'normal_gee', 'lme', 'lme2')) {
    estimates_table = estimates_table %>%
      mutate_at(vars(coef, low, up),
                ~ifelse(round(.,2) != 0, as.character(round(., 2)),
                        as.character(round(.,3)))) %>%
      mutate(CI = paste0('(', low, ',', up, ')')) %>%
      mutate(CI = ifelse(term == 'AIC', NA, CI))  %>%
      select(outcome, term, estimate=coef, CI, p)}
  else {
    estimates_table = estimates_table %>%
      mutate_at(vars(coef, low, up),
                ~ifelse(round(.,2) != 1, as.character(round(., 2)),
                        as.character(round(.,3)))) %>%
      mutate(CI = paste0('(', low, ',', up, ')')) %>%
      mutate(CI = ifelse(term == 'AIC', NA, CI))
    if(outcome_type %in% c('poisson_gee', 'poisson')){
      estimates_table = estimates_table %>%
        select(outcome, term, RR=coef, CI, p)
    }
    if(outcome_type == 'cox'){
      estimates_table = estimates_table %>%
        select(outcome, term, HR=coef, CI, p)
    }else{
      estimates_table = estimates_table %>%
        select(outcome, term, OR=coef, CI, p)
    }


  }

  if(!provide_fit & format & is.null(interaction) &outcome_type == 'binary'){
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


#' Format p values
#'
#' This function formats the p values in a table
#

#' @param df A tibble or dataframe of the data
#' @param cut A number of the significance cut off, default is 0.05
#' @param p_names A character vector of the column names of p values, default is 'p'
#' @param col_names A character vector of output table column names, default is NA, which uses original names
#' @return A html table
#' @import dplyr
#' @export
#'

format_p = function(df, cut = 0.05, p_names = 'p',col_names = NA){
  df %>%
    mutate_at(p_names, ~cell_spec(., background = ifelse((suppressWarnings(as.numeric(.))<0.05 |
                                                            . == '<0.001') & ! . %in% c('--', ''),
                                                         "yellow", "white"))) %>%
    kbl(escape = F, col.names = col_names) %>% kable_styling(full_width = F)

}



#' Get regression results with multiple outcomes and exposures
#'
#' This function genartes regression results with multiple outcomes and exposures.
#' For now, it only supports the interaction between one covariable and the exposure.

#' @param df A tibble or dataframe of the data
#' @param outcomes A character vector of outcomes
#' @param exposures A character vector of exposures
#' @param covariates A character vector of covariates
#' @param outcome_type A string, can be one of these options:linear, binary, poisson, ordinal, tobit, normal_gee,
#' binary_gee, poisson_gee, ordinal_gee, lme, logistic_glme
#' @param sub_var A string of the subgroup analysis variable
#' @param sub_group A string of the subgroup
#' @param interaction_var a sting of the interaction covarite
#' @param multiply logical, multiply = T specifies all possible combinations of outcomes and exposures
#' @return A tibble
#' @import dplyr
#' @export


map2_get_regression = function(df, outcomes, exposures, covariates,
                               outcome_type, random_slope_var = NULL,
                               lme_method = 'REML', sub_var = NULL,
                               sub_group = NULL, interaction_var =NULL, multiply = T){
  if(multiply){
    outcome_vec = rep(outcomes,length(exposures))
    exposure_vec = rep(exposures, each = length(outcomes))
  }else{
    outcome_vec = outcomes
    exposure_vec = exposures
  }
  if(is.null(sub_var)){
    if(is.null(interaction_var)){
      out = map2_df(outcome_vec,exposure_vec,
                    ~get_regression_estimates(df, .x, c(.y, covariates), outcome_type, random_slope_var, lme_method) %>%
                      mutate(exposure = .y))
    }
    else{
      out = map2_df(outcome_vec,exposure_vec,
                    ~get_regression_estimates(df, .x, c(.y, covariates), outcome_type, random_slope_var, lme_method, interaction = c(.y, interaction_var)) %>%
                      mutate(exposure = .y))
    }

  }else{
    sub_df = df %>% filter(!!sym(sub_var) == sub_group)
    covariates = covariates[covariates!=sub_var]
    out = map2_df(outcome_vec,exposure_vec,
                  ~get_regression_estimates(sub_df, .x, c(.y, covariates), outcome_type, random_slope_var, lme_method) %>%
                    mutate(exposure = .y))

  }
  out %>% select(outcome, exposure, everything())

}
