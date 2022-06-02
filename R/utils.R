#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



#' Recode NA
#'
#' This function recodes NA to unknown for all the character columns.
#' Factor variables will be converted to character variables.
#'
#' @param df A dataframe
#' @return A dataframe with NA recoded to Unknown
#' @import dplyr tibble purrr tidyr stringr
#' @export
#'
#'
na_to_unknown = function(df){
  df %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, ~ifelse(is.na(.), 'Unknown', .))

}
#' Capitalize the first letter
#'
#' This function capitalizes the first letter of a string or a string vec
#'
#' @param string A string or a string vec
#' @return A string or a string vec
#' @export
#'
#'

toupper_first = function(string){
  paste0(toupper(substring(string, 1,1)), substring(string, 2))
}



#' Output a string from a vector
#'
#' This function outputs a string from a vector and save it to the clipboard
#'
#' @param in_vec A vector
#' @param sep A character for the separating character, default is '(a single quotation mark)
#' @return A string
#' @export
#'

out_text = function(in_vec, sep="'"){
  out = paste0(sep, paste0(in_vec, collapse= paste0(sep, ',', sep)), sep)
  cat(out)
  writeClipboard(out)
}


#' Plot distribution
#'
#' This function plots the histogram of all the numeric variables in a df. A grouping variable can be provided.
#'
#' @param df A dataframe
#' @return histograms
#' @import dplyr
#' @import ggplot2
#' @export
#'
#'
#'
plot_numeric_dist = function(df, grouping = NULL){
  numeric_vars = df %>% select_if(is.numeric) %>% colnames()
  df = df %>% gather(key = 'variable', value = 'value', numeric_vars)
  if(is.null(grouping)){
    df %>% ggplot(aes(x = value)) + geom_histogram() + facet_wrap(~variable, scales = 'free') +theme_bw()
  }else{
    df %>% filter(!is.na(!!sym(grouping))) %>%
      ggplot(aes(x = value)) + geom_histogram() + facet_grid(c(grouping, 'variable'), scales = 'free') +theme_bw()

  }

}




#' Check outlier
#'
#' This function checks if each value in a vector is an outlier

#' @param x A numeric vector
#' @return A logical vector
#' @export
#'
#'

is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm=T) - 1.5 * IQR(x, na.rm =T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}



#' Get significant p values
#'
#' This function returns rows with a significant p value defined by a cutoff

#' @param df A tibble or a dataframe
#' @param cut A number of the cutoff to define significance, default is 0.05
#' @param p_names A character vector of column names of p values
#' @param any logical, if T, returns row with any significant p;
#' otherwise, return rows with all significant p s, default is T
#' @return A tibble
#' @import dplyr
#' @export
#'
#'

get_sig = function(df, cut = 0.05, p_names = 'p', any = T){
  if(any){
    df %>% filter_at(p_names, any_vars(. == '<0.001'|suppressWarnings(as.numeric(.))<cut))
  }else{
    df %>% filter_at(p_names, all_vars(. == '<0.001'|suppressWarnings(as.numeric(.))<cut))
  }}




#' rename regression results
#'
#' This function renames regression results

#' @param dat A tibble or a dataframe
#' @param n A character or a number as the suffix of renamed variables
#' @param est_name A character for the estimate column name, default is 'estimate'
#' @return A tibble
#' @import dplyr
#'

rename_results = function(dat, n, est_name = 'estimate'){
  dat %>%
    rename_at(c(est_name, 'CI', 'p'), ~paste0(., n))

}


#' Combine regression estimates
#'
#' This function combine a list of regression results

#' @param est_list A list of regression tibbles
#' @param est_name A character for the estimate column name, default is 'estimate'
#' @param seq a numeric vector to label each regression tibble, default is a sequential number;
#' @return A tibble
#' @import dplyr
#' @export
#'
combine_regression_estimates = function(est_list, est_name= 'estimate', seq = 1:length(est_list)){
  map2(.x = est_list, .y = seq,
       ~rename_results(.x, .y, est_name)) %>% reduce(full_join) %>%
    mutate_all(~ifelse(is.na(.), '--', .))
}






#' Center or standardize
#'
#' This function centers or standardizes a vector of variables,
#' specify id if the data is longitudinal(for example, educyrs in EAS longitudinal data)

#' @param dat A tibble or dataframe of the data
#' @param vars_vec A character vector of variables to center or standardize
#' @param center logical, T if you want to center the variables, default is T
#' @param standard logical, T if you want to standardize the variables, default is F
#' @param id A character of the id column name, optional
#' @return A tibble with centered or standardized variables
#' @import dplyr
#' @export
#'
center_standard = function(dat, vars_vec, center = T, standard = F, id = NULL){
  cols = colnames(dat)
  newnames = paste0(rep(vars_vec, each = 2), '_', c('c','s'))
  if(any(rep(cols, length(newnames)) == rep(newnames, each = length(cols))))
  {stop('generated variable names will duplicate the existent variable names') }
  if(is.null(id)){
    id_dat = dat %>% select(vars_vec)
  }else{
    id_dat = dat %>% select(id, vars_vec) %>% distinct()
  }
  if(center & !standard){
    #use across instead of mutate_at to change varnames when there is only one variable
    new = id_dat %>% mutate(across(vars_vec, list(c = ~.-mean(.))))
  }
  if(standard & !center){
    new = id_dat %>% mutate(across(vars_vec, list(s =  ~(.-mean(.))/sd(.))))
  }
  if(center & standard){
    new = id_dat %>% mutate(across(vars_vec, list(c = ~.-mean(.), s =  ~(.-mean(.))/sd(.))))
  }
  if(is.null(id)){
    dat %>% bind_cols(new %>% select(-vars_vec))}
  else{
    dat %>% left_join(new %>% select(-vars_vec))}


}



#####################################################################################
#     Function get_fct - covert a vector to a factor
#     if there is any missing, unknown will be the final level
######################################################################################

get_fct = function(vec){
  vec = factor(vec)

  if (anyNA(vec)) {
    levels = levels(vec)
    vec = as.character(vec)
    vec[is.na(vec)] = 'Unknown'
    vec = factor(vec, levels = c(levels, 'Unknown'))
  }
  vec

}


#' Get contigency table
#'
#' This function genartes contigency table and performs mcnemar.test if requested
#

#' @param df A tibble or dataframe of the data
#' @param left_var A character of the left variable name
#' @param right_var A character of the right variable name
#' @param test logical, T if you want to perfrom the test, default is F
#' @param highlight, logical, highlight=T highlights p values, default is F
#' @param highlight_p, a numeric value of the p value cutoff to highlight, default is 0.05
#' @return A tibble with centered or standardized variables
#' @import dplyr
#' @export
#'

get_contigency_table = function(df, left_var, right_var, test = F, highlight = F, highlight_p = 0.05){
  left = df %>% pull(left_var) %>% get_fct()
  right = df %>% pull(right_var) %>% get_fct()

  raw_table = table(left, right, useNA = 'ifany')
  n_right_var = right %>% n_distinct

  colsum = colSums(raw_table)
  raw_table = rbind(raw_table, ColSum = colsum)
  rowsum = rowSums(raw_table)
  header_vec = c(1, n_right_var, 1)
  names(header_vec) = c(' ', right_var, ' ')

  out = cbind(raw_table, RowSum = rowsum) %>% as.data.frame() %>% rownames_to_column(left_var)

  if(test){
    mc_result = mcnemar.test(table(left, right_var))
    p_char = ifelse(mc_result$p.value<0.001, '<0.001', round(mc_result$p.value, 3))
    out = out %>% add_column(`P Value` = c(p_char, rep('', nrow(out)-1)))
    if (highlight) {
      out = out %>% mutate(`P Value` = cell_spec(`P Value`,
                                                 background = ifelse((suppressWarnings(as.numeric(`P Value`)) <
                                                                        highlight_p | `P Value` == "<0.001") &
                                                                       `P Value` != "", "yellow",
                                                                     "white")))}
    header_vec = c(1, n_right_var, 2)
    names(header_vec) = c(' ', right_var, ' ')

  }
  out %>% kbl(escape = F) %>% kable_paper(full_width = F) %>% column_spec(1, bold = T, border_right = T) %>%  add_header_above(header_vec)

}

