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



#' Output variable names for R markdown
#'
#' This function outputs variable list for markdown and save to the clipboard
#'
#' @param in_vec A string vec
#' @return A string
#' @export
#'

out_text = function(in_vec){
  out = paste0("`", paste0(in_vec, collapse= "`,`"), "`")
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
