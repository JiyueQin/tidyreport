
#' Get row numbers to indent
#'
#' This function returns the row numbers to indent
#'
#' @param dat a dataset with the first two columns, variable and stat
#' @param use_blank select rows based on a blank second column
#' @return A vec of row numbers
#' @import dplyr
#' @export
#'
#'

get_row_number_indent = function(dat, use_blank = F){
  dat = dat %>% mutate(row = 1:nrow(dat))

  if(use_blank){
    dat %>% rename(stat = 2) %>% filter(stat != '') %>% pull(row)
  }else{
    dat %>% filter(!str_detect(variable, "\\(")) %>% pull(row)
  }

}

#############################################################################################
#     Function get_char_desc - get descriptive stat for one char variable,
#                              NA should be recoded into unknown before running the function
#
#############################################################################################

get_char_desc = function(df, col, raw = F){
  if (anyNA(df %>% pull(col))) {stop(paste('There is NA in', col, '. Please check.'))}
  raw_char = df %>% group_by(!!sym(col)) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n_total = sum(n), ratio = round(n/n_total*100, 1)) %>%
    gather(key = 'var', value = 'variable', col) %>%
    mutate_if(is.numeric, as.character) %>%
    mutate(stat = paste0(n, '(', ratio, ')'))


  if(raw){
    raw_char %>% unite('variable', var, variable) %>% select(variable, stat)

  }
  else{
    raw_char %>%  unite('variable', var, variable) %>%
      select(variable, stat) %>%
      add_row(stat = '',
              variable = paste0(toupper_first(col), ', N(%)') ,
              .before = 1)}

}


########################################################################################
#     Function get_numeric_desc - get descriptive stat for one numeric variable,
#                                 NA will be removed to calculate the statistics
#
######################################################################################
get_numeric_desc = function(df, col, median = F, detail = F, detail_simple = F, raw = F){
  if(!detail & !detail_simple){
    if (anyNA(df %>% pull(col))) {message(paste('NA in', col, 'was removed'))}
  }

  if (detail){
    out = df %>% summarise_at(col, list(min = min, Q1 = ~quantile(., 0.25, na.rm = T),
                                        median = ~median(., na.rm = T), mean = mean, Q3 = ~quantile(., 0.75, na.rm = T),
                                        max = max, sd = sd,
                                        nmiss = ~sum(is.na(.))), na.rm = T) %>%
      mutate_all(~as.double(round(., 1))) %>%
      gather(key = 'variable', value = 'stat') %>%
      mutate(variable = paste0(col,'_', variable)) %>%
      mutate(stat = as.character(stat))

    if(!raw){
      out = out %>% add_row(variable = toupper_first(col), stat = '', .before = 1)
    }

  }else if(detail_simple){
    out = df %>% summarise_at(col, list(min = min, Q1 = ~quantile(., 0.25, na.rm = T),
                                        median = ~median(., na.rm = T), mean = mean, Q3 = ~quantile(., 0.75, na.rm = T),
                                        max = max, sd = sd,
                                        nmiss = ~sum(is.na(.))), na.rm = T) %>%
      mutate(ratio_miss = 100*nmiss/nrow(df)) %>%
      mutate_all(~as.double(round(., 1))) %>%
      mutate(range = paste0('(', min, ', ', max, ')'),
             mean_sd = paste0(mean, '(', sd, ')'),
             median_iqr = paste0(median, '(', Q1, ', ', Q3, ')'),
             miss = paste0(nmiss, '(', ratio_miss, '%)')) %>%
      select(mean_sd, median_iqr, range, miss) %>%
      gather(key = 'variable', value = 'stat') %>%
      mutate(variable = recode(variable, 'mean_sd' = 'Mean(SD)', 'median_iqr' = 'Median(IQR)',
                               'range' = 'Range', 'miss' = 'Nmiss(%)')) %>%
      mutate(variable = paste0(col,'_', variable)) %>%
      mutate(stat = as.character(stat))

    if(!raw){
      out = out %>% add_row(variable = toupper_first(col), stat = '', .before = 1)
    }

  }

  else{
    if(!median) {out  = tibble(mean = mean(df %>% pull(col), na.rm = T),
                               sd = sd(df %>% pull(col), na.rm = T),
                               variable = paste0(toupper_first(col), ', Mean(SD)')) %>%
      mutate_if(is.double, ~round(.,1)) %>%
      mutate(stat = paste0(mean, '(', sd, ')'))}
    else {out = tibble(median = median(df %>% pull(col), na.rm = T),
                       Q1 = quantile(df %>% pull(col), 0.25, na.rm = T),
                       Q3 = quantile(df %>% pull(col), 0.75, na.rm = T),
                       variable = paste0(toupper_first(col), ', Median(IQR)')) %>%
      mutate_if(is.double, ~round(.,1)) %>%
      mutate(stat = paste0(median, '(', Q1, ', ', Q3, ')'))}

  }

  out %>%
    select(variable, stat) %>%
    mutate_if(is.numeric, as.character)


}

#' Get Descriptive Stat for one dataset
#'
#' This function generates descriptive stat for all the variables in a dataset
#'
#' @param dat A dataframe
#' @param raw if you want to get dataframe raw output, default is False
#' @param raw_name if you want to get raw name stat instead of sample size, default is False
#' @param raw_with_header if you want to get raw output with headers, default is false
#' @param median_vars specify the variables that need median(IQR) instead of mean(SD)
#' @param detail if you want to get all the descriptive stat for continuous variables, default is F
#' @param detail_simple if you  want to get all the descriptive stat in a condensed version for continuous variables, default is F
#' @param extra_col if you want to preserve the original variable column, default is F
#' @param sort logical, sort =T sorts the variables based on their sequence in the data, default is F, which puts continuous variables first
#' @return A html table for descriptive statistics
#' @importFrom kableExtra add_indent cell_spec collapse_rows kable_styling
#' @importFrom knitr kable
#' @export
#'
#'

get_desc_stat = function(dat, raw = F, raw_name = F, raw_with_header = F, median_vars = NULL, detail = F, detail_simple = F, extra_col = F, sort = F){
  if('grouped_df' %in% class(dat)){
    stop('The data is a grouped df. Please remove the grouping!')
  }
  dat = na_to_unknown(dat)
  vars_order = tibble(var = colnames(dat), seq = 1:ncol(dat))
  numeric_vars = dat %>% select_if(is.numeric) %>% colnames()
  if(length(numeric_vars != 0)){
    if(detail|detail_simple){
      if(raw){
        numeric_output = map_df(numeric_vars, ~get_numeric_desc(dat, .x, raw = T, detail = detail,
                                                                detail_simple = detail_simple) %>% mutate(var = .x))}
      else if (raw_with_header){
        numeric_output = map_df(numeric_vars, ~get_numeric_desc(dat, .x, detail = detail,
                                                                detail_simple = detail_simple) %>% mutate(var = .x))}
      else if(extra_col){
        numeric_output = map_df(numeric_vars, ~get_numeric_desc(dat, .x, detail = detail,
                                                                detail_simple = detail_simple) %>%
                                  mutate(name = str_remove(variable, paste0(.x, '_'))) %>%
                                  mutate(var = .x)) %>%
          mutate(name = toupper_first(name))}
      else{
        numeric_output = map_df(numeric_vars, ~get_numeric_desc(dat, .x, detail = detail,
                                                                detail_simple = detail_simple) %>%
                                  mutate(variable = str_remove(variable, paste0(.x, '_'))) %>%
                                  mutate(var = .x)) %>%
          mutate(variable = toupper_first(variable))}
    }else{
      mean_numeric_vars = numeric_vars[!numeric_vars %in% median_vars]
      numeric_output_mean = map_df(mean_numeric_vars, ~get_numeric_desc(dat, .x) %>%
                                     mutate(var = .x))
      numeric_output_median = map_df(median_vars, ~get_numeric_desc(dat, .x, median = T) %>%
                                       mutate(var = .x))
      numeric_output = bind_rows(numeric_output_mean, numeric_output_median)
      if(extra_col){numeric_output = numeric_output %>% mutate(name = variable)}}

    numeric_output = numeric_output %>% left_join(vars_order) %>%
      fill(seq) %>% group_by(seq) %>%
      mutate(seq1 = 1:n()) %>% ungroup()


  }



  character_vars = dat %>% na_to_unknown %>% select_if(is.character) %>% colnames()

  if(length(character_vars != 0)){
    if(raw){
      character_output = map_df(character_vars, ~get_char_desc(dat, .x, raw = T) %>%
                                  mutate(var = .x))
    }else if (raw_with_header){
      character_output = map_df(character_vars, ~get_char_desc(dat, .x) %>%
                                  mutate(var = .x))}
    else if(extra_col) {
      character_output = map_df(character_vars, ~get_char_desc(dat, .x) %>%
                                  mutate(name = str_remove(variable, paste0(.x, '_'))) %>%
                                  mutate(var = .x)) %>%
        mutate(name = toupper_first(name))}
    else{
      character_output = map_df(character_vars, ~get_char_desc(dat, .x) %>%
                                  mutate(variable = str_remove(variable, paste0(.x, '_'))) %>%
                                  mutate(var = .x)) %>%
        mutate(variable = toupper_first(variable))}

    character_output = character_output %>% left_join(vars_order) %>%
      fill(seq) %>% group_by(seq) %>%
      mutate(seq1 = 1:n()) %>% ungroup()


  }



  if(length(numeric_vars) == 0){
    desc_output = character_output
  }else if(length(character_vars) == 0){
    desc_output = numeric_output
  }else{
    desc_output = bind_rows(numeric_output, character_output)
  }

  if(!raw_name) {
    new_name = paste0('Full Sample(N=', nrow(dat), ')')
    desc_output = desc_output %>% rename(!!new_name := stat)

  }
  if(sort){
    desc_output = desc_output %>% arrange(seq, seq1)
  }

  desc_output = desc_output %>% select(-var, -seq, -seq1)


  if(raw|raw_with_header|extra_col) {desc_output} else {
    # remove "variable" won't work when you copy-paste the table into EXCEL
    # kbl_names = colnames(desc_output)
    # kbl_names[1] = ''
    if(detail|detail_simple){
      desc_output %>% kable() %>%  kable_styling(full_width = F, position = "left") %>%
        add_indent(get_row_number_indent(desc_output, use_blank = T))
    }else{
      desc_output %>% kable() %>%  kable_styling(full_width = F, position = "left") %>%
        add_indent(get_row_number_indent(desc_output))
    }

  }

}
#####################################################################################
#     Function get_numeric_p - get p value of non-parametric tests
#           to compare if a grouping variable affects a numeric variable
#           Wilcoxon test for 2 groups and KW test for more than 2 groups
#
######################################################################################

get_numeric_p = function(df, grouping, col, paired = F){
  groups = df %>% pull(grouping) %>% na.omit %>% unique() %>% sort
  n_groups = length(groups)
  if(paired){
    message("Please make sure the data has been sorted by id so repeated measures from the same id are together")
    if(n_groups !=2){stop(paste('Paired tests can only have two groups. Please check.'))}
    result = wilcox.test(df %>% filter(!!sym(grouping) == groups[1]) %>% pull(col),
                         df %>% filter(!!sym(grouping) == groups[2]) %>% pull(col), paired = T)
    test = 'Wilcoxon Signed Rank'
  }else{
    df = df %>% mutate(!!grouping:= as.factor(!!sym(grouping)))
    formula = as.formula(paste(col, '~', grouping))
    if(n_groups == 2){
      result = wilcox.test(formula, data = df)
      test = 'Wilcoxon Rank Sum'
    }else if(n_groups>2){
      result = kruskal.test(formula, data = df)
      test = 'Kruskal Wallis' }
  }

  p = result$p.value
  if(p<0.001){p = '<0.001'}
  else{p = as.character(round(p, 3))}
  tibble(variable = col, test, p)

}

#####################################################################################
#     Function get_chisq_p - get p value of chisq test.
#       assumption: each expected cell count>0 and more than 80% >=5.
#       if this assumption is not satisfied, then do fisher exact test.
#
######################################################################################
get_chisq_p = function(df, grouping, col, format = F){
  n_values = df %>% pull(col) %>% n_distinct()
  if(n_values==1) {
    p=''
    test = 'chisq'}else{
      tbl = table(df %>% pull(grouping), df %>% pull(col))
      result = suppressWarnings(chisq.test(tbl))
      test = 'chisq'
      expected = as.vector(result$expected)
      if(sum(expected<1)>0|sum(expected <5)/length(expected)>0.2){
        result = try(fisher.test(tbl), silent=T)
        if (inherits(result, "try-error")){result = fisher.test(tbl, workspace=2e8)}
        test = 'fisher'
      }

      p = result$p.value

      if(p<0.001){p = '<0.001'}
      else{p = as.character(round(p, 3))}}

  out_df = tibble(variable = col,test, p)

  if (!format) out_df
  else{
    out_df %>% add_row(variable = rep('', n_values), test = rep('', n_values), p = rep('', n_values))

  }

}


#' Test each variable in a df for multiple groups
#'
#' This function generates wilcoxon rank rum test(for 2 groups) or  KW test(for more than 2 groups) for every continuous variable and
#' Chisq test(or Fisher test) for each categorical variable
#'
#'
#' @param df A dataframe
#' @param grouping character, the column to separate the data into two subsets
#' @param raw logical, if you want to get table output, default is F
#' @param format logical, format = T outputs a formatted table, default is T
#' @param paired logical, paired=T uses wilcoxon signed rank tests, default is F
#' @return A html table for simple univariate tests
#' @import dplyr
#' @export
#'
#'

get_group_compare = function(df, grouping, raw = F, format = T, paired = F){
  df = df %>% mutate_at(grouping, as.character)
  numeric = df %>% select_if(is.numeric) %>% colnames()
  categorical = df %>% select(-!!sym(grouping)) %>% mutate_if(is.factor, as.character) %>%
    select_if(is.character) %>% colnames()

  numeric_output = map_df(numeric, ~get_numeric_p(df, grouping, .x, paired))
  if (format) categorical_output = map_df(categorical, ~get_chisq_p(df, grouping, .x, format = T))
  else categorical_output = map_df(categorical, ~get_chisq_p(df, grouping, .x, format = F))
  output = bind_rows(numeric_output, categorical_output) %>% rename('P Value' = p)
  if(raw) output else output %>% kable() %>% kable_styling(full_width = F)


}


#####################################################################################
#     Function dichotomize_variable - dichotomize a variable based on its group
#
######################################################################################

dichotomize_variable = function(dat, var, group){
  dat %>% mutate(!!var := ifelse(!!sym(var) == !!group, !!group, '_other_'))
}



#####################################################################################
#     Function dichotomize_compare - perform statistical testing
#          for each dichotomized variable
######################################################################################

dichotomize_compare = function(dat, var, grouping){
  groups = dat %>% pull(var) %>% unique()

  map_df(groups,
         ~get_group_compare(dichotomize_variable(dat, var, .x) %>%
                              select(grouping, var),grouping, raw = T, format = F) %>%
           add_column(group =.x)) %>%
    unite('variable', variable, group, sep = '_') %>%
    mutate(variable = tolower(variable))

}


#' Get Descriptive Stat for the whole sample and stratified by a grouping variable
#'
#' This function generates descriptive statistics for the whole sample and stratified by a grouping variable.
#' The grouping variable cannot have NA. For all other categorical variables, NA will be replaced with Unknown.
#'
#' @param dat A dataframe
#' @param grouping A string, the name of the grouping variable
#' @param test if you want to perform statistical testing, default is True
#' @param raw if you want to get dataframe raw output, default is False
#' @param median_vars specify the variables that need median(IQR) instead of mean(SD)
#' @param detail if you want to get all the descriptive stat for continuous variables, default is F
#' @param detail_simple if you  want to get all the descriptive stat in a condensed version for continuous variables, default is F
#' @param highlight logical, highlight=T highlights p values, default is F
#' @param highlight_p a numeric value of the p value cutoff to highlight, default is 0.05
#' @param paired logical, paired=T uses wilcoxon signed rank tests, default is F
#' @param sort logical, sort =T sorts the variables based on their sequence in the data, default is F, which puts continuous variables first
#' @param dichotomize logical, dichotomize =T calculates p values for each dichotomized version of a categorical variable with overall difference of p<highlight_p,defalt is F
#' @return A html table for descriptive statistics
#' @import dplyr
#' @export
#'
#'


get_desc_stat_grouping = function(dat, grouping, test=T, raw=F, median_vars = NULL, detail = F, detail_simple = F,
                                  highlight=F, highlight_p=0.05, paired = F,
                                  sort = F, dichotomize=F){
  if('grouped_df' %in% class(dat)){
    stop('The data is a grouped df. Please remove the grouping!')
  }
  dat = dat %>% mutate_at(grouping, as.character)

  groups = dat %>% pull(grouping) %>% unique() %>% sort(na.last=T)
  if(anyNA(groups)){stop(paste('There is NA in', grouping, '. Please check.'))}

  dat_lists = map(groups, ~dat %>% filter(!!sym(grouping) == .x) %>%
                    select(-grouping))

  summary_table = map2(dat_lists, groups,
                       ~get_desc_stat(.x, raw_name = T,raw_with_header = T,
                                      median_vars = median_vars, detail=detail, detail_simple = detail_simple, sort = sort) %>%
                         rename(!!paste0(toupper_first(.y), '(N=', nrow(.x),')') := stat)) %>%
    reduce(full_join, by = "variable") %>%
    mutate_all(~ifelse(is.na(.), '0(0)', .))

  summary_table = dat %>% select(-grouping)  %>%
    get_desc_stat(extra_col = T, median_vars = median_vars, detail = detail, detail_simple = detail_simple, sort = sort) %>%
    full_join(summary_table)


  if(test|highlight){
    test_summary =get_group_compare(dat, grouping, raw = T, format = T, paired = paired)
    if(dichotomize){
      cat_vars_atleast3 = dat %>% na_to_unknown %>% select(-!!sym(grouping)) %>%
        select_if(is.character) %>%
        map_df(~tibble(n_groups = n_distinct(.x)), .id = 'var') %>%
        filter(n_groups>2) %>% pull(var)

      sig_cat_vars = test_summary %>%
        filter(variable %in% cat_vars_atleast3) %>%
        filter(suppressWarnings(as.numeric(`P Value`))<highlight_p |`P Value` == '<0.001') %>% pull(variable)

      test_summary = bind_rows(test_summary,
                               map_df(sig_cat_vars, ~dichotomize_compare(dat, .x, grouping)))

    }
    summary_table = summary_table %>% rename(old_variable = variable) %>%
      separate(old_variable, c('variable', 'remove'), sep = ',', remove = F, fill = 'right') %>%
      mutate(variable = tolower(variable))%>% left_join(test_summary) %>%
      select(-test, -variable, -remove, -old_variable) %>%
      rename(variable = name) %>%
      mutate(`P Value` = ifelse(is.na(`P Value`), '', `P Value`)) %>%
      select(variable, everything())
  }else{
    summary_table = summary_table %>% mutate(variable = name) %>% select(-name)
  }

  if(raw) {summary_table} else {
    if(highlight){
      summary_table =  summary_table %>%
        mutate(`P Value`= cell_spec(`P Value`, background =
                                      ifelse((suppressWarnings(as.numeric(`P Value`))<highlight_p |
                                                `P Value` == '<0.001') & `P Value` != '',
                                             "yellow", "white")))
      if(detail|detail_simple){
        summary_table %>% kable(escape =F) %>% kable_styling(full_width = F, position = "left") %>%
          add_indent(get_row_number_indent(summary_table, use_blank = T))
      }else {
        summary_table %>% kable(escape=F) %>% kable_styling(full_width = F, position = "left") %>%
          add_indent(get_row_number_indent(summary_table))}
    }else{
      if(detail|detail_simple){
        summary_table %>% kable() %>% kable_styling(full_width = F, position = "left") %>%
          add_indent(get_row_number_indent(summary_table, use_blank = T))
      }else {
        summary_table %>% kable() %>% kable_styling(full_width = F, position = "left") %>%
          add_indent(get_row_number_indent(summary_table))}

    }

  }


}
