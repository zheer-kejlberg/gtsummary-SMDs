Adding SMDs to gtsummary tables with 3 or more groups
================
Zheer Kejlberg Al-Mashhadi
2023-11-04

## Functions to add SMDs with explanations and examples of use

In this document, the necessary functions are defined and explained.
First, wrapper functions are defined for the core functionality (e.g.,
*core_smd_function* and *applied_smd_function*). Finally, four separate
“caller” functions are created, each of which call the core functions in
distinct ways for distinct use cases.

<br>

<br>

### Defining the functions

<br>

#### 1) Load dependencies

``` r
library(gtsummary) # For creating a baseline characteristics table
library(tidyverse) # For data wrangling and misc.
library(smd) # for calculating the SMDs
```

<br>

#### 2) Create the core functionality, via **core_smd_function()**, for taking the data and outputting the SMD results

``` r
core_smd_function <- function(data, is_weighted, location, ref_group) {
  # MAKE A TABLE OF EVERY POSSIBLE COMBO OF TWO DIFFERENT GROUPS
  groups <- factor(unique(data$by))
  pairs <- expand.grid(groups, groups) %>%
    arrange(as.integer(.data$Var1), as.integer(.data$Var2)) %>% 
    filter(Var1 != Var2) %>%
    filter(!duplicated(
      paste0(pmax(as.character(Var1), as.character(Var2)), 
             pmin(as.character(Var1), as.character(Var2)))))
  if (ref_group) { # IF ref_group, KEEP ONLY PAIRS CONTAINING THE REF GROUP
    pairs <- pairs %>% filter(Var1 == first(levels(groups)))
  }
  
  # CREATE COLUMN NAMES FOR EACH CALCULATED SMD
  create_colname <- function(pair) {
    filtered_data <- data %>% filter(by %in% pair) %>% mutate(by = factor(by))
    paste(levels(filtered_data$by)[1], levels(filtered_data$by)[2], sep = " vs. ")
  }
  comparisons <- apply(pairs, 1, create_colname)
  
  if (location == "label") {
    comparisons <- paste0("SMD: ", comparisons)
  } else {
    comparisons <- paste0("SMD: ", comparisons, " ")
  }
  
  # CREATE SUBSETS OF DATA
  subsetting <- function(pair, data) {
    as.data.frame(data) %>%
      filter(by %in% pair) %>%
      mutate(by = factor(by)) %>%
      droplevels()
  }
  
  data_subsets <- apply(X = pairs, MARGIN = 1, FUN = subsetting, data = data)
  
  # CALCULATE SMD BETWEEN GROUPS WITHIN EACH DATA SUBSET
  clean_SMD <- purrr::possibly(.f = smd::smd, otherwise = NA_real_) 
  
  if (is_weighted) { 
    smd_estimates <- purrr::map(data_subsets, ~ clean_SMD(.x$variable, .x$by, .x$weight_var))
  } else {
    smd_estimates <- purrr::map(data_subsets, ~ clean_SMD(.x$variable, .x$by)) 
  }
  
  extract_SMD <- purrr::possibly(.f = function(x){x$estimate}, otherwise = NA_real_)
  
  smd_estimates <- purrr::map_dbl(smd_estimates, ~ extract_SMD(.x))
  
  # OUTPUT THE RESULTS
  tibble(comp = comparisons, smd = smd_estimates) %>%
    spread(comp, smd) %>%
    relocate(any_of(comparisons))
}
```

<br>

#### 3) Create a function *clean_smd_data()* to prepare the input data for use by the *core_smd_function()*

``` r
clean_smd_data <- function(data, variable, by, tbl) {
  tbl_type <- first(class(tbl))
  if (tbl_type != "tbl_svysummary" & tbl_type != "tbl_summary") {
    stop("Inappropriate input to smd function")
  }
  is_weighted <- tbl_type == "tbl_svysummary"
  
  if (is_weighted) {
    data <- data$variables %>% mutate(weight_var = 1 / data$allprob[[1]])
  } else {
    data <- data %>% mutate(weight_var = 1)
  }
  
  data <- dplyr::select(data, all_of(c(variable, by, "weight_var"))) %>%
    rlang::set_names(c("variable", "by", "weight_var")) %>%
    dplyr::filter(complete.cases(.))
  if (is.character(data$variable)) {
    data <- data %>% mutate(variable = factor(variable))
  }
  if (is.factor(data$variable)) {
    levels <- levels(data$variable)
  } else {
    levels <- NULL
  }
  return(list(data, levels, is_weighted))
}
```

<br>

#### 4) Create the **add_SMD()** function to be called by users.

This function first cleans the data and then applies the
*core_smd_function()* but allows two distinct things to vary according
to user preference:

1)  The **location** argument (*“label”*, *“level”*, or *“both”*):

- Specifying **“label”**, you get **one** SMD per variable. For
  categorical variables, a Mahalanobis distance is calculated between
  groups.
- Specifying **“level”**, you get an SMD *for each level* of all
  categorical variables. This option thus does not produce SMDs for
  continuous/numeric variables.
- Specifying **“both”**, you combine the output of the *level* and the
  *label* options.  

2)  The **ref_group** argument (*TRUE* or *FALSE*):

- **FALSE**: There is no reference group, and SMDs will be calculated
  between every possible pair of groups (i.e., groups being defined by
  the “by” argument in *tbl_summary()* or *tbl_svysummary()*)
- **TRUE**: The first group (the first level of the variable given in
  the “by” argument - which is also the leftmost group in the table)
  will be set as a reference group,

``` r
add_SMD <- function(tbl, ref_group = FALSE, location = "label") {
  fun <- function(data, variable, by, tbl, ...) {
    clean_data <- clean_smd_data(data, variable, by, tbl)
    data <- clean_data[[1]]
    levels <- clean_data[[2]]
    is_weighted <- clean_data[[3]]
    
    if (location == "label") {
      output <- core_smd_function(data, is_weighted, location = location, ref_group = ref_group)
    } else { # location == "level"
      execute_by_level <- function(data, level, is_weighted) {
        data <- data %>% mutate(variable = variable == level)
        core_smd_function(data, is_weighted, location = location, ref_group = ref_group)
      }
      output <- map_dfr(levels, .f = ~ execute_by_level(data, .x, is_weighted))
    }
    return(output)
  }
  
  if (location == "both") {
    location <- "label"
    tbl <- tbl %>% add_stat(fns = everything() ~ fun, location = ~ "label")
    location <- "level"
    tbl <- tbl %>% add_stat(fns = everything() ~ fun, location = ~ "level")
    
    duplicates <- stringr::str_subset(tbl$table_styling$header$column, "^SMD(\r\n|\r|\n|.)* $")
    duplicates <- stringr::str_remove(duplicates, " $")
    
    for (i in 1:length(duplicates)) {
      # Temporarily change column names for use by gtsummary
      column_names <- colnames(tbl$table_body)
      indices <- which(column_names == duplicates[i] | column_names == paste0(duplicates[i], " "))
      column_names[indices] <- stringr::str_replace_all(column_names[indices], "[: .]", "_")
      colnames(tbl$table_body) <- column_names
      
      # Adjust the digits of the SMDs and turn into character (while hiding NAs)
      format_smd <- function(column) {
        column <- round(column, 3)
        column <- format(column, nsmall = 3)
        column[str_detect(column, "NA")] <- ""
        return(column)
      }
      tbl$table_body[[column_names[indices][1]]] <- format_smd(tbl$table_body[[column_names[indices][1]]])
      tbl$table_body[[column_names[indices][2]]] <- format_smd(tbl$table_body[[column_names[indices][2]]])
      
      # Finally merge and reinstate the original column title
      merge_pattern <- paste0("{",column_names[indices][1],"}{",column_names[indices][2],"}")
      tbl <- tbl %>%
        modify_column_merge(pattern = merge_pattern) %>%
        modify_header(column_names[indices][1] ~ duplicates[i])
      
    }
    
  } else {
    tbl <- tbl %>% add_stat(fns = everything() ~ fun, location = ~ location)
  }
  return(tbl)
  
}
```

<br>

<br>

### Using the functions with gtsummary.

<br>

#### For unweighted data (with a *tbl_summary()* object), see the following examples:

For one SMD per variable, the location argument does not need to be
specified (as it defaults to *“label”*).

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_SMD()
```

<div id="yrxuzfnfhf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. II">SMD: I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. III">SMD: I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: II vs. III">SMD: II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.088</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.061</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">-0.096</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">-0.135</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">-0.042</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.287</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.193</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">0.314</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

For one SMD per level of every categorical variable, you must specify
*location = “level”*

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_SMD(location = "level")
```

<div id="nnqgccdfqi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. II">SMD: I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. III">SMD: I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: II vs. III">SMD: II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.088</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.061</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">-0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">-0.088</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">-0.061</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td>
<td headers="SMD: II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">-0.195</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.112</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">0.308</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.034</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">-0.072</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">-0.105</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.253</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.107</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">-0.146</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">-0.069</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">-0.140</td>
<td headers="SMD: II vs. III" class="gt_row gt_center">-0.071</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>
<center>

###### \*Notice, there are no SMDs for non-categorical variables due to the use of *pairwise_smd_level*

</center>

  

<br>

#### For weighted data, use *tbl_svysummary()*:

In this examply we use weights from **WeightIt** package. The **survey**
package delivers the necessary *svydesign* object.

``` r
library(WeightIt) # To calculate weights
library(survey) # To create a surveydesign object (a "weighted" dataset)
```

Application of the *add_SMD()* function is identical to the non-weighted
case, but it is applied to a *tbl_svysummary* object instead of a
*tbl_summary* object.

``` r
trial %>% mutate(
  w = weightit(grade ~ age + stage + trt, data = ., focal="I")$weights) %>% # create ATT weights
  survey::svydesign(~1, data = ., weights = ~w) %>% # create the svydesign object
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>%
  add_SMD(ref_group = TRUE)
```

<div id="bltlfabriv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. II">SMD: I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. III">SMD: I vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.030</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.064</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (48%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">35 (52%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">46 (34, 56)</td>
<td headers="stat_3" class="gt_row gt_center">45 (38, 54)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center">-0.013</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">-0.007</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">2</td>
<td headers="stat_3" class="gt_row gt_center">2</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. II" class="gt_row gt_center">0.078</td>
<td headers="SMD: I vs. III" class="gt_row gt_center">0.020</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">19 (27%)</td>
<td headers="stat_3" class="gt_row gt_center">17 (25%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">19 (27%)</td>
<td headers="stat_3" class="gt_row gt_center">17 (26%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (26%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (27%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">13 (19%)</td>
<td headers="stat_3" class="gt_row gt_center">15 (23%)</td>
<td headers="SMD: I vs. II" class="gt_row gt_center"><br /></td>
<td headers="SMD: I vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>
<center>

###### \*Notice, comparisons are only made here between *group I* and all other groups due to the use of *ref_group = TRUE*

</center>

<br>

Finally, there’s the option to set *location = “both”* to get both kinds
of SMDs simultaneously.

``` r
trial %>%
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_SMD(location = "both")
```

<div id="bvcmpxwuta" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. II">SMD: I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: I vs. III">SMD: I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="SMD: II vs. III">SMD: II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"> 0.088</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"> 0.061</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center"> 0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"> 0.088</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"> 0.061</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center">-0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center">-0.088</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center">-0.061</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center"> 0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center">-0.096</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center">-0.135</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center">-0.042</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"></td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"></td>
<td headers="SMD__II_vs__III" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"> 0.287</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"> 0.193</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center"> 0.314</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center">-0.195</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"> 0.112</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center"> 0.308</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"> 0.034</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center">-0.072</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center">-0.105</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center"> 0.253</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center"> 0.107</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center">-0.146</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="SMD__I_vs__II" class="gt_row gt_center">-0.069</td>
<td headers="SMD__I_vs__III" class="gt_row gt_center">-0.140</td>
<td headers="SMD__II_vs__III" class="gt_row gt_center">-0.071</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>
