##### First, we load dependencies

``` r
library(gtsummary) # For creating a baseline characteristics table
library(tidyverse) # For data wrangling and misc.


core_smd_function <- function(data, is_weighted, ref_group = FALSE) {
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
  name_comparison <- function(pair) {
    filtered_data <- data %>% filter(by %in% pair) %>% mutate(by = factor(by))
    paste(levels(filtered_data$by)[1], levels(filtered_data$by)[2], sep = " vs. ")
  }
  comparisons <- apply(pairs, 1, name_comparison)
  
  # CREATE SUBSETS OF DATA
  subsetting <- function(pair, data) {
    as.data.frame(data) %>%
      filter(by %in% pair) %>%
      mutate(by = factor(by)) %>%
      droplevels()
  }
  
  data_subsets <- apply(X = pairs, MARGIN = 1, FUN = subsetting, data = data)
  
  # CALCULATE SMD BETWEEN GROUPS WITHIN EACH DATA SUBSET
  clean_SMD <- possibly(.f = smd::smd, otherwise = NA_real_) 
  
  if (is_weighted) { 
    smd_estimates <- map(data_subsets, ~ clean_SMD(.x$variable, .x$by, .x$weight_var))
  } else {
    smd_estimates <- map(data_subsets, ~ clean_SMD(.x$variable, .x$by)) 
  }
  
  extract_SMD <- possibly(.f = function(x){x$estimate}, otherwise = NA_real_)
  
  smd_estimates <- map_dbl(smd_estimates, ~ extract_SMD(.x))
  
  # OUTPUT THE RESULTS
  tibble(comp = comparisons, smd = smd_estimates) %>%
    spread(comp, smd) %>%
    relocate(any_of(comparisons))
}



clean_smd_data <- function(data, variable, by, tbl) {
  data_type <- last(class(data))
  tbl_type <- first(class(tbl))
  if ((data_type != "survey.design" & data_type != "data.frame") | (tbl_type != "tbl_svysummary" & tbl_type != "tbl_summary")) {
    stop("Inappropriate input to smd function")
  }
  is_weighted <- data_type == "survey.design"
  
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



applied_smd_function <- function(data, variable, by, tbl, ref_group = FALSE, location = "label") {
  clean_data <- clean_smd_data(data, variable, by, tbl)
  data <- clean_data[[1]]
  levels <- clean_data[[2]]
  is_weighted <- clean_data[[3]]
  
  if (location == "label") {
    output <- core_smd_function(data, is_weighted, ref_group = ref_group)
  } else { # location == "level"
    execute_by_level <- function(data, level, is_weighted) {
      data <- data %>% mutate(variable = variable == level)
      core_smd_function(data, is_weighted, ref_group = ref_group)
    }
    output <- map_dfr(levels, .f = ~ execute_by_level(data, .x, is_weighted))
  }
  return(output)
}



pairwise_smd <- function(data, variable, by, tbl, ...) {
  applied_smd_function(data, variable, by, tbl)
}
pairwise_smd_level <- function(data, variable, by, tbl, ...) {
  applied_smd_function(data, variable, by, tbl, location = "level")
}

focal_smd <- function(data, variable, by, tbl, ...) {
  applied_smd_function(data, variable, by, tbl, ref_group = TRUE)
}
focal_smd_level <- function(data, variable, by, tbl, ...) {
  applied_smd_function(data, variable, by, tbl, ref_group = TRUE, location = "level")
}
```

#### Functions to add SMDs to *tbl_summary* and *tbl_svysummary* objects

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ pairwise_smd, location = ~ "label")
```

<div id="upiwbuzyti" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#upiwbuzyti table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#upiwbuzyti thead, #upiwbuzyti tbody, #upiwbuzyti tfoot, #upiwbuzyti tr, #upiwbuzyti td, #upiwbuzyti th {
  border-style: none;
}

#upiwbuzyti p {
  margin: 0;
  padding: 0;
}

#upiwbuzyti .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#upiwbuzyti .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#upiwbuzyti .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#upiwbuzyti .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#upiwbuzyti .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#upiwbuzyti .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#upiwbuzyti .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#upiwbuzyti .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#upiwbuzyti .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#upiwbuzyti .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#upiwbuzyti .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#upiwbuzyti .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#upiwbuzyti .gt_spanner_row {
  border-bottom-style: hidden;
}

#upiwbuzyti .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#upiwbuzyti .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#upiwbuzyti .gt_from_md > :first-child {
  margin-top: 0;
}

#upiwbuzyti .gt_from_md > :last-child {
  margin-bottom: 0;
}

#upiwbuzyti .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#upiwbuzyti .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#upiwbuzyti .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#upiwbuzyti .gt_row_group_first td {
  border-top-width: 2px;
}

#upiwbuzyti .gt_row_group_first th {
  border-top-width: 2px;
}

#upiwbuzyti .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#upiwbuzyti .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#upiwbuzyti .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#upiwbuzyti .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#upiwbuzyti .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#upiwbuzyti .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#upiwbuzyti .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#upiwbuzyti .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#upiwbuzyti .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#upiwbuzyti .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#upiwbuzyti .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#upiwbuzyti .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#upiwbuzyti .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#upiwbuzyti .gt_left {
  text-align: left;
}

#upiwbuzyti .gt_center {
  text-align: center;
}

#upiwbuzyti .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#upiwbuzyti .gt_font_normal {
  font-weight: normal;
}

#upiwbuzyti .gt_font_bold {
  font-weight: bold;
}

#upiwbuzyti .gt_font_italic {
  font-style: italic;
}

#upiwbuzyti .gt_super {
  font-size: 65%;
}

#upiwbuzyti .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#upiwbuzyti .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#upiwbuzyti .gt_indent_1 {
  text-indent: 5px;
}

#upiwbuzyti .gt_indent_2 {
  text-indent: 10px;
}

#upiwbuzyti .gt_indent_3 {
  text-indent: 15px;
}

#upiwbuzyti .gt_indent_4 {
  text-indent: 20px;
}

#upiwbuzyti .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="II vs. III">II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.088</td>
<td headers="I vs. III" class="gt_row gt_center">0.061</td>
<td headers="II vs. III" class="gt_row gt_center">0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.096</td>
<td headers="I vs. III" class="gt_row gt_center">-0.135</td>
<td headers="II vs. III" class="gt_row gt_center">-0.042</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.287</td>
<td headers="I vs. III" class="gt_row gt_center">0.193</td>
<td headers="II vs. III" class="gt_row gt_center">0.314</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level")
```

<div id="ekgeftemid" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ekgeftemid table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ekgeftemid thead, #ekgeftemid tbody, #ekgeftemid tfoot, #ekgeftemid tr, #ekgeftemid td, #ekgeftemid th {
  border-style: none;
}

#ekgeftemid p {
  margin: 0;
  padding: 0;
}

#ekgeftemid .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ekgeftemid .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ekgeftemid .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ekgeftemid .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ekgeftemid .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ekgeftemid .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ekgeftemid .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ekgeftemid .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ekgeftemid .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ekgeftemid .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ekgeftemid .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ekgeftemid .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ekgeftemid .gt_spanner_row {
  border-bottom-style: hidden;
}

#ekgeftemid .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ekgeftemid .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ekgeftemid .gt_from_md > :first-child {
  margin-top: 0;
}

#ekgeftemid .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ekgeftemid .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ekgeftemid .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ekgeftemid .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ekgeftemid .gt_row_group_first td {
  border-top-width: 2px;
}

#ekgeftemid .gt_row_group_first th {
  border-top-width: 2px;
}

#ekgeftemid .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekgeftemid .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ekgeftemid .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ekgeftemid .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ekgeftemid .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekgeftemid .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ekgeftemid .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ekgeftemid .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ekgeftemid .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ekgeftemid .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ekgeftemid .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekgeftemid .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ekgeftemid .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ekgeftemid .gt_left {
  text-align: left;
}

#ekgeftemid .gt_center {
  text-align: center;
}

#ekgeftemid .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ekgeftemid .gt_font_normal {
  font-weight: normal;
}

#ekgeftemid .gt_font_bold {
  font-weight: bold;
}

#ekgeftemid .gt_font_italic {
  font-style: italic;
}

#ekgeftemid .gt_super {
  font-size: 65%;
}

#ekgeftemid .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ekgeftemid .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ekgeftemid .gt_indent_1 {
  text-indent: 5px;
}

#ekgeftemid .gt_indent_2 {
  text-indent: 10px;
}

#ekgeftemid .gt_indent_3 {
  text-indent: 15px;
}

#ekgeftemid .gt_indent_4 {
  text-indent: 20px;
}

#ekgeftemid .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="II vs. III">II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.088</td>
<td headers="I vs. III" class="gt_row gt_center">0.061</td>
<td headers="II vs. III" class="gt_row gt_center">-0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.088</td>
<td headers="I vs. III" class="gt_row gt_center">-0.061</td>
<td headers="II vs. III" class="gt_row gt_center">0.028</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.195</td>
<td headers="I vs. III" class="gt_row gt_center">0.112</td>
<td headers="II vs. III" class="gt_row gt_center">0.308</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.034</td>
<td headers="I vs. III" class="gt_row gt_center">-0.072</td>
<td headers="II vs. III" class="gt_row gt_center">-0.105</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.253</td>
<td headers="I vs. III" class="gt_row gt_center">0.107</td>
<td headers="II vs. III" class="gt_row gt_center">-0.146</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.069</td>
<td headers="I vs. III" class="gt_row gt_center">-0.140</td>
<td headers="II vs. III" class="gt_row gt_center">-0.071</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ focal_smd, location = ~ "label")
```

<div id="gtwcgsxwmv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gtwcgsxwmv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gtwcgsxwmv thead, #gtwcgsxwmv tbody, #gtwcgsxwmv tfoot, #gtwcgsxwmv tr, #gtwcgsxwmv td, #gtwcgsxwmv th {
  border-style: none;
}

#gtwcgsxwmv p {
  margin: 0;
  padding: 0;
}

#gtwcgsxwmv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gtwcgsxwmv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gtwcgsxwmv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gtwcgsxwmv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gtwcgsxwmv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gtwcgsxwmv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gtwcgsxwmv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gtwcgsxwmv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gtwcgsxwmv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gtwcgsxwmv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gtwcgsxwmv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gtwcgsxwmv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gtwcgsxwmv .gt_spanner_row {
  border-bottom-style: hidden;
}

#gtwcgsxwmv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gtwcgsxwmv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gtwcgsxwmv .gt_from_md > :first-child {
  margin-top: 0;
}

#gtwcgsxwmv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gtwcgsxwmv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gtwcgsxwmv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gtwcgsxwmv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gtwcgsxwmv .gt_row_group_first td {
  border-top-width: 2px;
}

#gtwcgsxwmv .gt_row_group_first th {
  border-top-width: 2px;
}

#gtwcgsxwmv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gtwcgsxwmv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gtwcgsxwmv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gtwcgsxwmv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gtwcgsxwmv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gtwcgsxwmv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gtwcgsxwmv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gtwcgsxwmv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gtwcgsxwmv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gtwcgsxwmv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gtwcgsxwmv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gtwcgsxwmv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gtwcgsxwmv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gtwcgsxwmv .gt_left {
  text-align: left;
}

#gtwcgsxwmv .gt_center {
  text-align: center;
}

#gtwcgsxwmv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gtwcgsxwmv .gt_font_normal {
  font-weight: normal;
}

#gtwcgsxwmv .gt_font_bold {
  font-weight: bold;
}

#gtwcgsxwmv .gt_font_italic {
  font-style: italic;
}

#gtwcgsxwmv .gt_super {
  font-size: 65%;
}

#gtwcgsxwmv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gtwcgsxwmv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gtwcgsxwmv .gt_indent_1 {
  text-indent: 5px;
}

#gtwcgsxwmv .gt_indent_2 {
  text-indent: 10px;
}

#gtwcgsxwmv .gt_indent_3 {
  text-indent: 15px;
}

#gtwcgsxwmv .gt_indent_4 {
  text-indent: 20px;
}

#gtwcgsxwmv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.088</td>
<td headers="I vs. III" class="gt_row gt_center">0.061</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.096</td>
<td headers="I vs. III" class="gt_row gt_center">-0.135</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.287</td>
<td headers="I vs. III" class="gt_row gt_center">0.193</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ focal_smd_level, location = ~ "level")
```

<div id="dradkpxcuj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dradkpxcuj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dradkpxcuj thead, #dradkpxcuj tbody, #dradkpxcuj tfoot, #dradkpxcuj tr, #dradkpxcuj td, #dradkpxcuj th {
  border-style: none;
}

#dradkpxcuj p {
  margin: 0;
  padding: 0;
}

#dradkpxcuj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dradkpxcuj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dradkpxcuj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dradkpxcuj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dradkpxcuj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dradkpxcuj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dradkpxcuj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dradkpxcuj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dradkpxcuj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dradkpxcuj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dradkpxcuj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dradkpxcuj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dradkpxcuj .gt_spanner_row {
  border-bottom-style: hidden;
}

#dradkpxcuj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#dradkpxcuj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dradkpxcuj .gt_from_md > :first-child {
  margin-top: 0;
}

#dradkpxcuj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dradkpxcuj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dradkpxcuj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#dradkpxcuj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#dradkpxcuj .gt_row_group_first td {
  border-top-width: 2px;
}

#dradkpxcuj .gt_row_group_first th {
  border-top-width: 2px;
}

#dradkpxcuj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dradkpxcuj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dradkpxcuj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dradkpxcuj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dradkpxcuj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dradkpxcuj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dradkpxcuj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dradkpxcuj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dradkpxcuj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dradkpxcuj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dradkpxcuj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dradkpxcuj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dradkpxcuj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dradkpxcuj .gt_left {
  text-align: left;
}

#dradkpxcuj .gt_center {
  text-align: center;
}

#dradkpxcuj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dradkpxcuj .gt_font_normal {
  font-weight: normal;
}

#dradkpxcuj .gt_font_bold {
  font-weight: bold;
}

#dradkpxcuj .gt_font_italic {
  font-style: italic;
}

#dradkpxcuj .gt_super {
  font-size: 65%;
}

#dradkpxcuj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dradkpxcuj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dradkpxcuj .gt_indent_1 {
  text-indent: 5px;
}

#dradkpxcuj .gt_indent_2 {
  text-indent: 10px;
}

#dradkpxcuj .gt_indent_3 {
  text-indent: 15px;
}

#dradkpxcuj .gt_indent_4 {
  text-indent: 20px;
}

#dradkpxcuj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 68&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 68<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 64&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 64<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">35 (51%)</td>
<td headers="stat_2" class="gt_row gt_center">32 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.088</td>
<td headers="I vs. III" class="gt_row gt_center">0.061</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">33 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">33 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.088</td>
<td headers="I vs. III" class="gt_row gt_center">-0.061</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (37, 56)</td>
<td headers="stat_2" class="gt_row gt_center">49 (37, 57)</td>
<td headers="stat_3" class="gt_row gt_center">47 (38, 58)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (20%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.195</td>
<td headers="I vs. III" class="gt_row gt_center">0.112</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (30%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.034</td>
<td headers="I vs. III" class="gt_row gt_center">-0.072</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">18 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">11 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">14 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.253</td>
<td headers="I vs. III" class="gt_row gt_center">0.107</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">15 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">17 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (28%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.069</td>
<td headers="I vs. III" class="gt_row gt_center">-0.140</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

Here, we create 4 weighted tables:

``` r
library(WeightIt)
library(survey)
survey_obj <- trial %>% mutate(
  w = weightit(grade ~ age + stage + trt, data = .)$weights) %>%
  survey::svydesign(~1, data = ., weights = ~w) 
```

``` r
survey_obj %>%
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ pairwise_smd) 
```

<div id="qeueugxnhr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qeueugxnhr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#qeueugxnhr thead, #qeueugxnhr tbody, #qeueugxnhr tfoot, #qeueugxnhr tr, #qeueugxnhr td, #qeueugxnhr th {
  border-style: none;
}

#qeueugxnhr p {
  margin: 0;
  padding: 0;
}

#qeueugxnhr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qeueugxnhr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#qeueugxnhr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qeueugxnhr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qeueugxnhr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qeueugxnhr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qeueugxnhr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qeueugxnhr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qeueugxnhr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qeueugxnhr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qeueugxnhr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qeueugxnhr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qeueugxnhr .gt_spanner_row {
  border-bottom-style: hidden;
}

#qeueugxnhr .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#qeueugxnhr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qeueugxnhr .gt_from_md > :first-child {
  margin-top: 0;
}

#qeueugxnhr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qeueugxnhr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qeueugxnhr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#qeueugxnhr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#qeueugxnhr .gt_row_group_first td {
  border-top-width: 2px;
}

#qeueugxnhr .gt_row_group_first th {
  border-top-width: 2px;
}

#qeueugxnhr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeueugxnhr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#qeueugxnhr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#qeueugxnhr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qeueugxnhr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeueugxnhr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qeueugxnhr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#qeueugxnhr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qeueugxnhr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qeueugxnhr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qeueugxnhr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeueugxnhr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qeueugxnhr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qeueugxnhr .gt_left {
  text-align: left;
}

#qeueugxnhr .gt_center {
  text-align: center;
}

#qeueugxnhr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qeueugxnhr .gt_font_normal {
  font-weight: normal;
}

#qeueugxnhr .gt_font_bold {
  font-weight: bold;
}

#qeueugxnhr .gt_font_italic {
  font-style: italic;
}

#qeueugxnhr .gt_super {
  font-size: 65%;
}

#qeueugxnhr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#qeueugxnhr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#qeueugxnhr .gt_indent_1 {
  text-indent: 5px;
}

#qeueugxnhr .gt_indent_2 {
  text-indent: 10px;
}

#qeueugxnhr .gt_indent_3 {
  text-indent: 15px;
}

#qeueugxnhr .gt_indent_4 {
  text-indent: 20px;
}

#qeueugxnhr .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 201&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 201<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="II vs. III">II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.012</td>
<td headers="I vs. III" class="gt_row gt_center">0.034</td>
<td headers="II vs. III" class="gt_row gt_center">0.023</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">100 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">98 (49%)</td>
<td headers="stat_3" class="gt_row gt_center">96 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">101 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">101 (51%)</td>
<td headers="stat_3" class="gt_row gt_center">104 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (38, 57)</td>
<td headers="stat_2" class="gt_row gt_center">49 (34, 57)</td>
<td headers="stat_3" class="gt_row gt_center">46 (38, 56)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.005</td>
<td headers="II vs. III" class="gt_row gt_center">0.007</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">12</td>
<td headers="stat_2" class="gt_row gt_center">11</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.044</td>
<td headers="I vs. III" class="gt_row gt_center">0.077</td>
<td headers="II vs. III" class="gt_row gt_center">0.041</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">59 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">52 (26%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">53 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">54 (27%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_3" class="gt_row gt_center">44 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">46 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">47 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">49 (25%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level") 
```

<div id="hqkerxpmwa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hqkerxpmwa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hqkerxpmwa thead, #hqkerxpmwa tbody, #hqkerxpmwa tfoot, #hqkerxpmwa tr, #hqkerxpmwa td, #hqkerxpmwa th {
  border-style: none;
}

#hqkerxpmwa p {
  margin: 0;
  padding: 0;
}

#hqkerxpmwa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hqkerxpmwa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hqkerxpmwa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hqkerxpmwa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hqkerxpmwa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqkerxpmwa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqkerxpmwa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqkerxpmwa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hqkerxpmwa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hqkerxpmwa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hqkerxpmwa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hqkerxpmwa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hqkerxpmwa .gt_spanner_row {
  border-bottom-style: hidden;
}

#hqkerxpmwa .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hqkerxpmwa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hqkerxpmwa .gt_from_md > :first-child {
  margin-top: 0;
}

#hqkerxpmwa .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hqkerxpmwa .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hqkerxpmwa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hqkerxpmwa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hqkerxpmwa .gt_row_group_first td {
  border-top-width: 2px;
}

#hqkerxpmwa .gt_row_group_first th {
  border-top-width: 2px;
}

#hqkerxpmwa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqkerxpmwa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hqkerxpmwa .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hqkerxpmwa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqkerxpmwa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqkerxpmwa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hqkerxpmwa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hqkerxpmwa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hqkerxpmwa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqkerxpmwa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqkerxpmwa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqkerxpmwa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqkerxpmwa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqkerxpmwa .gt_left {
  text-align: left;
}

#hqkerxpmwa .gt_center {
  text-align: center;
}

#hqkerxpmwa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hqkerxpmwa .gt_font_normal {
  font-weight: normal;
}

#hqkerxpmwa .gt_font_bold {
  font-weight: bold;
}

#hqkerxpmwa .gt_font_italic {
  font-style: italic;
}

#hqkerxpmwa .gt_super {
  font-size: 65%;
}

#hqkerxpmwa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hqkerxpmwa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hqkerxpmwa .gt_indent_1 {
  text-indent: 5px;
}

#hqkerxpmwa .gt_indent_2 {
  text-indent: 10px;
}

#hqkerxpmwa .gt_indent_3 {
  text-indent: 15px;
}

#hqkerxpmwa .gt_indent_4 {
  text-indent: 20px;
}

#hqkerxpmwa .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 201&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 201<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="II vs. III">II vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">100 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">98 (49%)</td>
<td headers="stat_3" class="gt_row gt_center">96 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.012</td>
<td headers="I vs. III" class="gt_row gt_center">0.034</td>
<td headers="II vs. III" class="gt_row gt_center">0.023</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">101 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">101 (51%)</td>
<td headers="stat_3" class="gt_row gt_center">104 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.034</td>
<td headers="II vs. III" class="gt_row gt_center">-0.023</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (38, 57)</td>
<td headers="stat_2" class="gt_row gt_center">49 (34, 57)</td>
<td headers="stat_3" class="gt_row gt_center">46 (38, 56)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">12</td>
<td headers="stat_2" class="gt_row gt_center">11</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td>
<td headers="II vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">59 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">52 (26%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.043</td>
<td headers="I vs. III" class="gt_row gt_center">0.075</td>
<td headers="II vs. III" class="gt_row gt_center">0.033</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">53 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">54 (27%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.025</td>
<td headers="I vs. III" class="gt_row gt_center">-0.014</td>
<td headers="II vs. III" class="gt_row gt_center">0.010</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_3" class="gt_row gt_center">44 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.008</td>
<td headers="I vs. III" class="gt_row gt_center">-0.024</td>
<td headers="II vs. III" class="gt_row gt_center">-0.016</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">46 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">47 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">49 (25%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.041</td>
<td headers="II vs. III" class="gt_row gt_center">-0.029</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ focal_smd) 
```

<div id="zophusvtfw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zophusvtfw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zophusvtfw thead, #zophusvtfw tbody, #zophusvtfw tfoot, #zophusvtfw tr, #zophusvtfw td, #zophusvtfw th {
  border-style: none;
}

#zophusvtfw p {
  margin: 0;
  padding: 0;
}

#zophusvtfw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zophusvtfw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zophusvtfw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zophusvtfw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zophusvtfw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zophusvtfw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zophusvtfw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zophusvtfw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zophusvtfw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zophusvtfw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zophusvtfw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zophusvtfw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zophusvtfw .gt_spanner_row {
  border-bottom-style: hidden;
}

#zophusvtfw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zophusvtfw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zophusvtfw .gt_from_md > :first-child {
  margin-top: 0;
}

#zophusvtfw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zophusvtfw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zophusvtfw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zophusvtfw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zophusvtfw .gt_row_group_first td {
  border-top-width: 2px;
}

#zophusvtfw .gt_row_group_first th {
  border-top-width: 2px;
}

#zophusvtfw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zophusvtfw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zophusvtfw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zophusvtfw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zophusvtfw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zophusvtfw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zophusvtfw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zophusvtfw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zophusvtfw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zophusvtfw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zophusvtfw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zophusvtfw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zophusvtfw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zophusvtfw .gt_left {
  text-align: left;
}

#zophusvtfw .gt_center {
  text-align: center;
}

#zophusvtfw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zophusvtfw .gt_font_normal {
  font-weight: normal;
}

#zophusvtfw .gt_font_bold {
  font-weight: bold;
}

#zophusvtfw .gt_font_italic {
  font-style: italic;
}

#zophusvtfw .gt_super {
  font-size: 65%;
}

#zophusvtfw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zophusvtfw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zophusvtfw .gt_indent_1 {
  text-indent: 5px;
}

#zophusvtfw .gt_indent_2 {
  text-indent: 10px;
}

#zophusvtfw .gt_indent_3 {
  text-indent: 15px;
}

#zophusvtfw .gt_indent_4 {
  text-indent: 20px;
}

#zophusvtfw .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 201&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 201<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.012</td>
<td headers="I vs. III" class="gt_row gt_center">0.034</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">100 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">98 (49%)</td>
<td headers="stat_3" class="gt_row gt_center">96 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">101 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">101 (51%)</td>
<td headers="stat_3" class="gt_row gt_center">104 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (38, 57)</td>
<td headers="stat_2" class="gt_row gt_center">49 (34, 57)</td>
<td headers="stat_3" class="gt_row gt_center">46 (38, 56)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">12</td>
<td headers="stat_2" class="gt_row gt_center">11</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center">0.044</td>
<td headers="I vs. III" class="gt_row gt_center">0.077</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">59 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">52 (26%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">53 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">54 (27%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_3" class="gt_row gt_center">44 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">46 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">47 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">49 (25%)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

Check

``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ focal_smd_level, location = ~ "level") 
```

<div id="gpwdrcgvos" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gpwdrcgvos table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gpwdrcgvos thead, #gpwdrcgvos tbody, #gpwdrcgvos tfoot, #gpwdrcgvos tr, #gpwdrcgvos td, #gpwdrcgvos th {
  border-style: none;
}

#gpwdrcgvos p {
  margin: 0;
  padding: 0;
}

#gpwdrcgvos .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gpwdrcgvos .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gpwdrcgvos .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gpwdrcgvos .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gpwdrcgvos .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gpwdrcgvos .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gpwdrcgvos .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gpwdrcgvos .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gpwdrcgvos .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gpwdrcgvos .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gpwdrcgvos .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gpwdrcgvos .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gpwdrcgvos .gt_spanner_row {
  border-bottom-style: hidden;
}

#gpwdrcgvos .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gpwdrcgvos .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gpwdrcgvos .gt_from_md > :first-child {
  margin-top: 0;
}

#gpwdrcgvos .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gpwdrcgvos .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gpwdrcgvos .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gpwdrcgvos .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gpwdrcgvos .gt_row_group_first td {
  border-top-width: 2px;
}

#gpwdrcgvos .gt_row_group_first th {
  border-top-width: 2px;
}

#gpwdrcgvos .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpwdrcgvos .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gpwdrcgvos .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gpwdrcgvos .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gpwdrcgvos .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpwdrcgvos .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gpwdrcgvos .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gpwdrcgvos .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gpwdrcgvos .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gpwdrcgvos .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gpwdrcgvos .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpwdrcgvos .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gpwdrcgvos .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpwdrcgvos .gt_left {
  text-align: left;
}

#gpwdrcgvos .gt_center {
  text-align: center;
}

#gpwdrcgvos .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gpwdrcgvos .gt_font_normal {
  font-weight: normal;
}

#gpwdrcgvos .gt_font_bold {
  font-weight: bold;
}

#gpwdrcgvos .gt_font_italic {
  font-style: italic;
}

#gpwdrcgvos .gt_super {
  font-size: 65%;
}

#gpwdrcgvos .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gpwdrcgvos .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gpwdrcgvos .gt_indent_1 {
  text-indent: 5px;
}

#gpwdrcgvos .gt_indent_2 {
  text-indent: 10px;
}

#gpwdrcgvos .gt_indent_3 {
  text-indent: 15px;
}

#gpwdrcgvos .gt_indent_4 {
  text-indent: 20px;
}

#gpwdrcgvos .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;, N = 201&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>I</strong>, N = 201<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>II</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;, N = 199&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>III</strong>, N = 199<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. II">I vs. II</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="I vs. III">I vs. III</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Chemotherapy Treatment</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug A</td>
<td headers="stat_1" class="gt_row gt_center">100 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">98 (49%)</td>
<td headers="stat_3" class="gt_row gt_center">96 (48%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.012</td>
<td headers="I vs. III" class="gt_row gt_center">0.034</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Drug B</td>
<td headers="stat_1" class="gt_row gt_center">101 (50%)</td>
<td headers="stat_2" class="gt_row gt_center">101 (51%)</td>
<td headers="stat_3" class="gt_row gt_center">104 (52%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.034</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">47 (38, 57)</td>
<td headers="stat_2" class="gt_row gt_center">49 (34, 57)</td>
<td headers="stat_3" class="gt_row gt_center">46 (38, 56)</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_1" class="gt_row gt_center">12</td>
<td headers="stat_2" class="gt_row gt_center">11</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">T Stage</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="I vs. II" class="gt_row gt_center"><br /></td>
<td headers="I vs. III" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T1</td>
<td headers="stat_1" class="gt_row gt_center">59 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">52 (26%)</td>
<td headers="I vs. II" class="gt_row gt_center">0.043</td>
<td headers="I vs. III" class="gt_row gt_center">0.075</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T2</td>
<td headers="stat_1" class="gt_row gt_center">53 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">55 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">54 (27%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.025</td>
<td headers="I vs. III" class="gt_row gt_center">-0.014</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T3</td>
<td headers="stat_1" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (21%)</td>
<td headers="stat_3" class="gt_row gt_center">44 (22%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.008</td>
<td headers="I vs. III" class="gt_row gt_center">-0.024</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    T4</td>
<td headers="stat_1" class="gt_row gt_center">46 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">47 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">49 (25%)</td>
<td headers="I vs. II" class="gt_row gt_center">-0.012</td>
<td headers="I vs. III" class="gt_row gt_center">-0.041</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>
