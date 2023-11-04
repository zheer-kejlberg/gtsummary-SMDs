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

```{=html}
<div id="rjevqzpwla" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rjevqzpwla table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rjevqzpwla thead, #rjevqzpwla tbody, #rjevqzpwla tfoot, #rjevqzpwla tr, #rjevqzpwla td, #rjevqzpwla th {
  border-style: none;
}

#rjevqzpwla p {
  margin: 0;
  padding: 0;
}

#rjevqzpwla .gt_table {
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

#rjevqzpwla .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rjevqzpwla .gt_title {
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

#rjevqzpwla .gt_subtitle {
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

#rjevqzpwla .gt_heading {
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

#rjevqzpwla .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjevqzpwla .gt_col_headings {
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

#rjevqzpwla .gt_col_heading {
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

#rjevqzpwla .gt_column_spanner_outer {
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

#rjevqzpwla .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rjevqzpwla .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rjevqzpwla .gt_column_spanner {
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

#rjevqzpwla .gt_spanner_row {
  border-bottom-style: hidden;
}

#rjevqzpwla .gt_group_heading {
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

#rjevqzpwla .gt_empty_group_heading {
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

#rjevqzpwla .gt_from_md > :first-child {
  margin-top: 0;
}

#rjevqzpwla .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rjevqzpwla .gt_row {
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

#rjevqzpwla .gt_stub {
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

#rjevqzpwla .gt_stub_row_group {
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

#rjevqzpwla .gt_row_group_first td {
  border-top-width: 2px;
}

#rjevqzpwla .gt_row_group_first th {
  border-top-width: 2px;
}

#rjevqzpwla .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjevqzpwla .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rjevqzpwla .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rjevqzpwla .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjevqzpwla .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjevqzpwla .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rjevqzpwla .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rjevqzpwla .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rjevqzpwla .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjevqzpwla .gt_footnotes {
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

#rjevqzpwla .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjevqzpwla .gt_sourcenotes {
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

#rjevqzpwla .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjevqzpwla .gt_left {
  text-align: left;
}

#rjevqzpwla .gt_center {
  text-align: center;
}

#rjevqzpwla .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rjevqzpwla .gt_font_normal {
  font-weight: normal;
}

#rjevqzpwla .gt_font_bold {
  font-weight: bold;
}

#rjevqzpwla .gt_font_italic {
  font-style: italic;
}

#rjevqzpwla .gt_super {
  font-size: 65%;
}

#rjevqzpwla .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rjevqzpwla .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rjevqzpwla .gt_indent_1 {
  text-indent: 5px;
}

#rjevqzpwla .gt_indent_2 {
  text-indent: 10px;
}

#rjevqzpwla .gt_indent_3 {
  text-indent: 15px;
}

#rjevqzpwla .gt_indent_4 {
  text-indent: 20px;
}

#rjevqzpwla .gt_indent_5 {
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
```
``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level")
```

```{=html}
<div id="hwprvwyjys" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hwprvwyjys table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hwprvwyjys thead, #hwprvwyjys tbody, #hwprvwyjys tfoot, #hwprvwyjys tr, #hwprvwyjys td, #hwprvwyjys th {
  border-style: none;
}

#hwprvwyjys p {
  margin: 0;
  padding: 0;
}

#hwprvwyjys .gt_table {
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

#hwprvwyjys .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hwprvwyjys .gt_title {
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

#hwprvwyjys .gt_subtitle {
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

#hwprvwyjys .gt_heading {
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

#hwprvwyjys .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwprvwyjys .gt_col_headings {
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

#hwprvwyjys .gt_col_heading {
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

#hwprvwyjys .gt_column_spanner_outer {
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

#hwprvwyjys .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hwprvwyjys .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hwprvwyjys .gt_column_spanner {
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

#hwprvwyjys .gt_spanner_row {
  border-bottom-style: hidden;
}

#hwprvwyjys .gt_group_heading {
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

#hwprvwyjys .gt_empty_group_heading {
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

#hwprvwyjys .gt_from_md > :first-child {
  margin-top: 0;
}

#hwprvwyjys .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hwprvwyjys .gt_row {
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

#hwprvwyjys .gt_stub {
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

#hwprvwyjys .gt_stub_row_group {
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

#hwprvwyjys .gt_row_group_first td {
  border-top-width: 2px;
}

#hwprvwyjys .gt_row_group_first th {
  border-top-width: 2px;
}

#hwprvwyjys .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwprvwyjys .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hwprvwyjys .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hwprvwyjys .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwprvwyjys .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwprvwyjys .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hwprvwyjys .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hwprvwyjys .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hwprvwyjys .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hwprvwyjys .gt_footnotes {
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

#hwprvwyjys .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwprvwyjys .gt_sourcenotes {
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

#hwprvwyjys .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hwprvwyjys .gt_left {
  text-align: left;
}

#hwprvwyjys .gt_center {
  text-align: center;
}

#hwprvwyjys .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hwprvwyjys .gt_font_normal {
  font-weight: normal;
}

#hwprvwyjys .gt_font_bold {
  font-weight: bold;
}

#hwprvwyjys .gt_font_italic {
  font-style: italic;
}

#hwprvwyjys .gt_super {
  font-size: 65%;
}

#hwprvwyjys .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hwprvwyjys .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hwprvwyjys .gt_indent_1 {
  text-indent: 5px;
}

#hwprvwyjys .gt_indent_2 {
  text-indent: 10px;
}

#hwprvwyjys .gt_indent_3 {
  text-indent: 15px;
}

#hwprvwyjys .gt_indent_4 {
  text-indent: 20px;
}

#hwprvwyjys .gt_indent_5 {
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
```
``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ focal_smd, location = ~ "label")
```

```{=html}
<div id="xkrrybbgmu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xkrrybbgmu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xkrrybbgmu thead, #xkrrybbgmu tbody, #xkrrybbgmu tfoot, #xkrrybbgmu tr, #xkrrybbgmu td, #xkrrybbgmu th {
  border-style: none;
}

#xkrrybbgmu p {
  margin: 0;
  padding: 0;
}

#xkrrybbgmu .gt_table {
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

#xkrrybbgmu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xkrrybbgmu .gt_title {
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

#xkrrybbgmu .gt_subtitle {
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

#xkrrybbgmu .gt_heading {
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

#xkrrybbgmu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xkrrybbgmu .gt_col_headings {
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

#xkrrybbgmu .gt_col_heading {
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

#xkrrybbgmu .gt_column_spanner_outer {
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

#xkrrybbgmu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xkrrybbgmu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xkrrybbgmu .gt_column_spanner {
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

#xkrrybbgmu .gt_spanner_row {
  border-bottom-style: hidden;
}

#xkrrybbgmu .gt_group_heading {
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

#xkrrybbgmu .gt_empty_group_heading {
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

#xkrrybbgmu .gt_from_md > :first-child {
  margin-top: 0;
}

#xkrrybbgmu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xkrrybbgmu .gt_row {
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

#xkrrybbgmu .gt_stub {
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

#xkrrybbgmu .gt_stub_row_group {
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

#xkrrybbgmu .gt_row_group_first td {
  border-top-width: 2px;
}

#xkrrybbgmu .gt_row_group_first th {
  border-top-width: 2px;
}

#xkrrybbgmu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xkrrybbgmu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xkrrybbgmu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xkrrybbgmu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xkrrybbgmu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xkrrybbgmu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xkrrybbgmu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xkrrybbgmu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xkrrybbgmu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xkrrybbgmu .gt_footnotes {
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

#xkrrybbgmu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xkrrybbgmu .gt_sourcenotes {
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

#xkrrybbgmu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xkrrybbgmu .gt_left {
  text-align: left;
}

#xkrrybbgmu .gt_center {
  text-align: center;
}

#xkrrybbgmu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xkrrybbgmu .gt_font_normal {
  font-weight: normal;
}

#xkrrybbgmu .gt_font_bold {
  font-weight: bold;
}

#xkrrybbgmu .gt_font_italic {
  font-style: italic;
}

#xkrrybbgmu .gt_super {
  font-size: 65%;
}

#xkrrybbgmu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xkrrybbgmu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xkrrybbgmu .gt_indent_1 {
  text-indent: 5px;
}

#xkrrybbgmu .gt_indent_2 {
  text-indent: 10px;
}

#xkrrybbgmu .gt_indent_3 {
  text-indent: 15px;
}

#xkrrybbgmu .gt_indent_4 {
  text-indent: 20px;
}

#xkrrybbgmu .gt_indent_5 {
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
```
``` r
trial %>% 
  tbl_summary(by = grade, include = c(trt, age, stage)) %>%
  add_stat(fns = everything() ~ focal_smd_level, location = ~ "level")
```

```{=html}
<div id="sutitcsmfv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#sutitcsmfv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#sutitcsmfv thead, #sutitcsmfv tbody, #sutitcsmfv tfoot, #sutitcsmfv tr, #sutitcsmfv td, #sutitcsmfv th {
  border-style: none;
}

#sutitcsmfv p {
  margin: 0;
  padding: 0;
}

#sutitcsmfv .gt_table {
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

#sutitcsmfv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#sutitcsmfv .gt_title {
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

#sutitcsmfv .gt_subtitle {
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

#sutitcsmfv .gt_heading {
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

#sutitcsmfv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sutitcsmfv .gt_col_headings {
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

#sutitcsmfv .gt_col_heading {
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

#sutitcsmfv .gt_column_spanner_outer {
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

#sutitcsmfv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#sutitcsmfv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#sutitcsmfv .gt_column_spanner {
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

#sutitcsmfv .gt_spanner_row {
  border-bottom-style: hidden;
}

#sutitcsmfv .gt_group_heading {
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

#sutitcsmfv .gt_empty_group_heading {
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

#sutitcsmfv .gt_from_md > :first-child {
  margin-top: 0;
}

#sutitcsmfv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#sutitcsmfv .gt_row {
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

#sutitcsmfv .gt_stub {
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

#sutitcsmfv .gt_stub_row_group {
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

#sutitcsmfv .gt_row_group_first td {
  border-top-width: 2px;
}

#sutitcsmfv .gt_row_group_first th {
  border-top-width: 2px;
}

#sutitcsmfv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sutitcsmfv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#sutitcsmfv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#sutitcsmfv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sutitcsmfv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sutitcsmfv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#sutitcsmfv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#sutitcsmfv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#sutitcsmfv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sutitcsmfv .gt_footnotes {
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

#sutitcsmfv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sutitcsmfv .gt_sourcenotes {
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

#sutitcsmfv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sutitcsmfv .gt_left {
  text-align: left;
}

#sutitcsmfv .gt_center {
  text-align: center;
}

#sutitcsmfv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#sutitcsmfv .gt_font_normal {
  font-weight: normal;
}

#sutitcsmfv .gt_font_bold {
  font-weight: bold;
}

#sutitcsmfv .gt_font_italic {
  font-style: italic;
}

#sutitcsmfv .gt_super {
  font-size: 65%;
}

#sutitcsmfv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#sutitcsmfv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#sutitcsmfv .gt_indent_1 {
  text-indent: 5px;
}

#sutitcsmfv .gt_indent_2 {
  text-indent: 10px;
}

#sutitcsmfv .gt_indent_3 {
  text-indent: 15px;
}

#sutitcsmfv .gt_indent_4 {
  text-indent: 20px;
}

#sutitcsmfv .gt_indent_5 {
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
```
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

```{=html}
<div id="zwgtzivcpy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zwgtzivcpy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zwgtzivcpy thead, #zwgtzivcpy tbody, #zwgtzivcpy tfoot, #zwgtzivcpy tr, #zwgtzivcpy td, #zwgtzivcpy th {
  border-style: none;
}

#zwgtzivcpy p {
  margin: 0;
  padding: 0;
}

#zwgtzivcpy .gt_table {
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

#zwgtzivcpy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zwgtzivcpy .gt_title {
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

#zwgtzivcpy .gt_subtitle {
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

#zwgtzivcpy .gt_heading {
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

#zwgtzivcpy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwgtzivcpy .gt_col_headings {
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

#zwgtzivcpy .gt_col_heading {
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

#zwgtzivcpy .gt_column_spanner_outer {
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

#zwgtzivcpy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zwgtzivcpy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zwgtzivcpy .gt_column_spanner {
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

#zwgtzivcpy .gt_spanner_row {
  border-bottom-style: hidden;
}

#zwgtzivcpy .gt_group_heading {
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

#zwgtzivcpy .gt_empty_group_heading {
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

#zwgtzivcpy .gt_from_md > :first-child {
  margin-top: 0;
}

#zwgtzivcpy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zwgtzivcpy .gt_row {
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

#zwgtzivcpy .gt_stub {
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

#zwgtzivcpy .gt_stub_row_group {
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

#zwgtzivcpy .gt_row_group_first td {
  border-top-width: 2px;
}

#zwgtzivcpy .gt_row_group_first th {
  border-top-width: 2px;
}

#zwgtzivcpy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwgtzivcpy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zwgtzivcpy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zwgtzivcpy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwgtzivcpy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwgtzivcpy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zwgtzivcpy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zwgtzivcpy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zwgtzivcpy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwgtzivcpy .gt_footnotes {
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

#zwgtzivcpy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwgtzivcpy .gt_sourcenotes {
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

#zwgtzivcpy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwgtzivcpy .gt_left {
  text-align: left;
}

#zwgtzivcpy .gt_center {
  text-align: center;
}

#zwgtzivcpy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zwgtzivcpy .gt_font_normal {
  font-weight: normal;
}

#zwgtzivcpy .gt_font_bold {
  font-weight: bold;
}

#zwgtzivcpy .gt_font_italic {
  font-style: italic;
}

#zwgtzivcpy .gt_super {
  font-size: 65%;
}

#zwgtzivcpy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zwgtzivcpy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zwgtzivcpy .gt_indent_1 {
  text-indent: 5px;
}

#zwgtzivcpy .gt_indent_2 {
  text-indent: 10px;
}

#zwgtzivcpy .gt_indent_3 {
  text-indent: 15px;
}

#zwgtzivcpy .gt_indent_4 {
  text-indent: 20px;
}

#zwgtzivcpy .gt_indent_5 {
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
```
``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level") 
```

```{=html}
<div id="wzjyxdfxjv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wzjyxdfxjv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wzjyxdfxjv thead, #wzjyxdfxjv tbody, #wzjyxdfxjv tfoot, #wzjyxdfxjv tr, #wzjyxdfxjv td, #wzjyxdfxjv th {
  border-style: none;
}

#wzjyxdfxjv p {
  margin: 0;
  padding: 0;
}

#wzjyxdfxjv .gt_table {
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

#wzjyxdfxjv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wzjyxdfxjv .gt_title {
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

#wzjyxdfxjv .gt_subtitle {
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

#wzjyxdfxjv .gt_heading {
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

#wzjyxdfxjv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wzjyxdfxjv .gt_col_headings {
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

#wzjyxdfxjv .gt_col_heading {
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

#wzjyxdfxjv .gt_column_spanner_outer {
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

#wzjyxdfxjv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wzjyxdfxjv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wzjyxdfxjv .gt_column_spanner {
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

#wzjyxdfxjv .gt_spanner_row {
  border-bottom-style: hidden;
}

#wzjyxdfxjv .gt_group_heading {
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

#wzjyxdfxjv .gt_empty_group_heading {
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

#wzjyxdfxjv .gt_from_md > :first-child {
  margin-top: 0;
}

#wzjyxdfxjv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wzjyxdfxjv .gt_row {
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

#wzjyxdfxjv .gt_stub {
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

#wzjyxdfxjv .gt_stub_row_group {
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

#wzjyxdfxjv .gt_row_group_first td {
  border-top-width: 2px;
}

#wzjyxdfxjv .gt_row_group_first th {
  border-top-width: 2px;
}

#wzjyxdfxjv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzjyxdfxjv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wzjyxdfxjv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wzjyxdfxjv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wzjyxdfxjv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzjyxdfxjv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wzjyxdfxjv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wzjyxdfxjv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wzjyxdfxjv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wzjyxdfxjv .gt_footnotes {
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

#wzjyxdfxjv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzjyxdfxjv .gt_sourcenotes {
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

#wzjyxdfxjv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzjyxdfxjv .gt_left {
  text-align: left;
}

#wzjyxdfxjv .gt_center {
  text-align: center;
}

#wzjyxdfxjv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wzjyxdfxjv .gt_font_normal {
  font-weight: normal;
}

#wzjyxdfxjv .gt_font_bold {
  font-weight: bold;
}

#wzjyxdfxjv .gt_font_italic {
  font-style: italic;
}

#wzjyxdfxjv .gt_super {
  font-size: 65%;
}

#wzjyxdfxjv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wzjyxdfxjv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wzjyxdfxjv .gt_indent_1 {
  text-indent: 5px;
}

#wzjyxdfxjv .gt_indent_2 {
  text-indent: 10px;
}

#wzjyxdfxjv .gt_indent_3 {
  text-indent: 15px;
}

#wzjyxdfxjv .gt_indent_4 {
  text-indent: 20px;
}

#wzjyxdfxjv .gt_indent_5 {
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
```
``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ focal_smd) 
```

```{=html}
<div id="ptchjurikn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ptchjurikn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ptchjurikn thead, #ptchjurikn tbody, #ptchjurikn tfoot, #ptchjurikn tr, #ptchjurikn td, #ptchjurikn th {
  border-style: none;
}

#ptchjurikn p {
  margin: 0;
  padding: 0;
}

#ptchjurikn .gt_table {
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

#ptchjurikn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ptchjurikn .gt_title {
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

#ptchjurikn .gt_subtitle {
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

#ptchjurikn .gt_heading {
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

#ptchjurikn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ptchjurikn .gt_col_headings {
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

#ptchjurikn .gt_col_heading {
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

#ptchjurikn .gt_column_spanner_outer {
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

#ptchjurikn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ptchjurikn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ptchjurikn .gt_column_spanner {
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

#ptchjurikn .gt_spanner_row {
  border-bottom-style: hidden;
}

#ptchjurikn .gt_group_heading {
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

#ptchjurikn .gt_empty_group_heading {
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

#ptchjurikn .gt_from_md > :first-child {
  margin-top: 0;
}

#ptchjurikn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ptchjurikn .gt_row {
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

#ptchjurikn .gt_stub {
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

#ptchjurikn .gt_stub_row_group {
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

#ptchjurikn .gt_row_group_first td {
  border-top-width: 2px;
}

#ptchjurikn .gt_row_group_first th {
  border-top-width: 2px;
}

#ptchjurikn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ptchjurikn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ptchjurikn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ptchjurikn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ptchjurikn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ptchjurikn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ptchjurikn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ptchjurikn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ptchjurikn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ptchjurikn .gt_footnotes {
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

#ptchjurikn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ptchjurikn .gt_sourcenotes {
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

#ptchjurikn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ptchjurikn .gt_left {
  text-align: left;
}

#ptchjurikn .gt_center {
  text-align: center;
}

#ptchjurikn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ptchjurikn .gt_font_normal {
  font-weight: normal;
}

#ptchjurikn .gt_font_bold {
  font-weight: bold;
}

#ptchjurikn .gt_font_italic {
  font-style: italic;
}

#ptchjurikn .gt_super {
  font-size: 65%;
}

#ptchjurikn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ptchjurikn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ptchjurikn .gt_indent_1 {
  text-indent: 5px;
}

#ptchjurikn .gt_indent_2 {
  text-indent: 10px;
}

#ptchjurikn .gt_indent_3 {
  text-indent: 15px;
}

#ptchjurikn .gt_indent_4 {
  text-indent: 20px;
}

#ptchjurikn .gt_indent_5 {
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
```
Check

``` r
survey_obj %>% 
  tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
  add_stat(fns = everything() ~ focal_smd_level, location = ~ "level") 
```

```{=html}
<div id="dagtpgspdg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dagtpgspdg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dagtpgspdg thead, #dagtpgspdg tbody, #dagtpgspdg tfoot, #dagtpgspdg tr, #dagtpgspdg td, #dagtpgspdg th {
  border-style: none;
}

#dagtpgspdg p {
  margin: 0;
  padding: 0;
}

#dagtpgspdg .gt_table {
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

#dagtpgspdg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dagtpgspdg .gt_title {
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

#dagtpgspdg .gt_subtitle {
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

#dagtpgspdg .gt_heading {
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

#dagtpgspdg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dagtpgspdg .gt_col_headings {
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

#dagtpgspdg .gt_col_heading {
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

#dagtpgspdg .gt_column_spanner_outer {
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

#dagtpgspdg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dagtpgspdg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dagtpgspdg .gt_column_spanner {
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

#dagtpgspdg .gt_spanner_row {
  border-bottom-style: hidden;
}

#dagtpgspdg .gt_group_heading {
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

#dagtpgspdg .gt_empty_group_heading {
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

#dagtpgspdg .gt_from_md > :first-child {
  margin-top: 0;
}

#dagtpgspdg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dagtpgspdg .gt_row {
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

#dagtpgspdg .gt_stub {
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

#dagtpgspdg .gt_stub_row_group {
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

#dagtpgspdg .gt_row_group_first td {
  border-top-width: 2px;
}

#dagtpgspdg .gt_row_group_first th {
  border-top-width: 2px;
}

#dagtpgspdg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dagtpgspdg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dagtpgspdg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dagtpgspdg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dagtpgspdg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dagtpgspdg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dagtpgspdg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dagtpgspdg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dagtpgspdg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dagtpgspdg .gt_footnotes {
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

#dagtpgspdg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dagtpgspdg .gt_sourcenotes {
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

#dagtpgspdg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dagtpgspdg .gt_left {
  text-align: left;
}

#dagtpgspdg .gt_center {
  text-align: center;
}

#dagtpgspdg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dagtpgspdg .gt_font_normal {
  font-weight: normal;
}

#dagtpgspdg .gt_font_bold {
  font-weight: bold;
}

#dagtpgspdg .gt_font_italic {
  font-style: italic;
}

#dagtpgspdg .gt_super {
  font-size: 65%;
}

#dagtpgspdg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dagtpgspdg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dagtpgspdg .gt_indent_1 {
  text-indent: 5px;
}

#dagtpgspdg .gt_indent_2 {
  text-indent: 10px;
}

#dagtpgspdg .gt_indent_3 {
  text-indent: 15px;
}

#dagtpgspdg .gt_indent_4 {
  text-indent: 20px;
}

#dagtpgspdg .gt_indent_5 {
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
```
