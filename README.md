##### First, we load dependencies

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

#### Functions to add SMDs to *tbl\_summary* and *tbl\_svysummary* objects

    trial %>% 
      tbl_summary(by = grade, include = c(trt, age, stage)) %>%
      add_stat(fns = everything() ~ pairwise_smd, location = ~ "label")

<div id="llggcixzni" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#llggcixzni table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#llggcixzni thead, #llggcixzni tbody, #llggcixzni tfoot, #llggcixzni tr, #llggcixzni td, #llggcixzni th {
  border-style: none;
}

#llggcixzni p {
  margin: 0;
  padding: 0;
}

#llggcixzni .gt_table {
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

#llggcixzni .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#llggcixzni .gt_title {
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

#llggcixzni .gt_subtitle {
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

#llggcixzni .gt_heading {
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

#llggcixzni .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#llggcixzni .gt_col_headings {
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

#llggcixzni .gt_col_heading {
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

#llggcixzni .gt_column_spanner_outer {
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

#llggcixzni .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#llggcixzni .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#llggcixzni .gt_column_spanner {
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

#llggcixzni .gt_spanner_row {
  border-bottom-style: hidden;
}

#llggcixzni .gt_group_heading {
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

#llggcixzni .gt_empty_group_heading {
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

#llggcixzni .gt_from_md > :first-child {
  margin-top: 0;
}

#llggcixzni .gt_from_md > :last-child {
  margin-bottom: 0;
}

#llggcixzni .gt_row {
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

#llggcixzni .gt_stub {
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

#llggcixzni .gt_stub_row_group {
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

#llggcixzni .gt_row_group_first td {
  border-top-width: 2px;
}

#llggcixzni .gt_row_group_first th {
  border-top-width: 2px;
}

#llggcixzni .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#llggcixzni .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#llggcixzni .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#llggcixzni .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#llggcixzni .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#llggcixzni .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#llggcixzni .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#llggcixzni .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#llggcixzni .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#llggcixzni .gt_footnotes {
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

#llggcixzni .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#llggcixzni .gt_sourcenotes {
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

#llggcixzni .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#llggcixzni .gt_left {
  text-align: left;
}

#llggcixzni .gt_center {
  text-align: center;
}

#llggcixzni .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#llggcixzni .gt_font_normal {
  font-weight: normal;
}

#llggcixzni .gt_font_bold {
  font-weight: bold;
}

#llggcixzni .gt_font_italic {
  font-style: italic;
}

#llggcixzni .gt_super {
  font-size: 65%;
}

#llggcixzni .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#llggcixzni .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#llggcixzni .gt_indent_1 {
  text-indent: 5px;
}

#llggcixzni .gt_indent_2 {
  text-indent: 10px;
}

#llggcixzni .gt_indent_3 {
  text-indent: 15px;
}

#llggcixzni .gt_indent_4 {
  text-indent: 20px;
}

#llggcixzni .gt_indent_5 {
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

    trial %>% 
      tbl_summary(by = grade, include = c(trt, age, stage)) %>%
      add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level")

<div id="knpysqebez" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#knpysqebez table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#knpysqebez thead, #knpysqebez tbody, #knpysqebez tfoot, #knpysqebez tr, #knpysqebez td, #knpysqebez th {
  border-style: none;
}

#knpysqebez p {
  margin: 0;
  padding: 0;
}

#knpysqebez .gt_table {
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

#knpysqebez .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#knpysqebez .gt_title {
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

#knpysqebez .gt_subtitle {
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

#knpysqebez .gt_heading {
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

#knpysqebez .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knpysqebez .gt_col_headings {
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

#knpysqebez .gt_col_heading {
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

#knpysqebez .gt_column_spanner_outer {
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

#knpysqebez .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#knpysqebez .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#knpysqebez .gt_column_spanner {
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

#knpysqebez .gt_spanner_row {
  border-bottom-style: hidden;
}

#knpysqebez .gt_group_heading {
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

#knpysqebez .gt_empty_group_heading {
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

#knpysqebez .gt_from_md > :first-child {
  margin-top: 0;
}

#knpysqebez .gt_from_md > :last-child {
  margin-bottom: 0;
}

#knpysqebez .gt_row {
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

#knpysqebez .gt_stub {
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

#knpysqebez .gt_stub_row_group {
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

#knpysqebez .gt_row_group_first td {
  border-top-width: 2px;
}

#knpysqebez .gt_row_group_first th {
  border-top-width: 2px;
}

#knpysqebez .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knpysqebez .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#knpysqebez .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#knpysqebez .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knpysqebez .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knpysqebez .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#knpysqebez .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#knpysqebez .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#knpysqebez .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knpysqebez .gt_footnotes {
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

#knpysqebez .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#knpysqebez .gt_sourcenotes {
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

#knpysqebez .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#knpysqebez .gt_left {
  text-align: left;
}

#knpysqebez .gt_center {
  text-align: center;
}

#knpysqebez .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#knpysqebez .gt_font_normal {
  font-weight: normal;
}

#knpysqebez .gt_font_bold {
  font-weight: bold;
}

#knpysqebez .gt_font_italic {
  font-style: italic;
}

#knpysqebez .gt_super {
  font-size: 65%;
}

#knpysqebez .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#knpysqebez .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#knpysqebez .gt_indent_1 {
  text-indent: 5px;
}

#knpysqebez .gt_indent_2 {
  text-indent: 10px;
}

#knpysqebez .gt_indent_3 {
  text-indent: 15px;
}

#knpysqebez .gt_indent_4 {
  text-indent: 20px;
}

#knpysqebez .gt_indent_5 {
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

    trial %>% 
      tbl_summary(by = grade, include = c(trt, age, stage)) %>%
      add_stat(fns = everything() ~ focal_smd, location = ~ "label")

<div id="gginmuyoyp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gginmuyoyp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gginmuyoyp thead, #gginmuyoyp tbody, #gginmuyoyp tfoot, #gginmuyoyp tr, #gginmuyoyp td, #gginmuyoyp th {
  border-style: none;
}

#gginmuyoyp p {
  margin: 0;
  padding: 0;
}

#gginmuyoyp .gt_table {
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

#gginmuyoyp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gginmuyoyp .gt_title {
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

#gginmuyoyp .gt_subtitle {
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

#gginmuyoyp .gt_heading {
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

#gginmuyoyp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gginmuyoyp .gt_col_headings {
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

#gginmuyoyp .gt_col_heading {
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

#gginmuyoyp .gt_column_spanner_outer {
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

#gginmuyoyp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gginmuyoyp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gginmuyoyp .gt_column_spanner {
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

#gginmuyoyp .gt_spanner_row {
  border-bottom-style: hidden;
}

#gginmuyoyp .gt_group_heading {
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

#gginmuyoyp .gt_empty_group_heading {
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

#gginmuyoyp .gt_from_md > :first-child {
  margin-top: 0;
}

#gginmuyoyp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gginmuyoyp .gt_row {
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

#gginmuyoyp .gt_stub {
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

#gginmuyoyp .gt_stub_row_group {
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

#gginmuyoyp .gt_row_group_first td {
  border-top-width: 2px;
}

#gginmuyoyp .gt_row_group_first th {
  border-top-width: 2px;
}

#gginmuyoyp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gginmuyoyp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gginmuyoyp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gginmuyoyp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gginmuyoyp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gginmuyoyp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gginmuyoyp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gginmuyoyp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gginmuyoyp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gginmuyoyp .gt_footnotes {
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

#gginmuyoyp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gginmuyoyp .gt_sourcenotes {
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

#gginmuyoyp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gginmuyoyp .gt_left {
  text-align: left;
}

#gginmuyoyp .gt_center {
  text-align: center;
}

#gginmuyoyp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gginmuyoyp .gt_font_normal {
  font-weight: normal;
}

#gginmuyoyp .gt_font_bold {
  font-weight: bold;
}

#gginmuyoyp .gt_font_italic {
  font-style: italic;
}

#gginmuyoyp .gt_super {
  font-size: 65%;
}

#gginmuyoyp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gginmuyoyp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gginmuyoyp .gt_indent_1 {
  text-indent: 5px;
}

#gginmuyoyp .gt_indent_2 {
  text-indent: 10px;
}

#gginmuyoyp .gt_indent_3 {
  text-indent: 15px;
}

#gginmuyoyp .gt_indent_4 {
  text-indent: 20px;
}

#gginmuyoyp .gt_indent_5 {
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

    trial %>% 
      tbl_summary(by = grade, include = c(trt, age, stage)) %>%
      add_stat(fns = everything() ~ focal_smd_level, location = ~ "level")

<div id="daacbqwums" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#daacbqwums table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#daacbqwums thead, #daacbqwums tbody, #daacbqwums tfoot, #daacbqwums tr, #daacbqwums td, #daacbqwums th {
  border-style: none;
}

#daacbqwums p {
  margin: 0;
  padding: 0;
}

#daacbqwums .gt_table {
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

#daacbqwums .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#daacbqwums .gt_title {
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

#daacbqwums .gt_subtitle {
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

#daacbqwums .gt_heading {
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

#daacbqwums .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daacbqwums .gt_col_headings {
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

#daacbqwums .gt_col_heading {
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

#daacbqwums .gt_column_spanner_outer {
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

#daacbqwums .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#daacbqwums .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#daacbqwums .gt_column_spanner {
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

#daacbqwums .gt_spanner_row {
  border-bottom-style: hidden;
}

#daacbqwums .gt_group_heading {
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

#daacbqwums .gt_empty_group_heading {
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

#daacbqwums .gt_from_md > :first-child {
  margin-top: 0;
}

#daacbqwums .gt_from_md > :last-child {
  margin-bottom: 0;
}

#daacbqwums .gt_row {
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

#daacbqwums .gt_stub {
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

#daacbqwums .gt_stub_row_group {
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

#daacbqwums .gt_row_group_first td {
  border-top-width: 2px;
}

#daacbqwums .gt_row_group_first th {
  border-top-width: 2px;
}

#daacbqwums .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daacbqwums .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#daacbqwums .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#daacbqwums .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daacbqwums .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daacbqwums .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#daacbqwums .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#daacbqwums .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#daacbqwums .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daacbqwums .gt_footnotes {
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

#daacbqwums .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#daacbqwums .gt_sourcenotes {
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

#daacbqwums .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#daacbqwums .gt_left {
  text-align: left;
}

#daacbqwums .gt_center {
  text-align: center;
}

#daacbqwums .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#daacbqwums .gt_font_normal {
  font-weight: normal;
}

#daacbqwums .gt_font_bold {
  font-weight: bold;
}

#daacbqwums .gt_font_italic {
  font-style: italic;
}

#daacbqwums .gt_super {
  font-size: 65%;
}

#daacbqwums .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#daacbqwums .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#daacbqwums .gt_indent_1 {
  text-indent: 5px;
}

#daacbqwums .gt_indent_2 {
  text-indent: 10px;
}

#daacbqwums .gt_indent_3 {
  text-indent: 15px;
}

#daacbqwums .gt_indent_4 {
  text-indent: 20px;
}

#daacbqwums .gt_indent_5 {
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

    library(WeightIt)
    library(survey)
    survey_obj <- trial %>% mutate(
      w = weightit(grade ~ age + stage + trt, data = .)$weights) %>%
      survey::svydesign(~1, data = ., weights = ~w) 

    survey_obj %>%
      tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
      add_stat(fns = everything() ~ pairwise_smd) 

<div id="cgvwqrokdq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cgvwqrokdq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cgvwqrokdq thead, #cgvwqrokdq tbody, #cgvwqrokdq tfoot, #cgvwqrokdq tr, #cgvwqrokdq td, #cgvwqrokdq th {
  border-style: none;
}

#cgvwqrokdq p {
  margin: 0;
  padding: 0;
}

#cgvwqrokdq .gt_table {
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

#cgvwqrokdq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cgvwqrokdq .gt_title {
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

#cgvwqrokdq .gt_subtitle {
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

#cgvwqrokdq .gt_heading {
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

#cgvwqrokdq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cgvwqrokdq .gt_col_headings {
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

#cgvwqrokdq .gt_col_heading {
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

#cgvwqrokdq .gt_column_spanner_outer {
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

#cgvwqrokdq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cgvwqrokdq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cgvwqrokdq .gt_column_spanner {
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

#cgvwqrokdq .gt_spanner_row {
  border-bottom-style: hidden;
}

#cgvwqrokdq .gt_group_heading {
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

#cgvwqrokdq .gt_empty_group_heading {
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

#cgvwqrokdq .gt_from_md > :first-child {
  margin-top: 0;
}

#cgvwqrokdq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cgvwqrokdq .gt_row {
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

#cgvwqrokdq .gt_stub {
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

#cgvwqrokdq .gt_stub_row_group {
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

#cgvwqrokdq .gt_row_group_first td {
  border-top-width: 2px;
}

#cgvwqrokdq .gt_row_group_first th {
  border-top-width: 2px;
}

#cgvwqrokdq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cgvwqrokdq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cgvwqrokdq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cgvwqrokdq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cgvwqrokdq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cgvwqrokdq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cgvwqrokdq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cgvwqrokdq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cgvwqrokdq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cgvwqrokdq .gt_footnotes {
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

#cgvwqrokdq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cgvwqrokdq .gt_sourcenotes {
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

#cgvwqrokdq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cgvwqrokdq .gt_left {
  text-align: left;
}

#cgvwqrokdq .gt_center {
  text-align: center;
}

#cgvwqrokdq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cgvwqrokdq .gt_font_normal {
  font-weight: normal;
}

#cgvwqrokdq .gt_font_bold {
  font-weight: bold;
}

#cgvwqrokdq .gt_font_italic {
  font-style: italic;
}

#cgvwqrokdq .gt_super {
  font-size: 65%;
}

#cgvwqrokdq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cgvwqrokdq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cgvwqrokdq .gt_indent_1 {
  text-indent: 5px;
}

#cgvwqrokdq .gt_indent_2 {
  text-indent: 10px;
}

#cgvwqrokdq .gt_indent_3 {
  text-indent: 15px;
}

#cgvwqrokdq .gt_indent_4 {
  text-indent: 20px;
}

#cgvwqrokdq .gt_indent_5 {
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

    survey_obj %>% 
      tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
      add_stat(fns = everything() ~ pairwise_smd_level, location = ~ "level") 

<div id="uevddewsnx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uevddewsnx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#uevddewsnx thead, #uevddewsnx tbody, #uevddewsnx tfoot, #uevddewsnx tr, #uevddewsnx td, #uevddewsnx th {
  border-style: none;
}

#uevddewsnx p {
  margin: 0;
  padding: 0;
}

#uevddewsnx .gt_table {
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

#uevddewsnx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uevddewsnx .gt_title {
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

#uevddewsnx .gt_subtitle {
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

#uevddewsnx .gt_heading {
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

#uevddewsnx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uevddewsnx .gt_col_headings {
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

#uevddewsnx .gt_col_heading {
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

#uevddewsnx .gt_column_spanner_outer {
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

#uevddewsnx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uevddewsnx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uevddewsnx .gt_column_spanner {
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

#uevddewsnx .gt_spanner_row {
  border-bottom-style: hidden;
}

#uevddewsnx .gt_group_heading {
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

#uevddewsnx .gt_empty_group_heading {
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

#uevddewsnx .gt_from_md > :first-child {
  margin-top: 0;
}

#uevddewsnx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uevddewsnx .gt_row {
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

#uevddewsnx .gt_stub {
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

#uevddewsnx .gt_stub_row_group {
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

#uevddewsnx .gt_row_group_first td {
  border-top-width: 2px;
}

#uevddewsnx .gt_row_group_first th {
  border-top-width: 2px;
}

#uevddewsnx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevddewsnx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uevddewsnx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uevddewsnx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uevddewsnx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevddewsnx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uevddewsnx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#uevddewsnx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uevddewsnx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uevddewsnx .gt_footnotes {
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

#uevddewsnx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevddewsnx .gt_sourcenotes {
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

#uevddewsnx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevddewsnx .gt_left {
  text-align: left;
}

#uevddewsnx .gt_center {
  text-align: center;
}

#uevddewsnx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uevddewsnx .gt_font_normal {
  font-weight: normal;
}

#uevddewsnx .gt_font_bold {
  font-weight: bold;
}

#uevddewsnx .gt_font_italic {
  font-style: italic;
}

#uevddewsnx .gt_super {
  font-size: 65%;
}

#uevddewsnx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#uevddewsnx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uevddewsnx .gt_indent_1 {
  text-indent: 5px;
}

#uevddewsnx .gt_indent_2 {
  text-indent: 10px;
}

#uevddewsnx .gt_indent_3 {
  text-indent: 15px;
}

#uevddewsnx .gt_indent_4 {
  text-indent: 20px;
}

#uevddewsnx .gt_indent_5 {
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

    survey_obj %>% 
      tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
      add_stat(fns = everything() ~ focal_smd) 

<div id="qwdmshnvdm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qwdmshnvdm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#qwdmshnvdm thead, #qwdmshnvdm tbody, #qwdmshnvdm tfoot, #qwdmshnvdm tr, #qwdmshnvdm td, #qwdmshnvdm th {
  border-style: none;
}

#qwdmshnvdm p {
  margin: 0;
  padding: 0;
}

#qwdmshnvdm .gt_table {
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

#qwdmshnvdm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#qwdmshnvdm .gt_title {
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

#qwdmshnvdm .gt_subtitle {
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

#qwdmshnvdm .gt_heading {
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

#qwdmshnvdm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qwdmshnvdm .gt_col_headings {
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

#qwdmshnvdm .gt_col_heading {
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

#qwdmshnvdm .gt_column_spanner_outer {
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

#qwdmshnvdm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qwdmshnvdm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qwdmshnvdm .gt_column_spanner {
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

#qwdmshnvdm .gt_spanner_row {
  border-bottom-style: hidden;
}

#qwdmshnvdm .gt_group_heading {
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

#qwdmshnvdm .gt_empty_group_heading {
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

#qwdmshnvdm .gt_from_md > :first-child {
  margin-top: 0;
}

#qwdmshnvdm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qwdmshnvdm .gt_row {
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

#qwdmshnvdm .gt_stub {
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

#qwdmshnvdm .gt_stub_row_group {
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

#qwdmshnvdm .gt_row_group_first td {
  border-top-width: 2px;
}

#qwdmshnvdm .gt_row_group_first th {
  border-top-width: 2px;
}

#qwdmshnvdm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qwdmshnvdm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#qwdmshnvdm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#qwdmshnvdm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qwdmshnvdm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qwdmshnvdm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qwdmshnvdm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#qwdmshnvdm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qwdmshnvdm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qwdmshnvdm .gt_footnotes {
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

#qwdmshnvdm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qwdmshnvdm .gt_sourcenotes {
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

#qwdmshnvdm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qwdmshnvdm .gt_left {
  text-align: left;
}

#qwdmshnvdm .gt_center {
  text-align: center;
}

#qwdmshnvdm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qwdmshnvdm .gt_font_normal {
  font-weight: normal;
}

#qwdmshnvdm .gt_font_bold {
  font-weight: bold;
}

#qwdmshnvdm .gt_font_italic {
  font-style: italic;
}

#qwdmshnvdm .gt_super {
  font-size: 65%;
}

#qwdmshnvdm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#qwdmshnvdm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#qwdmshnvdm .gt_indent_1 {
  text-indent: 5px;
}

#qwdmshnvdm .gt_indent_2 {
  text-indent: 10px;
}

#qwdmshnvdm .gt_indent_3 {
  text-indent: 15px;
}

#qwdmshnvdm .gt_indent_4 {
  text-indent: 20px;
}

#qwdmshnvdm .gt_indent_5 {
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

    survey_obj %>% 
      tbl_svysummary(by = grade, include = c(trt, age, stage)) %>% 
      add_stat(fns = everything() ~ focal_smd_level, location = ~ "level") 

<div id="csunzltxst" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#csunzltxst table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#csunzltxst thead, #csunzltxst tbody, #csunzltxst tfoot, #csunzltxst tr, #csunzltxst td, #csunzltxst th {
  border-style: none;
}

#csunzltxst p {
  margin: 0;
  padding: 0;
}

#csunzltxst .gt_table {
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

#csunzltxst .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#csunzltxst .gt_title {
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

#csunzltxst .gt_subtitle {
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

#csunzltxst .gt_heading {
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

#csunzltxst .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#csunzltxst .gt_col_headings {
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

#csunzltxst .gt_col_heading {
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

#csunzltxst .gt_column_spanner_outer {
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

#csunzltxst .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#csunzltxst .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#csunzltxst .gt_column_spanner {
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

#csunzltxst .gt_spanner_row {
  border-bottom-style: hidden;
}

#csunzltxst .gt_group_heading {
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

#csunzltxst .gt_empty_group_heading {
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

#csunzltxst .gt_from_md > :first-child {
  margin-top: 0;
}

#csunzltxst .gt_from_md > :last-child {
  margin-bottom: 0;
}

#csunzltxst .gt_row {
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

#csunzltxst .gt_stub {
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

#csunzltxst .gt_stub_row_group {
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

#csunzltxst .gt_row_group_first td {
  border-top-width: 2px;
}

#csunzltxst .gt_row_group_first th {
  border-top-width: 2px;
}

#csunzltxst .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#csunzltxst .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#csunzltxst .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#csunzltxst .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#csunzltxst .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#csunzltxst .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#csunzltxst .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#csunzltxst .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#csunzltxst .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#csunzltxst .gt_footnotes {
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

#csunzltxst .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#csunzltxst .gt_sourcenotes {
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

#csunzltxst .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#csunzltxst .gt_left {
  text-align: left;
}

#csunzltxst .gt_center {
  text-align: center;
}

#csunzltxst .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#csunzltxst .gt_font_normal {
  font-weight: normal;
}

#csunzltxst .gt_font_bold {
  font-weight: bold;
}

#csunzltxst .gt_font_italic {
  font-style: italic;
}

#csunzltxst .gt_super {
  font-size: 65%;
}

#csunzltxst .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#csunzltxst .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#csunzltxst .gt_indent_1 {
  text-indent: 5px;
}

#csunzltxst .gt_indent_2 {
  text-indent: 10px;
}

#csunzltxst .gt_indent_3 {
  text-indent: 15px;
}

#csunzltxst .gt_indent_4 {
  text-indent: 20px;
}

#csunzltxst .gt_indent_5 {
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
