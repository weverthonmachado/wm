# Regression table using gtsummary + custom theme + some formatting
# TODO: deal with multiple models in automatic way. I.e. internally running
# tbl_merge. This must be done BEFORE converting to flex_table

# Custom theme
custom_theme <- list("tbl_regression-chr:tidy_columns"=c("estimate"))


# construct table
reg_tbl <- function(m){

  set_gtsummary_theme(custom_theme)

  out <- tbl_regression(m,
                        conf.int = FALSE) %>%
            add_significance_stars(
                hide_se = TRUE,
                pattern = "{estimate}{stars}  \n({std.error})"
              ) %>%
            add_glance_table(include = c(nobs)) %>%
            modify_footnote(estimate ~ NA_character_) %>%
            as_gt() %>%
            gt::tab_source_note("*p<0.05; **p<0.01; ***p<0.001") %>%
            gt::fmt_markdown(columns = everything())



  reset_gtsummary_theme()

  return(out)
}



# using flex_tablke instead of gt for cusomization
# # BETTER FOR exporting to WORD!
reg_tbl2 <- function(m){

  set_gtsummary_theme(custom_theme)

  out <- tbl_regression(m,
                        conf.int = FALSE) %>%
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}{stars}  \n({std.error})"
    ) %>%
    add_glance_table(include = c(nobs)) %>%
    modify_footnote(estimate ~ NA_character_) %>%
    as_flex_table() %>%
    add_footer_lines(values = "*p<0.05; **p<0.01; ***p<0.001",
                     top = TRUE)


  reset_gtsummary_theme()

  return(out)
}




# using flex_tablke instead of gt for cusomization
# # BETTER FOR exporting to WORD!
reg_tbl3 <- function(...){

  set_gtsummary_theme(custom_theme)

  model_list <- list(...)

  table_list <- list()

  for (m in model_list) {
    table <- tbl_regression(m,
                          conf.int = FALSE) %>%
              add_significance_stars(
                hide_se = TRUE,
                pattern = "{estimate}{stars}  \n({std.error})"
              ) %>%
              modify_footnote(estimate ~ NA_character_) %>%
              add_glance_table(include = c(nobs))

    table_list <- append(table_list, list(table))
  }

  if (length(table_list) > 1) {
    out <- tbl_merge(table_list)
  } else {
    out <- table_list[[1]]
  }

  out <- out %>%
            modify_table_body(
              ~.x %>%
                mutate(XXorderXX = row_type == "glance_statistic") %>%
                arrange(XXorderXX) %>%  select(-XXorderXX)
            ) %>%
          as_flex_table() %>%
          add_footer_lines(values = "*p<0.05; **p<0.01; ***p<0.001",
                           top = TRUE)

  reset_gtsummary_theme()
  return(out)
}




# Tests
# m1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data=iris)
# m2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Width*Species, data=iris)
# m3 <- glm(Sepal.Length ~ Sepal.Width + Petal.Width*Species,
#          data= filter(iris,Species!="versicolor"))
#
#
# hux
# reg_tbl2(m2)
#
# reg_tbl3(m1)
# reg_tbl3(m1, m2)
# reg_tbl3(m1, m2, m3)
