#--------------------------------------------------------------------
# ## plot_reveal
#--------------------------------------------------------------------


#' Create the plot layout object
#'
#' Given the full plot, this functions shows the layout of the plot with
#' all the elements names and returns a `gtable` object to be passed to
#' `plot_reveal_steps()`
#'
#' @param plot A ggplot2 plot
#' @param layout Whether to show the layout with names of elements
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, disp)) +
#'    geom_point() +
#'    facet_grid(vs~am) +
#'    theme_light()
#'
#' # Create starting object
#' p_start <- plot_reveal_start(p)
#'
#' # Examine layout and select elements to keep
#' steps <- list(
#'                 c("axis-l-1" , "panel-1-1", "strip-t-1",
#'                   "axis-t-1",  "axis-b-1", "axis-r-1", "strip-r-1",
#'                   "xlab-b", "ylab-l"),
#'
#'                 c("axis-l-2", "panel-2-1",
#'                   "strip-r-2", "axis-r-2")
#'              )
#'
#' # Create incremental plots
#' p_steps  <- plot_reveal_steps(p_start, steps)
#'
#'\dontrun{
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
#' }
plot_reveal_start <- function(plot, layout = T){

  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  if (layout){
    lemon::gtable_show_names(gt)
  }

  gt
}


#' Create plot steps
#'
#' Given the initial object created by `plot_reveal_start()`, creates the incremental
#' plots and returns a list of plot that can be exported.
#'
#' @param x The object created by `plot_reveal_start()`
#' @param steps A character vector, or list of character vectors, with the names
#'  of the plot elements (shown by `plot_reveal_start()`) that will be shown in
#'  each step. The steps are incremental, i.e., all the elements from the first
#'  step will be shown in the second, and so on.
#' @param layout Whether to show the layout (with names of elements) for each step
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, disp)) +
#'    geom_point() +
#'    facet_grid(vs~am) +
#'    theme_light()
#'
#' # Create starting object
#' p_start <- plot_reveal_start(p)
#'
#' # Examine layout and select elements to keep
#' steps <- list(
#'                 c("axis-l-1" , "panel-1-1", "strip-t-1",
#'                   "axis-t-1",  "axis-b-1", "axis-r-1", "strip-r-1",
#'                   "xlab-b", "ylab-l"),
#'
#'                 c("axis-l-2", "panel-2-1",
#'                   "strip-r-2", "axis-r-2")
#'              )
#'
#' # Create incremental plots
#' p_steps  <- plot_reveal_steps(p_start, steps)
#'
#'\dontrun{
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
#' }
plot_reveal_steps <- function(x, steps = NULL, layout = FALSE){

  previous_step <- character(0)
  plot_list <- list()


  for (step in steps) {

    drop <- !(x$layout$name %in% c(previous_step, step))
    gt_step <- x
    gt_step$grobs[drop] <- NULL
    gt_step$layout <- gt_step$layout[!drop, ]


    # save current selection of elements to use in the next
    # step
    previous_step <- c(previous_step, step)


    # Add to plot list
    p_step <- gridExtra::grid.arrange(gt_step)

    plot_list[[length(plot_list)+1]] <- p_step


    if (layout) {
      # Preview layout
      lemon::gtable_show_names(gt_step)

    } else {
      # Preview graph
      #grid::grid.newpage()
      #grid::grid.draw(gt_step)
    }

  }

  p_full <- gridExtra::grid.arrange(x)
  plot_list[[length(plot_list)+1]] <- p_full

  return(plot_list)


}


#' Saves plot steps
#'
#'
#' @param plot_list The list of plots returned by `plot_reveal_steps()`
#' @param basename The base filename for exported plots
#' @param ... Additional arguments (e.g. width, height) tobe passed to `ggsave`
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, disp)) +
#'    geom_point() +
#'    facet_grid(vs~am) +
#'    theme_light()
#'
#' # Create starting object
#' p_start <- plot_reveal_start(p)
#'
#' # Examine layout and select elements to keep
#' steps <- list(
#'                 c("axis-l-1" , "panel-1-1", "strip-t-1",
#'                   "axis-t-1",  "axis-b-1", "axis-r-1", "strip-r-1",
#'                   "xlab-b", "ylab-l"),
#'
#'                 c("axis-l-2", "panel-2-1",
#'                   "strip-r-2", "axis-r-2")
#'              )
#'
#' # Create incremental plots
#' p_steps  <- plot_reveal_steps(p_start, steps)
#'
#'\dontrun{
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
#' }
plot_reveal_save <- function(plot_list, basename = "plot", ...) {

  cli::cli_h2("Saving incremental plots")

  for (i in 1:length(plot_list)) {
    suffix <- ifelse(i == length(plot_list),
                      paste0("_", i, "_full"),
                      paste0("_", i))
    filename <- paste0(basename, suffix, ".png")

    ggplot2::ggsave(filename, plot_list[[i]], ...)
    cli::cli_alert_success("{filename}")
  }

}


# Util to select panels, strips and axes in in proper order
#' @noRd
select_sort_elements <- function(layout_obj, element="panel", type_facet = "wrap") {

  panel_df <- layout_obj |>
    dplyr::filter(stringr::str_detect(name, "panel")) |>
    dplyr::rename_all(~paste0("panel_", .))

  if (element=="panel"){

    # Sort panels by row,  using t and l coordinates
    # (To sort by col, just do by l then t)
    out <- panel_df |>
      dplyr::arrange(panel_t, panel_l) |>
      dplyr::pull(panel_name)


  } else if (element %in% c("axis", "strip")) {


    element_df <- layout_obj |>
      dplyr::filter(stringr::str_detect(name, element)) |>
      dplyr::rename_all(~paste0("element_", .))


    n_panels <- NROW(panel_df)


    element_df_list <- list()
    for (i in 1:n_panels) {
      element_df_list <- append(element_df_list, list(element_df))
    }


    panel_element_df <- panel_df |>
      dplyr::mutate(elements = element_df_list) |>
      tidyr::unnest(cols = elements)


    if (type_facet=="grid"){
      # If plot uses facet_grid, get all axes/strips
      # which have the t OR l coordinates for each panel
      v <- panel_element_df |>
        dplyr::mutate(element_letter = stringr::str_extract(element_name, "\\w(?=-\\d)")) |>
        dplyr::mutate(dist_t = abs(panel_t-element_t),
                      dist_l = abs(panel_l-element_l),
                      dist_sum = dist_t+dist_l) |>
        dplyr::group_by(panel_name, element_letter) |>
        dplyr::arrange(panel_t, panel_l, element_letter, dist_sum) |>
        dplyr::filter(dplyr::row_number()==1) |>
        dplyr::pull(element_name)


    } else if (type_facet=="wrap"){
      # If plot uses facet_wrap, get a number of all axes/strips
      # which are closer to the panel considering the t and l coordinates

      # How many to get per panel?
      n_elements <- NROW(element_df)/n_panels

      v <- panel_element_df |>
        dplyr::mutate(dist_t = abs(panel_t-element_t),
                      dist_l = abs(panel_l-element_l),
                      dist_sum = dist_t+dist_l) |>
        dplyr::group_by(panel_name) |>
        dplyr::arrange(panel_t, panel_l,dist_sum) |>
        dplyr::filter(dplyr::row_number() <= n_elements) |>
        dplyr::pull(element_name)

    }

    out <- split(v, ceiling(seq_along(v) / (length(v)/n_panels)))


  } else {
    stop("element should be one of 'panel', 'axis' or 'strip'")
  }


  return(out)

}


#' Reveal plots by facet
#'
#' Receives plot, creates steps by facet and returns objects ready for saving.
#' Order of facet is rowise.
#'
#' @param plot A ggplot2 plot
#' @param strip If TRUE (default), the facet labels will also be revealed incrementally.
#' Otherwise, labels are displayed in the first image.
#' @param axis If TRUE (default), the axes will also be revealed incrementally.
#' Otherwise, labels are displayed in the first image.
#' @param add Optional: named vector to add specific elements
# to a given step. E.g. `c("2"="axis-b-2-2")` would add the element
# "axis-b-2-2" to the second step. Useful for shared axes that need
# to appear earlier than what is computed for each facet.
#' @param layout Whether to show the layout (with names of elements) for each step
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, disp)) +
#'    geom_point() +
#'    facet_grid(vs~am) +
#'    theme_light()
#'
#' # Create starting object
#' p_steps <- plot_reveal_by_facet(p)
#'
#'\dontrun{
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
#' }
plot_reveal_by_facet <- function(plot, strip = T, axis = T, add, layout = T){

  p_start <- plot_reveal_start(plot, layout = layout)
  layout_obj <- p_start$layout
  type_facet <- stringr::str_extract(tolower(class(plot$facet)[1]), "grid|wrap")

  panels <- select_sort_elements(layout_obj, "panel")
  strips <- select_sort_elements(layout_obj, "strip", type_facet)
  axes <- select_sort_elements(layout_obj, "axis", type_facet)
  rest <- layout_obj$name[!(stringr::str_detect(layout_obj$name, "panel|strip|axis"))]

  # Handle facet titles (strips)
  if (strip) {
    strip_list <- strips
  } else {
    strip_list <- as.list(rep("", length(panels)))
    strip_list[[1]] <- unlist(strips)
  }


  # Handle axes
  if (axis) {
    axes_list <- axes
  } else {
    axes_list <- as.list(rep("", length(panels)))
    axes_list[[1]] <- unlist(axes)
  }


  # First first panel + corresponding axes/strips + rest
  steps <- list(c(panels[1], strip_list[[1]], axes_list[[1]], rest))

  # Now, add each remaining panel,
  # except for last, because this is accounted
  # for the the full graph
  # Not needed if there are only two panels
  if (length(panels) > 2) {
    for(i in 2:(length(panels)-1)){
      steps <- append(steps, list(c(panels[i], strip_list[[i]], axes_list[[i]])))
    }
  }

  # add custom elements
  if (!missing(add)){
    for (i in seq_along(add)) {
      x <- steps[[as.numeric(names(add)[i])]]
      steps[[as.numeric(names(add)[i])]] <- c(x,
                                              add[i])
    }
  }

  # Create incremental plots
  p_steps  <- plot_reveal_steps(p_start, steps)

  # Returns steps ready to be exported
  return(p_steps)

}







