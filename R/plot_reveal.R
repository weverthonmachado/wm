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
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
plot_reveal_start <- function(plot){

  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  lemon::gtable_show_names(gt)

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
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
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
    previous_step <- step


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
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
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











