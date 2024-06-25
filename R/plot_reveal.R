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
#'\dontrun{
#' # Save plots
#' plot_reveal_save(p_steps, "myplot")
#' }
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


# Util function to properly
# sort strips and axes
#' @noRd
util_sort_elements <- function(x, n_panels) {

  letters <- unique(
    unlist(
      stringr::str_extract_all(x, "[t|b|l|r](?=-\\d)")
    )
  )

  elements <- list()

  for (i in letters) {

    el <- x[stringr::str_detect(x,
                                paste0("-", i, "-"))]

    if (i %in% c("t", "b")){
      # t and b axes/strips are sorted by
      # second number, than first number
      el_order <-
        order(
          as.numeric(
            paste0(
              # second number
              stringr::str_extract(el, "\\d$"),
              # first number
              stringr::str_extract(el, "(?<=\\w-)\\d")
            )
          )
        )
    } else {

      el_order <-
        order(
          as.numeric(
            paste0(
              # first number
              stringr::str_extract(el, "(?<=\\w-)\\d"),
              # second number
              stringr::str_extract(el, "\\d$")

            )
          )
        )

    }

    elements[[i]] <- el[el_order]
  }

  out <- list()
  for (i in 1:n_panels) {
    out[[i]] <- sapply(elements, `[[`, i)
  }

  return(out)
}


#' Reveal plots by facet
#'
#' Receives plot, creates steps by facet and returns objects ready for saving.
#' Order of facet is rowise.
#'
#' @param plot A ggplot2 plot
#' @param labels If TRUE (default), the facet labels will also be revealed incrementally.
#' Otherwise, labels are displayed in the first image.
#' @param axis If TRUE (default), the axes will also be revealed incrementally.
#' Otherwise, labels are displayed in the first image.
#' @param add Optional: named vector to add specific elements
# to a given step. E.g. `c("2"="axis-b-2-2")` would add the element
# "axis-b-2-2" to the second step. Useful for shared axes that need
# to appear earlier than what is computed for each facet.
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
plot_reveal_by_facet <- function(plot, labels = T, axis = T, add){

  p_start <- plot_reveal_start(plot)

  ordered_names <- p_start$layout$name[order(p_start$layout$name)]

  panels <- ordered_names[grepl("panel", ordered_names)]
  strips <- ordered_names[grepl("strip", ordered_names)]
  axes <- ordered_names[grepl("axis", ordered_names)]
  rest <- ordered_names[!(ordered_names %in% c(panels, strips, axes))]

  # Handle facet titles (strips)
  if (labels) {
    # If there is more than one number, it's
    # facet_wrap, otherwise it's facet_grid
    if (stringr::str_count(strips[1], "\\d")>1){
      # facet_wrap
      strip_list <- util_sort_elements(strips , length(panels))

    } else {
      # facet_grid
      strip_letters <- unique(
        unlist(
          stringr::str_extract_all(strips,
                          "(?<=strip-)\\w{1}")
        )
      ) |>
        sort()

      # now, for each panel, get corresponding strips
      # some will be repeated, but that's fine
      strip_list <- list()
      for (i in 1:length(panels)){

        strips_panel_i <- vector("character")

        for (j in 1:length(strip_letters)){

          strip_ij <- paste0("strip-",
                             strip_letters[j],
                             "-",
                             unlist(stringr::str_extract_all(panels[i], "\\d"))[j])


          strips_panel_i <- c(strips_panel_i, strip_ij)
        }

        strip_list <- append(strip_list, list(strips_panel_i))
      }
    }
  } else {
    strip_list <- as.list(rep("", length(panels)))
    strip_list[[1]] <- strips
  }


  # Handle axes
  if (axis) {

    # If there is more than one number, it's
    # facet_wrap, otherwise it's facet_grid
    if (stringr::str_count(axes[1], "\\d")>1){
      # facet_wrap
      axes_list <- util_sort_elements(axes, length(panels))

    } else {
      # facet_grid
      axes_letters <- unique(
        unlist(
          stringr::str_extract_all(axes,
                                   "(?<=axis-)\\w{1}")
        )
      ) |>
        sort()

      # now, for each panel, get corresponding axes
      # some will be repeated, but that's fine
      axes_list <- list()
      for (i in 1:length(panels)){

        axes_panel_i <- vector("character")

        for (j in 1:length(axes_letters)){

          # "l" and "r"are for rows in the grid
          # index for row in panel name is 1, for col is 2
          k <- ifelse(axes_letters[j] %in% c("l", "r"),
                      1,
                      2)

          axis_ij <- paste0("axis-",
                            axes_letters[j],
                            "-",
                            unlist(stringr::str_extract_all(panels[i], "\\d"))[k])


          axes_panel_i <- c(axes_panel_i, axis_ij)
        }

        axes_list <- append(axes_list, list(axes_panel_i))
      }
    }
  } else {
    axes_list <- as.list(rep("", length(panels)))
    axes_list[[1]] <- axes
  }



  # First first panel + all axes, titles, etc
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
  p_steps  <- plot_reveal_steps(p_start, steps, T)

  # Returns steps ready to be exported
  return(p_steps)

}








