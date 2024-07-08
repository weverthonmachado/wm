#' Preview plot instead of saving. 
#' 
#' Useful for checking scale of graphs
#' @param ... arguments to be passed to [ggplot2::ggsave()]]
#' @export
ggpreview <- function(..) {
  path <- ggplot2::ggsave(filename = "preview_plot.png", path = tempdir(), ...)
  invisible(NULL)
  cli::cli_alert_success("{.file {path}}")
}