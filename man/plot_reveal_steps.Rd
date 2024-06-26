% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_reveal.R
\name{plot_reveal_steps}
\alias{plot_reveal_steps}
\title{Create plot steps}
\usage{
plot_reveal_steps(x, steps = NULL, layout = FALSE)
}
\arguments{
\item{x}{The object created by `plot_reveal_start()`}

\item{steps}{A character vector, or list of character vectors, with the names
of the plot elements (shown by `plot_reveal_start()`) that will be shown in
each step. The steps are incremental, i.e., all the elements from the first
step will be shown in the second, and so on.}

\item{layout}{Whether to show the layout (with names of elements) for each step}
}
\description{
Given the initial object created by `plot_reveal_start()`, creates the incremental
plots and returns a list of plot that can be exported.
}
\examples{
# Create full plot
library(ggplot2)
data("mtcars")

p <- mtcars |>
  ggplot(aes(mpg, disp)) +
   geom_point() +
   facet_grid(vs~am) +
   theme_light()

# Create starting object
p_start <- plot_reveal_start(p)

# Examine layout and select elements to keep
steps <- list(
                c("axis-l-1" , "panel-1-1", "strip-t-1",
                  "axis-t-1",  "axis-b-1", "axis-r-1", "strip-r-1",
                  "xlab-b", "ylab-l"),

                c("axis-l-2", "panel-2-1",
                  "strip-r-2", "axis-r-2")
             )

# Create incremental plots
p_steps  <- plot_reveal_steps(p_start, steps)

\dontrun{
# Save plots
plot_reveal_save(p_steps, "myplot")
}
}
