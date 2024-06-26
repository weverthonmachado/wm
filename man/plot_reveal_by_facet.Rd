% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_reveal.R
\name{plot_reveal_by_facet}
\alias{plot_reveal_by_facet}
\title{Reveal plots by facet}
\usage{
plot_reveal_by_facet(plot, strip = T, axis = T, add, layout = T)
}
\arguments{
\item{plot}{A ggplot2 plot}

\item{strip}{If TRUE (default), the facet labels will also be revealed incrementally.
Otherwise, labels are displayed in the first image.}

\item{axis}{If TRUE (default), the axes will also be revealed incrementally.
Otherwise, labels are displayed in the first image.}

\item{add}{Optional: named vector to add specific elements}

\item{layout}{Whether to show the layout (with names of elements) for each step}
}
\description{
Receives plot, creates steps by facet and returns objects ready for saving.
Order of facet is rowise.
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
p_steps <- plot_reveal_by_facet(p)

\dontrun{
# Save plots
plot_reveal_save(p_steps, "myplot")
}
}
