% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_th_title.R
\name{geom_th_title}
\alias{geom_th_title}
\title{The row or column title}
\usage{
geom_th_title(
  mapping = NULL,
  name = NULL,
  side = "left",
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{name}{the name of the heatmap to add row or column labels.}

\item{side}{a character value selected from \strong{left}, \strong{right},
\strong{top} or \strong{bottom}. \strong{left} and \strong{right} to
add the row title; \strong{top} or \strong{bottom} to add the column
title.}

\item{nudge_x}{a value to shift the title horizontally.}

\item{nudge_y}{a value to shift the title vertically.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\value{
geom layer
}
\description{
add the row or column title
}
\author{
Ruizhu Huang
}
