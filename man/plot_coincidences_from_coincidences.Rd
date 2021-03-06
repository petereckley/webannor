% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_coincidences_from_coincidences}
\alias{plot_coincidences_from_coincidences}
\title{Bubble plot of coincidences on square matrix of potential annotation values}
\usage{
plot_coincidences_from_coincidences(coincidences, tagset_sorted)
}
\arguments{
\item{coincidences}{data.frame containing pairwise_cooccurence_counts}

\item{tagset_sorted}{character vector of tag names in the order to be plotted

\code{coincidences} must contain columns \code{n}, \code{item1}, and \code{item2}.
These are interpreted as the coincidence count of item1 and item2.}
}
\description{
Low-level plotting function, typically called from a higher-level wrapper
function. Example usage: plotting number of co-occurrences of tags on same
document. Considering subsets of annotations would then give different
interpretations. With a single annotator, this would be co-occurrence within
multiple tagging. With two annotators, but one tag per document, this is a
measure of pairwise agreement. WIth >2 annotators, but one tag per document,
this can be interpreted in terms of agreement at the category level
(elaboration required).
}
