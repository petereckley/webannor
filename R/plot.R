# Plotting functions

#' @import ggplot2

## Marginal tag use

long_labeller <- function(x) stringr::str_wrap(x, width = 15)

#' Plot marginal frequency of tags
#'
#' Automatically faceted by annotator, with a marginal facet added optionally by
#' default.
#'
#' @param annotations data.frame containing annotations
#' @param tagset_planned Character vector. Use to include unused tags in plot
#' @param sort One of "value" or "frequency"
#' @param margins TRUE | FALSE
#' @param labeller function to format label text for the tagset axis
#'
#' @export
plot_marginal_tag_frequency <- function(annotations,
                                        tagset_planned = NULL,
                                        sort = c("value", "frequency"),
                                        margins = TRUE,
                                        labeller = long_labeller) {
  sort <- match.arg(sort)
  ggplot(annotations, aes(x = annotation_value)) +
    geom_bar(aes(y = ..prop.., group = 1)) + # group = 1 needed to get proportions within annotator, rather than as proportion of margin
    facet_grid(annotator ~ ., margins = margins) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
    scale_x_discrete(name = NULL,
                     labels = labeller,
                     limits = get_tagset_complete(annotations = annotations,
                                                  tagset_planned = tagset_planned,
                                                  sort = sort)) +
    scale_y_continuous(name = "Share of tags by annotator")
}


## Tag coincidences

#' Bubble plot of coincidences on square matrix of potential annotation values
#'
#' Low-level plotting function, typically called from a higher-level wrapper
#' function. Example usage: plotting number of co-occurrences of tags on same
#' document. Considering subsets of annotations would then give different
#' interpretations. With a single annotator, this would be co-occurrence within
#' multiple tagging. With two annotators, but one tag per document, this is a
#' measure of pairwise agreement. WIth >2 annotators, but one tag per document,
#' this can be interpreted in terms of agreement at the category level
#' (elaboration required).
#'
#' @param coincidences data.frame containing pairwise_cooccurence_counts
#' @param tagset_sorted character vector of tag names in the order to be plotted
#'
#' \code{coincidences} must contain columns \code{n}, \code{item1}, and \code{item2}.
#' These are interpreted as the coincidence count of item1 and item2.
#'
#' @export
plot_coincidences_from_coincidences <- function(coincidences, tagset_sorted) {

  # WARNING! Hardwired column names "item1" and "item2"
  # WARNING! Hardwired constant `tags_sorted`, to be provided from calling environment
  stopifnot(length(setdiff(c("n", "item1", "item2"), colnames(coincidences))) == 0)
  stopifnot(is.numeric(coincidences$n))

  ggplot(coincidences, aes(x = item1, y = item2)) +
    ### A: fix so that size and alpha scale are normalised by sum of n within
    ### plot which is subsamples when faceted, rather than over whole dataset
    #geom_point(aes(size = n / sum(n), alpha = n / sum(n))) +
    geom_count(aes(size = ..prop.., alpha = ..prop.., group = 1)) +
    #geom_count(aes(size = ..n.., alpha = ..n.., group = 1), stat = "sum") +
    scale_y_discrete(name = NULL, limits = rev(tagset_sorted), position = "right") +
    scale_x_discrete(name = NULL, limits = tagset_sorted, position = "top")  +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 0))
}

#' Wrapper to construct the coincidences data.frame and then call the plot function
#'
#' @param annotations data.frame containing annotations data
#' @param annotators list of annotators between whom to count coincidences
#'
#' @export
plot_coincidences <- function(annotations, annotators = get_annotators(annotations)) {
  coincidences  <- coincidences_pooled(annotations = annotations,
                                       annotators = annotators)
  tagset_sorted <- get_tagset_complete(annotations = annotations,
                                       tagset_planned = tagset_planned,
                                       sort = sort)
  plot_coincidences_from_coincidences(coincidences = coincidences,
                                      tagset_sorted = tagset_sorted)
}

#' Cross-plots of tags from a pair of annotators, faceted by pairs of annotators
#'
#' Pairwise coccurrences of topics on the same document, from any annotator,
#' including multiple topic tags applied by same annotator to same document. A
#' given tag will be counted (m-1) times, where m is the number of tags applied
#' by any annotator on a given document, i.e. once for each pair of tags that
#' the tag appears in. Note that to the extent that there are multiple tags from
#' the same annotator (on a given document) with the tags applied by different
#' annotators, this plot does *not* provide a completely clean measure of
#' agreement between annotators (it conflates cooccurence within annotator).
#'
#' But I don't see a straightforward how to measure agreement where there are
#' multiple tags per annotator.
#'
#' @param annotations data.frame containing annotations data
#' @param annotators list of annotators between whom to count coincidences
#' @param tagset_planned character vector of tag names offered to annotators
#' @param margins Whether to add marginal plots
#' @param cutoff Threshold number of coincidences per cell below which to leave cell blank
#' @param sort Method to order the tagset on plot
#'
#' @export
plot_coincidences_annotators_pairwise_old <-
  function(annotations,
           annotators = get_annotators(annotations),
           tagset_planned = NULL,
           margins = TRUE,
           cutoff = 0,
           sort = c("value", "frequency")) {
    # WARNING! Hardwired column name "n" from agreement_pairwise() output
    coincidences <- coincidences_annotators_pairwise(annotations, annotators)
    coincidences <- coincidences[coincidences$n > cutoff,]
    sort <- match.arg(sort)
    tagset_sorted <- get_tagset_complete(annotations = annotations,
                                         tagset_planned = tagset_planned,
                                         sort = sort)
    # WARNING! Hardwired column names "annotator1" and "annotator2"
    plot_coincidences_from_coincidences(coincidences, tagset_sorted) +
      facet_grid(annotator1 ~ annotator2,
                 margins = margins,
                 switch = "y")
  }

#' @describeIn plot_coincidences_annotators_pairwise_old Alternative version of
#'   the function while I try to work around a bug
#' @export
plot_coincidences_annotators_pairwise <-
  function(annotations,
           annotators = get_annotators(annotations),
           tagset_planned = NULL,
           margins = TRUE,
           cutoff = 0,
           sort = c("value", "frequency")) {
    # WARNING! Hardwired column name "n" from agreement_pairwise() output
    coincidences <- coincidences_annotators_pairwise(annotations, annotators)
    coincidences <- coincidences[coincidences$n > cutoff,]
    sort <- match.arg(sort)
    tagset_sorted <- get_tagset_complete(annotations = annotations,
                                         tagset_planned = tagset_planned,
                                         sort = sort)
    # WARNING! Hardwired column names "annotator1" and "annotator2"
    ggplot(coincidences, aes(x = item1, y = item2)) +
      facet_grid(annotator1 ~ annotator2,
                 margins = margins,
                 switch = "y") +
      ### A: fix so that size and alpha scale are normalised by sum of n within
      ### plot which is subsamples when faceted, rather than over whole dataset
      geom_point(aes(size = n, alpha = n, group = 1)) +
      #geom_point(aes(size = n / sum(n), alpha = n / sum(n), group = 1)) +
      #geom_point(aes(size = n / sum(n), alpha = n / sum(n)), group = 1) +
      #geom_count(aes(size = ..prop.., alpha = ..prop.., group = 1)) +
      #geom_count(aes(size = ..prop../sum(..prop..), alpha = ..prop../sum(..prop..), group = 1)) +
      #geom_count(aes(size = ..prop../sum(..prop..), alpha = ..prop../sum(..prop..)), group = 1) +
      #geom_count(aes(size = (..n..)/tapply(..n..,..PANEL..,sum)[..PANEL..],
      #                alpha = (..sum..)/tapply(..sum..,..PANEL..,sum)[..PANEL..],
      #                group = 1), stat = "sum") +
      scale_y_discrete(name = NULL, limits = rev(tagset_sorted), position = "right") +
      scale_x_discrete(name = NULL, limits = tagset_sorted, position = "top")  +
      theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 0))

  }
