# Agreement

#' Calculate cooccurence of tags pooled across annotators
#'
#' Take a list of annotations, by document, annotator, and annotation value
#' (tag), and calculate agreement among a specified set of annotators.
#'
#' @param annotations data.frame containing annotation data
#' @param annotators character vector of annotator names
#'
#' @family coincidences
#'
#' @export
coincidences_pooled <- function(annotations,
                                annotators = get_annotators(annotations)) {

  ### Q: Should we complete missing pairs with zeros? Or provide an option to do
  ### so, with corresponding function argument? Or will our ggplot functions
  ### impute these anyway?

  ### A: consider removing the annotators argument -- separation of concerns?

  annotations_filtered <- filter_annotators(annotations, annotators)
  # WARNING! Hardwired column name "annotation_value", "doc_id"
  widyr::pairwise_count(annotations_filtered, annotation_value, doc_id, diag = TRUE)
}


### A: check that item1 corresponds to annotator1 and likewise for 2

#' Calculate annotation coincides for a pair of annotators
#'
#' @family coincidences
#' @export
coincidences_annotator_pair <- function(annotations, annotators) {
  stopifnot(is.character(annotators) && length(annotators) == 2)
  coincidences <- coincidences_pooled(annotations, annotators)
  coincidences$annotator1 = annotators[1]
  coincidences$annotator2 = annotators[2]
  coincidences
}

#' Row-bind together all pairwise combinations of annotators, plus optionally
#' margins
#'
#' Coincidences can also be counted within subsets of annotators. One special
#' case is within the same annotator, i.e. where annotators apply multiple tags,
#' which do they tend to apply together.
#'
#' If an annotator is listed in the annotators argument, but doesn't appear in
#' the annotations dataset, that annotator, that annotator will be imputed to
#' exist and implicitly have NA tags. Note this may differ from imputation
#' policies for a given annotation project. [Q: Is that the default behaviour we
#' want?]
#'
#' @param annotations data.frame containing annotation data
#' @param annotators character vector of annotator names
#' @param diag Include the diagonal facets showing agreement within annotator
#' @param impute_annotators Impute NA tags for annotators who applied no tags
#'
#' @family coincidences
#' @export
coincidences_annotators_pairwise <- function(annotations,
                                            annotators = get_annotators(annotations),
                                            diag = TRUE,
                                            impute_annotators = FALSE) {
  if (!impute_annotators)
    annotators <- intersect(annotators, get_annotators(annotations))
  annotations_filtered <- filter_annotators(annotations, annotators)
  annotator_pairs <-
    if (diag) {
      temp <- gtools::permutations(length(annotators), 2, annotators, repeats.allowed = TRUE)
      split(temp, row(temp))} else utils::combn(annotators, 2, simplify = FALSE)
  purrr::map_df(annotator_pairs, ~coincidences_annotator_pair(annotations_filtered, .x))
}
