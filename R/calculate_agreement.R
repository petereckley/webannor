# TO DO:
# - decide how much hardwiring column names matters
# - function to check that df is a valid annotations df (consider making a class?)
# - consider making less specific to annotation, e.g. generalise to tags

### Q: Should I treat the one-graph case as a special case, rather than
### different function (but ggplot facet names would probably still appear?)

# Agreement

#' Calculate agreement among annotators
#'
#' Take a list of annotations, by document, annotator, and annotation value (tag), and calculate agreement among a specified subset of annotators.
### Q: Should we complete missing pairs with zeros? Or provide an option to do so, with corresponding function argument?
### A: consider removing the annotators argument -- separation of concerns?
#' @export
agreement <- function(annotations, annotators = NULL) {
  df <- filter_annotators(annotations, annotators)
  # WARNING! Hardwired column name "annotation_value", "doc_id"
  widyr::pairwise_count(df, annotation_value, doc_id, diag = TRUE)
}


### A: check that item1 corresponds to annotator1 and likewise for 2
agreement_pair <- function(annotations, annotators) {
  stopifnot(is.character(annotators) && length(annotators) == 2)
  df <- agreement(annotations, annotators)
  df$annotator1 = annotators[1]
  df$annotator2 = annotators[2]
  df
}

# Row-bind together all pairwise combinations of annotators, plus optionally margins
agreement_pairwise <- function(annotations, annotators = NULL, diag = TRUE) {
  # If an annotator is listed in the annotators argument, but doesn't appear in the annotations dataset, that annotator will be imputed to exist and implicitly have
  df <- filter_annotators(annotations, annotators)
  # If annotators argument is left NULL then infer annotators list from annotations dataframe (so won't see annotators who exist but didn't contribute any annotations)
  if (is.null(annotators)) annotators <- get_annotators(annotations)
  annotator_pairs <-
    if (diag) {
      temp <- gtools::permutations(length(annotators), 2, annotators, repeats.allowed = TRUE)
      split(temp, row(temp))} else combn(annotators, 2, simplify = FALSE)
  purrr::map_df(annotator_pairs, ~agreement_pair(annotations, .x))
}
