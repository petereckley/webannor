# Manipulate `annotation` data.frames

#' Filter to annotations made by specified annotator(s)
#'
#' \code{annotation} data.frames must contain the following columns:
#' document_name
#' annotator
#' annotation_id (in case of multiple annotations per annotator per document)
#' annotation_type
#' annotation_value
#'
#' @param annotations data.frame containing annotations
#'
#' @export
filter_annotators <- function(annotations, annotators = NULL) {
  # Default to all annotators
  # WARNING! Hardwired column name "annotator"
  if (!is.null(annotators))
    df <- dplyr::filter(annotations, annotator %in% annotators)
  else
    df <- annotations
  df
}


# Extract properties of `annotation` data.frames

## documents
get_doc_ids <- function(annotations) unique(annotations$doc_id)

## annotators
get_annotators <- function(annotations) unique(annotations$annotator)

## tagset

get_tagset_used_unsorted <- function(annotations) unique(annotations$annotation_value)

#' Get list of tags used in annotations
#'
#' @param sort Order in which to store tagset
#'
#' @export
get_tagset_used <- function(annotations, sort = c("value",
                                                   "frequency",
                                                   "first_appearance")) {
  sort <- match.arg(sort)
  switch(sort,
         value = sort(get_tagset_used_unsorted(annotations)),
         first_appearance = unique(get_tagset_used_unsorted(annotations)),
         frequency = annotations %>%
           group_by(annotation_value) %>%
           summarise(n = n()) %>%
           arrange(desc(n)) %>%
           select(annotation_value) %>%
           unlist(use.names = FALSE)
         )
}

#' Combine used tagset and canonical tagset, and raise warnings if they differ
#'
#' @param sort
#'
#'   If \code{sort = "first_appearance"} then unused tags are added to the end,
#'   in the order that they appear in `\code{tagset_planned}.
#'
#' @export
get_tagset_complete <- function(annotations,
                                tagset_planned = NULL,
                                sort = c("value",
                                         "frequency",
                                         "first_appearance")) {
  sort <- match.arg(sort)
  tagset_used <- get_tagset_used(annotations, sort)
  if (!is.null(tagset_planned)) {
    pretty_print_tagset <- function(tagset) paste0(tagset, "", collapse = "\n")
    # ... tags used in the annotations that aren't also in `tagset_planned`
    tagset_unexpected <- setdiff(tagset_used, tagset_planned)
    if (length(tagset_unexpected) > 0)
      warning("Tags in `annotations` that were not in `tagset_planned`: ",
              pretty_print_tagset(tagset_unexpected))
    # ... unused tags cf. `tagset_planned`
    tagset_unused <- setdiff(tagset_planned, tagset_used)
    if (length(tagset_unused) > 0)
      warning("Tags in `tagset_planned` that were not used in `annotations`: ",
              pretty_print_tagset(tagset_unused))
    tagset_complete <- c(tagset_used, tagset_unused)
  } else {
    tagset_complete <- tagset_used
  }
  tagset_complete
}
