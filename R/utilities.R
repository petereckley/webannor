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
#' @param annotators character vector of annotator names
#'
#' @export
filter_annotators <- function(annotations, annotators = get_annotators(annotations)) {
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

#' @family get_annotation_attributes
#' @describeIn get_annotators Get types of annotation
#' @export
get_doc_ids_from_project_path <-
  function(path) {
    # WARNING! Hardwired to pattern of filenames input to WebAnno, which is not
    # managed by, or visible to, this package.
    # Here we assume that
    # 1) the file basename is also the intended doc_id
    # 2) any file extension is not part of the doc_id
    doc_paths <- list.files(file.path(path, "annotation"))
    # 2017-04-26 no longer removing file extension
    #purrr::map_chr(doc_paths, tools::file_path_sans_ext)
  }

#' @family get_annotation_attributes
#' @param tagged_only Logical to indicate whether to impute docs not in
#'   \code{annotations} because no tags applied
#' @describeIn get_annotators Get types of annotation
#' @export
get_doc_ids <- function(annotations, tagged_only = TRUE) {
  doc_ids_tagged <- unique(annotations$doc_id)
  if (tagged_only) {
    doc_ids <- doc_ids_tagged
  } else {
    project_path <- attr(annotations, "project_path", exact = TRUE)
    doc_ids <- get_doc_ids_from_project_path(project_path)
    doc_ids_untagged <- setdiff(doc_ids, doc_ids_tagged)
    message("WebAnno project folder contains documents not in `annotations`
            because entirely untagged.\n",
            "Project path: ", project_path, "\n",
            "Untagged documents: \n",
            paste0(doc_ids_untagged, collapse = "\n"))
    doc_ids_unexpected <- setdiff(doc_ids_tagged, doc_ids)
    if (length(doc_ids_unexpected) > 0) {
      stop("doc_id's found in `annotations` but not in `project_path`",
           "Project path: ", project_path, "\n",
           "Unexpected documents: \n",
           paste0(doc_ids_unexpected, collapse = "\n"))
    }
  }
}


## annotators
#' Getters for annotation attributes stored in \code{annotations}
#'
#' @param annotations data.frame of annotations data
#' @param path Path to the WebAnno project directory
#'
#' @family get_annotation_attributes
#'
#' @export
get_annotators <- function(annotations) unique(annotations$annotator)

## annotation types
#' @family get_annotation_attributes
#' @describeIn get_annotators Get types of annotation
#' @export
get_annotation_types <- function(annotations) unique(annotations$annotation_type)

## tagset
#' @family get_annotation_attributes
#' @describeIn get_annotators Get a character vector of the tags used in \code{annotations}
#' @export
get_tagset_used_unsorted <- function(annotations) unique(annotations$annotation_value)

#' Get list of tags used in annotations
#'
#' @param sort Order in which to store tagset
#'
#' @family get_annotation_attributes
#'
#' @describeIn get_annotators Get a character vector of the tags used in \code{annotations}
#' @export
get_tagset_used <- function(annotations, sort = c("value",
                                                  "frequency",
                                                  "first_appearance")) {
  sort <- match.arg(sort)
  switch(sort,
         value = sort(get_tagset_used_unsorted(annotations)),
         first_appearance = unique(get_tagset_used_unsorted(annotations)),
         frequency = annotations %>%
           dplyr::group_by(annotation_value) %>%
           dplyr::summarise(n = n()) %>%
           dplyr::arrange(desc(n)) %>%
           dplyr::select(annotation_value) %>%
           unlist(use.names = FALSE)
  )
}

#' Combine used tagset and canonical tagset, and raise warnings if they differ
#'
#' @param annotations data.frame containing annotation data
#' @param tagset_planned character vector of tags available to annotators
#' @param sort type of sorting on the tagset
#'
#'   If \code{sort = "first_appearance"} then unused tags are added to the end,
#'   in the order that they appear in `\code{tagset_planned}.
#'
#' @family get_annotation_attributes
#'
#' @describeIn get_annotators Get a character vector of the tags used in \code{annotations}
#' @export
get_tagset_complete <- function(annotations,
                                tagset_planned = NULL,
                                sort = c("value",
                                         "frequency",
                                         "first_appearance")) {
  sort <- match.arg(sort)
  tagset_used <- get_tagset_used(annotations = annotations,
                                 sort =  sort)
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
  if (sort == "value") tagset_complete <- sort(tagset_complete)
  tagset_complete
}
