## Import annotations from WebAnno UIMA CAS XMI files

## TO DO
# To reliably identify the words and phrases associated with the annotated
# character spans, we would ideally also import the `sofa` from the XMI's
# cas:sofa element. This is in case the string is stored or parsed in any way
# differently than the input text string, such that character offsets result. If
# there is no risk of such character offsets, then we could instead use the span
# being and end attributes on the original text string as stored in R before
# exporting to WebAnno.

## Constants:
# used to decouple the naming in WebAnno and the CAS XMI files exported from it

# element type that contains custom annotations
.element_type <- "custom:Credibility"
# attribute names:
# (hardwired in our WebAnno setup), from the naming in our output tdl_df's
# (hardwired in the code below)
.annotation_type_attribute_names <- list(sentiment = "Score",
                                         topic = "Context",
                                         nature = "hypothetical")


##  Functions

#' Import custom annotations from files exported by WebAnno
#'
#' Import custom annotation layers as exported by annotation software
#' \href{https://webanno.github.io/webanno/releases/3.0.0/docs/user-guide.html#_export}{WebAnno
#' v3.0.0}. in UIMA CAS XMI format.
#'
#' The different functions provide different entry points in the directory
#' structure exported by WebAnno, from the innermost XMI file, to the outermost
#' project folder.
#'
#' Note the sentence_id, which was encoded as the name of the .txt file input to
#' WebAnno, is not recorded in any of these attributes. I think the `sofa`
#' (Subject of Analysis) number is just the order of the file within the WebAnno
#' corpus, which is generally a subset of the documents in our overall corpus,
#' since we don't manually annotate them all.
#'
#' These functions will not work with other WebAnno export formats. They
#' \emph{may} work with other versions of WebAnno, but this has not been tested.
#'
#' @section Implementation notes:
#'
#'   The core functionalilty is contained in
#'   \code{get_annotations_by_type.WebAnno_XMI}. We extract the data is a
#'   slightly hacky way, using XPath expressions based on hardwired knowledge of
#'   the layers to be extracted and their representation in XMI. The XPath
#'   expressions may need to be tweaked for different WebAnno annotation types
#'   (layers). (A more robust approach would be to use the UIMA framework, but
#'   would be considerably more complicated. Also, it is implemented in Java,
#'   and R bindings were not available at the time of writing.)
#'
#'   Our layer names may differ from the naming in WebAnno. To abstract the code
#'   from the names used in WebAnno, the attribute names used for the layers are
#'   stored in .annotation_type_attribute_names (not exported) in the parent
#'   environment of the functions, rather than hardwired in the code.
#'
#'   The other functions form nested wrappers around this.
#'   \code{get_annotations.WebAnno_XMI} calls
#'   \code{get_annotations_by_type.WebAnno_XMI} iteratively over the default
#'   types hardwired in \code{} in the parent environment of that function (not
#'   exported). \code{get_annotations.WebAnno_document} and
#'   \code{get_annotations.WebAnno_project} include \code{doc_id}. This
#'   is taken from the filename of the unannotated document input to WebAnno,
#'   which is preserved in the folder name at an intermediate level in the
#'   project directory structure. \code{doc_id} can therefore be used to
#'   keep track of external identifiers for the texts fed into WebAnno, which
#'   are not otherwise known to WebAnno.
#'
#'   TO DO: \itemize{ \item Consider if my function naming convention is the
#'   most appropriate, given that its not really an S3 generic. I could create
#'   classes for the different WebAnno file types, but provide a minimal
#'   implementation (essentially just passing through file / path names, without
#'   checking if the file / paths are well-formed, though could check existence)
#'   \item Add examples that aren't subject to copyright. \item Write unit
#'   tests. \item Add error handling, e.g. for bad file | zipfile | path
#'   arguments. \item Add warning if no the result contains no annotations.
#'   \item Consider removing the suppressWarnings(), or dealing only with
#'   particular types of warnings at that stage, rather than a catch-all. \item
#'   Separate function logic to process the XML, from providing an XML source,
#'   which need not be a file (e.g. a network connection). \item Consider
#'   whether should use xml2 rather than XML package? \item Disentangle our
#'   hardwired names for annotation types, from
#'   \code{get_annotations_by_type.WebAnno_XMI}, so that function can be called
#'   with the name as appears in the XMI, and a wrapper takes care of converting
#'   that name to our preferred default type name. This will make the function
#'   useable for arbitrary layers without having to edit
#'   \code{.annotation_type_attribute_names}. \item Make the element type a
#'   parameter, instead of hardwired to "custom:Credibility" to facilitate
#'   extraction of other types of annotations. }
#' @param file File containing the annotations
#' @param zipfile Zipfile containing the annotations
#' @param path Path containing the annotations
#' @param type String containing the name of the XMI attribute that holds the
#'   annotation.
#'
#' @examples
#' \dontrun{sentiments <-
#' get_annotations_by_type.WebAnno_XMI("temp/webanno/out/admin.xmi", "sentiment")}
#' \dontrun{topics
#' <- get_annotations_by_type.WebAnno_XMI("temp/webanno/out/admin.xmi", "topic")}
#'
#' @return A dataframe containing all custom annotations visible from the
#'   specified entry point, or an empty dataframe (no rows or columns) if no
#'   custom annotations are visible.
#'
#' @family get_annotations
#'
#' @describeIn get_annotations_by_type.WebAnno_XMI Start from the XMI file for a
#'   single annotator, and get all custom annotations of the specified type.
#' @export
get_annotations_by_type.WebAnno_XMI <- function(file, type) {

  x <- XML::xmlParse(file)

  # returns a list of S4 XMLAttributes objects
  XPath_expr  <- paste0("//", .element_type, "[@", type, "]")
  annotations <- XML::xpathApply(x, XPath_expr, XML::xmlAttrs)

  # extract into a tibble -- note overriding dispatch of as.data.frame, since
  # the method for this S4 object puts the attribute names as rownames rather
  # than colnames
  # NOTE: as.data.frame.list doesn't produce desired tdl_df layout when invoked
  # directly (`as.data.frame.list(annotations)`) so not sure why it works under
  # map_df but it does
  annotations <- purrr::map_df(annotations, as.data.frame.list)

  # If no anotations of the specified type, then the result is an empty tdl_df
  # in which case we must skip some of the following logic to avoid an error
  if (!nrow(annotations) == 0) {
    annotations[, "annotation_type"] <- type
    colnames(annotations) <- gsub(type, "annotation_value", colnames(annotations))
    colnames(annotations) <- gsub("^id$", "id_annotation", colnames(annotations))

    # get annotator name from stem of filename
    annotator <- tools::file_path_sans_ext(basename(file))
    annotations[, "annotator"] <- annotator

    # discard unwanted annotations (for example, if the span attracts two tags)
    # then these are recorded on the same element
    cols_wanted <- c("annotations",
                     "id_annotation",
                     "sofa",
                     "begin",
                     "end",
                     "annotation_type",
                     "annotation_value",
                     "annotator")
    cols_available <- colnames(annotations)
    cols_to_request <- intersect(cols_wanted, cols_available)
    annotations <- annotations[,cols_to_request]
  }
  annotations
}
# Example with no annotations of the specified type
#get_annotations_by_type.WebAnno_XMI("temp/webanno/out/pawel.xmi", "topic")


## Wrapper functions starting from entry points increasingly high up in the
## hierarchical/nested file structure.

#' @describeIn get_annotations_by_type.WebAnno_XMI Start from the XMI file for a
#'   single annotator, and get all custom annotations of types listed in the
#'   unexported list \code{.annotation_type_attribute_names} contained in this
#'   package.
#' @export
get_annotations.WebAnno_XMI <- function(file) {
  suppressWarnings(
    purrr::map_df(.annotation_type_attribute_names, ~get_annotations_by_type.WebAnno_XMI(file = file, type = .x))
  )
}

#' @describeIn get_annotations_by_type.WebAnno_XMI Start from the zipfile for a
#'   single annotator of a single document and get all annotations of the
#'   default types.
#' @export
get_annotations.WebAnno_annotator <- function(zipfile) {
  files_in_zip <- utils::unzip(zipfile, list = TRUE)$Name
  is_XMI_file <- function(file) identical(tolower(tools::file_ext(file)), "xmi")
  XMI_file <- purrr::keep(files_in_zip, is_XMI_file)
  if (length(XMI_file) != 1) stop(zipfile, " unexpectedly contains <> 1 XMI file")
  # Using unz() which returns an objection of class == c("unz", "connection")
  # doesn't work, I guess because xmlParse() doesn't accept these more general
  # 'file' types (unlike e.g. readr::read_file((unz(zipfile, XMI_file)))) so
  # instead I unzip to a temmpfile and then read this.
  tmpdir <- tempdir()
  tmpfile <- utils::unzip(zipfile, files = XMI_file, exdir = tmpdir)
  get_annotations.WebAnno_XMI(tmpfile)
}
#get_annotations.WebAnno_annotator("temp/webanno/out/annotation/s3873.txt/webanno2866292571186473689export.zip")
#get_annotations.WebAnno_annotator("temp/webanno/out/annotation/s3873.txt/webanno7107845074063737238export.zip")

#' @describeIn get_annotations_by_type.WebAnno_XMI Start from the folder for a
#'   single document and all annotations of the default types from all
#'   annotators.
#' @export
get_annotations.WebAnno_document <- function(path) {
  doc_id <- basename(path)
  is_zipfile <- function(file) identical(tolower(tools::file_ext(file)), "zip")
  annotator_zipfiles <- purrr::keep(list.files(path), is_zipfile)
  annotations <- purrr::map_df(annotator_zipfiles, ~get_annotations.WebAnno_annotator(file.path(path, .x)))
  if (!nrow(annotations) == 0) {
    annotations[,"doc_id"] <- doc_id
  }
  annotations
}
#get_annotations.WebAnno_document("temp/webanno/out/annotation/s3873.txt")

#' @describeIn get_annotations_by_type.WebAnno_XMI Start from the outer WebAnno
#'   project directory (not zipped) and get all annotations of the default types
#'   for all documents and annotators.
#' @export
get_annotations.WebAnno_project <- function(path) {
  doc_folders <- list.dirs(file.path(path, "annotation"))
  suppressWarnings(purrr::map_df(doc_folders, get_annotations.WebAnno_document))
}
#View(get_annotations.WebAnno_project("temp/webanno/out"))
