# Plotting functions

## Marginal tag use

plot_marginal_tag_use_by_annotator <-
  function(annotations, tags = NULL, tag_order = c("value", "frequency")) {

    tag_order <- match.arg(tag_order)
    tags_ordered <- switch(tag_order,
                           frequency = get_tagset_completed(annotations),
                           value = sort(unique(annotations$annotation_value)))

    annotations %>%
      ggplot(aes(x = annotation_value)) +
      geom_bar(aes(y = ..prop.., group = 1)) + # group = 1 needed to get proportions within annotator, rather than as proportion of margin
      facet_grid(annotator ~ ., margins = TRUE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
      scale_x_discrete(name = NULL,
                       labels = function(x) str_wrap(x, width = 15),
                       limits = tags_ordered) +
      scale_y_continuous(name = "Share of tags by annotator")
  }

## Tag co-occurrence

# Bubble plot on square matrix of potential annotation values
crossplot <- function(df, tags_ordered) {
  # WARNING! Hardwired column names "item1" and "item2"
  # WARNING! Hardwired constant "topics_ordered", to be provided from calling environment
  ggplot(df, aes(x = item1, y = item2)) +
    geom_point(aes(size = n, colour = log(n))) +
    scale_y_discrete(name = NULL, limits = rev(tags_ordered), position = "right") +
    scale_x_discrete(name = NULL, limits = tags_ordered, position = "top")  +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 0))
}

crossplot_pairwise <- function(annotations, annotators = NULL, margins = TRUE, cutoff = 0, tag_order = c("value", "popularity")) {
  # WARNING! Hardwired column name "n"
  df <- agreement_pairwise(annotations, annotators)
  df <- df[df$n > cutoff,]
  # WARNING! Hardwired column names "annotator1" and "annotator2"
  tag_order <- match.arg(tag_order)
  tags_ordered <- switch(tag_order,
                         popularity = get_tagset_completed(annotations),
                         value = sort(unique(annotations$annotation_value)))
  crossplot(df, tags_ordered) + facet_grid(annotator1 ~ annotator2, margins, switch = "y")
}

# Wrapper to shape the df and then call the plot function
crossplot_from_annotations <- function(annotations, annotators = NULL) {
  df <- agreement(annotations)
  crossplot(df)
}
