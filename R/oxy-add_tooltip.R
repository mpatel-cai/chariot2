

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes_and_edges PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{unite}}
#'  \code{\link[tibble]{rownames}}
#' @rdname add_tooltip
#' @export 
#' @importFrom dplyr left_join select any_of group_by summarize ungroup distinct mutate_all
#' @importFrom tidyr pivot_longer unite
#' @importFrom tibble rowid_to_column
add_tooltip <-
  function(nodes_and_edges) {
    nodes_and_edges@nodes@data <-
    dplyr::left_join(
    nodes_and_edges@nodes@data,
    nodes_and_edges@nodes@data %>%
      dplyr::select(!dplyr::any_of(c('type',
                'label',
                'shape',
                'style',
                'penwidth',
                'color',
                'fillcolor',
                'image',
                'fontname',
                'fontsize',
                'fontcolor',
                'peripheries',
                'height',
                'width',
                'x',
                'y',
                'group',
                'tooltip',
                'xlabel',
                'URL',
                'sides',
                'orientation',
                'skew',
                'distortion',
                'gradientangle',
                'fixedsize',
                'labelloc',
                'margin'))) %>%
      tidyr::pivot_longer(cols = !id) %>%
      tidyr::unite(col = tooltip_row,
                   name,
                   value,
                   sep = ": ",
                   na.rm = FALSE) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(tooltip =
                         paste(tooltip_row,
                               collapse = "\n"),
                       .groups = "drop") %>%
      dplyr::ungroup(),
    by = "id") %>%
      dplyr::distinct()

    nodes_and_edges@edges@data <-
      dplyr::left_join(
        nodes_and_edges@edges@data %>%
          dplyr::select(
            !dplyr::any_of(
              c('label',
                'rel',
                'style',
                'penwidth',
                'color',
                'arrowsize',
                'arrowhead',
                'arrowtail',
                'fontname',
                'fontsize',
                'fontcolor',
                'len',
                'tooltip',
                'URL',
                'label',
                'labelfontname',
                'labelfontsize',
                'labelfontcolor',
                'labeltooltip',
                'labelURL',
                'edgetooltip',
                'edgeURL',
                'dir',
                'headtooltip',
                'headURL',
                'headclip',
                'headlabel',
                'headport',
                'tailtooltip',
                'tailURL',
                'tailclip',
                'taillabel',
                'tailport',
                'decorate'))) %>%
          dplyr::select(
            relationship_id,
            relationship_name,
            relationship_source,
            is_hierarchical,
            defines_ancestry,
            from,
            ends_with("_1"),
            to,
            ends_with("_2")) %>%
          dplyr::mutate_all(as.character) %>%
          tibble::rowid_to_column(),
    nodes_and_edges@edges@data %>%
      dplyr::select(
        !dplyr::any_of(
          c('label_1',
            'label_2',
            'label',
            'rel',
            'style',
            'penwidth',
            'color',
            'arrowsize',
            'arrowhead',
            'arrowtail',
            'fontname',
            'fontsize',
            'fontcolor',
            'len',
            'tooltip',
            'URL',
            'label',
            'labelfontname',
            'labelfontsize',
            'labelfontcolor',
            'labeltooltip',
            'labelURL',
            'edgetooltip',
            'edgeURL',
            'dir',
            'headtooltip',
            'headURL',
            'headclip',
            'headlabel',
            'headport',
            'tailtooltip',
            'tailURL',
            'tailclip',
            'taillabel',
            'tailport',
            'decorate'))) %>%
      dplyr::select(
        relationship_id,
        relationship_name,
        relationship_source,
        is_hierarchical,
        defines_ancestry,
        from,
        ends_with("_1"),
        to,
        ends_with("_2")) %>%
      dplyr::mutate_all(as.character) %>%
      tibble::rowid_to_column() %>%
      tidyr::pivot_longer(cols = !rowid) %>%
      tidyr::unite(tooltip_row,
                   name,
                   value,
                   sep = ": ",
                   na.rm = FALSE) %>%
      dplyr::group_by(rowid) %>%
      dplyr::summarize(labeltooltip =
                         paste(tooltip_row,
                               collapse = "\n"),
                       .groups = "drop") %>%
      dplyr::ungroup(),
    by = "rowid") %>%
      dplyr::select(-rowid)


    nodes_and_edges
  }
