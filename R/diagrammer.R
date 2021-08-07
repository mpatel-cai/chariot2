#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes_and_edges PARAM_DESCRIPTION
#' @param fontsize PARAM_DESCRIPTION, Default: 12
#' @param fontname PARAM_DESCRIPTION, Default: NULL
#' @param width PARAM_DESCRIPTION, Default: 1.5
#' @param height PARAM_DESCRIPTION, Default: 1.5
#' @param shape_from PARAM_DESCRIPTION, Default: standard_concept
#' @param shape_map PARAM_DESCRIPTION, Default: c(C = "square", S = "circle", `NA` = "circle")
#' @param shape_map_other PARAM_DESCRIPTION, Default: 'circle'
#' @param style_from PARAM_DESCRIPTION, Default: standard_concept
#' @param style_map PARAM_DESCRIPTION, Default: c(C = "filled", S = "filled", `NA` = "filled")
#' @param style_map_other PARAM_DESCRIPTION, Default: NULL
#' @param penwidth_from PARAM_DESCRIPTION, Default: NULL
#' @param penwidth_map PARAM_DESCRIPTION, Default: NULL
#' @param color_from PARAM_DESCRIPTION, Default: vocabulary_id
#' @param color_map PARAM_DESCRIPTION, Default: vocabulary_id_standard_colors
#' @param color_map_other PARAM_DESCRIPTION, Default: 'gray20'
#' @param fillcolor_from PARAM_DESCRIPTION, Default: vocabulary_id
#' @param fillcolor_map PARAM_DESCRIPTION, Default: vocabulary_id_standard_colors
#' @param fillcolor_map_other PARAM_DESCRIPTION, Default: 'gray20'
#' @param image_from PARAM_DESCRIPTION, Default: NULL
#' @param image_map PARAM_DESCRIPTION, Default: NULL
#' @param fontname_from PARAM_DESCRIPTION, Default: NULL
#' @param fontname_map PARAM_DESCRIPTION, Default: NULL
#' @param fontcolor_from PARAM_DESCRIPTION, Default: standard_concept
#' @param fontcolor_map PARAM_DESCRIPTION, Default: c(C = "black", S = "black", `NA` = "gray40")
#' @param fontcolor_map_other PARAM_DESCRIPTION, Default: NULL
#' @param peripheries_from PARAM_DESCRIPTION, Default: NULL
#' @param peripheries_map PARAM_DESCRIPTION, Default: NULL
#' @param x_from PARAM_DESCRIPTION, Default: NULL
#' @param x_map PARAM_DESCRIPTION, Default: NULL
#' @param y_from PARAM_DESCRIPTION, Default: NULL
#' @param y_map PARAM_DESCRIPTION, Default: NULL
#' @param group_from PARAM_DESCRIPTION, Default: NULL
#' @param group_map PARAM_DESCRIPTION, Default: NULL
#' @param tooltip_from PARAM_DESCRIPTION, Default: NULL
#' @param tooltip_map PARAM_DESCRIPTION, Default: NULL
#' @param xlabel_from PARAM_DESCRIPTION, Default: NULL
#' @param xlabel_map PARAM_DESCRIPTION, Default: NULL
#' @param URL_from PARAM_DESCRIPTION, Default: NULL
#' @param URL_map PARAM_DESCRIPTION, Default: NULL
#' @param sides_from PARAM_DESCRIPTION, Default: NULL
#' @param sides_map PARAM_DESCRIPTION, Default: NULL
#' @param orientation_from PARAM_DESCRIPTION, Default: NULL
#' @param orientation_map PARAM_DESCRIPTION, Default: NULL
#' @param skew_from PARAM_DESCRIPTION, Default: NULL
#' @param skew_map PARAM_DESCRIPTION, Default: NULL
#' @param distortion_from PARAM_DESCRIPTION, Default: NULL
#' @param distortion_map PARAM_DESCRIPTION, Default: NULL
#' @param gradientangle_from PARAM_DESCRIPTION, Default: NULL
#' @param gradientangle_map PARAM_DESCRIPTION, Default: NULL
#' @param fixedsize_from PARAM_DESCRIPTION, Default: NULL
#' @param fixedsize_map PARAM_DESCRIPTION, Default: NULL
#' @param labelloc_from PARAM_DESCRIPTION, Default: NULL
#' @param labelloc_map PARAM_DESCRIPTION, Default: NULL
#' @param margin_from PARAM_DESCRIPTION, Default: NULL
#' @param margin_map PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[cli]{cli_alert}}
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{keep}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname map_node_attributes
#' @export
#' @importFrom cli cli_alert_info
#' @importFrom dplyr enquo mutate
#' @importFrom purrr keep
#' @importFrom tibble as_tibble
map_node_attributes <-
  function(nodes_and_edges,
           fontsize = 26,
           fontname = NULL,
           width    = 4,
           height   = 4,
           shape_from = standard_concept,
           shape_map =   c(C = "square",
                           S = "circle",
                           `NA` = "circle"),
           shape_map_other = "circle",
           style_from = standard_concept,
           style_map = c(C = "filled",
                           S = "filled",
                           `NA` = "filled"),
           style_map_other = NULL,
           penwidth_from = NULL,
           penwidth_map = NULL,
           color_from = vocabulary_id,
           color_map = vocabulary_id_standard_colors,
           color_map_other = "gray20",
           fillcolor_from = vocabulary_id,
           fillcolor_map = vocabulary_id_standard_colors,
           fillcolor_map_other = "gray20",
           image_from = NULL,
           image_map = NULL,
           fontname_from = NULL,
           fontname_map = NULL,
           fontcolor_from = standard_concept,
           fontcolor_map = c(C = "black",
                             S = "black",
                             `NA` = "gray40"),
           fontcolor_map_other = NULL,
           peripheries_from = NULL,
           peripheries_map = NULL,
           x_from = NULL,
           x_map = NULL,
           y_from = NULL,
           y_map = NULL,
           group_from = NULL,
           group_map = NULL,
           xlabel_from = NULL,
           xlabel_map = NULL,
           URL_from = NULL,
           URL_map = NULL,
           sides_from = NULL,
           sides_map = NULL,
           orientation_from = NULL,
           orientation_map = NULL,
           skew_from = NULL,
           skew_map = NULL,
           distortion_from = NULL,
           distortion_map = NULL,
           gradientangle_from = NULL,
           gradientangle_map = NULL,
           fixedsize_from = NULL,
           fixedsize_map = NULL,
           labelloc_from = NULL,
           labelloc_map = NULL,
           margin_from = NULL,
           margin_map = NULL) {


    all_args <- formals()

    # shape_from <- dplyr::enquo(shape_from)
    # return(nodes_and_edges@nodes@data %>% dplyr::dplyr::mutate(shape = !!shape_from))
    #

    if (!is.null(all_args$shape_from)) {
      cli::cli_alert_info("Shape from: {all_args$shape_from}")
      shape_from <- dplyr::enquo(shape_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(shape = map_to_value(!!shape_from,
                                    map_assignment = shape_map,
                                    other = shape_map_other))
    }

    if (!is.null(all_args$style_from)) {
      cli::cli_alert_info("Style from: {all_args$style_from}")
      style_from <- dplyr::enquo(style_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(style = map_to_value(!!style_from,
                                    map_assignment = style_map,
                                    other = style_map_other))
    }

    if (!is.null(all_args$penwidth_from)) {
      cli::cli_alert_info("Penwidth from: {all_args$penwidth_from}")
      penwidth_from <- dplyr::enquo(penwidth_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(penwidth = map_to_value(!!penwidth_from,
                                       map_assignment = penwidth_map,
                                       other = penwidth_map_other))
    }

    if (!is.null(all_args$color_from)) {
      cli::cli_alert_info("Color from: {all_args$color_from}")
      color_from <- dplyr::enquo(color_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(color = map_to_value(!!color_from,
                                    map_assignment = color_map,
                                    other = color_map_other))
    }

    if (!is.null(all_args$fillcolor_from)) {
      cli::cli_alert_info("Fillcolor from: {all_args$fillcolor_from}")
      fillcolor_from <- dplyr::enquo(fillcolor_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(fillcolor = map_to_value(!!fillcolor_from,
                                        map_assignment = fillcolor_map,
                                        other = fillcolor_map_other))
    }

    if (!is.null(all_args$image_from)) {
      cli::cli_alert_info("Image from: {all_args$image_from}")
      image_from <- dplyr::enquo(image_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(image = map_to_value(!!image_from,
                                    map_assignment = image_map,
                                    other = image_map_other))
    }

    if (!is.null(all_args$fontcolor_from)) {
      cli::cli_alert_info("Fontcolor from: {all_args$fontcolor_from}")
      fontcolor_from <- dplyr::enquo(fontcolor_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(fontcolor = map_to_value(!!fontcolor_from,
                                        map_assignment = fontcolor_map,
                                        other = fontcolor_map_other))
    }

    if (!is.null(all_args$peripheries_from)) {
      peripheries_from <- dplyr::enquo(peripheries_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(peripheries = map_to_value(!!peripheries_from,
                                          map_assignment = peripheries_map,
                                          other = peripheries_map_other))
    }

    if (!is.null(all_args$x_from)) {
      x_from <- dplyr::enquo(x_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(x = map_to_value(!!x_from,
                                map_assignment = x_map,
                                other = x_map_other))
    }

    if (!is.null(all_args$y_from)) {
      y_from <- dplyr::enquo(y_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(y = map_to_value(!!y_from,
                                map_assignment = y_map,
                                other = y_map_other))
    }

    if (!is.null(all_args$group_from)) {
      group_from <- dplyr::enquo(group_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(group = map_to_value(!!group_from,
                                    map_assignment = group_map,
                                    other = group_map_other))
    }

    if (!is.null(all_args$xlabel_from)) {
      xlabel_from <- dplyr::enquo(xlabel_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(xlabel = map_to_value(!!xlabel_from,
                                     map_assignment = xlabel_map,
                                     other = xlabel_map_other))
    }

    if (!is.null(all_args$URL_from)) {
      URL_from <- dplyr::enquo(URL_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(URL = map_to_value(!!URL_from,
                                  map_assignment = URL_map,
                                  other = URL_map_other))
    }

    if (!is.null(all_args$sides_from)) {
      sides_from <- dplyr::enquo(sides_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(sides = map_to_value(!!sides_from,
                                    map_assignment = sides_map,
                                    other = sides_map_other))
    }

    if (!is.null(all_args$orientation_from)) {
      orientation_from <- dplyr::enquo(orientation_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(orientation = map_to_value(!!orientation_from,
                                          map_assignment = orientation_map,
                                          other = orientation_map_other))
    }

    if (!is.null(all_args$skew_from)) {
      skew_from <- dplyr::enquo(skew_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(skew = map_to_value(!!skew_from,
                                   map_assignment = skew_map,
                                   other = skew_map_other))
    }

    if (!is.null(all_args$distortion_from)) {
      distortion_from <- dplyr::enquo(distortion_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(distortion = map_to_value(!!distortion_from,
                                         map_assignment = distortion_map,
                                         other = distortion_map_other))
    }

    if (!is.null(all_args$gradientangle_from)) {
      gradientangle_from <- dplyr::enquo(gradientangle_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(gradientangle = map_to_value(!!gradientangle_from,
                                            map_assignment = gradientangle_map,
                                            other = gradientangle_map_other))
    }

    if (!is.null(all_args$fixedsize_from)) {
      fixedsize_from <- dplyr::enquo(fixedsize_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(fixedsize = map_to_value(!!fixedsize_from,
                                        map_assignment = fixedsize_map,
                                        other = fixedsize_map_other))
    }

    if (!is.null(all_args$labelloc_from)) {
      labelloc_from <- dplyr::enquo(labelloc_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(labelloc = map_to_value(!!labelloc_from,
                                       map_assignment = labelloc_map,
                                       other = labelloc_map_other))
    }

    if (!is.null(all_args$margin_from)) {
      margin_from <- dplyr::enquo(margin_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        dplyr::mutate(margin = map_to_value(!!margin_from,
                                     map_assignment = margin_map,
                                     other = margin_map_other))
    }

    # Constant Attributes
    constant_attrs <-
      c("fontsize",
        "fontname",
        "width",
        "height")

    constant_attrs <-
      formals()[names(formals()) %in% constant_attrs] %>%
      purrr::keep(~!is.null(.))
    constant_attrs_df <-
      tibble::as_tibble(constant_attrs)
    nodes_and_edges@nodes@data <-
      cbind(nodes_and_edges@nodes@data,
            constant_attrs_df)
    cli::cli_alert_info("{length(constant_attrs)} constant attribute{?s} added: {names(constant_attrs)}.")
    nodes_and_edges

  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes_and_edges PARAM_DESCRIPTION
#' @param fontsize PARAM_DESCRIPTION, Default: 12
#' @param len PARAM_DESCRIPTION, Default: 1
#' @param label_from PARAM_DESCRIPTION, Default: relationship_name
#' @param style_from PARAM_DESCRIPTION, Default: NULL
#' @param style_map PARAM_DESCRIPTION, Default: NULL
#' @param penwidth_from PARAM_DESCRIPTION, Default: NULL
#' @param penwidth_map PARAM_DESCRIPTION, Default: NULL
#' @param color_from PARAM_DESCRIPTION, Default: NULL
#' @param color_map PARAM_DESCRIPTION, Default: NULL
#' @param arrowsize_from PARAM_DESCRIPTION, Default: NULL
#' @param arrowsize_map PARAM_DESCRIPTION, Default: NULL
#' @param arrowhead_from PARAM_DESCRIPTION, Default: is_hierarchical
#' @param arrowhead_map PARAM_DESCRIPTION, Default: c(`1` = "vee", `0` = "none")
#' @param arrowtail_from PARAM_DESCRIPTION, Default: NULL
#' @param arrowtail_map PARAM_DESCRIPTION, Default: NULL
#' @param fontname_from PARAM_DESCRIPTION, Default: NULL
#' @param fontname_map PARAM_DESCRIPTION, Default: NULL
#' @param fontsize_from PARAM_DESCRIPTION, Default: NULL
#' @param fontsize_map PARAM_DESCRIPTION, Default: NULL
#' @param fontcolor_from PARAM_DESCRIPTION, Default: NULL
#' @param fontcolor_map PARAM_DESCRIPTION, Default: NULL
#' @param tooltip_from PARAM_DESCRIPTION, Default: NULL
#' @param tooltip_map PARAM_DESCRIPTION, Default: NULL
#' @param URL_from PARAM_DESCRIPTION, Default: NULL
#' @param URL_map PARAM_DESCRIPTION, Default: NULL
#' @param edgetooltip_from PARAM_DESCRIPTION, Default: NULL
#' @param edgetooltip_map PARAM_DESCRIPTION, Default: NULL
#' @param edgeURL_from PARAM_DESCRIPTION, Default: NULL
#' @param edgeURL_map PARAM_DESCRIPTION, Default: NULL
#' @param dir_from PARAM_DESCRIPTION, Default: NULL
#' @param dir_map PARAM_DESCRIPTION, Default: NULL
#' @param headtooltip_from PARAM_DESCRIPTION, Default: NULL
#' @param headtooltip_map PARAM_DESCRIPTION, Default: NULL
#' @param headURL_from PARAM_DESCRIPTION, Default: NULL
#' @param headURL_map PARAM_DESCRIPTION, Default: NULL
#' @param headclip_from PARAM_DESCRIPTION, Default: NULL
#' @param headclip_map PARAM_DESCRIPTION, Default: NULL
#' @param headlabel_from PARAM_DESCRIPTION, Default: NULL
#' @param headlabel_map PARAM_DESCRIPTION, Default: NULL
#' @param headport_from PARAM_DESCRIPTION, Default: NULL
#' @param headport_map PARAM_DESCRIPTION, Default: NULL
#' @param tailtooltip_from PARAM_DESCRIPTION, Default: NULL
#' @param tailtooltip_map PARAM_DESCRIPTION, Default: NULL
#' @param tailURL_from PARAM_DESCRIPTION, Default: NULL
#' @param tailURL_map PARAM_DESCRIPTION, Default: NULL
#' @param tailclip_from PARAM_DESCRIPTION, Default: NULL
#' @param tailclip_map PARAM_DESCRIPTION, Default: NULL
#' @param taillabel_from PARAM_DESCRIPTION, Default: NULL
#' @param taillabel_map PARAM_DESCRIPTION, Default: NULL
#' @param tailport_from PARAM_DESCRIPTION, Default: NULL
#' @param tailport_map PARAM_DESCRIPTION, Default: NULL
#' @param decorate_from PARAM_DESCRIPTION, Default: NULL
#' @param decorate_map PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{distinct}}
#' @rdname map_edge_attributes
#' @export
#' @importFrom dplyr enquo mutate distinct
map_edge_attributes <-
  function(nodes_and_edges,
           fontsize = 26,
           len = 5,
           label_from = relationship_name,
           style_from = NULL,
           style_map = NULL,
           penwidth_from = NULL,
           penwidth_map = NULL,
           color_from = NULL,
           color_map = NULL,
           arrowsize_from = NULL,
           arrowsize_map = NULL,
           arrowhead_from = is_hierarchical,
           arrowhead_map = c(`1` = "vee",
                             `0` = "none"),
           arrowtail_from = NULL,
           arrowtail_map = NULL,
           fontname_from = NULL,
           fontname_map = NULL,
           fontsize_from = NULL,
           fontsize_map = NULL,
           fontcolor_from = NULL,
           fontcolor_map = NULL,
           tooltip_from = NULL,
           tooltip_map = NULL,
           URL_from = NULL,
           URL_map = NULL,
           edgetooltip_from = NULL,
           edgetooltip_map = NULL,
           edgeURL_from = NULL,
           edgeURL_map = NULL,
           dir_from = NULL,
           dir_map = NULL,
           headtooltip_from = NULL,
           headtooltip_map = NULL,
           headURL_from = NULL,
           headURL_map = NULL,
           headclip_from = NULL,
           headclip_map = NULL,
           headlabel_from = NULL,
           headlabel_map = NULL,
           headport_from = NULL,
           headport_map = NULL,
           tailtooltip_from = NULL,
           tailtooltip_map = NULL,
           tailURL_from = NULL,
           tailURL_map = NULL,
           tailclip_from = NULL,
           tailclip_map = NULL,
           taillabel_from = NULL,
           taillabel_map = NULL,
           tailport_from = NULL,
           tailport_map = NULL,
           decorate_from = NULL,
           decorate_map = NULL) {


    arrowhead_from <- dplyr::enquo(arrowhead_from)
    nodes_and_edges@edges@data <-
      nodes_and_edges@edges@data %>%
      dplyr::mutate(arrowhead = map_to_value(!!arrowhead_from,
                                      map_assignment = arrowhead_map))

    label_from <- dplyr::enquo(label_from)
    nodes_and_edges@edges@data <-
      nodes_and_edges@edges@data %>%
      dplyr::mutate(rel = relationship_id,
             label = !!label_from,
             fontsize = fontsize,
             len = len) %>%
      dplyr::distinct()

    nodes_and_edges

  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes_and_edges PARAM_DESCRIPTION
#' @param attr_theme PARAM_DESCRIPTION, Default: 'lr'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[DiagrammeR]{create_graph}}
#' @rdname construct_graph
#' @export
#' @importFrom rlang parse_expr
#' @importFrom glue glue
#' @importFrom DiagrammeR create_graph
construct_graph <-
  function(nodes_and_edges,
           attr_theme = "lr") {


    attrs <-
      colnames(nodes_and_edges@nodes@data)[!(colnames(nodes_and_edges@nodes@data) %in%
                                       c("id"))]
    omop_ndf <-
      eval(
      rlang::parse_expr(
      c(
      "DiagrammeR::create_node_df(\n",
      "   n = nrow(nodes_and_edges@nodes@data),\n",
       paste(glue::glue("  {attrs} = nodes_and_edges@nodes@data${attrs}"),
             collapse = ",\n"),
      "\n)") %>%
        paste(collapse = ""))
      )

    omop_ndf$id <- nodes_and_edges@nodes@data$id


    attrs <-
      colnames(nodes_and_edges@edges@data)

    omop_edf <-
      eval(
        rlang::parse_expr(
          c(
            "DiagrammeR::create_edge_df(\n",
            paste(glue::glue("  {attrs} = nodes_and_edges@edges@data${attrs}"),
                  collapse = ",\n"),
            "\n)") %>%
            paste(collapse = ""))
      )

    final_graph <-
      DiagrammeR::create_graph(
        nodes_df = omop_ndf,
        edges_df = omop_edf,
        attr_theme = attr_theme
      )

    final_data <-
      list(
        graph = final_graph,
        src =   nodes_and_edges
      )
    final_data
  }

