library(tidyverse)
library(chariot)
library(glue)

map_node_attributes <-
  function(nodes_and_edges,
           fontsize = 12,
           fontname = NULL,
           width    = 1.5,
           height   = 1.5,
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
           tooltip_from = NULL,
           tooltip_map = NULL,
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

    # shape_from <- enquo(shape_from)
    # return(nodes_and_edges@nodes@data %>% mutate(shape = !!shape_from))
    #

    if (!is.null(all_args$shape_from)) {
      cli::cli_alert_info("Shape from: {all_args$shape_from}")
      shape_from <- enquo(shape_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(shape = map_to_value(!!shape_from,
                                    map_assignment = shape_map,
                                    other = shape_map_other))
    }

    if (!is.null(all_args$style_from)) {
      cli::cli_alert_info("Style from: {all_args$style_from}")
      style_from <- enquo(style_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(style = map_to_value(!!style_from,
                                    map_assignment = style_map,
                                    other = style_map_other))
    }

    if (!is.null(all_args$penwidth_from)) {
      cli::cli_alert_info("Penwidth from: {all_args$penwidth_from}")
      penwidth_from <- enquo(penwidth_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(penwidth = map_to_value(!!penwidth_from,
                                       map_assignment = penwidth_map,
                                       other = penwidth_map_other))
    }

    if (!is.null(all_args$color_from)) {
      cli::cli_alert_info("Color from: {all_args$color_from}")
      color_from <- enquo(color_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(color = map_to_value(!!color_from,
                                    map_assignment = color_map,
                                    other = color_map_other))
    }

    if (!is.null(all_args$fillcolor_from)) {
      cli::cli_alert_info("Fillcolor from: {all_args$fillcolor_from}")
      fillcolor_from <- enquo(fillcolor_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(fillcolor = map_to_value(!!fillcolor_from,
                                        map_assignment = fillcolor_map,
                                        other = fillcolor_map_other))
    }

    if (!is.null(all_args$image_from)) {
      cli::cli_alert_info("Image from: {all_args$image_from}")
      image_from <- enquo(image_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(image = map_to_value(!!image_from,
                                    map_assignment = image_map,
                                    other = image_map_other))
    }

    if (!is.null(all_args$fontcolor_from)) {
      cli::cli_alert_info("Fontcolor from: {all_args$fontcolor_from}")
      fontcolor_from <- enquo(fontcolor_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(fontcolor = map_to_value(!!fontcolor_from,
                                        map_assignment = fontcolor_map,
                                        other = fontcolor_map_other))
    }

    if (!is.null(all_args$peripheries_from)) {
      peripheries_from <- enquo(peripheries_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(peripheries = map_to_value(!!peripheries_from,
                                          map_assignment = peripheries_map,
                                          other = peripheries_map_other))
    }

    if (!is.null(all_args$x_from)) {
      x_from <- enquo(x_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(x = map_to_value(!!x_from,
                                map_assignment = x_map,
                                other = x_map_other))
    }

    if (!is.null(all_args$y_from)) {
      y_from <- enquo(y_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(y = map_to_value(!!y_from,
                                map_assignment = y_map,
                                other = y_map_other))
    }

    if (!is.null(all_args$group_from)) {
      group_from <- enquo(group_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(group = map_to_value(!!group_from,
                                    map_assignment = group_map,
                                    other = group_map_other))
    }

    if (!is.null(all_args$tooltip_from)) {
      tooltip_from <- enquo(tooltip_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(tooltip = map_to_value(!!tooltip_from,
                                      map_assignment = tooltip_map,
                                      other = tooltip_map_other))
    }

    if (!is.null(all_args$xlabel_from)) {
      xlabel_from <- enquo(xlabel_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(xlabel = map_to_value(!!xlabel_from,
                                     map_assignment = xlabel_map,
                                     other = xlabel_map_other))
    }

    if (!is.null(all_args$URL_from)) {
      URL_from <- enquo(URL_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(URL = map_to_value(!!URL_from,
                                  map_assignment = URL_map,
                                  other = URL_map_other))
    }

    if (!is.null(all_args$sides_from)) {
      sides_from <- enquo(sides_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(sides = map_to_value(!!sides_from,
                                    map_assignment = sides_map,
                                    other = sides_map_other))
    }

    if (!is.null(all_args$orientation_from)) {
      orientation_from <- enquo(orientation_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(orientation = map_to_value(!!orientation_from,
                                          map_assignment = orientation_map,
                                          other = orientation_map_other))
    }

    if (!is.null(all_args$skew_from)) {
      skew_from <- enquo(skew_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(skew = map_to_value(!!skew_from,
                                   map_assignment = skew_map,
                                   other = skew_map_other))
    }

    if (!is.null(all_args$distortion_from)) {
      distortion_from <- enquo(distortion_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(distortion = map_to_value(!!distortion_from,
                                         map_assignment = distortion_map,
                                         other = distortion_map_other))
    }

    if (!is.null(all_args$gradientangle_from)) {
      gradientangle_from <- enquo(gradientangle_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(gradientangle = map_to_value(!!gradientangle_from,
                                            map_assignment = gradientangle_map,
                                            other = gradientangle_map_other))
    }

    if (!is.null(all_args$fixedsize_from)) {
      fixedsize_from <- enquo(fixedsize_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(fixedsize = map_to_value(!!fixedsize_from,
                                        map_assignment = fixedsize_map,
                                        other = fixedsize_map_other))
    }

    if (!is.null(all_args$labelloc_from)) {
      labelloc_from <- enquo(labelloc_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(labelloc = map_to_value(!!labelloc_from,
                                       map_assignment = labelloc_map,
                                       other = labelloc_map_other))
    }

    if (!is.null(all_args$margin_from)) {
      margin_from <- enquo(margin_from)
      nodes_and_edges@nodes@data <-
        nodes_and_edges@nodes@data %>%
        mutate(margin = map_to_value(!!margin_from,
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
      keep(~!is.null(.))
    constant_attrs_df <-
      tibble::as_tibble(constant_attrs)
    nodes_and_edges@nodes@data <-
      cbind(nodes_and_edges@nodes@data,
            constant_attrs_df)
    cli::cli_alert_info("{length(constant_attrs)} constant attribute{?s} added: {names(constant_attrs)}.")
    nodes_and_edges

  }


map_edge_attributes <-
  function(nodes_and_edges,
           fontsize = 12,
           len = 1.0,
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


    arrowhead_from <- enquo(arrowhead_from)
    nodes_and_edges@edges@data <-
      nodes_and_edges@edges@data %>%
      mutate(arrowhead = map_to_value(!!arrowhead_from,
                                      map_assignment = arrowhead_map))

    label_from <- enquo(label_from)
    nodes_and_edges@edges@data <-
      nodes_and_edges@edges@data %>%
      mutate(rel = relationship_id,
             label = !!label_from,
             fontsize = fontsize,
             len = len) %>%
      distinct()

    nodes_and_edges

  }

construct_graph <-
  function(nodes_and_edges,
           attr_theme = "lr") {

    require(DiagrammeR)

    attrs <-
      colnames(nodes_and_edges@nodes@data)[!(colnames(nodes_and_edges@nodes@data) %in%
                                       c("id"))]
    omop_ndf <-
      eval(
      rlang::parse_expr(
      c(
      "create_node_df(\n",
      "   n = nrow(nodes_and_edges@nodes@data),\n",
       paste(glue("  {attrs} = nodes_and_edges@nodes@data${attrs}"),
             collapse = ",\n"),
      "\n)") %>%
        paste(collapse = ""))
      )

    omop_ndf$id <- nodes_and_edges@nodes@data$id

    # omop_edf <-
    #   create_edge_df(
    #     from = nodes_and_edges@edges@data$from,
    #     to = nodes_and_edges@edges@data$to,
    #     rel = nodes_and_edges@edges@data$rel,
    #     label = nodes_and_edges@edges@data$label,
    #     arrowhead = nodes_and_edges@edges@data$arrowhead,
    #     len = nodes_and_edges@edges@data$len,
    #     fontsize = nodes_and_edges@edges@data$fontsize
    #   )

    attrs <-
      colnames(nodes_and_edges@edges@data)

    omop_edf <-
      eval(
        rlang::parse_expr(
          c(
            "create_edge_df(\n",
            paste(glue("  {attrs} = nodes_and_edges@edges@data${attrs}"),
                  collapse = ",\n"),
            "\n)") %>%
            paste(collapse = ""))
      )

    final_graph <-
      create_graph(
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

