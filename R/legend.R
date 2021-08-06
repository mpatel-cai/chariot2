create_legend <-
  function(graph) {

    color_cols <-
      c("color",
        "fillcolor",
        "fontcolor")

    color_names <- vector(mode = "list",
                         length = length(color_cols)) %>%
      purrr::set_names(color_cols)
    col_index <- vector(mode = "list",
                        length = length(color_cols)) %>%
      purrr::set_names(color_cols)

    # Colorize
    node_legend <- graph$nodes_df
    edge_legend <- graph$edges_df


    for (color_col in color_cols) {
      if (!(color_col %in% colnames(node_legend))) {
        color_names[[color_col]] <- NULL
        col_index[[color_col]] <- NULL
      } else {

      col_index[[color_col]] <-
        grep(
          pattern = glue::glue("^{color_col}$"),
          x = colnames(node_legend))

      color_names[[color_col]] <-
        node_legend %>%
        select(all_of(color_col)) %>%
        unlist() %>%
        unname()

      }

    }

    ht_params <-
    list(index = col_index,
         colors = color_names) %>%
      purrr::transpose()

    node_legend_ht <-
      huxtable::hux(node_legend)

    for (j in seq_along(ht_params)) {
      node_legend_ht <-
        huxtable::map_background_color(node_legend_ht,
                                       row = 2:nrow(node_legend_ht),
                                       col = ht_params[[j]]$index,
                                       huxtable::by_rows(ht_params[[j]]$colors))

    }


    for (color_col in color_cols) {
      if (!(color_col %in% colnames(edge_legend))) {
        color_names[[color_col]] <- NULL
        col_index[[color_col]] <- NULL
      } else {

        col_index[[color_col]] <-
          grep(
            pattern = glue::glue("^{color_col}$"),
            x = colnames(edge_legend))

        color_names[[color_col]] <-
          edge_legend %>%
          select(all_of(color_col)) %>%
          unlist() %>%
          unname()

      }

    }

    ht_params <-
      list(index = col_index,
           colors = color_names) %>%
      purrr::transpose()

    edge_legend_ht <-
      huxtable::hux(edge_legend)

    for (j in seq_along(ht_params)) {
      edge_legend_ht <-
        huxtable::map_background_color(edge_legend_ht,
                                       row = 2:nrow(edge_legend_ht),
                                       col = ht_params[[j]]$index,
                                       huxtable::by_rows(ht_params[[j]]$colors))

    }

    list(node = node_legend_ht,
         edge = edge_legend_ht %>%
                  dplyr::select(-from,
                                -to)) %>%
      purrr::map(dplyr::select,
                 -contains("label"),
                 -contains("fontsize"),
                 -contains("len"),
                 -contains("width"),
                 -contains("height"),
                 -ends_with("_1"),
                 -ends_with("_2")) %>%
      purrr::map(huxtable::theme_article)


  }


hux_colorize <-
  function(ht,
           ...) {

    require(huxtable)
    require(plotrix)

    if ("huxtable" %in% class(ht)) {

      data <-
        as_tibble(ht)

      colnames(data) <-
        unlist(data[1,])

      data <-
        data[-1,]
    } else {

      data <- ht

    }

    fillcolor_cols <- enquos(...)

    legend <- data
    fillcolors <- vector(mode = "list",
                         length = length(fillcolor_cols))
    col_index <- vector(mode = "list",
                        length = length(fillcolor_cols))
    i <- 0
    for (fillcolor_col in fillcolor_cols) {
      i <- i+1
      legend <-
        legend %>%
        mutate(!!fillcolor_col := map(!!fillcolor_col, function(x) color.id(x)[1])) %>%
        mutate(!!fillcolor_col := unlist(!!fillcolor_col))

      col_index[[i]] <-
        grep(
          legend %>%
            select(!!fillcolor_col) %>%
            colnames(),
          colnames(data))

      fillcolors[[i]] <-
        legend %>%
        select(!!fillcolor_col) %>%
        unlist() %>%
        unname()

    }

    legend_ht <-
      huxtable::hux(legend)

    for (j in 1:i) {
      legend_ht <-
        huxtable::map_background_color(legend_ht,
                                       row = 2:nrow(legend_ht),
                                       col = col_index[[j]],
                                       by_rows(fillcolors[[j]]))

    }
    legend_ht
  }

hux_pretty_numbers <-
  function(ht,
           ...,
           big.mark = ",",
           scientific = FALSE) {

    if (!("huxtable" %in% class(ht))) {
      ht <- huxtable::hux(ht)
    }

    number_cols <- enquos(...)
    number_cols <-
      ht %>%
      select(!!!number_cols) %>%
      colnames()
    number_format(ht)[2:nrow(ht), number_cols] <- fmt_pretty(big.mark = big.mark,
                                                             scientific = scientific)
    ht
  }

hux_pretty_percents <-
  function(ht,
           ...,
           digits = 1) {

    if (!("huxtable" %in% class(ht))) {
      ht <- huxtable::hux(ht)
    }

    number_cols <- enquos(...)
    number_cols <-
      ht %>%
      select(!!!number_cols) %>%
      colnames()
    number_format(ht)[2:nrow(ht), number_cols] <- fmt_percent(digits = digits)
    ht
  }


hux_bold_values <-
  function(ht,
           ...,
           values,
           ignore_na = TRUE) {

    target_cols <- enquos(...)
    target_cols <-
      ht %>%
      select(!!!target_cols) %>%
      colnames()

    for (target_col in target_cols) {
      ht <-
        map_bold(ht = ht,
                 row = everywhere,
                 col = target_col,
                 fn  = by_values(values, ignore_na = ignore_na))

    }

    ht


  }
