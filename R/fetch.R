
read_sql_template <-
  function(file) {
      readLines(
        system.file(package = "chariot2",
                    "sql",
                    file)) %>%
      paste(collapse = "\n")
  }



get_version_key <-
  function(conn,
           conn_fun = "pg13::local_connect(verbose=FALSE)",
           log_schema = "public",
           log_table = "setup_athena_log",
           log_timestamp_field = "sa_datetime",
           template_only = FALSE,
           sql_only = FALSE) {

    sql_template <-
      read_sql_template(file = "get_version_key.sql")

    if (template_only) {

      return(sql_template)

    }

    if (sql_only) {

      return(glue::glue(sql_template))

    }

    version <-
      pg13::query(conn = conn,
                  conn_fun = conn_fun,
                  checks = "",
                  sql_statement = glue::glue(sql_template),
                  verbose = FALSE,
                  render_sql = FALSE)
    as.list(version)

  }


save_to_cache <-
  function(
    resultset,
    sql,
    version_key) {

    R.cache::saveCache(
      object = resultset,
      key    = c(sql, version_key),
      dirs   = "chariot2"
    )
  }

load_from_cache <-
  function(sql,
           version_key) {

      R.cache::loadCache(
        key = c(sql, version_key),
        dirs = "chariot2")

  }

fetch_omop <-
  function(conn,
           conn_fun = "pg13::local_connect(verbose=FALSE)",
           type_from = concept_class_id,
           label_glue = "{vocabulary_id}\n{concept_class_id}\n({standard_concept})\n",
           schema = "omop_vocabulary",
           verbose = FALSE,
           render_sql = FALSE,
           version_key = get_version_key()) {

    stopifnot(!missing(version_key))

    sql <- read_sql_template(file = "total_concept_class_ct.sql")
    sql <- glue::glue(sql)


    total_concept_class_ct <-
      load_from_cache(sql = sql,
                      version_key = version_key)

    if (is.null(total_concept_class_ct)) {

      total_concept_class_ct <-
        pg13::query(
          conn = conn,
          checks = "",
          conn_fun = conn_fun,
          sql_statement = sql,
          verbose = verbose,
          render_sql = render_sql)

      save_to_cache(resultset = total_concept_class_ct,
                    sql       = sql,
                    version_key = version_key)

    }
    Sys.sleep(0.5)

    sql <- read_sql_template(file = "total_vocabulary_ct.sql")
    sql <- glue::glue(sql)


    total_vocabulary_ct <-
      load_from_cache(sql = sql,
                      version_key = version_key)

    if (is.null(total_vocabulary_ct)) {

      total_vocabulary_ct <-
        pg13::query(
          conn = conn,
          checks = "",
          conn_fun = conn_fun,
          sql_statement = sql,
          verbose = verbose,
          render_sql = render_sql)

      save_to_cache(resultset = total_vocabulary_ct,
                    sql       = sql,
                    version_key = version_key)

    }
    Sys.sleep(0.5)
  ############
    vocabulary_ids <-
      total_vocabulary_ct %>%
      dplyr::arrange(total_vocabulary_ct) %>%
      dplyr::distinct(vocabulary_id) %>%
      unlist() %>%
      unname()

    relationship_output <-
      vector(mode = "list",
             length =
               length(vocabulary_ids))
    names(relationship_output) <- vocabulary_ids

    cli::cli_progress_bar(
      format = "\nQuerying {vocabulary_id} | {pb_bar} {pb_current}/{pb_total} {pb_percent} ({pb_elapsed})\n",
      clear = FALSE,
      total = 2*length(vocabulary_ids))

    for (vocabulary_id in vocabulary_ids) {

      sql <- read_sql_template(file = "relationship.sql")
      sql <- glue::glue(sql)

      relationship <-
        load_from_cache(sql = sql,
                        version_key = version_key)

      if (is.null(relationship)) {
        Sys.sleep(0.5)
        relationship <-
          pg13::query(
            conn = conn,
            checks = "",
            conn_fun = conn_fun,
            sql_statement = sql,
            verbose = verbose,
            render_sql = render_sql)
        Sys.sleep(0.5)

        save_to_cache(resultset = relationship,
                      sql       = sql,
                      version_key = version_key)

      } else {

        Sys.sleep(0.01)

      }

      relationship_output[[vocabulary_id]] <-
        relationship
      cli::cli_progress_update()

    }

    relationship_ct_output <-
      vector(mode = "list",
             length =
               length(vocabulary_ids))
    names(relationship_ct_output) <- vocabulary_ids

    for (vocabulary_id in vocabulary_ids) {

      sql <- read_sql_template(file = "relationship_ct.sql")
      sql <- glue::glue(sql)

      relationship_ct <-
        load_from_cache(sql = sql,
                        version_key = version_key)

      if (is.null(relationship_ct)) {

        Sys.sleep(0.5)
        relationship_ct <-
          pg13::query(
            conn = conn,
            checks = "",
            conn_fun = conn_fun,
            sql_statement = sql,
            verbose = verbose,
            render_sql = render_sql)
        Sys.sleep(0.5)

        save_to_cache(resultset = relationship_ct,
                      sql       = sql,
                      version_key = version_key)

      } else {

        Sys.sleep(0.01)

      }
      relationship_ct_output[[vocabulary_id]] <-
        relationship_ct
      cli::cli_progress_update()

    }

    omop_relationships <-
      list(relationships = relationship_output,
           relationship_counts = relationship_ct_output) %>%
      purrr::transpose()


    omop_relationship_errors <-
      list()

    # Check to make sure a join will not lead to duplicates
    for (vocabulary_id in vocabulary_ids) {

      relationship_rows <-
      omop_relationships[[vocabulary_id]]$relationships %>%
        dplyr::select(dplyr::any_of(colnames(omop_relationships[[vocabulary_id]]$relationship_counts))) %>%
        dplyr::distinct() %>%
        nrow()

      relationship_count_rows <-
        nrow(omop_relationships[[vocabulary_id]]$relationship_counts)

      if (relationship_rows != relationship_count_rows) {

        omop_relationship_errors[[length(omop_relationship_errors)+1]] <-
          list(relationship_rows = relationship_rows,
               relationship_count_rows = relationship_count_rows)
        names(omop_relationship_errors)[length(omop_relationship_errors)] <-
          vocabulary_id

      }


    }

    if (length(omop_relationship_errors)>0) {

      cli::cli_warn("{length(omop_relationship_errors)} error{?s} in relationship counts found: {names(omop_relationship_errors)}!")
      return(omop_relationship_errors)


    }


    omop_relationships1 <-
    omop_relationships %>%
      purrr::map(purrr::reduce, dplyr::left_join, by = c("relationship_id", "vocabulary_id_1", "concept_class_id_1", "vocabulary_id_2", "concept_class_id_2")) %>%
      purrr::map(dplyr::distinct) %>%
      dplyr::bind_rows()


    omop_relationships2 <-
      omop_relationships1 %>%
      dplyr::left_join(total_vocabulary_ct,
                       by = c("vocabulary_id_1" = "vocabulary_id")) %>%
      dplyr::distinct() %>%
      dplyr::rename(total_vocabulary_ct_1 = total_vocabulary_ct) %>%
      dplyr::left_join(total_vocabulary_ct,
                       by = c("vocabulary_id_2" = "vocabulary_id")) %>%
      dplyr::distinct() %>%
      dplyr::rename(total_vocabulary_ct_2 = total_vocabulary_ct) %>%
      dplyr::distinct()

    if (nrow(omop_relationships2) != nrow(omop_relationships1)) {

      cli::cli_warn("Duplicates introduced when joining `total_vocabulary_ct` with `omop_relationships`!")
      return(list(total_vocabulary_ct = total_vocabulary_ct,
                  omop_relationships = omop_relationships1,
                  omop_relationships2 = omop_relationships2))

    }

    omop_relationships3 <-
      omop_relationships2


    omop_relationships4 <-
      omop_relationships3 %>%
      dplyr::left_join(total_concept_class_ct,
                       by = c("vocabulary_id_1" = "vocabulary_id",
                              "concept_class_id_1" = "concept_class_id")) %>%
      dplyr::distinct() %>%
      dplyr::rename(total_concept_class_ct_1 = total_concept_class_ct) %>%
      dplyr::left_join(total_concept_class_ct,
                       by = c("vocabulary_id_2" = "vocabulary_id",
                              "concept_class_id_2" = "concept_class_id")) %>%
      dplyr::distinct() %>%
      dplyr::rename(total_concept_class_ct_2 = total_concept_class_ct) %>%
      dplyr::distinct()

    if (nrow(omop_relationships4) != nrow(omop_relationships3)) {

      cli::cli_warn("Duplicates introduced when joining `total_vocabulary_ct` with `omop_relationships`!")
      return(list(total_concept_class_ct = total_concept_class_ct,
                  omop_relationships = omop_relationships3,
                  omop_relationships2 = omop_relationships4))

    }

    new("omop.relationships",
        data =
    omop_relationships4 %>%
      dplyr::select(
        relationship_id,
        relationship_name,
        is_hierarchical,
        defines_ancestry,
        domain_id_1,
        vocabulary_id_1,
        concept_class_id_1,
        standard_concept_1,
        concept_count_1,
        total_concept_class_ct_1,
        total_vocabulary_ct_1,
        domain_id_2,
        vocabulary_id_2,
        concept_class_id_2,
        standard_concept_2,
        concept_count_2,
        total_concept_class_ct_2,
        total_vocabulary_ct_2))

  }




create_nodes_and_edges <-
  function(omop_relationships,
           type_from = concept_class_id,
           label_glue = "{vocabulary_id}\n{concept_class_id}\n({standard_concept})\n") {


    ccr_df <- omop_relationships@data

    type_from <- dplyr::enquo(type_from)

    omop_node <-
      dplyr::bind_rows(
        ccr_df %>%
          dplyr::select(ends_with("_1")) %>%
          dplyr::rename_all(stringr::str_remove_all, "_1"),
        ccr_df %>%
          dplyr::select(ends_with("_2")) %>%
          dplyr::rename_all(stringr::str_remove_all, "_2")) %>%
      dplyr::distinct() %>%
      tibble::rowid_to_column("id") %>%
      dplyr::mutate(type = !!type_from) %>%
      dplyr::mutate(label = glue::glue(label_glue))


    # Add label_1 and label_2 fields
    omop_edge <-
      dplyr::bind_cols(
        ccr_df %>%
          dplyr::select(dplyr::ends_with("_1")) %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with("_1")),
                           stringr::str_remove_all, "_1") %>%
          dplyr::mutate(label_1 = glue::glue(label_glue)) %>%
          dplyr::select(label_1),
        ccr_df %>%
          dplyr::select(dplyr::ends_with("_2")) %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with("_2")),
                           stringr::str_remove_all, "_2") %>%
          dplyr::mutate(label_2 = glue::glue(label_glue)) %>%
          dplyr::select(label_2),
        ccr_df)


    # Join by any matches to "(^.*)_[1,2]"

    omop_edge <-
      omop_edge %>%
      dplyr::left_join(omop_node %>%
                         dplyr::rename(from = id) %>%
                         dplyr::rename_at(dplyr::vars(!from),
                                          ~paste0(., "_1")),
                       by = c("label_1", "domain_id_1", "vocabulary_id_1", "concept_class_id_1", "standard_concept_1", "concept_count_1", "total_concept_class_ct_1", "total_vocabulary_ct_1")) %>%
      dplyr::distinct() %>%
      dplyr::left_join(omop_node %>%
                         dplyr::rename(to = id) %>%
                         dplyr::rename_at(dplyr::vars(!to),
                                          ~paste0(., "_2")),
                       by = c("label_2", "domain_id_2", "vocabulary_id_2", "concept_class_id_2", "standard_concept_2", "concept_count_2", "total_concept_class_ct_2", "total_vocabulary_ct_2")) %>%
      dplyr::distinct()


    if (nrow(ccr_df) != nrow(omop_edge)) {


      cli::cli_warn("Modified edges contains different row count than provided edges.")

      return(list(edges = ccr_df,
                  modified_edges = omop_edge))

    }

    omopNode <-
      new("nodes",
          data = omop_node)

    omopEdge <-
      new("edges",
          data = omop_edge)
    new("nodes.and.edges",
        nodes = omopNode,
        edges = omopEdge)

    }
