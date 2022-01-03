
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_sql_template
#' @export
read_sql_template <-
  function(file) {
      readLines(
        system.file(package = "chariot2",
                    "sql",
                    file)) %>%
      paste(collapse = "\n")
  }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect(verbose=FALSE)'
#' @param log_schema PARAM_DESCRIPTION, Default: 'public'
#' @param log_table PARAM_DESCRIPTION, Default: 'setup_athena_log'
#' @param log_timestamp_field PARAM_DESCRIPTION, Default: 'sa_datetime'
#' @param template_only PARAM_DESCRIPTION, Default: FALSE
#' @param sql_only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[pg13]{query}}
#' @rdname get_version_key
#' @export
#' @importFrom glue glue
#' @importFrom pg13 query
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param resultset PARAM_DESCRIPTION
#' @param sql PARAM_DESCRIPTION
#' @param version_key PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[R.cache]{saveCache}}
#' @rdname save_to_cache
#' @export
#' @importFrom R.cache saveCache
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sql PARAM_DESCRIPTION
#' @param version_key PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[R.cache]{loadCache}}
#' @rdname load_from_cache
#' @export
#' @importFrom R.cache loadCache
load_from_cache <-
  function(sql,
           version_key) {

      R.cache::loadCache(
        key = c(sql, version_key),
        dirs = "chariot2")

  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect(verbose=FALSE)'
#' @param type_from PARAM_DESCRIPTION, Default: concept_class_id
#' @param schema PARAM_DESCRIPTION, Default: 'omop_vocabulary'
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param version_key PARAM_DESCRIPTION, Default: get_version_key()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[pg13]{query}}
#'  \code{\link[dplyr]{arrange}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{rename}}
#'  \code{\link[cli]{cli_progress_bar}},\code{\link[cli]{cli_abort}}
#'  \code{\link[purrr]{transpose}},\code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#' @rdname fetch_omop
#' @export
#' @importFrom glue glue
#' @importFrom pg13 query
#' @importFrom dplyr arrange distinct select any_of left_join bind_rows rename
#' @import cli
#' @importFrom purrr transpose map reduce
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

    omop_relationships5 <-
      omop_relationships4 %>%
      tidyr::extract(col = "relationship_name",
                     into = "relationship_source",
                     remove = FALSE,
                     regex = "^.*[(]{1}(.*?)[)]{1}")

    new("omop.relationships",
        data =
    omop_relationships5 %>%
      dplyr::select(
        relationship_id,
        relationship_name,
        relationship_source,
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




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param omop_relationships PARAM_DESCRIPTION
#' @param type_from PARAM_DESCRIPTION, Default: concept_class_id
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{rename}}
#'  \code{\link[stringr]{str_remove}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[cli]{cli_abort}}
#' @rdname create_nodes_and_edges
#' @export
#' @importFrom dplyr enquo bind_rows select rename_all distinct mutate bind_cols ends_with rename_at vars left_join rename
#' @importFrom stringr str_remove_all
#' @importFrom tibble rowid_to_column
#' @importFrom glue glue
#' @importFrom cli cli_warn
create_nodes_and_edges <-
  function(omop_relationships,
           type_from = concept_class_id,
           label_glue = "{vocabulary_id}\n{concept_class_id}\n({standard_concept})\n",
           edge_label_from = relationship_name) {


    ccr_df <- omop_relationships@data
    edge_label_from <- dplyr::enquo(edge_label_from)
    type_from <- dplyr::enquo(type_from)

    omop_node <-
      dplyr::bind_rows(
        ccr_df %>%
          dplyr::select(ends_with("_1")) %>%
          dplyr::rename_all(stringr::str_remove_all, "_1"),
        ccr_df %>%
          dplyr::select(ends_with("_2")) %>%
          dplyr::rename_all(stringr::str_remove_all, "_2")) %>%
      dplyr::select(-concept_count) %>%
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
                       by = c("label_1", "domain_id_1", "vocabulary_id_1", "concept_class_id_1", "standard_concept_1", "total_concept_class_ct_1", "total_vocabulary_ct_1")) %>%
      dplyr::distinct() %>%
      dplyr::left_join(omop_node %>%
                         dplyr::rename(to = id) %>%
                         dplyr::rename_at(dplyr::vars(!to),
                                          ~paste0(., "_2")),
                       by = c("label_2", "domain_id_2", "vocabulary_id_2", "concept_class_id_2", "standard_concept_2", "total_concept_class_ct_2", "total_vocabulary_ct_2")) %>%
      dplyr::distinct() %>%
      mutate(concept_1_coverage_frac = glue::glue("{concept_count_1}/{total_concept_class_ct_1}"),
             concept_2_coverage_frac = glue::glue("{concept_count_2}/{total_concept_class_ct_2}")) %>%
      mutate(tailtooltip = map(concept_1_coverage_frac, function(x) scales::percent(eval(rlang::parse_expr(x))))) %>%
      mutate(headtooltip = map(concept_2_coverage_frac, function(x) scales::percent(eval(rlang::parse_expr(x))))) %>%
      mutate(tailtooltip = unlist(tailtooltip)) %>%
      mutate(headtooltip = unlist(headtooltip)) %>%
      mutate(label = !!edge_label_from)


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
