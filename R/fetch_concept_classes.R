#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION
#' @param type_from PARAM_DESCRIPTION, Default: concept_class_id
#' @param label_glue PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[pg13]{query}}
#'  \code{\link[R.cache]{loadCache}},\code{\link[R.cache]{saveCache}}
#'  \code{\link[cli]{cli_progress_bar}}
#'  \code{\link[tidyr]{extract}}
#'  \code{\link[glue]{glue}}
#' @rdname fetch_concept_classes
#' @export
#' @importFrom pg13 query
#' @importFrom R.cache loadCache saveCache
#' @importFrom cli cli_progress_bar cli_progress_update
#' @importFrom tidyr extract
#' @importFrom glue glue
#' @import dplyr
#' @import tibble
#' @import stringr

fetch_concept_classes <-
  function(conn,
           conn_fun = "pg13::local_connect(verbose=FALSE)",
           type_from = concept_class_id,
           label_glue = "{vocabulary_id}\n{concept_class_id}\n({standard_concept})\n",
           schema = "omop_vocabulary",
           verbose = FALSE,
           render_sql = FALSE) {

    sql_statement <-
      c(
      "SELECT * ",
      "FROM public.setup_athena_log",
      "WHERE sa_datetime IN (SELECT MAX(sa_datetime) FROM public.setup_athena_log);") %>%
      paste(collapse = "\n")

    version <-
      pg13::query(conn = conn,
                  conn_fun = conn_fun,
                  checks = "",
                  sql_statement = sql_statement,
                  verbose = FALSE,
                  render_sql = FALSE)
    version <- as.list(version)

    ccr_df <-
      R.cache::loadCache(
        key = c("ccr_df", version),
        dirs = "chariot2"
      )

    if (is.null(ccr_df)) {

      vocabulary_ids <-
        pg13::query(conn = conn,
                    conn_fun = conn_fun,
                    checks   = "",
                    sql_statement =
                      glue::glue(
                        "SELECT ",
                        "  vocabulary_id,",
                        "  COUNT(*) ",
                        "FROM {schema}.concept ",
                        "GROUP BY vocabulary_id ",
                        "ORDER BY COUNT(*)",
                        .sep = "\n"),
                    verbose = FALSE,
                    render_sql = FALSE) %>%
        select(vocabulary_id) %>%
        unlist() %>%
        unname()

      output <-
        vector(mode = "list",
               length =
                 length(vocabulary_ids))
      names(output) <-
        vocabulary_ids

      cli::cli_progress_bar(
        format = "\nQuerying {vocabulary_id} | {pb_bar} {pb_current}/{pb_total} {pb_percent} ({pb_elapsed})\n",
        clear = FALSE,
        total = length(vocabulary_ids))

      for (vocabulary_id in vocabulary_ids) {
        sql <-
          as.character(
            glue(
              "SELECT DISTINCT ",
              "  cr.relationship_id,",
              "  r.relationship_name,",
              "  r.is_hierarchical,",
              "  r.defines_ancestry,",
              "  c.domain_id AS domain_id_1,",
              "  c.vocabulary_id AS vocabulary_id_1,",
              "  c.concept_class_id AS concept_class_id_1,",
              "  c.standard_concept AS standard_concept_1,",
              "  c2.domain_id AS domain_id_2,",
              "  c2.vocabulary_id AS vocabulary_id_2,",
              "  c2.concept_class_id AS concept_class_id_2,",
              "  c2.standard_concept AS standard_concept_2  ",
              "FROM omop_vocabulary.concept_relationship cr ",
              "INNER JOIN omop_vocabulary.concept c ",
              "ON c.concept_id = cr.concept_id_1 ",
              "INNER JOIN omop_vocabulary.concept c2 ",
              "ON c2.concept_id = cr.concept_id_2 ",
              "INNER JOIN omop_vocabulary.relationship r ",
              "ON r.relationship_id = cr.relationship_id ",
              "WHERE ",
              "c.vocabulary_id = '{vocabulary_id}' AND ",
              "c.concept_class_id <> c2.concept_class_id AND ",
              "cr.invalid_reason IS NULL AND ",
              "c.invalid_reason IS NULL AND ",
              "c2.invalid_reason IS NULL;",
              .sep = "\n"
            ))


        vocabulary_id_data <-
          R.cache::loadCache(
            key = c(sql, version),
            dirs = "chariot2"
          )

        if (is.null(vocabulary_id_data)) {

          vocabulary_id_data <-
            pg13::query(
              conn = conn,
              checks = "",
              conn_fun = conn_fun,
              sql_statement = sql,
              verbose = verbose,
              render_sql = render_sql)

          R.cache::saveCache(
            object = vocabulary_id_data,
            key    = c(sql, version),
            dirs   = "chariot2"
          )

        }

        output[[vocabulary_id]] <-
          vocabulary_id_data

        cli::cli_progress_update()


      }

      ccr_df <-
        dplyr::bind_rows(output) %>%
        tidyr::extract(col = relationship_name,
                       into = "relationship_source",
                       regex = "^.*[(]{1}(.*?)[)]{1}",
                       remove = FALSE)

      R.cache::saveCache(object = ccr_df,
                         key = c("ccr_df", version),
                         dirs = "chariot2")
    }



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
            ccr_df) %>%
          dplyr::left_join(omop_node %>%
                             dplyr::select(from = id, label),
                    by = c("label_1" = "label")) %>%
          dplyr::left_join(omop_node %>%
                             dplyr::select(to = id, label),
                    by = c("label_2" = "label"))

        omopNode <-
          new("nodes",
              data = omop_node)

        omopEdge <-
          new("edges",
              data = omop_edge)
        new("nodes.and.edges",
            nodes = omopNode,
            edges = omopEdge)
        # list(node = omop_node,
        #      edge = omop_edge)

  }




