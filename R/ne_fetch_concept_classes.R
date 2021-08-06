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
#' @rdname ne_fetch_concept_classes
#' @export
#' @importFrom pg13 query
#' @importFrom R.cache loadCache saveCache
#' @importFrom cli cli_progress_bar cli_progress_update
#' @importFrom tidyr extract
#' @importFrom glue glue

ne_fetch_concept_classes <-
  function(conn,
           conn_fun,
           type_from = concept_class_id,
           label_glue = "{vocabulary_id}\n{concept_class_id}\n({standard_concept})\n") {
    sql_statement <-
      "SELECT
      *
   FROM public.setup_athena_log
   WHERE sa_datetime IN (SELECT max(sa_datetime) from public.setup_athena_log);"

    version <-
      pg13::query(conn = conn,
                  conn_fun = conn_fun,
                  sql_statement = sql_statement)

    ccr_df <-
    R.cache::loadCache(
      key = as.list(version),
      dirs = "chariot2"
    )

    if (is.null(ccr_df)) {

      vocabulary_ids <-
        queryAthena("SELECT vocabulary_id,COUNT(*) FROM omop_vocabulary.concept GROUP BY vocabulary_id ORDER BY COUNT(*)") %>%
        select(vocabulary_id) %>%
        unlist() %>%
        unname()


      output <-
        vector(mode = "list",
               length = length(vocabulary_ids))
      names(output) <-
        vocabulary_ids

      cli::cli_progress_bar(format = "Querying {vocabulary_id} | {pb_bar} {pb_current}/{pb_total} {pb_percent} ({pb_elapsed})", clear = FALSE, total = length(vocabulary_ids))

      for (vocabulary_id in vocabulary_ids) {
    sql <-
      as.character(
        glue(
          "    SELECT DISTINCT cr.relationship_id, r.relationship_name, r.is_hierarchical, r.defines_ancestry, c.domain_id AS domain_id_1, c.vocabulary_id AS vocabulary_id_1, c.concept_class_id AS concept_class_id_1, c.standard_concept AS standard_concept_1, c2.domain_id AS domain_id_2, c2.vocabulary_id AS vocabulary_id_2, c2.concept_class_id AS concept_class_id_2, c2.standard_concept AS standard_concept_2  ",
          "    FROM omop_vocabulary.concept_relationship cr ",
          "    INNER JOIN omop_vocabulary.concept c ",
          "    ON c.concept_id = cr.concept_id_1 ",
          "    INNER JOIN omop_vocabulary.concept c2 ",
          "    ON c2.concept_id = cr.concept_id_2 ",
          "    INNER JOIN omop_vocabulary.relationship r ",
          "    ON r.relationship_id = cr.relationship_id ",
          "    WHERE ",
          "      c.vocabulary_id = '{vocabulary_id}' AND ",
          "      c.concept_class_id <> c2.concept_class_id AND ",
          "      cr.invalid_reason IS NULL AND ",
          "      c.invalid_reason IS NULL AND ",
          "      c2.invalid_reason IS NULL;",
          .sep = "\n"
        ))


    cli::cli_progress_update()

    output[[vocabulary_id]] <-
      queryAthena(sql)


      }

      ccr_df <-
        bind_rows(output) %>%
        tidyr::extract(col = relationship_name,
                       into = "relationship_source",
                       regex = "^.*[(]{1}(.*?)[)]{1}",
                       remove = FALSE)

      R.cache::saveCache(object = ccr_df,
                         key = as.list(version),
                         dirs = "chariot2")
    }



        type_from <- enquo(type_from)

        omop_node <-
          bind_rows(
            ccr_df %>%
              select(ends_with("_1")) %>%
              rename_all(str_remove_all, "_1"),
            ccr_df %>%
              select(ends_with("_2")) %>%
              rename_all(str_remove_all, "_2")) %>%
          distinct() %>%
          rowid_to_column("id") %>%
          mutate(type = !!type_from) %>%
          mutate(label = glue::glue(label_glue))

        omop_edge <-
          bind_cols(
            ccr_df %>%
              select(ends_with("_1")) %>%
              rename_at(vars(ends_with("_1")),
                        str_remove_all, "_1") %>%
              mutate(label_1 = glue::glue(label_glue)) %>%
              select(label_1),
            ccr_df %>%
              select(ends_with("_2")) %>%
              rename_at(vars(ends_with("_2")),
                        str_remove_all, "_2") %>%
              mutate(label_2 = glue::glue(label_glue)) %>%
              select(label_2),
            ccr_df) %>%
          left_join(omop_node %>%
                      select(from = id, label),
                    by = c("label_1" = "label")) %>%
          left_join(omop_node %>%
                      select(to = id, label),
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




