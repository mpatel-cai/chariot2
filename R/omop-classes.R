concept.class.relationships <-
  setClass(
    Class = "concept.class.relationships",
    slots = c(data = "data.frame",
              required_fields = "character"),
    prototype = list(data = tibble::tibble(),
                     required_fields =
                       c('relationship_id',
                         'relationship_name',
                         'relationship_source',
                         'is_hierarchical',
                         'defines_ancestry',
                         'domain_id_1',
                         'vocabulary_id_1',
                         'concept_class_id_1',
                         'standard_concept_1',
                         'domain_id_2',
                         'vocabulary_id_2',
                         'concept_class_id_2',
                         'standard_concept_2')
    ))



