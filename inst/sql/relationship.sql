SELECT DISTINCT
  cr.relationship_id,
  r.relationship_name,
  r.is_hierarchical,
  r.defines_ancestry,
  c.domain_id AS domain_id_1,
  c.vocabulary_id AS vocabulary_id_1,
  c.concept_class_id AS concept_class_id_1,
  c.standard_concept AS standard_concept_1,
  c2.domain_id AS domain_id_2,
  c2.vocabulary_id AS vocabulary_id_2,
  c2.concept_class_id AS concept_class_id_2,
  c2.standard_concept AS standard_concept_2
FROM (SELECT * FROM {schema}.concept WHERE vocabulary_id = '{vocabulary_id}' AND invalid_reason IS NULL) c
INNER JOIN (SELECT * FROM {schema}.concept_relationship WHERE invalid_reason IS NULL) cr
ON c.concept_id = cr.concept_id_1
INNER JOIN (SELECT * FROM {schema}.concept WHERE invalid_reason IS NULL) c2
ON c2.concept_id = cr.concept_id_2
INNER JOIN {schema}.relationship r
ON r.relationship_id = cr.relationship_id
WHERE
c.concept_class_id <> c2.concept_class_id;
