SELECT DISTINCT
  cr.relationship_id,
  c.vocabulary_id AS vocabulary_id_1,
  c.concept_class_id AS concept_class_id_1,
  COUNT(DISTINCT c.concept_id) AS concept_count_1,
  c2.vocabulary_id AS vocabulary_id_2,
  c2.concept_class_id AS concept_class_id_2,
  COUNT(DISTINCT c2.concept_id) AS concept_count_2
FROM (SELECT * FROM {schema}.concept WHERE vocabulary_id = '{vocabulary_id}' AND invalid_reason IS NULL) c
INNER JOIN (SELECT * FROM {schema}.concept_relationship WHERE invalid_reason IS NULL) cr
ON c.concept_id = cr.concept_id_1
INNER JOIN (SELECT * FROM {schema}.concept WHERE invalid_reason IS NULL) c2
ON c2.concept_id = cr.concept_id_2
WHERE
c.concept_class_id <> c2.concept_class_id
GROUP BY
  cr.relationship_id,
  c.vocabulary_id,
  c.concept_class_id,
  c2.vocabulary_id,
  c2.concept_class_id
;
