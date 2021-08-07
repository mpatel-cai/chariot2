SELECT
  vocabulary_id,
  concept_class_id,
  COUNT(DISTINCT concept_id) AS total_concept_class_ct
FROM {schema}.concept
WHERE invalid_reason IS NULL
GROUP BY vocabulary_id, concept_class_id;
