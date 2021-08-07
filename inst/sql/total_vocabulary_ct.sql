SELECT
  vocabulary_id,
  COUNT(DISTINCT concept_id) AS total_vocabulary_ct
FROM {schema}.concept
WHERE invalid_reason IS NULL
GROUP BY vocabulary_id, concept_class_id
ORDER BY COUNT(DISTINCT concept_id);
