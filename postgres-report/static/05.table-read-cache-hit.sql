select schemaname, relname, indexrelname,
  (idx_blks_hit+idx_blks_read) total_reads,
  case when idx_blks_read+idx_blks_hit = 0 then null else
    (100*idx_blks_hit)/(idx_blks_hit+idx_blks_read) end
  cache_hit
from pg_statio_all_indexes
where not schemaname like 'pg_%'
  order by schemaname, (idx_blks_hit+idx_blks_read) desc;
