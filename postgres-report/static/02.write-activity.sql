select s.schemaname, s.relname,
  pg_size_pretty(pg_relation_size(relid)),
  coalesce(n_tup_ins,0) + 2 * coalesce(n_tup_upd,0) - coalesce(n_tup_hot_upd,0) + coalesce(n_tup_del,0) AS total_writes,
  (coalesce(n_tup_hot_upd,0)::float * 100 / (case when n_tup_upd > 0 then n_tup_upd else 1 end)::float)::numeric(10,2) AS hot_rate,
  coalesce(substring(array_to_string(reloptions, ' ')
          FROM 'fillfactor=([0-9]+)')::smallint, 100) AS fillfactor
from pg_stat_user_tables s join pg_class c ON c.oid=relid
  where schemaname not like 'pg_%'
order by pg_relation_size(relid) desc;
