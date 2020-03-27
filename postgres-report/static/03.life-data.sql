select s.schemaname, s.relname, n_live_tup, n_dead_tup, last_autovacuum, autovacuum_count
from pg_stat_user_tables s
order by n_dead_tup desc;
