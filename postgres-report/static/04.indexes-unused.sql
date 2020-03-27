select * from pg_stat_all_indexes where idx_scan = 0 and not schemaname like 'pg_%'
order by schemaname, relname;
