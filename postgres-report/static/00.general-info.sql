-- Data anomalies.
--
-- hit_ration > 90%
-- c_commit_ratio should be > 95%
-- c_rollback_ratio should be < 5%
-- deadlocks should be close to 0
-- conflicts should be close to 0
-- temp_files and temp_bytes watch out for them
select datname,
       stats_reset,
       (xact_rollback*100)/nullif(xact_commit+xact_rollback, 0) as c_rollback_ratio,
       case when blks_hit+blks_read = 0 then null
            else blks_hit*100/(blks_hit+blks_read)
           end as hit_ratio,
       xact_commit,
       (xact_commit*100)/nullif(xact_commit+xact_rollback,0) as c_commit_ratio,
       xact_rollback,
       deadlocks,
       conflicts,
       tup_deleted,
       temp_files,
       pg_size_pretty(temp_bytes) temp_bytes
from pg_stat_database;
