#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (p: [p.hasql p.blaze-colonnade p.vector p.text p.bytestring p.hasql-th p.lens ])"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import qualified Control.Foldl as Foldl
import Control.Lens
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Foldable
import Data.Maybe
import Data.Scientific
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder.RealFloat
import qualified Data.ByteString as B
import qualified Data.Vector as V
import GHC.Stack
import Hasql.Connection
import Hasql.Session
import Hasql.TH
import System.Environment
import Colonnade hiding (fromMaybe)
import System.FilePath
import Text.Blaze.Html5 hiding (map, head, main)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Colonnade
import TextShow (showt, toLazyText)

main :: IO ()
main = bracket connect disconnect $ \conn -> do
  tables <- throwLeft $ flip run conn $ sequence $ reports <&> \(n, r) -> do
     x <- r
     pure (n,x) <$> r
  for_ tables $ \(n, table) -> do
     let file_name = mkPath $ "tmp" </> n <> ".html"
     B.writeFile file_name ""
     renderMarkupToByteStringIO (B.appendFile file_name)
       $ docTypeHtml $ do
           H.head $ do
              title $ "DB report " <> toMarkup n
              H.link ! A.rel "stylesheet"
                     ! A.type_ "text/css"
                     ! A.href "./main.css"
           body $ do
             h1 $ "DB: " <> toMarkup n
             H.div $ do
               a ! A.href "./index.html" $ "index"
             table
  B.writeFile "tmp/index.html" ""
  renderMarkupToByteStringIO (B.appendFile "tmp/index.html")
    $ docTypeHtml $ do
        H.head $ do
           title "DB report"
           H.link ! A.rel "stylesheet"
                  ! A.type_ "text/css"
                  ! A.href "./main.css"
        body $ do
          h1 "DB"
          ul $ do
            for_ tables $ \(n, _) -> do
              li $ do
                 a ! A.href (fromString $ mkPath $ n <> ".html") $ toMarkup n
 where
   connect = do
     connect_to <- BS8.pack . fromJust <$> lookupEnv "PG_HOST" 
     throwLeft $ acquire connect_to
   disconnect = release

mkPath = map (\case
  ' ' -> '_'
  c -> c)


mkReport1 attrs f mkHeader stmt = statement () stmt <&> \x ->
   let vars = f x
   in encodeCellTable attrs (mconcat $ mkHeader vars) x

mkReport attrs hdr stmt =
   encodeHtmlTable attrs (mconcat hdr) <$> statement () stmt

throwLeft :: HasCallStack => Show e => IO (Either e a) -> IO a
throwLeft f = f >>= \case
  Left e -> throwIO $ userError $ show e
  Right x -> pure x

greenToRed min_v max_v current_v min_a max_a current_a = textValue
    $ "background-color: rgba(" <> showt red <> ", " <> showt green <> ", 0, "<> prettyDouble alpha <> ")"
  where
    red = round @_ @Int64 $ 255 * (1-p_v)
    green = round @_ @Int64 $ 255 * p_v
    p_v = min 1 (max 0 ((current_v-min_v) / (max_v-min_v)))
    alpha = min 1 (max 0 ((current_a-min_a) / (max_a - min_a)))

prettyDouble = TL.toStrict . toLazyText . formatRealFloat Fixed (Just 2)

n =>> s = (n,s)
infixr 0 =>>

reports =
  [ "Relation size" =>>
    mkReport mempty
      [ headed "relation name" (^._1.to(toHtml))
      , headed "relation size" (^._2.to(toHtml))
      ]
      [vectorStatement|
         SELECT (nspname || '.' || relname)::text AS relation,
            (pg_size_pretty(pg_total_relation_size(C.oid)))::text AS total_size
         FROM pg_class C
         LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
         WHERE nspname NOT IN ('pg_catalog', 'information_schema') AND C.relkind <> 'i' AND nspname !~ '^pg_toast'
         ORDER BY pg_total_relation_size(C.oid) DESC
      |]
  , "Writes" =>>
    mkReport1 mempty
      (maximum1Of (folded._4))
      (\total_writes -> 
        [ headed "schema"         (^._1.to(htmlCell.toHtml))
        , headed "relation"       (^._2.to(htmlCell.toHtml))
        , headed "pg_size_pretty" (^._3.to(htmlCell.toHtml))
        , headed "total_writes"   (^._4.to(htmlCell.toHtml))
        , headed "hot rate"
           (\v -> 
              let rate = v^._5
                  writes = v^._4
              in (htmlCell (toMarkup $ prettyDouble rate))
                   { cellAttribute = A.style (greenToRed 0 100 rate 0 (fromIntegral total_writes) (fromIntegral writes))
                   }
           )
        , headed "fillfactor"     (^._6.to(htmlCell.toHtml))
        ])
      $ fmap (V.filter (^._2.to(not.T.isPrefixOf "mv_")))
      [vectorStatement|
         select
           s.schemaname::text,
           s.relname::text,
           pg_size_pretty(pg_relation_size(relid)) :: text,
           (coalesce(n_tup_ins,0) + 2 * coalesce(n_tup_upd,0) - coalesce(n_tup_hot_upd,0) + coalesce(n_tup_del,0)) ::int8 AS total_writes,
           (coalesce(n_tup_hot_upd,0)::float8 * 100 / (case when n_tup_upd > 0 then n_tup_upd else 1 end)::float8)::float8 AS hot_rate,
         coalesce(substring(array_to_string(reloptions, ' ') FROM 'fillfactor=([0-9]+)')::int4, 100)::int8 AS fillfactor
         from pg_stat_user_tables s join pg_class c ON c.oid=relid
         where schemaname not like 'pg_%'
         order by s.schemaname, pg_relation_size(relid) desc
      |]
   , "dead data" =>>
     mkReport mempty
       [ headed "schema"          (^._1.to(toHtml))
       , headed "relation"        (^._2.to(toHtml))
       , headed "live_tuples"     (^._3.to(toHtml))
       , headed "dead_tuples"     (^._4.to(toHtml))
       , headed "last_autovacum"  (^._5.to(maybe mempty (toHtml.show)))
       , headed "last_age"        (^._6.to(maybe mempty (toHtml.show)))
       , headed "count"           (^._7.to(toHtml))
       ]
       [vectorStatement|
          select s.schemaname :: text,
                 s.relname :: text,
                 n_live_tup :: int8,
                 n_dead_tup :: int8,
                 last_autovacuum :: timestamptz?,
                 age(now(),last_autovacuum) :: interval?,
                 autovacuum_count :: int8
          from pg_stat_user_tables s
          where n_dead_tup>0
          order by n_dead_tup desc
          |]
   , "index hit rate" =>>
     mkReport1 mempty 
      (Foldl.foldOver (each._4) ((,) <$> Foldl.premap fromIntegral Foldl.mean <*> Foldl.maximum))
      (\(mean_reads, max_reads) ->
         [ headed "schema"          (^._1.to(htmlCell.toHtml))
         , headed "relation"        (^._2.to(htmlCell.toHtml))
         , headed "index"           (^._3.to(htmlCell.toHtml))
         , headed "total_reads"     (^._4.to(htmlCell.toHtml))
         , headed "cache_hit"       
         $ \v ->
          let hit = v ^._5 
              current_reads = v ^. _4
          in (htmlCell (toMarkup hit))
               { cellAttribute = A.style (greenToRed 90 100 hit
                   (mean_reads)
                   (maybe 0 fromIntegral max_reads)
                   (fromIntegral current_reads))
               }
         ])
      [vectorStatement|
        select schemaname :: text,
               relname :: text,
               indexrelname :: text,
               (idx_blks_hit+idx_blks_read)::int8 total_reads,
               (100*idx_blks_hit/nullif(idx_blks_hit+idx_blks_read,0))::float8 cache_hit
        from pg_statio_all_indexes
        where not schemaname like 'pg_%' and  (idx_blks_hit+idx_blks_read)>0
        order by schemaname, (idx_blks_hit+idx_blks_read) desc
        |]
    , "table reads cache hit rate" =>>
      mkReport1 mempty
        (Foldl.foldOver (each._3)
          ((,) <$> Foldl.premap fromIntegral Foldl.mean
               <*> Foldl.maximum))
        (\(mean_reads, max_reads) ->
           [ headed "schema"          (^._1.to(htmlCell.toHtml))
           , headed "relation"        (^._2.to(htmlCell.toHtml))
           , headed "total_reads"     (^._3.to(htmlCell.toHtml))
           , headed "cache_hit"       
           $ \v ->
            let hit = v ^._4 .to(fromMaybe(0))
                current_reads = v ^. _3
            in (htmlCell (toMarkup hit))
                 { cellAttribute = A.style (greenToRed 90 100 hit
                     (mean_reads)
                     (maybe 0 fromIntegral max_reads)
                     (fromIntegral current_reads))
                 }
           ])
        [vectorStatement|
           select schemaname::text, relname::text,
                  (heap_blks_hit+heap_blks_read)::int8 total_reads,
                  (heap_blks_hit*100/nullif(heap_blks_hit+heap_blks_read,0))::float8? cache_hit
           from pg_statio_user_tables where heap_blks_hit+heap_blks_read>0
           order by schemaname, total_reads desc
        |]
    , "blocks read cache hit rate" =>>
      mkReport1 mempty
        (Foldl.foldOver (each._3)
          ((,) <$> Foldl.premap fromIntegral Foldl.mean
               <*> Foldl.maximum))
        (\(mean_reads, max_reads) ->
           [ headed "schema"          (^._1.to(htmlCell.toHtml))
           , headed "relation"        (^._2.to(htmlCell.toHtml))
           , headed "total_reads"     (^._3.to(htmlCell.toHtml))
           , headed "cache_hit"       
           $ \v ->
            let hit = v ^._4 .to(fromMaybe(0))
                current_reads = v ^. _3
            in (htmlCell (toMarkup hit))
                 { cellAttribute = A.style (greenToRed 90 100 hit
                     (mean_reads)
                     (maybe 0 fromIntegral max_reads)
                     (fromIntegral current_reads))
                 }
           ])
        [vectorStatement|
           select schemaname :: text,
                  relname :: text,
                  (idx_blks_hit+idx_blks_read) ::int8 total_index,
                  (idx_blks_hit*100/nullif(idx_blks_hit+idx_blks_read,0))::float8? index_hit
           from pg_statio_user_tables where (idx_blks_hit+idx_blks_read)>0
           order by schemaname, total_index desc
        |]
    , "toast read cache hit rate" =>>
      mkReport1 mempty
        (Foldl.foldOver (each._3)
          ((,) <$> Foldl.premap fromIntegral Foldl.mean
               <*> Foldl.maximum))
        (\(mean_reads, max_reads) ->
           [ headed "schema"          (^._1.to(htmlCell.toHtml))
           , headed "relation"        (^._2.to(htmlCell.toHtml))
           , headed "total_reads"     (^._3.to(htmlCell.toHtml))
           , headed "cache_hit"       
           $ \v ->
            let hit = v ^._4 .to(fromMaybe(0))
                current_reads = v ^. _3
            in (htmlCell (toMarkup hit))
                 { cellAttribute = A.style (greenToRed 90 100 hit
                     (mean_reads)
                     (maybe 0 fromIntegral max_reads)
                     (fromIntegral current_reads))
                 }
           ])
        [vectorStatement|
           select schemaname::text, relname::text,
                  (toast_blks_hit+toast_blks_read)::int8 total_toast,
                  (toast_blks_hit*100/(toast_blks_hit+toast_blks_read))::float8? toast_hit
           from pg_statio_user_tables where toast_blks_hit+toast_blks_read>0
           order by schemaname, total_toast desc
        |]
    , "does table needs an index" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "schema"          (^._1.to(htmlCell.toHtml))
           , headed "relation"        (^._2.to(htmlCell.toHtml))
           , headed "seq diff"        (^._3.to(maybe mempty (htmlCell.toHtml)))
           , headed "need index"      (^._4.to(htmlCell.toHtml))
           , headed "size"            (^._5.to(htmlCell.toHtml))
           , headed "size pretty"     (^._6.to(htmlCell.toHtml))
           , headed "seq scan"        (^._7.to(htmlCell.toHtml))
           , headed "index scan"      (^._8.to(maybe mempty (htmlCell.toHtml)))
           ])
        [vectorStatement|
           SELECT schemaname::text,
                  relname::text, (seq_scan-idx_scan)::int8? AS too_much_seq,
                (CASE WHEN seq_scan-idx_scan>0 THEN 'Missing Index?' ELSE 'OK' END)::text result,
                pg_relation_size((schemaname||'.'||relname)::regclass)::int8 AS rel_size,
                pg_size_pretty(pg_total_relation_size((schemaname||'.'||relname)::regclass))::text AS human_readable_size,
                seq_scan::int8, idx_scan::int8?
            FROM pg_stat_all_tables WHERE schemaname not like 'pg_%' AND pg_relation_size((schemaname||'.'||relname)::regclass)>80000
           ORDER BY too_much_seq DESC
        |]
    , "index usage" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "schema"              (^._1.to(htmlCell.toHtml))
           , headed "relation"            (^._2.to(htmlCell.toHtml))
           , headed "index_used, percent" (^._3.to(maybe mempty (htmlCell.toHtml.prettyDouble)))
           , headed "alive tuples"        (^._4.to(htmlCell.toHtml))
           ])
        [vectorStatement|
             SELECT schemaname::text, relname::text,
             (100 * idx_scan / nullif(seq_scan + idx_scan,0))::float8? percent_of_times_index_used,
             n_live_tup::int8 rows_in_table
             FROM pg_stat_user_tables
             where not schemaname like 'pg_%' and n_live_tup>1000
             order by percent_of_times_index_used asc
        |]
    , "sequential scans" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "schema"       (^._1.to(htmlCell.toHtml))
           , headed "relation"     (^._2.to(htmlCell.toHtml))
           , headed "size"         (^._3.to(htmlCell.toHtml))
           , headed "seq scan"     (^._4.to(htmlCell.toHtml))
           , headed "tuples read"  (^._5.to(htmlCell.toHtml))
           , headed "average read" (^._6.to(htmlCell.toHtml))
           ])
        [vectorStatement|
           select schemaname::text,
             relname::text,
             pg_size_pretty(pg_relation_size((schemaname||'.'||relname)::regclass))::text as size,
             seq_scan::int8,
             seq_tup_read::int8,
             (seq_tup_read / seq_scan)::float8 as seq_tup_avg
           from pg_stat_user_tables
           where seq_tup_read > 0
           order by pg_relation_size((schemaname||'.'||relname)::regclass) desc,seq_scan desc limit 50
        |]
    , "duplicate indices" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "size" (^._1.to(htmlCell.toHtml))
           , headed "idx1" (^._2.to(htmlCell.toHtml))
           , headed "idx2" (^._3.to(htmlCell.toHtml))
           , headed "idx3" (^._4.to(maybe mempty (htmlCell.toHtml)))
           , headed "idx4" (^._5.to(maybe mempty (htmlCell.toHtml)))
           ])
        [vectorStatement|
            SELECT pg_size_pretty(SUM(pg_relation_size(idx))::int8)::text AS SIZE,
                   (array_agg(idx))[1] ::text AS idx1, (array_agg(idx))[2] :: text AS idx2,
                   (array_agg(idx))[3] ::text? AS idx3, (array_agg(idx))[4] :: text? AS idx4
            FROM (
                     SELECT indexrelid::regclass AS idx, (indrelid::text || '\n' || indclass::text || '\n' || indkey::text || '\n' ||
                                                          COALESCE(indexprs::text,'')|| '\n' || COALESCE(indpred::text,'')) AS KEY
                     FROM pg_index) sub
            GROUP BY KEY HAVING COUNT(*)>1
            ORDER BY SUM(pg_relation_size(idx)) DESC
        |]
    , "index bloat" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "schema"          (^._1.to(htmlCell.toHtml))
           , headed "table"           (^._2.to(htmlCell.toHtml))
           , headed "wasted bytes, %" (^._3.to((htmlCell.toHtml.prettyDouble)))
           , headed "toast bloat"     (^._4.to(htmlCell.toHtml))
           , headed "index name"      (^._5.to((htmlCell.toHtml)))
           , headed "index bloat"     (^._6.to((htmlCell.toHtml.prettyDouble)))
           , headed "wasted bytes"    (^._7.to((htmlCell.toHtml)))
           ])
        [vectorStatement|
SELECT
    schemaname::text,
    tablename::text,
    (CASE WHEN otta=0 THEN 0.0 ELSE sml.relpages::float8/otta END)::float8 AS tbloat,
    (CASE WHEN relpages < otta THEN 0 ELSE bs*(sml.relpages-otta) END)::int8 AS wastedbytes,
    iname::text,
    (CASE WHEN iotta=0 OR ipages=0 THEN 0.0 ELSE ipages::float8/iotta END)::float8 AS ibloat,
    (CASE WHEN ipages < iotta THEN 0 ELSE bs*(ipages-iotta) END)::int8 AS wastedibytes
FROM (
         SELECT
             schemaname, tablename, cc.reltuples, cc.relpages, bs,
             CEIL((cc.reltuples*((datahdr+ma-
                                  (CASE WHEN datahdr%ma=0 THEN ma ELSE datahdr%ma END))+nullhdr2+4))/(bs-20::float8)) AS otta,
             COALESCE(c2.relname,'?') AS iname, COALESCE(c2.reltuples,0) AS ituples, COALESCE(c2.relpages,0) AS ipages,
             COALESCE(CEIL((c2.reltuples*(datahdr-12))/(bs-20::float8)),0) AS iotta
         FROM (
                  SELECT
                      ma,bs,schemaname,tablename,
                      (datawidth+(hdr+ma-(CASE WHEN hdr%ma=0 THEN ma ELSE hdr%ma END)))::int8 AS datahdr, (maxfracsum*(nullhdr+ma-(CASE WHEN nullhdr%ma=0 THEN ma ELSE nullhdr%ma END))) AS nullhdr2
                  FROM (
                           SELECT
                               schemaname, tablename, hdr, ma, bs,
                               SUM((1-"null_frac")*avg_width) AS datawidth,
                               MAX("null_frac") AS maxfracsum,
                               hdr+(
                                   SELECT 1+COUNT(*)/8
                                   FROM pg_stats s2
                                   WHERE "null_frac"<>0 AND s2.schemaname = s.schemaname AND s2.tablename = s.tablename
                               ) AS nullhdr
                           FROM pg_stats s, (
                               SELECT
                                   (SELECT current_setting('block_size')::float8) AS bs,
                                   CASE WHEN SUBSTRING(v,12,3) IN ('8.0','8.1','8.2') THEN 27 ELSE 23 END AS hdr,
                                   CASE WHEN v ~ 'mingw32' THEN 8 ELSE 4 END AS ma
                               FROM (SELECT version() AS v) AS foo
                           ) AS constants
                           GROUP BY 1,2,3,4,5
                       ) AS foo
              ) AS rs
                  JOIN pg_class cc ON cc.relname = rs.tablename
                  JOIN pg_namespace nn ON cc.relnamespace = nn.oid AND nn.nspname = rs.schemaname AND nn.nspname <> 'information_schema'
                  LEFT JOIN pg_index i ON indrelid = cc.oid
                  LEFT JOIN pg_class c2 ON c2.oid = i.indexrelid
     ) AS sml
where schemaname not like 'pg_%'
ORDER BY wastedbytes DESC
        |]
    , "table bloat" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "schema"       (^._1.to(htmlCell.toHtml))
           , headed "table"        (^._2.to(htmlCell.toHtml))
           , headed "realsize"     (^._3.to((htmlCell.toHtml)))
           , headed "extra_size"   (^._4.to(htmlCell.toHtml))
           , headed "extra ratio"  (^._5.to((htmlCell.toHtml.prettyDouble)))
           , headed "fill factor"  (^._6.to((htmlCell.toHtml)))
           , headed "bloat size"   (^._7.to((htmlCell.toHtml)))
           , headed "bloat ratio"  (^._8.to((htmlCell.toHtml.prettyDouble)))
           ])
        [vectorStatement|
SELECT schemaname::text,
       tblname::text,
       (bs*tblpages)::int8 AS real_size,
       ((tblpages-est_tblpages)*bs)::int8 AS extra_size,
       (CASE WHEN tblpages - est_tblpages > 0
         THEN 100 * (tblpages - est_tblpages)/tblpages::float8
         ELSE 0
       END) :: float8 AS extra_ratio,
       fillfactor::int8,
       (CASE WHEN tblpages - est_tblpages_ff > 0
         THEN (tblpages-est_tblpages_ff)*bs
       ELSE 0
       END) :: int8 AS bloat_size,
       (CASE WHEN tblpages - est_tblpages_ff > 0
         THEN 100 * (tblpages - est_tblpages_ff)/tblpages::float8
         ELSE 0
       END) :: float8 AS bloat_ratio
FROM (
         SELECT ceil( reltuples / ( (bs-page_hdr)/tpl_size ) ) + ceil( toasttuples / 4 ) AS est_tblpages,
                ceil( reltuples / ( (bs-page_hdr)*fillfactor/(tpl_size*100) ) ) + ceil( toasttuples / 4 ) AS est_tblpages_ff,
                tblpages, fillfactor, bs, tblid, schemaname, tblname, heappages, toastpages, is_na
         FROM (
                  SELECT
                      ( 4 + tpl_hdr_size + tpl_data_size + (2*ma)
                          - CASE WHEN tpl_hdr_size%ma = 0 THEN ma ELSE tpl_hdr_size%ma END
                          - CASE WHEN ceil(tpl_data_size)::int8%ma = 0 THEN ma ELSE ceil(tpl_data_size)::int8%ma END
                          ) AS tpl_size, bs - page_hdr AS size_per_block, (heappages + toastpages) AS tblpages, heappages,
                      toastpages, reltuples, toasttuples, bs, page_hdr, tblid, schemaname, tblname, fillfactor, is_na
                  FROM (
                           SELECT
                               tbl.oid AS tblid, ns.nspname AS schemaname, tbl.relname AS tblname, tbl.reltuples,
                               tbl.relpages AS heappages, coalesce(toast.relpages, 0) AS toastpages,
                               coalesce(toast.reltuples, 0) AS toasttuples,
                               coalesce(substring(
                                                array_to_string(tbl.reloptions, ' ')
                                                FROM 'fillfactor=([0-9]+)')::int4, 100) AS fillfactor,
                               current_setting('block_size')::int8 AS bs,
                               CASE WHEN version()~'mingw32' OR version()~'64-bit|x86_64|ppc64|ia64|amd64' THEN 8 ELSE 4 END AS ma,
                               24 AS page_hdr,
                               23 + CASE WHEN MAX(coalesce(s.null_frac,0)) > 0 THEN ( 7 + count(s.attname) ) / 8 ELSE 0::int4 END
                                   + CASE WHEN bool_or(att.attname = 'oid' and att.attnum < 0) THEN 4 ELSE 0 END AS tpl_hdr_size,
                               sum( (1-coalesce(s.null_frac, 0)) * coalesce(s.avg_width, 0) ) AS tpl_data_size,
                               bool_or(att.atttypid = 'pg_catalog.name'::regtype)
                                   OR sum(CASE WHEN att.attnum > 0 THEN 1 ELSE 0 END) <> count(s.attname) AS is_na
                           FROM pg_attribute AS att
                                    JOIN pg_class AS tbl ON att.attrelid = tbl.oid
                                    JOIN pg_namespace AS ns ON ns.oid = tbl.relnamespace
                                    LEFT JOIN pg_stats AS s ON s.schemaname=ns.nspname
                               AND s.tablename = tbl.relname AND s.inherited=false AND s.attname=att.attname
                                    LEFT JOIN pg_class AS toast ON tbl.reltoastrelid = toast.oid
                           WHERE NOT att.attisdropped
                             AND tbl.relkind in ('r','m')
                           GROUP BY 1,2,3,4,5,6,7,8,9,10
                           ORDER BY 2,3
                       ) AS s
              ) AS s2
     ) AS s3
where not schemaname like 'pg_%'
ORDER BY schemaname, tblname
    |]
    , "query statistics" =>>
      mkReport1 mempty
        (const ())
        (\() ->
           [ headed "query"           (^. _1.to(htmlCell.toHtml))
           , headed "calls"           (^. _2.to(htmlCell.toHtml))
           , headed "time spent, %"   (^. _3.to((htmlCell.toHtml.prettyDouble)))
           , headed "total_time"      (^. _4.to(htmlCell.toHtml.prettyDouble))
           , headed "min_time"        (^. _5.to((htmlCell.toHtml.prettyDouble)))
           , headed "mean_time"       (^. _6.to((htmlCell.toHtml.prettyDouble)))
           , headed "stddev"          (^. _7.to((htmlCell.toHtml.prettyDouble)))
           , headed "max_time"        (^. _8.to((htmlCell.toHtml.prettyDouble)))
           , headed "read hit"        (^. _9.to(maybe mempty (htmlCell.toHtml.prettyDouble)))
           , headed "row read"        (^._10.to((htmlCell.toHtml)))
           , headed "blks dirtied"    (^._11.to((htmlCell.toHtml)))
           , headed "blks read time"  (^._12.to((htmlCell.toHtml)))
           , headed "blks write time" (^._13.to((htmlCell.toHtml)))
           , headed "calls fraction"  (^._14.to((htmlCell.toHtml.prettyDouble)))
           ])
        $ fmap (V.filter (^._8.to(> 100)))
        [vectorStatement|
SELECT query::text,
       calls::int8,
       (100*total_time / sum(total_time) over ())::float8 fraction,
       total_time::float8 as "total, ms",
       min_time::float8 as "minimum, ms",
       mean_time::float8,
       stddev_time::float8,
       max_time::float8,
       (shared_blks_hit*100/nullif(shared_blks_hit+shared_blks_read,0))::float8? shared_read_hit,
       rows::int8,
       shared_blks_dirtied::int8,
       blk_read_time::int8,
       blk_write_time::int8,
       (100*calls/sum(calls) over ())::float8 calls_fraction
FROM  pg_stat_statements
where calls>10 and query not in ('BEGIN', 'COMMIT', 'DISCARD ALL')
ORDER BY total_time DESC
      |]

    ]

