# data retrieval functions ----

#' Retrieve Taxon Children from Database
#'
#' Queries the taxonomy table to find all child taxa of a given taxonID,
#' using a recursive CTE. Returns a tibble with taxon details and depth levels.
#'
#' `taxonID` is a `taxon_key` (`"worms:137202"` / `"itis:1255050"`), so the walk
#' stays inside one authority's tree by construction — a key's parent is always
#' in the same authority. That is what lets seabirds expand through their ITIS
#' families and orders, which have no WoRMS ids to walk at all.
#'
#' @param taxonID Character `taxon_key` of the parent to query
#' @param con DuckDB database connection object
#' @param authority optional authority to scope to ("WoRMS"/"ITIS"); NULL (the
#'   default) walks whichever tree the key belongs to, which is what callers want
get_taxon_children <- function(taxonID, con, authority = NULL) {

  query_sql <- glue("
    WITH RECURSIVE taxon_children AS (
      -- Base case: find the parent taxon
      SELECT
        taxonID,
        acceptedNameUsageID,
        parentNameUsageID,
        scientificName,
        taxonRank,
        0 as depth_level
      FROM taxon
      WHERE taxonID = ?

      UNION ALL

      -- Recursive case: find children taxa
      SELECT
        t.taxonID,
        t.acceptedNameUsageID,
        t.parentNameUsageID,
        t.scientificName,
        t.taxonRank,
        tc.depth_level + 1 as depth_level
      FROM taxon t
      INNER JOIN taxon_children tc ON t.parentNameUsageID = tc.taxonID
      WHERE
        t.parentNameUsageID IS NOT NULL
        {if (is.null(authority)) '' else glue(\"AND t.authority = '{authority}'\")}
    )
    SELECT tc.*, COALESCE(tr.rank_order, 99) as rank_order
    FROM taxon_children tc
    LEFT JOIN taxa_rank tr ON tc.taxonRank = tr.taxonRank
    ORDER BY tc.depth_level, COALESCE(tr.rank_order, 99), tc.scientificName")

  dbGetQuery(con, query_sql, params = list(taxonID)) |>
    tibble()
}


get_taxon_parentage <- function(taxonID, con, authority = "WoRMS"){

  query_sql <- glue("
    WITH RECURSIVE taxon_hierarchy AS (
      -- Base case: find the initial taxon (and resolve to accepted if it's a synonym)
      SELECT
        taxonID,
        acceptedNameUsageID,
        parentNameUsageID,
        scientificName,
        scientificNameAuthorship,
        taxonRank,
        taxonomicStatus,
        nomenclaturalStatus,
        namePublishedInYear,
        0 as level
      FROM taxon
      WHERE taxonID = ?

      UNION ALL

      -- Recursive case: find parent taxa (using the accepted parent)
      SELECT
        t.taxonID,
        t.acceptedNameUsageID,
        t.parentNameUsageID,
        t.scientificName,
        t.scientificNameAuthorship,
        t.taxonRank,
        t.taxonomicStatus,
        t.nomenclaturalStatus,
        t.namePublishedInYear,
        th.level + 1 as level
      FROM taxon t
      INNER JOIN taxon_hierarchy th ON t.taxonID = th.parentNameUsageID
      -- Ensure we're getting the accepted version of the parent
      WHERE t.taxonID = t.acceptedNameUsageID  -- Only accepted taxa (not synonyms)
        AND th.level < 50  -- Safety limit
    )
    SELECT * FROM taxon_hierarchy
    ORDER BY level, taxonRank")

  dbGetQuery(con, query_sql, params = list(taxonID)) |>
    mutate(
      authority           = !!authority,
      namePublishedInYear = as.character(namePublishedInYear)) |>
    relocate(authority) |>
    tibble()
}

#' Retrieve Species Larval Abundance Data from Database
#'
#' Queries species, larva, net, tow, and site tables with temporal filters,
#' computing standardized tally values. Returns a dbplyr lazy table for
#' efficient downstream processing.
#'
#' @param sp_name Character vector of species names (format: "Common Name (Scientific Name)")
#' @param qtr Numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#' @param ck_children Boolean (TRUE or FALSE) whether to include taxonomic children
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{name} - species name (common + scientific)
#'     \item \code{tally} - raw larval count
#'     \item \code{tow_type} - net gear code (C1/CB/CV/PV oblique/vertical; MT manta)
#'     \item \code{std_haul_factor}, \code{prop_sorted}, \code{volume_sampled} - tow effort
#'     \item \code{std_tally} - CPUE where the gear supports it, else the value
#'       the source published; net-type-aware, see details
#'     \item \code{cpue_unit} - \code{count/10m2} (oblique/vertical),
#'       \code{count/100m3} (manta), or the source's own unit where neither
#'       formula applies (e.g. cdfw_dungeness-crab, an occurrence count in a
#'       lab-examined aliquot, which is NOT a density)
#'     \item \code{time_start} - tow start datetime
#'     \item \code{longitude}, \code{latitude} - spatial coordinates
#'     \item \code{quarter} - quarter (1-4)
#'     \item \code{hex_id} - H3 cell index at resolution 10 (coarser resolutions
#'       derived at query time via \code{h3_cell_to_parent()})
#'   }
#'
#' @details
#' \code{std_tally} is CPUE (catch per unit effort / density), standardized by
#' net type (materialized in \code{prep_db.R::bio_obs}): oblique & vertical tows
#' (C1, CB, CV, PV) give counts per 10 m^2 via
#' \code{tally * std_haul_factor / prop_sorted}; manta surface tows (MT) give
#' counts per 100 m^3 via \code{tally / prop_sorted / volume_sampled * 100}
#' (the manta haul factor does not standardize to volume).
#' Only records with non-NA tally values are returned.
#'
#' @examples
#' \dontrun{
#' # retrieve anchovy data for all quarters 2010-2020
#' df_sp <- get_sp(
#'   sp_name    = "Anchovy (Engraulis mordax)",
#'   qtr        = 1:4,
#'   date_range = as.Date(c("2010-01-01", "2020-12-31"))
#' )
#' df_sp |> collect()
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_sp}} for temporal aggregation
#'
#' @importFrom dplyr tbl mutate filter left_join between
#' @importFrom lubridate quarter
#'
#' @export
#' Resolve Selected Taxa Names to the Ids `bio_obs` is Filtered On
#'
#' Turns the picker's display names into the two id sets that identify the
#' selection in \code{bio_obs}: WoRMS ids (optionally including the taxonomic
#' children) and, for taxa with no WoRMS id, scientific names.
#'
#' Split out of \code{\link{get_sp}} so the h3t tile SQL
#' (\code{build_sp_sql()}) filters on the SAME ids the tables and plots do.
#' While the tile query resolved its own taxa — with a recursive CTE, and only
#' ever for one name — the map could and did show a different set of
#' observations than the Time Series beside it.
#'
#' @param sp_name Character vector of picker labels, "Common (rank: Scientific)"
#' @param ck_children Include taxonomic children of the selected taxa
#'   (default: FALSE, matching the "Include taxonomic children" checkbox the
#'   taxa combo renders. This defaulted to TRUE while every caller had moved
#'   to FALSE, so the next caller to omit it would silently have re-enabled
#'   the descendant walk.)
#'
#' @return list with \code{taxon_keys} (character `taxon_key`s)
#'
#' @export
resolve_sp_ids <- function(sp_name, ck_children = FALSE) {
  # Memoized: a Submit resolves the same selection twice — once for get_sp()'s
  # table query and once for the tile SQL — and the children walk runs one
  # recursive CTE PER selected taxon, so a 50-taxon selection is 50 round trips
  # each time. Keyed on the arguments; nothing below depends on session state.
  key <- rlang::hash(list(sort(sp_name), ck_children))
  if (!is.null(sp_ids_cache[[key]])) return(sp_ids_cache[[key]])
  # resolve selected names to taxon_keys via species + taxon tables
  # name format must match global.R: "Common Name (rank: Scientific Name)"
  #
  # Joined on taxon_key, not worms_id, and NOT filtered to authority WoRMS: the
  # seabirds and marine mammals key `itis:`, and scoping this join to one
  # authority is what left them with no taxonRank and no hierarchy to walk.
  # collect() before the string mutations below, to exactly mirror global.R's
  # d_sp -- substr()/nchar() don't translate to this backend's SQL when
  # nested, and the capitalization must match d_sp's or a taxon whose common
  # name got capitalized there (e.g. "diatoms" -> "Diatoms") stops matching
  # here, silently resolving to zero rows (see the format-drift warning above)
  sp_taxon <- tbl(con, "species") |>
    left_join(
      tbl(con, "taxon"),
      by = join_by(taxon_key == taxonID)) |>
    select(scientific_name, common_name, taxonRank, taxon_key) |>
    collect() |>
    mutate(
      rank_part = ifelse(
        is.na(taxonRank) | taxonRank == "",
        "",
        paste0(tolower(taxonRank), ": ")),
      common_name = ifelse(
        is.na(common_name) | common_name == "",
        common_name,
        paste0(toupper(substr(common_name, 1, 1)), substr(common_name, 2, nchar(common_name)))),
      name_part = ifelse(
        is.na(common_name) | common_name == "",
        "",
        paste0(common_name, " ")),
      name = paste0(name_part, "(", rank_part, scientific_name, ")"))

  sel <- sp_taxon |>
    filter(name %in% sp_name) |>
    select(name, scientific_name, taxon_key)

  if (ck_children) {
    # walks whichever authority's tree the key belongs to — WoRMS for most,
    # ITIS for the seabirds and marine mammals
    df_sel <- sel |>
      filter(!is.na(taxon_key)) |>
      mutate(children = map(taxon_key, get_taxon_children, con = con)) |>
      unnest(children)
    taxon_keys <- unique(df_sel$acceptedNameUsageID)
  } else {
    taxon_keys <- unique(sel$taxon_key[!is.na(sel$taxon_key)])
  }

  # One key space for both authorities, so there is no id to be missing and no
  # name-matching fallback to keep in sync. The previous version matched on
  # `worms_id` and fell back to `scientific_name` for the ITIS-keyed taxa —
  # which worked, but meant two code paths whose results had to agree, and the
  # name path could not expand children at all.
  out <- list(taxon_keys = taxon_keys[!is.na(taxon_keys)])

  sp_ids_cache[[key]] <- out
  out
}
# survives for the life of the R process; the taxonomy it reads is fixed by the
# release the app opened, so a stale entry is not possible without a restart
sp_ids_cache <- new.env(parent = emptyenv())


get_sp <- function(sp_name, qtr, date_range, ck_children = FALSE, datasets = NULL) {
  if (debug)
    message(
      "get_sp: sp_name = ", paste(sp_name, collapse = ", "),
      ", qtr = ", paste(qtr, collapse = ","),
      ", date_range = ", paste(date_range, collapse = " to "),
      ", ck_children = ", ck_children)

  ids        <- resolve_sp_ids(sp_name, ck_children)
  taxon_keys <- ids$taxon_keys

  df_sp <- tbl(con, "bio_obs")
  df_sp <- if (length(taxon_keys) > 0) {
    df_sp |> filter(taxon_key %in% taxon_keys)
  } else {
    df_sp |> filter(FALSE)   # nothing selected resolves; an empty IN () is a SQL error
  }

  # a taxon sampled by two programs keeps only the datasets asked for
  if (!is.null(datasets) && length(datasets) > 0)
    df_sp <- df_sp |> filter(dataset_key %in% datasets)

  df_sp <- df_sp |>
    filter(
      between(time_start, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      name = ifelse(
        is.na(common_name) | common_name == "",
        paste0("(", scientific_name, ")"),
        paste0(common_name, " (", scientific_name, ")")))

  if (debug) {
    n_rows <- df_sp |> summarize(n = n()) |> pull(n)
    message("get_sp: returning lazy table with ", n_rows, " rows")
  }

  df_sp
}


#' Retrieve Environmental Data from Database
#'
#' Queries environmental bottle cast data with temporal, depth, and variable filters.
#' Returns a dbplyr lazy table for efficient downstream processing.
#'
#' @param env_var Character string of database column name for environmental variable (e.g., "temperature", "salnty")
#' @param qtr Character or numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#' @param min_depth Numeric minimum depth in meters
#' @param max_depth Numeric maximum depth in meters
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{date} - date of cast
#'     \item \code{time} - time of cast (seconds since midnight)
#'     \item \code{dtime} - datetime (computed via SQL CAST and INTERVAL)
#'     \item \code{depth_m} - depth in meters
#'     \item \code{lat_dec} - latitude (decimal degrees)
#'     \item \code{lon_dec} - longitude (decimal degrees)
#'     \item \code{qty} - environmental variable value
#'     \item \code{hex_id} - H3 cell index at resolution 10 (coarser resolutions
#'       derived at query time via \code{h3_cell_to_parent()})
#'   }
#'
#' @details
#' Queries \code{env_obs} materialized table (pre-joined casts + bottle +
#' bottle_measurement with H3 columns and quarter). Only records with non-NA
#' values for the selected measurement_type are returned.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env(
#'   env_var    = "temperature",
#'   qtr        = c(1, 2),
#'   date_range = as.Date(c("2010-01-01", "2020-12-31")),
#'   min_depth  = 0,
#'   max_depth  = 100)
#' df_env |> collect()
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_env}} for temporal aggregation
#'
#' @importFrom dplyr tbl filter rename select starts_with between
#'
#' @export
get_env <- function(env_var, qtr, date_range, min_depth, max_depth) {
  if (debug) message("get_env: env_var = ", env_var, ", qtr = ", paste(qtr, collapse = ","),
                     ", date_range = ", paste(date_range, collapse = " to "),
                     ", depth = ", min_depth, "-", max_depth)

  df_env <- tbl(con, "env_obs") |>
    filter(
      measurement_type == env_var,
      !is.na(qty),
      between(depth_m, min_depth, max_depth),
      between(datetime_utc, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    rename(dtime = datetime_utc) |>
    select(
      dtime,
      cast_id,
      depth_m,
      lat_dec,
      lon_dec,
      qty,
      hex_id)

  if (debug) {
    n_rows <- df_env |> summarize(n = n()) |> pull(n)
    message("get_env: returning lazy table with ", n_rows, " rows")
  }

  df_env
}


# data preparation functions ----

#' Hexagon Geometry, Loaded on First Use
#'
#' \code{data/hex.geojson} is 153 MB — 434,218 polygons covering all 10 H3
#' resolutions — and \code{st_read()}ing it costs ~5.6 s and ~370 MB of RSS in
#' every R process the app starts. global.R used to read it eagerly, so every
#' session paid that before the UI appeared.
#'
#' Exactly two callers need it: \code{\link{prep_sp_hex}} and
#' \code{\link{prep_env_hex}}, which attach geometry to their aggregates. When
#' \code{USE_H3T} is on — the normal case — neither runs, because the tile
#' service derives each cell's boundary per tile. So the eager read paid the
#' full cost for something nothing then used.
#'
#' Loading here instead means the cost is paid once, in the one session that
#' actually asks for hexagon geometry (a classic-path fallback, or a "Map data"
#' download that wants geometry), and never otherwise.
#'
#' @return sf object of hexagons with \code{hex_id} and \code{hex_res}
#'
#' @export
get_sf_hex <- function() {
  if (is.null(hex_cache$sf_hex)) {
    if (debug) message("get_sf_hex: reading ", hex_geo, " (first use) ...")
    hex_cache$sf_hex <- sf::st_read(hex_geo, quiet = TRUE)
  }
  hex_cache$sf_hex
}
hex_cache <- new.env(parent = emptyenv())

# TODO (long-term): replace preloaded hex layers with h3t tile endpoint
#   - plumber API at /api/h3t/{z}/{x}/{y} accepting SQL query params
#   - determines H3 resolution from zoom z, filters by tile extent x/y
#   - returns h3j-format JSON ([{h3: "hex_id", value: ...}, ...])
#   - map sources switch from add_fill_layer(source = sf_data) to
#     add_h3j_source(url = api_endpoint) or future add_h3t_source()
#   - eliminates preloading all resolutions; data fetched on-demand per viewport
#   - see: https://github.com/INSPIDE/h3j-h3t
#   - see: https://walker-data.com/mapgl/reference/add_h3j_source.html

#' Aggregate Species Data into H3 Hexagons
#'
#' Converts species occurrence/abundance data into multi-resolution H3 hexagonal
#' bins with aggregated statistics and geometries for mapping.
#'
#' @param df_sp dbplyr lazy table with columns: \code{hex_id}, \code{std_tally}
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{sp.value} - mean standardized tally
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' }
#'
#' @seealso \code{\link{map_sp}} for visualization
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
#' Aggregate Species Data into H3 Cells (no geometry)
#'
#' The database half of \code{\link{prep_sp_hex}}: one row per (resolution,
#' cell) with the aggregate, and no polygon attached.
#'
#' Separated out because attaching geometry means reading the 153 MB
#' \code{hex.geojson} (see \code{\link{get_sf_hex}}), and the callers that
#' write a CSV do not want it — the sfc column write.csv() emits is an R
#' \code{list(c(...))} literal, not WKT, so it was unusable anyway.
#'
#' @inheritParams prep_sp_hex
#' @return tibble with \code{resolution}, \code{hexid}, \code{sp.value},
#'   \code{n}, \code{min_dtime}, \code{max_dtime}, \code{tooltip}
#' @export
agg_sp_hex <- function(df_sp, res_range) {
  if (debug) message("agg_sp_hex: aggregating species data for resolutions ", paste(res_range, collapse = ","))

  # precompute and store joins in a temporary table
  df_sp_temp <- df_sp |>
    compute()

  # create and combine tables for each resolution — derive the parent H3 cell at
  # resolution .x from the stored res-10 hex_id (runs in DuckDB via h3_cell_to_parent)
  combined_res_tbl <- map(res_range, ~{
    df_sp_temp |>
      mutate(hex_int = h3_cell_to_parent(hex_id, .x)) |>
      select(hex_int, std_tally, time_start) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_sp_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value   =  mean(std_tally, na.rm = T),
      n          =  sum(std_tally,  na.rm = T),
      min_dtime  =  min(time_start, na.rm = T),
      max_dtime  =  max(time_start, na.rm = T),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(sp.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      # unit-free: the hex average spans cpue_units, and std_tally is a
      # gear-standardized density only where a net tow supports it (see prep_db.R)
      tooltip = paste0("Avg. CPUE: ", round(sp.value, 2),
                 "</br>Num. Samples: ", n,
                 "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, sp.value, n, min_dtime, max_dtime, tooltip) |>
    collect()

  if (debug) message("agg_sp_hex: collected ", nrow(hex_sp_collected), " hex records")

  hex_sp_collected
}


prep_sp_hex <- function(df_sp, res_range) {
  hex_sp <- agg_sp_hex(df_sp, res_range) |>
    left_join(
      get_sf_hex() |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_sp_hex: created ", length(hex_sp), " hex layers")
    for (i in seq_along(hex_sp)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_sp[[i]]), " hexagons")
    }
  }

  return(hex_sp)
}


#' Aggregate Environmental Data into H3 Hexagons
#'
#' Converts environmental point data into multi-resolution H3 hexagonal bins
#' with aggregated statistics and geometries for mapping. Uses dbplyr lazy
#' evaluation to defer collection until after aggregation.
#'
#' @param df_env dbplyr lazy table with H3 index column (\code{hex_id}) and \code{qty} column
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#' @param env_stat Character string specifying aggregation function: "mean", "median", "min", "max", "sd"
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{env.value} - aggregated environmental value
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{map_env}} for visualization
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
#' Aggregate Environmental Data into H3 Cells (no geometry)
#'
#' The database half of \code{\link{prep_env_hex}} — see \code{\link{agg_sp_hex}}
#' for why the geometry join is separable.
#'
#' @inheritParams prep_env_hex
#' @return tibble with \code{resolution}, \code{hexid}, \code{env.value},
#'   \code{tooltip}
#' @export
agg_env_hex <- function(df_env, res_range, env_stat) {
  if (debug) message("agg_env_hex: aggregating env data for resolutions ", paste(res_range, collapse = ","),
                     ", stat = ", env_stat)

  # precompute and store joins in a temporary table
  df_env_temp <- df_env |>
    compute()

  # create and combine tables for each resolution — derive the parent H3 cell at
  # resolution .x from the stored res-10 hex_id (runs in DuckDB via h3_cell_to_parent)
  combined_res_tbl <- map(res_range, ~{
    df_env_temp |>
      mutate(hex_int = h3_cell_to_parent(hex_id, .x)) |>
      select(hex_int, qty, dtime) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_env_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      env.value = case_when(
        env_stat == "mean"   ~ mean(qty, na.rm = TRUE),
        env_stat == "median" ~ median(qty, na.rm = TRUE),
        env_stat == "min"    ~ min(qty, na.rm = TRUE),
        env_stat == "max"    ~ max(qty, na.rm = TRUE),
        env_stat == "sd"     ~ sd(qty, na.rm = TRUE),
        TRUE ~ mean(qty, na.rm = TRUE)
      ),
      n          =  sum(!is.na(qty)),
      min_dtime  =  min(dtime, na.rm = TRUE),
      max_dtime  =  max(dtime, na.rm = TRUE),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(env.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      tooltip = paste0("Value: ", round(env.value, 2),
                       "</br>Num. Samples: ", n,
                       "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, env.value, tooltip) |>
    collect()

  if (debug) message("agg_env_hex: collected ", nrow(hex_env_collected), " hex records")

  hex_env_collected
}


prep_env_hex <- function(df_env, res_range, env_stat) {
  hex_env <- agg_env_hex(df_env, res_range, env_stat) |>
    left_join(
      get_sf_hex() |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_env_hex: created ", length(hex_env), " hex layers")
    for (i in seq_along(hex_env)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_env[[i]]), " hexagons")
    }
  }

  return(hex_env)
}


# spatial polygon summaries ----
# The hex path bins observations into H3 cells; this path bins them into the
# polygons of a named boundary layer (an MPA, a county, an ecoregion) instead,
# and reports one value per polygon.
#
# The join is `sample_spatial`, materialized by prep_db.R at the SAMPLE grain —
# an observation's position IS its sample's position, so a 1.5M-row
# point-in-polygon join answers the same question as a 20M-row one. bio_obs
# carries `sample_key`; env_obs carries the same key named `cast_id`.
#
# ONE layer at a time, deliberately: the layers overlap (a station sits inside a
# county AND an ecoregion AND an MPA, and every one of those is true), so a
# summary spanning layers would count the same observation more than once. The
# UI picker is single-select for exactly this reason — see `agg_unit_choices`
# in global.R.

# boundary geometry, keyed "{layer}@{tolerance}". Read once per R process rather
# than per switch: the summary query is ~0.05s but the geometry is up to ~900 KB
# of GeoJSON, and users toggle back and forth between layers.
poly_geom_cache <- new.env(parent = emptyenv())


#' Summarize Species Observations Within a Spatial Layer's Polygons
#'
#' Aggregates species CPUE into the polygons of one boundary layer (e.g.
#' "Marine Protected Areas"), the polygon counterpart of \code{\link{prep_sp_hex}}.
#'
#' @param df_sp dbplyr lazy table from \code{\link{get_sp}} (carries
#'   \code{sample_key}, \code{std_tally}, \code{cpue_unit}, \code{time_start})
#' @param sel_layer Character name of the spatial layer, matching
#'   \code{sample_spatial.layer} (e.g. "CA Counties")
#'
#' @return List with:
#'   \itemize{
#'     \item \code{data} - tibble of \code{spatial_key}, \code{spatial_name},
#'       \code{value} (mean CPUE), \code{n}, date range, \code{tooltip}
#'     \item \code{unit} - the single \code{cpue_unit} summarized
#'     \item \code{n_excluded} - observations dropped because they carry a
#'       different unit
#'     \item \code{units} - the full unit mix, most-represented first
#'   }
#'
#' @details
#' \strong{One unit, named.} \code{std_tally} is a gear-standardized density only
#' where a net tow supports it; elsewhere it is the value the source published,
#' in its own \code{cpue_unit}. A mean over a mix of units is not a quantity —
#' and the mix is not hypothetical: \emph{Sardinops sagax} alone spans
#' \code{count/10m2} (oblique/vertical tows), \code{count/100m3} (manta) and a
#' bare \code{count} from \code{swfsc_cufes}, the last outnumbering the others
#' 4:1. So this summarizes the most-represented unit only, returns that unit for
#' the legend to name, and returns how many observations that excluded.
#'
#' @seealso \code{\link{map_poly}} for visualization
#' @seealso \code{\link{prep_sp_hex}} for the hexagon equivalent
#'
#' @importFrom dplyr filter inner_join select count group_by summarize collect mutate arrange desc
#'
#' @export
prep_sp_poly <- function(df_sp, sel_layer) {
  if (debug)
    message("prep_sp_poly: summarizing species within '", sel_layer, "'")

  d <- df_sp |>
    filter(!is.na(std_tally), !is.na(cpue_unit)) |>
    inner_join(
      tbl(con, "sample_spatial") |>
        filter(layer == !!sel_layer) |>
        select(sample_key, spatial_key, spatial_name),
      by = "sample_key")

  # unit mix first, so the summary below is over a single unit
  d_units <- d |>
    count(cpue_unit) |>
    collect() |>
    arrange(desc(n))

  if (nrow(d_units) == 0) {
    if (debug) message("prep_sp_poly: no observations fall in this layer")
    # SHAPED empty, not tibble(): map_poly() joins this onto the layer geometry
    # by name, so a zero-COLUMN tibble errors where a zero-ROW one draws every
    # polygon as "no data" — which is the honest answer here.
    return(list(
      data = tibble(
        spatial_key  = character(), spatial_name = character(),
        value        = numeric(),   n            = integer(),
        min_dtime    = as.POSIXct(character()),
        max_dtime    = as.POSIXct(character()),
        tooltip      = character()),
      unit = NA_character_, n_excluded = 0, units = d_units))
  }

  unit       <- d_units$cpue_unit[1]
  n_excluded <- sum(d_units$n[-1])

  d_sum <- d |>
    filter(cpue_unit == !!unit) |>
    group_by(spatial_key, spatial_name) |>
    summarize(
      value     = mean(std_tally, na.rm = TRUE),
      n         = n(),
      min_dtime = min(time_start, na.rm = TRUE),
      max_dtime = max(time_start, na.rm = TRUE),
      .groups   = "drop") |>
    collect() |>
    mutate(
      tooltip = paste0(
        "<strong>", spatial_name, "</strong>",
        "<br>Avg. CPUE (", fmt_cpue_unit(unit), "): ", round(value, 2),
        "<br>Num. Obs.: ", n,
        "<br>Date Range: ", as.Date(min_dtime), " to ", as.Date(max_dtime)))

  if (debug)
    message("prep_sp_poly: ", nrow(d_sum), " polygons with data, unit = ", unit,
            ", ", n_excluded, " obs excluded in ", nrow(d_units) - 1,
            " other unit(s)")

  list(data = d_sum, unit = unit, n_excluded = n_excluded, units = d_units)
}


#' Summarize Environmental Observations Within a Spatial Layer's Polygons
#'
#' Aggregates an environmental variable into the polygons of one boundary layer,
#' the polygon counterpart of \code{\link{prep_env_hex}}.
#'
#' @param df_env dbplyr lazy table from \code{\link{get_env}} (carries
#'   \code{cast_id}, \code{qty}, \code{dtime})
#' @param sel_layer Character name of the spatial layer
#' @param env_stat Character aggregation: "mean", "median", "min", "max", "sd"
#'
#' @return tibble of \code{spatial_key}, \code{spatial_name}, \code{value},
#'   \code{n}, date range and \code{tooltip}
#'
#' @details
#' \code{env_obs} carries the sample key as \code{cast_id}, so the join to
#' \code{sample_spatial} is \code{cast_id = sample_key}.
#'
#' @seealso \code{\link{map_poly}} for visualization
#'
#' @importFrom dplyr filter inner_join select group_by summarize collect mutate case_when join_by
#'
#' @export
prep_env_poly <- function(df_env, sel_layer, env_stat) {
  if (debug)
    message("prep_env_poly: summarizing ", env_stat, " within '", sel_layer, "'")

  d_sum <- df_env |>
    filter(!is.na(qty)) |>
    inner_join(
      tbl(con, "sample_spatial") |>
        filter(layer == !!sel_layer) |>
        select(sample_key, spatial_key, spatial_name),
      by = join_by(cast_id == sample_key)) |>
    group_by(spatial_key, spatial_name) |>
    summarize(
      value = case_when(
        env_stat == "mean"   ~ mean(qty, na.rm = TRUE),
        env_stat == "median" ~ median(qty, na.rm = TRUE),
        env_stat == "min"    ~ min(qty, na.rm = TRUE),
        env_stat == "max"    ~ max(qty, na.rm = TRUE),
        env_stat == "sd"     ~ sd(qty, na.rm = TRUE),
        TRUE                 ~ mean(qty, na.rm = TRUE)),
      n         = n(),
      min_dtime = min(dtime, na.rm = TRUE),
      max_dtime = max(dtime, na.rm = TRUE),
      .groups   = "drop") |>
    collect() |>
    mutate(
      tooltip = paste0(
        "<strong>", spatial_name, "</strong>",
        "<br>Value: ", round(value, 2),
        "<br>Num. Obs.: ", n,
        "<br>Date Range: ", as.Date(min_dtime), " to ", as.Date(max_dtime)))

  if (debug)
    message("prep_env_poly: ", nrow(d_sum), " polygons with data")

  d_sum
}


#' Lazy Table of Sample Keys Within Selected Spatial-Layer Polygons
#'
#' The FILTER counterpart to \code{\link{prep_sp_poly}}/\code{\link{prep_env_poly}}'s
#' \code{sample_spatial} join (which those two use to AGGREGATE) -- factored
#' out so the Data Selection modal's Spatial tab can use the same mechanism to
#' keep only matching samples. Used for any \code{spatial_layers.csv} layer not
#' already covered by a \code{calcofi4r::cc_places} category with real polygon
#' geometry (see \code{global.R::sample_spatial_layers}) -- those layers have
#' no polygon loaded to run \code{ST_Within()} against, but \code{sample_spatial}
#' already has each sample's membership pre-computed.
#'
#' @param sel_layer Character name of the spatial layer (\code{d_spatial_layers$layer})
#' @param sel_names Character vector of \code{spatial_name} values to keep
#'
#' @return lazy \code{tbl(con, "sample_spatial")}, filtered down to distinct \code{sample_key}
#'
#' @seealso \code{\link{prep_sp_poly}}, \code{\link{prep_env_poly}}
#'
#' @importFrom dplyr tbl filter select distinct
#'
#' @export
sample_spatial_keys <- function(sel_layer, sel_names) {
  tbl(con, "sample_spatial") |>
    filter(layer == !!sel_layer, spatial_name %in% !!sel_names) |>
    select(sample_key) |>
    distinct()
}


#' Read a Spatial Layer's Polygon Geometry
#'
#' Reads the boundary geometry for one layer out of the app's local DuckDB
#' \code{spatial} table, simplified for the browser, and caches it for the life
#' of the R process.
#'
#' @param sel_layer Character name of the spatial layer
#' @param tolerance Numeric simplification tolerance in degrees
#'   (\code{POLY_SIMPLIFY_DEG})
#'
#' @return sf data frame with \code{spatial_key}, \code{spatial_name}, geometry
#'
#' @details
#' The geometry comes from the DB rather than the PMTiles overlay so the join to
#' the summary is the exact \code{spatial_key} — the PMTiles carry only a
#' per-file \code{id}, which equals \code{spatial.id} for single-layer groups but
#' NOT for a multi-layer group like \code{noaa_maritime_boundaries}.
#' \code{ST_SimplifyPreserveTopology} keeps a MULTIPOLYGON's parts, and at the
#' default tolerance (\code{POLY_SIMPLIFY_DEG}, ~11 m) stays finer than the
#' smallest hexagon the map draws.
#'
#' \code{NOT ST_IsEmpty(geom)} is not paranoia: releases up to and including
#' v2026.08.02 ship National Marine Sanctuaries, CA Watersheds (HUC8) and Ocean
#' Disposal Sites as \code{GEOMETRYCOLLECTION EMPTY} — the ingest bound the
#' per-layer sf objects together by column name, so any source whose geometry
#' column was not called \code{geometry} lost its shape. Fixed in
#' \code{workflows/ingest_spatial.qmd} (\code{normalize_geom_col()}); this guard
#' keeps the app honest against a release built before that.
#'
#' @importFrom glue glue_sql
#' @importFrom sf st_as_sf st_set_geometry
#'
#' @export
get_layer_sf <- function(sel_layer, tolerance = POLY_SIMPLIFY_DEG) {
  key <- paste0(sel_layer, "@", tolerance)
  if (!is.null(poly_geom_cache[[key]])) {
    if (debug) message("get_layer_sf: cache hit for '", sel_layer, "'")
    return(poly_geom_cache[[key]])
  }

  q <- glue_sql(
    "SELECT spatial_key,
            COALESCE(name, spatial_key) AS spatial_name,
            ST_AsText(ST_SimplifyPreserveTopology(geom, {tolerance})) AS geom_wkt
       FROM spatial
      WHERE layer = {sel_layer}
        AND geom IS NOT NULL
        AND NOT ST_IsEmpty(geom)",
    .con = con)

  d <- dbGetQuery(con, q)
  if (nrow(d) == 0) {
    if (debug) message("get_layer_sf: no geometry for '", sel_layer, "'")
    return(NULL)
  }

  sf_poly <- d |>
    st_as_sf(wkt = "geom_wkt", crs = 4326) |>
    st_set_geometry("geometry")

  if (debug)
    message("get_layer_sf: ", nrow(sf_poly), " polygons for '", sel_layer, "'")

  poly_geom_cache[[key]] <- sf_poly
  sf_poly
}


# cache helpers ----

#' compute cache key from default parameters and database modification time
cache_key <- function(db_path) {
  params <- list(
    sp_name    = default_sp_name,
    env_var    = "temperature",
    quarters   = 1:4,
    date_range = as.character(min_max_date),
    depth      = c(0, 515),
    children   = TRUE,
    env_stat   = "mean",
    res_range  = res_range)
  db_mtime <- as.character(file.info(db_path)$mtime)
  rlang::hash(c(params, db_mtime = db_mtime))
}

#' load cached default data if valid; returns list or NULL
load_cache <- function(cache_dir, db_path) {
  key_file <- file.path(cache_dir, "cache_key.rds")
  if (!file.exists(key_file)) return(NULL)

  saved_key   <- readRDS(key_file)
  current_key <- cache_key(db_path)
  if (saved_key != current_key) return(NULL)

  files <- c("sp_hex_list.rds", "env_hex_list.rds", "summary_stats.rds")
  paths <- file.path(cache_dir, files)
  if (!all(file.exists(paths))) return(NULL)

  list(
    sp_hex_list   = readRDS(paths[1]),
    env_hex_list  = readRDS(paths[2]),
    summary_stats = readRDS(paths[3]))
}

#' save default data to cache
save_cache <- function(cache_dir, db_path, sp_hex_list, env_hex_list, summary_stats) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  saveRDS(cache_key(db_path), file.path(cache_dir, "cache_key.rds"))
  saveRDS(sp_hex_list,        file.path(cache_dir, "sp_hex_list.rds"))
  saveRDS(env_hex_list,       file.path(cache_dir, "env_hex_list.rds"))
  saveRDS(summary_stats,      file.path(cache_dir, "summary_stats.rds"))

  if (debug) message("cache saved to ", cache_dir)
}


#' Build Species Time Series Data
#'
#' Aggregates species abundance data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_sp dbplyr lazy table or data.frame with columns: \code{time_start}, \code{name}, \code{std_tally}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{name} - species name
#'     \item \code{avg} - mean standardized tally
#'     \item \code{std} - standard error (sd/n)
#'     \item \code{n} - number of observations
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database before aggregation.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_sp}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows
#'
#' @export
prep_ts_sp <- function(df_sp, ts_res) {

  sp_ts_data <- df_sp |>
    mutate(
      time = !!expr_time_sp(ts_res)
    ) |>
    group_by(time, name) |>
    summarize(
      avg = mean(std_tally, na.rm = TRUE),
      std = sd(std_tally, na.rm = TRUE),
      n = n(),
      .groups = "drop") |>
    mutate(
      upr = avg + std/n,
      lwr = avg - std/n,
      std = std/n) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    sp_ts_data <- sp_ts_data |>
      bind_rows(
        sp_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  # Break the line where nothing was sampled. Highcharts connects consecutive
  # points and a species series is mostly zeros, so an unsampled stretch drew a
  # flat line along zero — which reads as "we looked and found none" when the
  # truth is "nobody looked".
  #
  # Metacarcinus magister is the case that surfaced it: its sorted-archive effort
  # exists in nine years only (1984, 1988, 1998, 2004-2009), because the sorting
  # log records which archived jars have been examined and most have not, yet the
  # chart drew a continuous zero from 1984 to 2008.
  #
  # Calls calcofi4r rather than reimplementing: this file carries its own copy of
  # prep_ts_sp() which SHADOWS the package's, so fixing only the package left the
  # app unchanged and the fix silently inert (2026-08-14). One implementation,
  # two callers. Needs calcofi4r >= 1.7.0.
  sp_ts_data <- calcofi4r::cc_ts_gaps(sp_ts_data, ts_res)

  return(sp_ts_data)
}


#' Build Environmental Time Series Data
#'
#' Aggregates environmental data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_env dbplyr lazy table with columns: \code{dtime}, \code{qty}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{avg} - mean of \code{qty}
#'     \item \code{std} - standard error of \code{qty} (sd/n)
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database only at the end of aggregation.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_ts <- prep_ts_env(df_env, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_env}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows n
#'
#' @export
prep_ts_env <- function(df_env, ts_res) {

  env_ts_data <- df_env |>
    mutate(
      time = !!expr_time_env(ts_res) ) |>
    group_by(time) |>
    summarize(
      avg = mean(qty, na.rm = T),
      std = sd(qty, na.rm = T) / n(),
      .groups = "drop"
    ) |>
    mutate(
      upr = avg + std,
      lwr = avg - std) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    env_ts_data <- env_ts_data |>
      bind_rows(
        env_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  return(env_ts_data)
}


#' Prepare Data for Species-Environment Scatterplot
#'
#' Joins species and environmental data by matching observations that are close
#' in time and space, enabling correlation analysis between abundance and
#' environmental variables.
#'
#' @param df_sp dbplyr lazy table or data.frame with species data
#' @param df_env dbplyr lazy table or data.frame with environmental data
#' @param env_stat Character string specifying aggregation function (e.g., "mean", "median")
#' @param max_hours_diff Numeric maximum time difference (in hours) for matching observations (default: 6)
#' @param max_meters_diff Numeric maximum spatial distance (in meters) for matching observations (default: 2000)
#'
#' @return data.frame with matched species-environment observations
#'
#' @details
#' This function performs a fuzzy join based on temporal proximity using
#' \code{fuzzyjoin::difference_inner_join()}. For each species observation,
#' the closest environmental measurement (within \code{max_hours_diff}) is
#' selected. Data is collected from database before joining.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' df_splot <- prep_splot(df_sp, df_env, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{get_sp}} for species data retrieval
#' @seealso \code{\link{get_env}} for environmental data retrieval
#'
#' @importFrom dplyr select collect mutate group_by slice_min ungroup
#' @importFrom fuzzyjoin difference_inner_join
#'
#' @export
prep_splot <- function(df_sp, df_env, env_stat, method = "nearest_time",
                       max_hours_diff = 6, max_meters_diff = 2000) {

  # prepare species data
  d_sp <- df_sp |>
    select(
      sp_name  = name,
      sp_dtime = time_start,
      sp_tally = std_tally,
      sp_lon   = longitude,
      sp_lat   = latitude) # |>
  # compute()

  # prepare environmental data
  d_env <- df_env |>
    select(
      env_dtime = dtime,
      env_qty   = qty,
      env_cst   = cast_id,
      env_depth = depth_m,
      env_lon   = lon_dec,
      env_lat   = lat_dec) |>
    mutate(
      env_dtime_lwr = sql(glue("env_dtime - INTERVAL {max_hours_diff} HOUR")),
      env_dtime_upr = sql(glue("env_dtime + INTERVAL {max_hours_diff} HOUR")))

  # join by time difference
  d_sp_env_raw <- d_sp |>
      left_join(
        d_env,
        # join species to env observations within desired time interval
        by = join_by(between(sp_dtime, env_dtime_lwr, env_dtime_upr))) |>
      # compute distance between species and ocean observations
      mutate(
        dist_m = sql("ST_Distance_Sphere(ST_Point(sp_lon, sp_lat), ST_Point(env_lon, env_lat))")) |>
      # get pairs within desired distance
      filter(
        dist_m <= max_meters_diff)

  order_by <- if (method == "nearest_time") {
    expr(tibble(time_diff,dist_m,env_cst))
  } else if (method == "nearest_dist" ) {
    expr(tibble(dist_m,time_diff,env_cst))
  }

  d_sp_env <- if (method == "nearest_time" | method == "nearest_dist") {
    d_sp_env_raw |>
      mutate(
        time_diff = if_else(sp_dtime - env_dtime > lubridate::seconds(0),
                            sp_dtime - env_dtime,
                            env_dtime - sp_dtime)) |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      slice_min(
        !!order_by,
        with_ties = TRUE) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop")
  } else {
    d_sp_env_raw |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop") |>
      select(
        sp_name, sp_dtime, sp_lon, sp_lat, sp_tally,
        env_qty) }

  d_sp_env
}


#' Build Filter Summary for Display
#'
#' Creates a formatted list of filter criteria for display in the UI.
#' Summarizes species, environmental variables, temporal filters, depth ranges,
#' and spatial constraints into human-readable markdown strings.
#'
#' @param sel_name Character vector of selected species names (format: "Common Name (Scientific Name)")
#' @param sel_env_var Character string of selected environmental variable (e.g., "temperature")
#' @param sel_qtr Numeric vector of selected quarters (1-4)
#' @param sel_date_range Date vector of length 2 (start date, end date)
#' @param sel_depth_range Numeric vector of length 2 (min depth, max depth) in meters
#' @param drawn_polygon sf object or data.frame representing user-drawn polygon (or NULL)
#'
#' @return Character vector of markdown-formatted filter descriptions
#'
#' @examples
#' prep_filter_summary(
#'   sel_name        = c("Anchovy (Engraulis mordax)", "Sardine (Sardinops sagax)"),
#'   sel_env_var     = "temperature",
#'   sel_qtr         = c(1, 2),
#'   sel_date_range  = as.Date(c("2000-01-01", "2020-12-31")),
#'   sel_depth_range = c(0, 100),
#'   drawn_polygon   = NULL
#' )
#'
#' @seealso \code{\link{modal_edit_filters}}, \code{\link{top_bar_taxa_ui}}, \code{\link{top_bar_env_ui}} for the controls that capture these filters
#'
#' @export
prep_filter_summary <- function(sel_name, sel_env_var, sel_qtr, sel_date_range,
                                sel_depth_range, drawn_polygon, selected_grid_zones,
                                ck_children, bio_datasets = NULL) {
  # The full picture of what's selected -- every row, every time.
  # `Label: **value**` -- value bold, label muted (ui.R #filter_summary CSS).
  out <- list()

  # Taxa
  taxa_val <- if (is.null(sel_name) || length(sel_name) == 0) "none"
              else if (length(sel_name) == 1) sel_name[1]
              else paste0(length(sel_name), " taxa selected")
  out <- c(out, paste0("Taxa: **", taxa_val, "**"))

  # Which datasets the numbers came from -- a CPUE could be ichthyo net tows,
  # CUFES egg-pump counts, or both, and a ?datasets= link opens on one of them
  # (it said "all 10" over a one-dataset map before 61d9524). Zero selected
  # reads as all because that is what the filter does.
  n_bio_all <- nrow(d_bio_datasets)
  ds_val <- if (is.null(bio_datasets) || length(bio_datasets) == 0 ||
                length(bio_datasets) == n_bio_all) paste0("all ", n_bio_all)
            else paste(dataset_label(bio_datasets), collapse = ", ")
  out <- c(out, paste0("Taxa datasets: **", ds_val, "**"))

  # Environmental variable -- "<label> — <dataset>", matching the top-bar card
  var_val <- if (is.null(sel_env_var) || !nzchar(sel_env_var)) {
    "none"
  } else {
    i <- match(sel_env_var, d_env_vars$measurement_type)
    if (is.na(i)) sel_env_var
    else paste0(d_env_vars$label[i], " — ", dataset_label(d_env_vars$dataset_key[i]))
  }
  out <- c(out, paste0("Variable: **", var_val, "**"))

  # Quarters -- always spelled out
  qn <- c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4")
  q_val <- if (length(sel_qtr) == 0) "none"
           else paste(qn[as.character(sort(as.integer(sel_qtr)))], collapse = ", ")
  out <- c(out, paste0("Quarters: **", q_val, "**"))

  # Date range -- exact window
  out <- c(out, paste0(
    "Date Range: **", format(as.Date(sel_date_range[1]), "%Y-%m-%d"),
    " to ", format(as.Date(sel_date_range[2]), "%Y-%m-%d"), "**"))

  # Depth range (environmental layers only -- get_sp() has no depth filter)
  out <- c(out, paste0(
    "Depth Range: **", sel_depth_range[1], " - ", sel_depth_range[2], " m**"))

  # Spatial
  spatial_val <- if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "drawn polygon"
    else if (!is.null(selected_grid_zones) && length(selected_grid_zones) > 0)
      (if (length(selected_grid_zones) <= 3) paste(selected_grid_zones, collapse = ", ")
       else paste(length(selected_grid_zones), "regions selected"))
    else "All locations"
  out <- c(out, paste0("Spatial: **", spatial_val, "**"))

  # Include children
  out <- c(out, paste0("Include Children: **", ifelse(ck_children, "Yes", "No"), "**"))

  out
}

#' Summary-statistics cards for the Map panel's "Summary Statistics" section.
#'
#' One SQL summarize per side (everything computed IN the database -- the old
#' version did nrow(collect()), materializing millions of rows for two
#' integers). Returns a data.frame of `value` (pre-formatted) + `label`, which
#' \code{output$summary_statistics} renders as a card grid.
#'
#' @param df_sp,df_env dbplyr lazy tables from \code{get_sp()}/\code{get_env()}
#' @param env_label short label for the environmental variable (e.g. "Water temp")
#' @return data.frame(label, sp, env, is_head) -- a matched Species/Environment
#'   matrix. Row 1 (`is_head = TRUE`) is the identity of each side (taxon name /
#'   variable); the rest are matched metrics, "—" where a side has no equivalent.
prep_summary_stats <- function(df_sp, df_env, env_label = "Env. value") {
  sp_agg <- df_sp |>
    summarize(
      n_obs    = n(),
      n_taxa   = n_distinct(taxon_key),
      sci      = min(scientific_name, na.rm = TRUE),
      mean_ab  = mean(std_tally, na.rm = TRUE),
      p02_ab   = sql("quantile_cont(std_tally, 0.02)"),
      p98_ab   = sql("quantile_cont(std_tally, 0.98)"),
      date_min = min(time_start, na.rm = TRUE),
      date_max = max(time_start, na.rm = TRUE)) |>
    collect()
  env_agg <- df_env |>
    summarize(
      n_obs    = n(),
      mean_v   = mean(qty, na.rm = TRUE),
      # 2nd/98th percentile -- the raw min/max exposes 99-type missing-value
      # sentinels; this matches the range the map legend actually spans
      p02_v    = sql("quantile_cont(qty, 0.02)"),
      p98_v    = sql("quantile_cont(qty, 0.98)"),
      d_min    = min(depth_m, na.rm = TRUE),
      d_max    = max(depth_m, na.rm = TRUE),
      date_min = min(dtime,   na.rm = TRUE),
      date_max = max(dtime,   na.rm = TRUE)) |>
    collect()

  fmt_n <- function(x) {
    x <- as.numeric(x)
    if (!is.finite(x)) return("—")
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  fmt_v <- function(x) {
    x <- as.numeric(x)
    if (!is.finite(x)) return("—")
    a <- abs(x)
    if (a >= 1000) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
    else if (x == round(x)) format(round(x), big.mark = ",", trim = TRUE)
    else if (a >= 10) sprintf("%.1f", x)
    else sprintf("%.2f", x)
  }
  fmt_rng <- function(a, b) {
    a <- as.numeric(a); b <- as.numeric(b)
    if (!is.finite(a) || !is.finite(b)) return("—")
    paste0(fmt_v(a), "–", fmt_v(b))
  }
  fmt_span <- function(a, b) {
    y <- suppressWarnings(c(as.integer(format(as.Date(a), "%Y")),
                            as.integer(format(as.Date(b), "%Y"))))
    if (anyNA(y)) return("—")
    if (y[1] == y[2]) as.character(y[1]) else paste0(y[1], "–", y[2])
  }
  fmt_depth <- function(a, b) {
    a <- as.numeric(a); b <- as.numeric(b)
    if (!is.finite(a) || !is.finite(b)) return("—")
    sprintf("%.0f–%.0f m", a, b)
  }

  # short variable name for the header ("temp", "oxygen", ...)
  el <- tolower(sub("\\s*\\(.*$", "", env_label))
  env_what <- if (grepl("temperature", el)) "Temperature"
    else if (grepl("oxygen", el)) "Oxygen"
    else if (grepl("salinit", el)) "Salinity"
    else if (grepl("chlorophyll", el)) "Chlorophyll-a"
    else tools::toTitleCase(el)

  sp_what <- if (isTRUE(sp_agg$n_taxa > 1)) paste0(sp_agg$n_taxa, " taxa")
             else if (is.na(sp_agg$sci) || !nzchar(sp_agg$sci)) "—"
             # abbreviate the genus in a binomial so it fits the narrow column
             else sub("^([A-Za-z])[a-z]+ ", "\\1. ", sp_agg$sci)

  data.frame(
    label = c("", "Obs.", "Years", "Mean", "Typ.", "Depth"),
    sp = c(
      sp_what,
      fmt_n(sp_agg$n_obs),
      fmt_span(sp_agg$date_min, sp_agg$date_max),
      fmt_v(sp_agg$mean_ab),
      fmt_rng(sp_agg$p02_ab, sp_agg$p98_ab),
      "—"),
    env = c(
      env_what,
      fmt_n(env_agg$n_obs),
      fmt_span(env_agg$date_min, env_agg$date_max),
      fmt_v(env_agg$mean_v),
      fmt_rng(env_agg$p02_v, env_agg$p98_v),
      fmt_depth(env_agg$d_min, env_agg$d_max)),
    is_head = c(TRUE, rep(FALSE, 5)),
    stringsAsFactors = FALSE)
}

taxa_tree_builder <- function(df_sp) {
  # observed taxa: match by taxon_key, NOT the display-name string. get_sp() builds
  # df_sp$name as "common (scientific)" while the species/taxon join below builds
  # it as "common (rank: scientific)" — since the unified-taxon reprep populated
  # taxonRank, those two formats diverge and a name-based filter silently returns
  # zero rows (then the downstream select on acceptedNameUsageID errors).
  # taxon_key is the stable key both sides already carry, and unlike worms_id it
  # exists for every taxon — no bird family or order has an AphiaID.
  sel_keys <- unique(df_sp |> pull(taxon_key))

  # get counts by taxa in data
  df_counts <- df_sp |>
    summarize(
      n = sum(!is.na(std_tally)),
      .by = c(taxon_key, parent_id)) |>
    collect()

  # build data.tree of taxa
  tree_counts <- tbl(con, "species") |>
    filter(taxon_key %in% sel_keys) |>
    select(taxon_key) |>
    collect() |>
    # get children of user-selected taxa
    mutate(
      children = map(taxon_key, get_taxon_children, con = con) ) |>
    unnest(children) |>
    select(taxon_key = acceptedNameUsageID, parent_id = parentNameUsageID, sci_name = scientificName) |>
    unique() |>
    # combine with observation counts
    left_join(
      df_counts, by = join_by(taxon_key, parent_id)
    ) |>
    mutate(
      # a parent outside the selected set becomes the tree root. "0" rather than
      # 0 because taxon_key is a string now.
      parent_id = ifelse(!(parent_id %in% taxon_key) | is.na(parent_id), "0", parent_id),
      sci_name = paste0("<i>", sci_name, "</i>"),
      n = ifelse(is.na(n),0,n)
    ) |>
    # transform to data.tree
    FromDataFrameNetwork()

  # aggregate counts and add leaves for unidentified observations
  tree_counts$Do(function(node) {
    if (!isLeaf(node)) {
      unident_n <- node$n %||% 0
      if (unident_n > 0) {
        child <- node$AddChild("Unidentified")
        child$sci_name  <-  paste0("Unidentified ", node$sci_name)
        child$n                <-  unident_n
      }
      node$n <- Aggregate(node, "n", sum)
    }
  }, traversal = "post-order")

  # remove taxa with no observations
  Prune(tree_counts, function(node) {node$n > 0})

  # helper function to transform nodes to HTML <li>
  makeTreeItem <- function(node) {
    id <- paste0("node_", gsub("[^A-Za-z0-9_]", "_", node$path))

    # Flex container for name + obs
    label_inner <- tags$span(
      class = "tree-label-inner",
      tags$span(class = "tree-name",  HTML(node$sci_name %||% node$name)),
      tags$span(class = "tree-obs",   HTML(format(node$n, big.mark = ",")  %||% ""))
    )

    if (length(node$children) == 0) {
      tags$li(
        class = "tree-leaf",
        tags$span(class = "tree-label", label_inner)
      )
    } else {
      tags$li(
        class = "tree-branch",
        tags$input(type = "checkbox", id = id, class = "tree-toggle"),
        tags$label(`for` = id, class = "tree-label", label_inner),
        tags$ul(lapply(node$children, makeTreeItem))
      )
    }
  }

  # helper function to make full HTML tree
  makeTree <- function(root) {
    tags$ul(class = "treeview", lapply(root$children, makeTreeItem))
  }

  taxa_tree_html <- makeTree(tree_counts)

  return(taxa_tree_html)
}

# spatial boundary layers ----

#' Add spatial boundary layers from PMTiles sources
#'
#' Adds all spatial boundary layers registered in \code{d_spatial_layers}
#' to a maplibre map. Layers are created hidden by default; visibility
#' controlled via the layers control or proxy. Includes yellow hover
#' highlight and tooltip from the \code{tooltip_field} column.
#'
#' @param map A maplibre map object
#' @param d_layers Data frame from \code{metadata/spatial_layers.csv}
#' @param is_dark Logical; TRUE for dark theme styling
#'
#' @return Modified maplibre map with PMTiles sources and layers (no control)
add_spatial_layers <- function(map, d_layers, visible_ids = NULL, is_dark = TRUE) {

  # determine which layers are visible
  if (is.null(visible_ids))
    visible_ids <- d_layers |> filter(default_visible) |> pull(dataset_id)

  # add one pmtiles source per unique dataset_group
  # promote_id = "id" so setFeatureState works for hover highlighting
  for (grp in unique(d_layers$dataset_group)) {
    url <- glue("{pmtiles_base_url}/{grp}.pmtiles")
    map <- map |>
      add_pmtiles_source(id = grp, url = url, promote_id = "id")
  }

  # add one layer per row
  for (i in seq_len(nrow(d_layers))) {
    row <- d_layers[i, ]
    vis <- ifelse(row$dataset_id %in% visible_ids, "visible", "none")

    # parse filter expression if present
    filt <- if (!is.na(row$filter_expr)){
      jsonlite::fromJSON(row$filter_expr, simplifyVector = FALSE)
    } else {
      NULL
    }

    # tooltip: "<strong>name</strong> - layer" (always available in PMTiles)
    tt <- list(
      "concat",
      "<strong>", list("get", "name"), "</strong>",
      " - ", list("get", "layer"))

    if (row$geom_type == "line") {
      map <- map |>
        add_line_layer(
          id            = row$dataset_id,
          source        = row$dataset_group,
          source_layer  = row$dataset_group,
          line_color    = row$line_color,
          line_width    = row$line_width,
          line_opacity  = 0.7,
          visibility    = vis,
          filter        = filt,
          tooltip       = tt,
          popup         = tt,
          hover_options = list(
            line_color = "#ffeb3b",
            line_width = row$line_width + 2))

    } else if (row$geom_type == "polygon") {
      map <- map |>
        add_fill_layer(
          id                 = row$dataset_id,
          source             = row$dataset_group,
          source_layer       = row$dataset_group,
          fill_color         = row$fill_color,
          fill_opacity       = row$fill_opacity,
          fill_outline_color = row$line_color,
          visibility         = vis,
          filter             = filt,
          tooltip            = tt,
          popup              = tt,
          hover_options      = list(
            # line_opacity = 0.7
            # fill_color = "#ffeb3b")
            # fill_color = "yellow",
            # fill_opacity = 1
            fill_opacity = min(row$fill_opacity + 0.35, 0.6))) |>
        add_line_layer(
          id           = paste0(row$dataset_id, "_outline"),
          source       = row$dataset_group,
          source_layer = row$dataset_group,
          line_color   = row$line_color,
          line_width   = row$line_width,
          line_opacity = 0.7,
          visibility   = vis,
          filter       = filt,
          hover_options = list(
            line_color = "yellow",
            line_opacity = 1,
            line_width = row$line_width * 2))

    } else if (row$geom_type == "point") {
      map <- map |>
        add_circle_layer(
          id              = row$dataset_id,
          source          = row$dataset_group,
          source_layer    = row$dataset_group,
          circle_color    = row$fill_color,
          circle_radius   = 4,
          circle_opacity  = 0.8,
          visibility      = vis,
          filter          = filt,
          tooltip         = tt,
          popup           = tt,
          hover_options   = list(
            circle_color  = "#ffeb3b",
            circle_radius = 7))
    }
  }

  map
}

#' Build grouped layers control list for the map
#'
#' Constructs the named list for \code{add_layers_control(layers = ...)}
#' with one entry per visible spatial layer (polygons pair fill + outline)
#' plus a "Hexagon Data" entry for all hex layer IDs.
#'
#' @param visible_ids Character vector of currently visible spatial layer IDs
#' @param d_layers Full spatial layers registry data frame
#' @param hex_layer_ids Character vector of data layer IDs (both sp + env)
#' @param label Character name for the data-layer entry. Defaults to
#'   "Hexagon Data"; the polygon-summary path passes its own so the control
#'   names what is actually drawn.
#'
#' @return Named list suitable for \code{add_layers_control(layers = ...)}
build_layers_control <- function(visible_ids, d_layers, hex_layer_ids,
                                 label = "Hexagon Data") {
  visible <- d_layers |> filter(dataset_id %in% visible_ids)

  # each layer is its own toggle entry; polygons pair fill + outline
  layer_entries <- lapply(seq_len(nrow(visible)), function(i) {
    row <- visible[i, ]
    ids <- row$dataset_id
    if (row$geom_type == "polygon")
      ids <- c(ids, paste0(ids, "_outline"))
    setNames(list(ids), row$layer)
  })

  # combine data layers (BOTH sp + env IDs) + individual layer entries
  c(setNames(list(hex_layer_ids), label),
    unlist(layer_entries, recursive = FALSE))
}

# visualization functions ----

#' Create Interactive Species Distribution Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying species abundance
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param sp_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated species abundance
#' @param sp_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#'
#' @return maplibre object with multi-resolution hexagonal layers, legend, and scale control
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance. Abundance values are standardized as count per 10m² surface area.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' sp_scale <- lapply(sp_hex, function(x) scales::col_numeric("YlOrRd", domain = range(x$sp.value)))
#' map_sp(sp_hex, sp_scale)
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for data aggregation
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend add_scale_control
#'
#' @export
map_sp <- function(sp_hex_list, sp_scale_list, is_dark = T) {
  if (debug) {
    message("map_sp: creating species map with ", length(sp_hex_list), " resolution layers")
    message("map_sp: first layer has ", nrow(sp_hex_list[[1]]), " hexagons")
  }

  # base map
  sp_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(sp_hex_list[[1]])) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # boundary layers first, BELOW the hexes -- above them they intercept the
  # hex hover/tooltip
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  sp_map  <- sp_map |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  # add each resolution layer with hover highlight
  for (i in 1:length(res_range)) {
    sp_map <- sp_map |>
      add_fill_layer(
        id                 = paste0("sp", res_range[i]),
        source             = st_as_sf(sp_hex_list[[i]]),
        fill_color         = sp_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.6,
        min_zoom           = zoom_breaks[i],
        max_zoom           = zoom_breaks[i+1],
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  # add layers control with hexagons + visible spatial layers. Only THIS map's
  # own layer ids: a control listing the other side's ids cannot toggle them
  # (different map) and throws on the way back ON, because the client does not
  # guard set_layout_property against a missing layer.
  ctrl    <- build_layers_control(vis_ids, d_spatial_layers,
                                  paste0("sp", res_range))
  sp_map  <- sp_map |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)

  return(sp_map)
}


#' Create Interactive Environmental Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying environmental data
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param env_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated environmental values
#' @param env_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#' @param env_stat_label Character string describing the statistic (e.g., "Mean", "Median")
#' @param env_var_label Character string describing the variable (e.g., "Temperature (°C)")
#'
#' @return maplibre object with multi-resolution hexagonal layers and legend
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("temperature", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' env_scale <- lapply(env_hex, function(x) scales::col_numeric("viridis", domain = range(x$env.value)))
#' map_env(env_hex, env_scale, "Mean", "Temperature (°C)")
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for data aggregation
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend
#'
#' @export
map_env <- function(env_hex_list, env_scale_list, env_stat_label, env_var_label, is_dark = T) {
  if (debug) {
    message("map_env: creating environmental map with ", length(env_hex_list), " resolution layers")
    message("map_env: first layer has ", nrow(env_hex_list[[1]]), " hexagons")
  }

  # create base map
  env_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(env_hex_list[[1]])) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # boundary layers first, BELOW the hexes (see map_sp)
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  env_map <- env_map |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  # add each resolution layer with hover highlight
  for (i in 1:length(res_range)) {
    env_map <- env_map |>
      add_fill_layer(
        id                 = paste0("env", res_range[i]),
        source             = st_as_sf(env_hex_list[[i]]),
        fill_color         = env_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.6,
        min_zoom           = zoom_breaks[i],
        max_zoom           = zoom_breaks[i+1],
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  # add layers control with hexagons + visible spatial layers (this map's ids
  # only — see the note in map_sp())
  ctrl    <- build_layers_control(vis_ids, d_spatial_layers,
                                  paste0("env", res_range))
  env_map <- env_map |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)

  return(env_map)
}


#' Build a Colour Scale for a Polygon Summary
#'
#' \code{interpolate_palette} wrapper that survives the two degenerate cases a
#' polygon summary hits routinely: no polygon has data, and every polygon that
#' does has the same value (one sampled MPA, say). MapLibre rejects an
#' \code{interpolate} expression whose stops are not strictly ascending, so the
#' single-value case returns a flat colour instead — the same shape
#' \code{build_h3t_scale} returns.
#'
#' @param d tibble with a numeric \code{value} column
#' @param palette Function of n returning n colours
#' @param n_stops Integer number of colour stops
#'
#' @return List with \code{breaks}, \code{colors}, \code{expression}, or NULL
#'   when there is nothing to scale
#'
#' @export
poly_scale <- function(d, palette, n_stops = 5L) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  v <- d$value[is.finite(d$value)]
  if (length(v) == 0) return(NULL)

  if (min(v) == max(v)) {
    cols <- palette(2)
    return(list(breaks = c(min(v), max(v)), colors = cols, expression = cols[1]))
  }

  interpolate_palette(d, column = "value", palette = palette, n = n_stops)
}


#' Create Interactive Map Summarized Within a Spatial Layer's Polygons
#'
#' The polygon counterpart of \code{\link{map_sp}} / \code{\link{map_env}}: one
#' fill layer coloured by the per-polygon summary, plus an outline-only layer for
#' the polygons of the same boundary layer that contain no observations.
#'
#' @param sf_poly sf of the layer's polygons from \code{\link{get_layer_sf}}
#' @param d_val tibble from \code{\link{prep_sp_poly}}\code{$data} or
#'   \code{\link{prep_env_poly}}, joined on \code{spatial_key}
#' @param scale Colour scale list (\code{$expression}) from
#'   \code{interpolate_palette}, or NULL when no polygon has data
#' @param side Either "sp" (species, left) or "env" (environment, right);
#'   determines the layer IDs
#' @param is_dark Logical, dark basemap
#'
#' @return maplibre object
#'
#' @details
#' \strong{Empty polygons are drawn, not omitted.} Most polygons in a layer
#' contain no CalCOFI samples — 125 of 155 MPAs, for instance. Dropping them
#' would make the layer look sparser than it is and leave no way to tell an
#' unsampled polygon from one outside the layer; filling them would read as zero.
#' So they get an outline and a "no data" tooltip, and no fill.
#'
#' The map fits to the polygons that \emph{have} data, so switching aggregation
#' unit lands the user where the observations are rather than at the full extent
#' of a statewide layer. That \code{fit_bounds} is also what makes the legend
#' appear: legends are drawn by the \code{map_before_view} observer in server.R,
#' which fires on the resulting \code{moveend}.
#'
#' @seealso \code{\link{prep_sp_poly}}, \code{\link{prep_env_poly}},
#'   \code{\link{get_layer_sf}}
#'
#' @importFrom mapgl maplibre add_fill_layer add_line_layer add_scale_control
#' @importFrom dplyr left_join filter
#'
#' @export
map_poly <- function(sf_poly, d_val, scale, side = c("sp", "env"),
                     is_dark = TRUE) {
  side <- match.arg(side)

  sf_all <- sf_poly |>
    left_join(d_val |> select(-spatial_name), by = "spatial_key")

  sf_dat <- sf_all |> filter(!is.na(value))
  sf_nul <- sf_all |>
    filter(is.na(value)) |>
    mutate(tooltip = paste0("<strong>", spatial_name, "</strong><br>no data"))

  if (debug)
    message("map_poly (", side, "): ", nrow(sf_dat), " polygons with data, ",
            nrow(sf_nul), " without")

  # Set the view at CONSTRUCTION as well as via fit_bounds.
  #
  # fit_bounds() alone is not enough here: `output$map` re-renders the compare
  # widget when the aggregation unit changes, and on a re-render the client
  # applies the constructor's center/zoom but NOT `fitBounds` — leaving a map
  # parked at the default zoom 0, which on the globe projection is a pea-sized
  # planet and reads as "the feature is broken". Constructing with the view
  # already set works either way, and fit_bounds() stays because it is what
  # fires the `moveend` the legend observer in server.R listens for.
  bb    <- st_bbox(if (nrow(sf_dat) > 0) sf_dat else sf_all)
  span  <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
  zoom  <- if (is.finite(span) && span > 0)
    max(0, min(14, log2(360 / span) - 0.5)) else 5

  m <- maplibre(
    style  = carto_style(ifelse(is_dark, "dark-matter", "voyager")),
    center = c(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")])),
    zoom   = zoom) |>
    fit_bounds(bbox = if (nrow(sf_dat) > 0) sf_dat else sf_all) |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  # boundary reference layers first (below the summary)
  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |>
    add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  id_dat <- paste0(side, "_poly")
  id_nul <- paste0(side, "_poly_nodata")

  # Unsampled polygons: outline, so they read as "no data" and never as zero.
  #
  # The near-transparent grey fill underneath is there to be HOVERED, not seen —
  # an outline alone is a ~1px hit target, so "why does this one not have a
  # tooltip?" becomes indistinguishable from "I missed it by two pixels". A
  # neutral grey at 8% cannot be mistaken for a value on either scale (viridis
  # runs dark purple → yellow, Spectral blue → red).
  if (nrow(sf_nul) > 0) {
    m <- m |>
      add_fill_layer(
        id            = paste0(id_nul, "_hit"),
        source        = sf_nul,
        fill_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        fill_opacity  = 0.08,
        tooltip       = "tooltip",
        hover_options = list(fill_opacity = 0.25)) |>
      add_line_layer(
        id            = id_nul,
        source        = sf_nul,
        line_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        line_width    = 1,
        line_opacity  = 0.6,
        hover_options = list(line_color = "#ffeb3b", line_opacity = 1))
  }

  if (nrow(sf_dat) > 0 && !is.null(scale)) {
    m <- m |>
      add_fill_layer(
        id                 = id_dat,
        source             = sf_dat,
        fill_color         = scale$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.65,
        tooltip            = "tooltip",
        hover_options      = list(
          fill_outline_color = "#ffeb3b",
          fill_opacity       = 0.85))
  }

  ctrl <- build_layers_control(
    vis_ids, d_spatial_layers,
    c(id_dat, id_nul, paste0(id_nul, "_hit")),
    label = "Polygon Summary")

  m |>
    add_layers_control(
      position     = "top-right",
      layers       = ctrl,
      collapsible  = TRUE,
      margin_right = 45)
}


# UI component functions ----

#' Label for the Taxa tab's dataset-picker trigger
#'
#' States what the filter will actually do, which is why zero selected reads the
#' same as all selected: an empty selection is not a filter that matches nothing
#' (\code{\link{get_sp}} skips the dataset predicate entirely), so calling it
#' "none" would be a label that contradicts the results.
#'
#' Mirrored in JavaScript in \code{app/ui.R}, which rewrites this label on every
#' click; R only paints it the first time. Change the two together, or the
#' button tells the truth exactly once.
#'
#' @param selected character vector of selected \code{dataset_key}
#' @param n_all total number of datasets on offer
#' @return length-1 character label, e.g. \code{"3 of 14 datasets"}
#'
#' @export
bio_ds_label <- function(selected, n_all = nrow(d_bio_datasets)) {
  n <- length(selected)
  if (n == 0 || n >= n_all) sprintf("All %d datasets", n_all)
  else                      sprintf("%d of %d datasets", n, n_all)
}

#' Datasets List for the Filters Panel
#'
#' The Filters panel's "Datasets" section: a single flat, always-visible list
#' of \code{sel_bio_ds} checkboxes -- one row per dataset, sorted by category
#' (Fish / Crustaceans / Plankton / Birds & Mammals; see
#' \code{global.R::BIO_DATASET_CATEGORY}) with a small category tag on the
#' right of each row, rather than a grouped tree or a wrapping row of pills.
#'
#' This replaced two earlier designs in the same 2026-09-07 session (the
#' original collapsed-by-default category tree, then a toggle-chip layout)
#' after Betty reviewed 5 mockups side by side and picked this one --
#' "searchable flat list" minus the search box, since 10 datasets is too few
#' to need searching. \code{sel_bio_ds} itself is unchanged underneath: still
#' the one real \code{checkboxGroupInput}, still \code{dataset_key} values,
#' still collected as a flat vector -- only \code{choiceNames} carries custom
#' HTML per row (name + tag) instead of a plain string label, so no
#' client-side re-parenting/grouping JS is needed at all (the previous two
#' designs each needed one; this one doesn't, which is part of why it's
#' simpler to keep correct).
#'
#' This card lives inside \code{modal_edit_filters()}, which only exists in
#' the DOM once \code{showModal()} puts it there -- so, unlike the old
#' always-on-page popover, \code{input$sel_bio_ds} is \code{NULL} until a user
#' opens "Edit filters" for the first time. That is safe: every reader of it
#' (\code{sp_choices()}, \code{sp_names_for()}, \code{get_sp()},
#' \code{build_sp_sql()}/\code{build_env_sql()}) already treats
#' \code{datasets = NULL} as "no dataset filter", which is exactly the
#' all-datasets default \code{selected = d_bio_datasets$dataset_key} would
#' have produced anyway -- verified by reading every call site before making
#' this change, not assumed.
#'
#' Because \code{modal_edit_filters()} rebuilds this whole card from scratch
#' every time \code{showModal()} runs, whatever \code{selected} this function
#' was called with is what \code{sel_bio_ds} resets to the instant the modal
#' reopens -- a hardcoded "always every dataset" here means Applying a
#' narrowed selection and reopening the modal shows all datasets checked
#' again, as if nothing had been filtered (bug report 2026-09-07: "when i
#' filter to a certain dataset. then apply. then hit edit filters again, it
#' does not show the dataset that was filtered. it just shows all 10
#' again"). \code{selected} defaults to every dataset only so a first-ever
#' open (before anything has been applied) still starts unfiltered; every
#' later open should be passed the LAST APPLIED selection -- see
#' \code{modal_edit_filters()}'s \code{bio_datasets} param, and server.R's
#' \code{edit_filters} observer, which is what makes qtr/date_range/depth_range
#' survive a reopen the same way.
#'
#' @param selected character vector of dataset_key currently applied;
#'   defaults to every dataset (the correct state only on a session's first,
#'   never-yet-applied open of this modal)
#'
#' @return a \code{tagList} for the Filters panel's "Datasets" section
#'
#' @importFrom shiny checkboxGroupInput
#'
#' @export
dataset_list_picker_ui <- function(selected = d_bio_datasets$dataset_key) {
  cat_order <- c("Fish", "Crustaceans", "Plankton", "Birds & Mammals")
  cats      <- c(intersect(cat_order, unique(d_bio_datasets$category)),
                 setdiff(unique(d_bio_datasets$category), cat_order))

  # flat order, grouped by category (Fish rows together, then Crustaceans,
  # ...) so the tag column alone is enough to read the grouping -- no
  # section headers or "All"-per-category control needed at 10 datasets.
  ord <- order(match(d_bio_datasets$category, cats), d_bio_datasets$label)
  df  <- d_bio_datasets[ord, , drop = FALSE]

  # dataset_short_label() is scalar-only (its DATASET_LABELS[[key]] lookup
  # errors on a multi-element index) -- every other call site in the app
  # already respects that, applying it per-key inside a loop/lapply rather
  # than handing it a whole vector at once; do the same here.
  labels <- vapply(df$dataset_key, dataset_short_label, character(1), USE.NAMES = FALSE)

  # unname() matters: Map()/mapply() names its result from the first
  # argument's own values when that argument is an unnamed character vector
  # (labels here) -- checkboxGroupInput's normalizeChoicesArgs() rejects a
  # *named* choiceNames/choiceValues outright ("must not be named"), so a
  # named list here breaks the whole picker even though every element is
  # otherwise exactly what's wanted.
  choice_names <- unname(Map(function(lab, cat) {
    tagList(
      tags$span(class = "cc-ds-row-name", lab),
      tags$span(class = "cc-ds-row-tag", toupper(cat)))
  }, labels, df$category))

  div(
    class = "cc-ds-list",
    div(
      class = "d-flex justify-content-between align-items-center mb-2",
      tags$span(
        class = "d-inline-flex align-items-center gap-2",
        bs_icon("layers"), tags$strong("Datasets")),
      tags$span(
        id    = "ds_tree_total",
        class = "small text-muted",
        bio_ds_label(selected))),
    checkboxGroupInput(
      "sel_bio_ds",
      NULL,
      choiceNames  = choice_names,
      choiceValues = df$dataset_key,
      selected     = selected,
      width        = "100%"),
    div(
      class = "small text-muted",
      "Uncheck a dataset to exclude it from the map and species list.")
  )
}


#' Top-bar Taxa Search
#'
#' The Species/Taxa control that lives directly in the main content top bar
#' (not in a modal) -- taxa search is the PRIMARY control, per the redesign:
#' search is always visible, and changing it applies immediately rather than
#' waiting behind a Submit button. \code{sel_name}/\code{ck_children} keep the
#' SAME input ids they always had, so every server.R reader of
#' \code{input$sel_name} etc. needs no change -- only where the widgets live
#' in the DOM moved, not their identity.
#'
#' Per the approved mockup, this card shows the search box alone -- no
#' Datasets picker inline. That control (\code{sel_bio_ds}) still exists with
#' the same id and choices; it moved into \code{modal_edit_filters()}'s
#' Datasets list, alongside Depth/Layers/Time, since the mockup's top bar
#' never shows a dataset toggle. Also: single-select now (\code{multiple =
#' FALSE}), matching the mockup showing exactly one taxon at a time -- this
#' session's own earlier multi-select was never something Betty asked for or
#' tested against; get_sp() already accepts a length-1 vector unchanged.
#'
#' Visually restyled (see the \code{.cc-search-combo} CSS + selectize
#' \code{render} templates in ui.R) to read as a search box -- magnifying-
#' glass icon, bold selected name with a muted dataset suffix, chevron -- with
#' a grouped, dataset-headed dropdown, instead of the plain stock selectize
#' control. Purely a client-side skin: choices/selected/the input id are
#' unchanged, so this is not a behavior change server.R needs to know about.
#'
#' @return a \code{tagList} for the top-bar "Species / Taxa" card
#'
#' @importFrom shiny selectizeInput checkboxInput tagList
#'
#' @export
top_bar_taxa_ui <- function() {
  # dataset LABEL -> "Program (Institution)" lookup, embedded once as JSON so
  # the .cc-combo2 JS (ui.R's CCCombo) can show a provider line under each
  # group header without a server round-trip. Keyed by dataset_label(key) --
  # the release's own short name when it has one, NOT the static
  # DATASET_LABELS string -- because that release-first name is exactly what
  # sp_choices()/env_var_choices() use as the optgroup label the JS sees; a
  # release with its own dataset_name_short would otherwise never match.
  providers_by_key   <- attrib_provider_summary(names(DATASET_LABELS))
  providers_by_label <- setNames(as.character(providers_by_key),
                                  dataset_label(names(providers_by_key)))

  tagList(
    tags$script(HTML(sprintf(
      "window.CC_ATTRIB_PROVIDERS = %s;",
      jsonlite::toJSON(as.list(providers_by_label), auto_unbox = TRUE)))),
    div(class = "cc-card-label small text-muted text-uppercase mb-1", "Species / Taxa"),
    div(
      # .cc-combo2: the custom grouped/drilldown search panel (matching the
      # approved mockup) that ui.R's JS builds around the REAL selectizeInput
      # below -- see that JS for why the widget stays in the DOM (server.R's
      # updateSelectizeInput(session, "sel_name", ...) still targets it) but
      # is visually hidden, driven entirely through its own JS API
      # (setValue()/.options/.optgroups) rather than being replaced by
      # something Shiny doesn't know about. `sel_name`'s id, choices shape
      # and reactive value are completely unchanged.
      #
      # data-extra-*: "Include taxonomic children" (ck_children) now lives as a
      # row at the TOP of the dropdown panel rather than a separate control in
      # the card -- the real checkboxInput stays in the DOM, hidden, and the
      # combo JS renders a mirror row that toggles it (same pattern as the
      # hidden selectize). Keeps input$ck_children and every reader unchanged.
      class = "cc-combo2",
      `data-placeholder-main`  = "Search species / taxa…",
      `data-placeholder-drill` = "Search taxa — sardine, krill, seabird…",
      `data-noun`              = "taxa",
      `data-extra-id`    = "ck_children",
      `data-extra-label` = "Include taxonomic children",
      `data-extra-tip`   = paste(
        "Include observations recorded at finer taxonomic levels, e.g.",
        "observations to Genus and Species when the taxon picked is a Family;",
        "otherwise only observations recorded to the chosen level."),
      div(
        class = "cc-combo2-btn", tabindex = "0", role = "button",
        `aria-haspopup` = "listbox",
        bs_icon("search", class = "cc-combo2-icon"),
        tags$span(class = "cc-combo2-label"),
        bs_icon("caret-down-fill", class = "cc-combo2-chevron")),
      div(class = "cc-combo2-panel"),
      selectizeInput(
        "sel_name",
        label    = NULL,
        # dataset-grouped choices, mirroring env_var_choices()'s optgroups
        # (global.R::sp_choices()) -- this widget always exists now (no
        # longer rebuilt each time a modal opens), so its choices are set
        # once, here, at UI build time
        choices  = sp_choices(),
        selected = default_sp_name,
        multiple = FALSE,
        width    = "100%",
        options  = list(
          placeholder = "Search species / taxa…",
          # render templates split the item's own dataset-suffixed label
          # (sp_choices() bakes "Name — Dataset" into every option) back apart
          # using selectize's own `optgroup` field -- which Shiny sets to the
          # SAME dataset string -- rather than a fragile string split, so a
          # taxon common name that happens to contain " — " itself can't
          # misparse. See the .cc-search-combo CSS in ui.R for the styling.
          # I(...) passes this through as literal JS, not a quoted R string.
          render = I("{
            option: function(item, escape) {
              var ds = item.optgroup || '';
              var nm = ds && item.label.slice(-ds.length) === ds
                ? item.label.slice(0, -(ds.length + 3)) : item.label;
              return '<div class=\"cc-search-opt\"><span class=\"cc-search-opt-name\">' +
                escape(nm) + '</span>' +
                (ds ? '<span class=\"cc-search-opt-ds\"> — ' + escape(ds) + '</span>' : '') +
                '</div>';
            },
            item: function(item, escape) {
              var ds = item.optgroup || '';
              var nm = ds && item.label.slice(-ds.length) === ds
                ? item.label.slice(0, -(ds.length + 3)) : item.label;
              return '<div class=\"cc-search-item\"><span class=\"cc-search-item-name\">' +
                escape(nm) + '</span>' +
                (ds ? '<span class=\"cc-search-item-ds\"> — ' + escape(ds) + '</span>' : '') +
                '</div>';
            },
            optgroup_header: function(data, escape) {
              return '<div class=\"cc-search-grp\">' + escape(data.label) + '</div>';
            }
          }"))),
      # real input, kept in the DOM but visually hidden -- the combo JS shows a
      # mirror of it at the top of the dropdown panel (see data-extra-* above)
      div(
        class = "cc-combo2-extra-src",
        checkboxInput("ck_children", label = "Include taxonomic children",
                      # off by default: most picks here are specific, curated
                      # entries (e.g. "Decapods -- Dungeness Crab Megalopae"),
                      # not a taxonomic node meant to be browsed broadly, and
                      # a broad order/family node has plenty of descendants to
                      # pull in unrelated programs' data if this defaulted on
                      # (see the cpue_unit mixing this exact case turned up)
                      value = FALSE)))
  )
}

#' Map-tab controls (Split / Swipe)
#'
#' Was Split/Swipe + Stats + Options + Map Layers on the filter-chip row. Stats,
#' Options and Map Layers moved into the always-visible left panel
#' (\code{map_panel_ui()}); only the compare-layout toggle still rides the chip
#' row, so \code{cmp_layout} keeps its id, placement and reactive wiring.
#'
#' @return a tagList holding just the Split/Swipe control
#' @export
map_tab_controls <- function() {
  tagList(
    # compare layout: side-by-side synced maps (default) vs the draggable swipe
    # divider. Only meaningful while comparing -- hidden by CSS (body:has on
    # #cmp_env_toggle) when compare is off, rather than a conditionalPanel that
    # misbehaves as a flex item. Drives server.R's renderMaplibreCompare()
    # `mode=`; both keep the "map" element id and before/after proxy sides.
    div(
      class = "cc-cmp-layout-wrap",
      radioGroupButtons(
        "cmp_layout",
        label    = NULL,
        choices  = c("Split" = "split", "Swipe" = "swipe"),
        selected = "split",
        size     = "sm",
        status   = "outline-secondary"))
  )
}

#' Per-group thumbnail for the Map Layers modal
#'
#' A tiny abstract map -- a shared coastline wedge plus a group-specific accent
#' motif -- rendered as an inline data-URI so each card in \code{modal_map_layers()}
#' shows what kind of thing the layer is. Colours track the group.
cc_lyr_thumb <- function(group) {
  motif <- list(
    "Administrative"     = list(a = "#7fbfff",
      m = "<path d='M60 2v62M80 2v62M44 20h48M40 42h52' stroke='%COL%' stroke-width='2.6' opacity='.9'/>"),
    "Maritime Zones"     = list(a = "#4aa8ff",
      m = "<path d='M42 2C31 18 35 30 24 45 19 53 26 59 22 68' fill='none' stroke='%COL%' stroke-width='3' stroke-dasharray='4 4'/>"),
    "Ecological"         = list(a = "#5fd08f",
      m = "<ellipse cx='54' cy='16' rx='11' ry='7' fill='%COL%' opacity='.85'/><ellipse cx='41' cy='41' rx='10' ry='6' fill='%COL%' opacity='.85'/><ellipse cx='52' cy='60' rx='10' ry='6' fill='%COL%' opacity='.85'/>"),
    "Protected Areas"    = list(a = "#f79b57",
      m = "<circle cx='54' cy='14' r='4.5' fill='%COL%'/><circle cx='41' cy='37' r='4.5' fill='%COL%'/><circle cx='52' cy='58' r='4.5' fill='%COL%'/>"),
    "Energy & Industry"  = list(a = "#f5c94e",
      m = "<rect x='43' y='13' width='10' height='10' rx='1.5' fill='%COL%'/><rect x='55' y='13' width='10' height='10' rx='1.5' fill='%COL%' opacity='.75'/><rect x='43' y='25' width='10' height='10' rx='1.5' fill='%COL%' opacity='.75'/>"))
  cfg <- motif[[group]]
  if (is.null(cfg)) cfg <- list(a = "#7fbfff",
    m = "<path d='M42 2C31 18 35 30 24 45 19 53 26 59 22 68' fill='none' stroke='%COL%' stroke-width='2.8'/>")
  # a soft blue coastline wedge (not black) with a thicker outline
  coast <- paste0(
    "<path d='M42 2C31 18 35 30 24 45 19 53 26 59 22 68H0V0Z' fill='#1c3a55' opacity='.55'/>",
    "<path d='M42 2C31 18 35 30 24 45 19 53 26 59 22 68' fill='none' stroke='#3f7fb8' stroke-width='2' opacity='.7'/>")
  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 66'>",
    "<rect width='100' height='66' fill='#0e1c2b'/>", coast,
    gsub("%COL%", cfg$a, cfg$m, fixed = TRUE), "</svg>")
  paste0("url('data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE), "')")
}

#' Corner dismiss (X) button for a modalDialog title bar
#'
#' Bootstrap handles \code{data-bs-dismiss="modal"} itself and Shiny removes the
#' modal wrapper on \code{hidden.bs.modal}, so this needs no server observer --
#' the same close path as the backdrop click / Esc these modals already use.
#' Drop it into a modal's \code{title = tagList(...)}; \code{.cc-modal-x} (ui.R)
#' floats it to the top-right corner.
#'
#' @return a \code{<button class="btn-close">} tag
#' @export
modal_x_btn <- function() tags$button(
  type = "button", class = "btn-close cc-modal-x",
  `data-bs-dismiss` = "modal", `aria-label` = "Close")

#' Feedback modal
#'
#' A note + optional email + an optional full-view screenshot (captured by
#' \code{app/www/cc-feedback.js} via the browser's screen-share prompt, with a
#' pen/box/arrow/text annotator), posted CLIENT-SIDE -- no server round-trip --
#' to the Google Apps Script at \code{endpoint}, which writes a Sheet row, saves
#' the screenshot to Drive, mails the recipients tab and files a public issue in
#' \code{CalCOFI/db-viz-hex} without the email, then answers \code{{ok, id,
#' image_url, issue_url, status}}. The script is generated by
#' \code{calcofi4r::cc_feedback_script()} (see \code{feedback/README.md}); same
#' contract as \code{CalCOFI/explore}'s \code{src/feedback.tsx}. Opened from the
#' \code{.cc-feedback-link} navbar item (server: \code{input$open_feedback}).
#'
#' @param release the frozen DB release string, stamped into the report
#' @param endpoint the Apps Script \code{/exec} URL; \code{""} -> the dialog
#'   offers only the "open a GitHub issue" fallback
#' @return a Shiny \code{modalDialog}
#'
#' @importFrom shiny modalDialog textAreaInput textInput tags tagList
#' @export
modal_feedback <- function(release = "", endpoint = "") {
  modalDialog(
    title = tagList(bs_icon("chat-square-text"), "Feedback", modal_x_btn()),
    div(
      class = "cc-feedback-modal",
      `data-endpoint` = endpoint,
      `data-release`  = release,
      div(
        class = "cc-fb-grid",
        # left: the note
        div(
          class = "cc-fb-col",
          textAreaInput(
            "fb_text",
            "What happened, or what would help?",
            rows = 4, width = "100%",
            placeholder = paste0(
              "e.g. the 1955 oxygen spike near 1,144 m looks wrong — ",
              "the station reads 2.2 ml/L…")),
          textInput(
            "fb_email",
            tagList("Email ", tags$span(
              class = "cc-fb-sub",
              "— optional, for a reply. Never shown publicly.")),
            width = "100%", placeholder = "you@example.org"),
          tags$p(
            class = "cc-fb-sent",
            sprintf("Sends your note, this view’s link, the release (%s) and screen size. ",
                    if (nzchar(release)) release else "this build"),
            tags$span(
              class = "cc-fb-peek", role = "button", tabindex = "0",
              `data-bs-toggle` = "collapse", `data-bs-target` = "#cc-fb-detail",
              "What that looks like ›")),
          div(
            class = "collapse", id = "cc-fb-detail",
            tags$ul(
              class = "cc-fb-detail",
              tags$li(tags$b("Link"), " — reopens this exact view"),
              tags$li(tags$b("Context"),
                      sprintf(" — release %s, viewport, theme",
                              if (nzchar(release)) release else "(unversioned dev build)")),
              tags$li(tags$b("Not sent"),
                      " — anything else; the email is stripped from the public issue")))),
        # right: the screenshot (captured + filled by the CCFeedback script in
        # ui.R when the modal opens; the maps carry preserveDrawingBuffer)
        div(
          class = "cc-fb-col",
          tags$label(class = "cc-fb-shotlabel", "Screenshot of this view"),
          div(
            class = "cc-fb-shot",
            # cc-feedback.js fills this with a Capture button on modal open, then
            # the screenshot after the browser's share-tab prompt.
            div(class = "cc-fb-shot-img",
                div(class = "cc-fb-shot-wait", "…")),
            div(
              class = "cc-fb-shot-bar",
              tags$label(
                class = "cc-fb-inc",
                tags$input(type = "checkbox", id = "fb_include", checked = NA),
                " include"),
              tags$button(type = "button", class = "cc-fb-edit",
                          bs_icon("pencil"), " Annotate"),
              tags$button(type = "button", class = "cc-fb-retake",
                          bs_icon("arrow-repeat"), " retake")))))),
    footer = tagList(
      tags$a(class = "cc-fb-gh", href = "#", "Open a GitHub issue instead"),
      tags$span(class = "cc-fb-status", `aria-live` = "polite"),
      tags$button(type = "button", class = "btn btn-primary cc-fb-send",
                  bs_icon("send"), "Send")),
    size = "l", easyClose = TRUE, fade = FALSE)
}

#' The "Map Layers" overlay (functions.R -> server.R input$open_map_layers)
#'
#' A full-width modal of reference-boundary layers as thumbnail cards, grouped
#' by \code{d_spatial_layers$group}. Each group is a \code{checkboxGroupInput}
#' with the SAME ids the panel used (\code{lyr_<make.names(group)>}), so
#' server.R's apply observer and \code{rx$spatial_visible} need no change -- the
#' checkboxes just live in a modal again, styled as cards (CSS .cc-lyr-grid).
#'
#' Visibility ONLY. Filtering to an area is Edit filters > Area
#' (\code{modal_spatial_filter()}, or Plot Options > Restrict map to area), and
#' rolling data up per polygon is Plot Options > Summarize Within -- the
#' subtitle says so, because the three share a layer registry and read as one
#' control when it did not.
#'
#' @return a \code{modalDialog}
#' @export
modal_map_layers <- function() {
  layer_choices <- split(
    setNames(d_spatial_layers$dataset_id, d_spatial_layers$layer),
    d_spatial_layers$group)
  grps <- names(layer_choices)
  mid  <- ceiling(length(grps) / 2)

  grp_block <- function(grp) {
    ic <- switch(grp,
      "Administrative" = "bank", "Maritime Zones" = "bullseye",
      "Ecological" = "tree", "Protected Areas" = "shield",
      "Energy & Industry" = "lightning-charge", "diagram-3")
    div(
      class = "cc-lyr-block",
      div(class = "cc-lyr-block-head",
          bs_icon(ic, class = "cc-lyr-block-icon"),
          tags$span(class = "cc-lyr-block-name", grp),
          tags$span(class = "cc-lyr-block-count small",
                    sprintf("%d layers", length(layer_choices[[grp]])))),
      div(
        class = "cc-lyr-grid",
        style = paste0("--cc-lyr-thumb: ", cc_lyr_thumb(grp), ";"),
        checkboxGroupInput(
          paste0("lyr_", make.names(grp)),
          label    = NULL,
          choices  = layer_choices[[grp]],
          selected = intersect(
            d_spatial_layers$dataset_id[d_spatial_layers$default_visible],
            layer_choices[[grp]]))))
  }

  modalDialog(
    title = tagList(
      modal_x_btn(),
      "Map Layers",
      div(class = "small text-muted fw-normal",
          "Draw reference boundaries on the map. To keep only the samples inside
           an area use Edit filters \u203a Area; to report one value per polygon
           use Plot Options \u203a Summarize Within.")),
    size = "l", easyClose = TRUE, fade = FALSE,
    fluidRow(
      column(6, lapply(grps[seq_len(mid)], grp_block)),
      column(6, lapply(grps[(mid + 1):length(grps)], grp_block))),
    footer = tagList(
      div(class = "small text-muted me-auto",
          textOutput("n_layers_selected", inline = TRUE)),
      modalButton("Cancel"),
      actionButton("btn_layers_apply", "Apply", class = "btn-primary")))
}

#' The Map tab's left panel -- a stacked, collapsible pane
#'
#' NOT a tab set (no pill nav -- the section headers ARE the navigation): one
#' vertical scroll holding three collapsible sections -- Filter Summary, Summary
#' Statistics, Plot Options. Map Layers moved OUT to a chip-row button + rich
#' modal (functions.R::modal_map_layers()). Collapse the whole pane and it
#' becomes a vertical icon rail; a rail icon expands + scrolls to its section.
#'
#' Every input/output id already existed -- this only relocates them:
#'   * Summary -> \code{filter_summary}
#'   * Stats   -> \code{summary_statistics} + \code{taxa_tree}
#'   * Options -> \code{sel_agg_unit} + \code{poly_note} + \code{sel_env_stat}
#'
#' @return the \code{.cc-panel} div (the stacked-section panel)
#' @export
map_panel_ui <- function() {
  sec_head <- function(sec, icon, label) div(
    class = "cc-sec-head", `data-sec` = sec, role = "button", tabindex = "0",
    bs_icon(icon, class = "cc-sec-icon"),
    tags$span(class = "cc-sec-title", label),
    bs_icon("chevron-down", class = "cc-sec-chev"))

  # one entry per tags$section below (sec id, icon, label) -- kept as a single
  # list so the rail and the section headers can't quietly drift apart
  rail_secs <- list(
    list(sec = "summary", icon = "funnel",       label = "Filter Summary"),
    list(sec = "stats",   icon = "bar-chart",     label = "Summary Statistics"),
    list(sec = "options", icon = "sliders",       label = "Plot Options"),
    list(sec = "download",icon = "download",      label = "Download Data"),
    list(sec = "sources", icon = "info-circle",   label = "Sources & Citations"))

  div(
    class = "cc-panel",
    div(
      class = "cc-panel-full",
      # slim bar: a left chevron that collapses the panel to a thin strip, and
      # a little visual breathing room above the first section
      div(
        class = "cc-panel-bar",
        tags$button(
          type = "button", class = "cc-panel-toggle",
          `aria-label` = "Collapse panel", title = "Collapse panel",
          bs_icon("chevron-left"))),
      # collapsed-panel icon rail: hidden while the panel is open (CSS), shown
      # in its place once collapsed to the 40px strip -- clicking an icon
      # re-expands the panel, opens that section and scrolls to it. Previously
      # collapsing just hid .cc-panel-scroll outright with nothing standing in
      # for it, so the strip had nothing clickable on it at all (bug report
      # 2026-09-07: "when you click collapse pane it should show icons right
      # now its just empty").
      div(
        class = "cc-panel-rail",
        lapply(rail_secs, function(s) tags$button(
          type = "button", class = "cc-panel-rail-btn", `data-sec` = s$sec,
          `aria-label` = s$label, title = s$label,
          bs_icon(s$icon)))),
      div(
        class = "cc-panel-scroll",

        # Filter Summary starts collapsed -- fine print; one click opens it.
        tags$section(
          class = "cc-sec is-shut", id = "cc-sec-summary",
          sec_head("summary", "funnel", "Filter Summary"),
          div(class = "cc-sec-body", uiOutput("filter_summary"))),

        tags$section(
          class = "cc-sec is-shut", id = "cc-sec-stats",
          sec_head("stats", "bar-chart", "Summary Statistics"),
          div(class = "cc-sec-body",
              uiOutput("summary_statistics"),
              # observations by selected taxa (server.R output$taxa_tree,
              # functions.R::taxa_tree_builder) -- the per-taxon breakdown the
              # matrix above cannot show; styled by ui.R's .cc-taxa rules
              uiOutput("taxa_tree"))),

        tags$section(
          class = "cc-sec", id = "cc-sec-options",
          sec_head("options", "sliders", "Plot Options"),
          div(
            class = "cc-sec-body",
            selectizeInput(
              "sel_agg_unit",
              tagList(
                "Summarize Within",
                popover(
                  bs_icon("question-circle"),
                  HTML("Choose the unit the map aggregates into.<br>
                        <strong>Hexagons (H3)</strong> bins observations into
                        equal-area cells that get finer as you zoom in.<br>
                        A <strong>boundary layer</strong> instead reports one
                        value per polygon — per MPA, per county, per ecoregion.<br><br>
                        One layer at a time: the layers overlap (a station can sit
                        inside a county, an ecoregion <em>and</em> an MPA, and all
                        three are true), so summarizing across them would count the
                        same observation more than once.<br><br>
                        Polygons with no CalCOFI samples are drawn as an outline
                        and labelled <em>no data</em> — they are not zero."))),
              choices  = agg_unit_choices,
              selected = "hex",
              width    = "100%",
              # render the dropdown at <body> level so it is not clipped by the
              # panel's own overflow scroll, and give it room
              options  = list(dropdownParent = "body")),
            # only renders when the current selection is published in more
            # than one cpue_unit -- see filter_cpue_unit()/cpue_unit_selector_ui()
            uiOutput("cpue_unit_selector"),
            uiOutput("poly_note"),
            # Restrict the map (and the plot/download data) to one boundary
            # layer's polygons -- the hexes stay hexes, just clipped to that
            # area. Only relevant in Hexagons mode: a polygon summary already
            # shows exactly one layer, so "restrict" would be redundant -- hide
            # it then (and the server ignores its value, see do_apply_filters).
            conditionalPanel(
              "input.sel_agg_unit == 'hex'",
              div(
                class = "cc-restrict-wrap",
                selectizeInput(
                  "sel_map_area",
                  tagList(
                    "Restrict map to area",
                    popover(
                      bs_icon("question-circle"),
                      HTML("Show only the hexes that fall inside a boundary
                            layer &mdash; e.g. just the BOEM Wind Planning Areas.
                            The data stays binned as hexagons; it is only clipped
                            to the area."))),
                  choices  = map_area_choices,
                  selected = "__all__",
                  width    = "100%",
                  options  = list(dropdownParent = "body")))),
            selectInput(
              "sel_env_stat",
              "Environmental Summary Statistic",
              choices  = env_stat_choices,
              selected = "mean",
              width    = "100%"))),

        # Download -- moved off the navbar into the panel, under Plot Options.
        # Starts collapsed. Leads with the INTEGRATED dataset (always in the
        # bundle -- server.R forces "int" into all_sel); the per-chart tables
        # and the bulky raw source files are behind <details>. Ids unchanged
        # (sel_proc_data_download, sel_raw_data_download, download_data). The
        # bio<->env match tolerance uses the defaults (default_max_hours_diff /
        # _meters_diff); server.R falls back to those when input$time_window is
        # absent, so the two numericInputs were dropped as noise for most users.
        tags$section(
          class = "cc-sec is-shut", id = "cc-sec-download",
          sec_head("download", "download", "Download Data"),
          div(
            class = "cc-sec-body cc-dl-body",
            tags$details(
              class = "cc-dl-more",
              tags$summary("Also include the chart data"),
              checkboxGroupInput(
                "sel_proc_data_download", NULL,
                c("Map data"           = "map",
                  "Time-series data"   = "ts",
                  "Scatterplot data"   = "splot",
                  "Depth Profile data" = "dprof"),
                width = "100%")),
            tags$details(
              class = "cc-dl-more",
              tags$summary("Raw source files"),
              checkboxGroupInput(
                "sel_raw_data_download", NULL,
                c("Raw environmental data" = "raw_env",
                  "Raw species data"       = "raw_sp"),
                width = "100%")),
            div(
              class = "cc-dl-actions",
              downloadButton("download_data", "Download",
                             class = "btn-primary w-100"),
              actionButton("btn_cite_data", tagList(bs_icon("quote"), "Cite this data"),
                           class = "btn-outline-secondary w-100")))),

        # Provider + citation info for whatever species + environmental
        # variable are currently shown -- the left-panel counterpart to the
        # Data Sources tab. Moved to the bottom, under Download, since it's
        # the least likely section someone opens first. Starts collapsed like
        # Filter Summary. Citations show inline here (no separate "Dataset
        # citations & license" toggle under Download anymore -- that repeated
        # this same content); "Cite this data" above stays as the one-click
        # copy-everything shortcut, and the "View full citation" link inside
        # still jumps to the full tab for PI/provider + acknowledgement text.
        tags$section(
          class = "cc-sec is-shut", id = "cc-sec-sources",
          sec_head("sources", "info-circle", "Sources & Citations"),
          div(class = "cc-sec-body", uiOutput("data_sources_box")))))
  )
}

#' Top-bar "Compare Environmental Variable" Card
#'
#' The redesign's second top-bar card: off by default (map shows species
#' distribution only), toggled on to reveal the variable picker and overlay a
#' matching environmental layer. `cmp_env_toggle`/`sel_env_var`/
#' `sel_env_all_vars` are read the same way server.R always read them
#' (`sel_env_var` keeps its id) -- this only relocates the picker into
#' the always-visible top bar instead of a modal tab.
#'
#' @param env_var currently selected environmental measurement_type
#'
#' @return a tagList for the top-bar "Compare Environmental Variable" card
#'
#' @importFrom shinyWidgets switchInput
#' @importFrom shiny selectizeInput checkboxInput conditionalPanel tagList
#'
#' @export
top_bar_env_ui <- function(env_var = "temperature") {
  tagList(
    div(
      # label left; card's right edge shows a "+ Add a variable to compare"
      # button while OFF, the BS5 switch while ON. Both always in the DOM -- CSS
      # swaps them on `#cmp_env_toggle:checked`. `add_env_var` -> server
      # `update_switch("cmp_env_toggle", TRUE)`. Same id / TRUE-FALSE value:
      # every reader / conditionalPanel is unaffected.
      class = "cc-env-row",
      div(class = "cc-card-label small text-muted text-uppercase", "Environment"),
      actionLink(
        "add_env_var",
        tagList(bs_icon("plus-lg"), "Add a variable to compare"),
        class = "cc-env-add"),
      div(class = "cc-env-switch",
          input_switch("cmp_env_toggle", label = NULL, value = FALSE))),
    conditionalPanel(
      "input.cmp_env_toggle",
      div(
        # same .cc-combo2 pattern as top_bar_taxa_ui()'s Species/Taxa box, but
        # FLAT (`data-flat`): only ~30 headline variables across 5 groups, so
        # every group renders open and in full -- no accordion, no drill-in.
        class = "cc-combo2 cc-combo2-flat",
        `data-flat`              = "true",
        `data-placeholder-main`  = "Search environmental variables…",
        `data-placeholder-drill` = "Search variables — temperature, oxygen…",
        `data-noun`              = "variables",
        `data-extra-id`    = "sel_env_all_vars",
        `data-extra-label` = sprintf("Show all %d variables", nrow(d_env_vars)),
        `data-extra-tip`   = paste(
          "Off: just the headline variables people usually plot.",
          "On: adds instrument channels (transmissometer, ISUS voltage, PAR),",
          "the pre-QC series, and averaged / corrected / per-replicate variants."),
        div(
          class = "cc-combo2-btn", tabindex = "0", role = "button",
          `aria-haspopup` = "listbox",
          bs_icon("search", class = "cc-combo2-icon"),
          tags$span(class = "cc-combo2-label"),
          bs_icon("caret-down-fill", class = "cc-combo2-chevron")),
        div(class = "cc-combo2-panel"),
        selectizeInput(
          "sel_env_var",
          label    = NULL,
          # grouped by dataset; `selected` is a measurement_type, not a label
          choices  = env_var_choices(),
          selected = env_var,
          width    = "100%",
          options  = list(
            placeholder = "Search environmental variables…",
            # env_var_choices()'s labels carry no dataset suffix (unlike
            # sp_choices()) -- the dataset only lives in `optgroup` -- so this
            # template just appends it, no stripping needed.
            render = I("{
              option: function(item, escape) {
                return '<div class=\"cc-search-opt\"><span class=\"cc-search-opt-name\">' +
                  escape(item.label) + '</span>' +
                  (item.optgroup ? '<span class=\"cc-search-opt-ds\"> — ' + escape(item.optgroup) + '</span>' : '') +
                  '</div>';
              },
              item: function(item, escape) {
                return '<div class=\"cc-search-item\"><span class=\"cc-search-item-name\">' +
                  escape(item.label) + '</span>' +
                  (item.optgroup ? '<span class=\"cc-search-item-ds\"> — ' + escape(item.optgroup) + '</span>' : '') +
                  '</div>';
              },
              optgroup_header: function(data, escape) {
                return '<div class=\"cc-search-grp\">' + escape(data.label) + '</div>';
              }
            }"))),
        # real input, hidden -- mirrored at the top of the dropdown panel
        div(
          class = "cc-combo2-extra-src",
          checkboxInput("sel_env_all_vars",
                        label = sprintf("Show all %d variables", nrow(d_env_vars)),
                        value = FALSE)))),
    conditionalPanel(
      "!input.cmp_env_toggle",
      div(
        class = "small text-muted",
        "Add temperature, salinity, oxygen or chlorophyll to compare against where the species was found."))
  )
}

#' Edit Filters Modal Dialog
#'
#' The redesign's "Edit filters" panel, matching the approved mockup: a
#' single compact stack -- Datasets, Depth, Area (spatial filter, opens
#' \code{modal_spatial_filter()} via "Change"), Time (quarter + date range) --
#' rather than the old two-tab, full-width dialog. Taxa and Environmental
#' Variable live in the always-visible top bar
#' (\code{top_bar_taxa_ui()}/\code{top_bar_env_ui()}); everything here is
#' set-once/rarely-touched and collapses behind the "Edit filters" link.
#'
#' Every field here IS rebuilt from scratch on each \code{showModal()} call
#' (this whole dialog is a fresh R object every time "Edit filters" is
#' clicked, not a persistent DOM node) -- so, despite an earlier version of
#' this doc claiming otherwise, none of \code{sel_qtr}/\code{sel_date_range}/
#' \code{sel_depth_range}/\code{sel_bio_ds} "just keep their own state": each
#' one's initial value has to be threaded through as a parameter from the
#' last APPLIED filters (\code{rx$params}, in server.R's \code{edit_filters}
#' observer), or reopening the modal silently resets it, which is exactly
#' what shipped broken for \code{sel_bio_ds} until this fix (bug report
#' 2026-09-07: "when i filter to a certain dataset. then apply. then hit
#' edit filters again, it does not show the dataset that was filtered").
#' \code{sel_places_cat} lives in the separate \code{modal_spatial_filter()}
#' dialog (reached via this one's "Layers" > "Change" link) and has the same
#' shape of gap -- not fixed here, since narrowing it down needs its own look
#' at how \code{rx$sel_places} ties to a category.
#'
#' \code{sel_qtr} is now \code{shinyWidgets::checkboxGroupButtons()} (a row of
#' toggle pills, matching the mockup) instead of a multi-select dropdown, and
#' \code{sel_depth_range} is now \code{sliderInput()} (a range slider) instead
#' of two numeric boxes -- both keep their id and both still resolve to the
#' same value shape (`sel_qtr`: a character vector of "1".."4"; `sel_depth_range`:
#' a length-2 numeric vector), so nothing downstream that reads
#' \code{input$sel_qtr}/\code{input$sel_depth_range} needed to change.
#' \code{sel_date_range} deliberately stays a \code{dateRangeInput} rather than
#' becoming a numeric year slider -- it feeds real Date-typed SQL filtering
#' downstream, and changing its value TYPE (not just its widget) is a
#' correctness risk not worth taking for a cosmetic match.
#'
#' @return Shiny modal dialog object
#'
#' @importFrom shiny modalDialog sliderInput dateRangeInput actionLink modalButton tagList textOutput
#' @importFrom shinyWidgets checkboxGroupButtons
#'
#' @export
#' @param depth_range current applied depth range (m); defaults to the full
#'   span (no filter) only when nothing has been applied yet
#' @param qtr current applied quarter selection; defaults to all four
#' @param date_range current applied date range; defaults to the full
#'   min_max_date span
#' @param bio_datasets current applied dataset_key selection; defaults to
#'   every dataset (see dataset_list_picker_ui() -- same "reopen loses the
#'   filter" bug qtr/date_range/depth_range were already fixed for)
modal_edit_filters <- function(depth_range = c(0, 515),
                                qtr = 1:4,
                                date_range = min_max_date,
                                bio_datasets = d_bio_datasets$dataset_key) {
  modalDialog(
    title = tagList("Filters", modal_x_btn()),
    class = "cc-filters-modal",

    # Order (2026-09-07 request): Time, Depth, Layers, then Datasets last --
    # the dataset tree is the longest/most involved section (checkboxes,
    # nested categories), so it now sits at the bottom instead of pushing the
    # other three below the fold.
    div(
      class = "d-flex align-items-center gap-2 mb-2",
      bs_icon("calendar3"), tags$strong("Time")),
    checkboxGroupButtons(
      "sel_qtr",
      label    = NULL,
      choices  = c(Q1 = 1, Q2 = 2, Q3 = 3, Q4 = 4),
      selected = qtr,
      status   = "outline-primary",
      size     = "sm"),
    dateRangeInput(
      "sel_date_range",
      label     = NULL,
      startview = "year",
      start = date_range[1],
      end   = date_range[2],
      min   = min_max_date[1],
      max   = min_max_date[2],
      width = "100%"),
    hr(),

    div(
      class = "d-flex align-items-center gap-2 mb-1",
      bs_icon("arrow-down"), tags$strong("Depth")),
    sliderInput(
      "sel_depth_range",
      label = NULL,
      # full range = 0-515; `value` reopens on whatever is currently applied
      # (see server.R's edit_filters observer) instead of always resetting
      min = 0, max = 515, value = depth_range,
      post  = " m",
      width = "100%"),
    div(class = "small text-muted mt-1",
        "Environmental layers only — species observations aren't depth-filtered."),
    hr(),

    div(
      class = "d-flex justify-content-between align-items-start gap-2 mb-1",
      div(
        div(
          class = "d-flex align-items-center gap-2",
          # "Area", not "Layers": this is the spatial FILTER (keep only the
          # samples inside the picked polygons). The Map Layers button on the
          # chip row only draws reference boundaries, and Plot Options'
          # Summarize Within rolls data up per polygon -- three different
          # things, and calling all three "layers" read as if picking one did
          # all three.
          bs_icon("geo-alt"), tags$strong("Area"),
          tags$span(class = "text-muted", " — "),
          textOutput("spatial_layers_summary", inline = TRUE)),
        div(
          class = "small text-muted",
          "Keep only samples inside the selected areas (or a drawn polygon)")),
      actionLink("edit_spatial", "Change", class = "flex-shrink-0")),
    hr(),

    dataset_list_picker_ui(selected = bio_datasets),

    footer = tagList(
      actionLink("reset_filters", "Reset all", class = "me-auto text-muted"),
      modalButton("Cancel"),
      input_task_button("submit", "Apply") ),

    size = "s",
    fade = FALSE
  )
}

#' Spatial Filter Modal ("Area" > Change)
#'
#' The pre-defined-zones / drawn-polygon picker -- unchanged from the
#' redesign's earlier "Spatial" tab, just its own dialog now instead of a tab
#' inside \code{modal_edit_filters()}, reached via the "Area" row's "Change"
#' link. `sel_places_cat`/`spatial_filter_map`/`tbl_places` keep the exact
#' same ids, so server.R's map-click/table-click handlers need no change --
#' only the trigger that opens this dialog moved (`input$edit_spatial`
#' instead of being rendered as part of `input$edit_filters`).
#'
#' \code{selected_cat}, added 2026-09-08, fixes the same "reopen resets to
#' default" gap \code{dataset_list_picker_ui()} had until this same
#' session: this dialog is rebuilt from scratch every time
#' \code{showModal()} runs (same as \code{modal_edit_filters()}), so a
#' hardcoded \code{selected = "CalCOFI Zones"} inside here meant switching
#' to, say, "Ichthyoplankton Zones" and reopening "Layers" > "Change" always
#' snapped the Category dropdown back to "CalCOFI Zones" -- not what was
#' last being looked at. The caller (server.R's \code{edit_spatial}
#' observer) passes \code{input$sel_places_cat}, read at the moment "Change"
#' is clicked, i.e. before this call rebuilds (and so resets) that same
#' input.
#'
#' @param selected_cat category to preselect in the "Category" dropdown;
#'   defaults to "CalCOFI Zones" (correct only before any category has ever
#'   been chosen this session)
#'
#' @return Shiny modal dialog object
#'
#' @importFrom shiny modalDialog selectInput fluidRow column modalButton tagList
#' @importFrom maplibre maplibreOutput
#'
#' @export
modal_spatial_filter <- function(selected_cat = "CalCOFI Zones") {
  if (!selected_cat %in% unlist(places_cat_choices)) selected_cat <- "CalCOFI Zones"
  modalDialog(
    title = tagList("Area", modal_x_btn()),
    # cc-spatial-modal: hooks this dialog into the shared centred/fit-
    # viewport CSS in ui.R (#shiny-modal:has(.cc-spatial-modal) ...) --
    # this modal never had a class of its own before, so it fell back to
    # Shiny's plain top-anchored default and had to be scrolled at the
    # BROWSER-PAGE level to see its full height (bug report 2026-09-07:
    # "can layers be centered on page. rn need to scroll to see"). Title
    # says "Area" to match the "Area" row in modal_edit_filters() that
    # opens this dialog (not "Layers" -- see that row's own comment), and
    # the validation guard (from main) keeps a stale/invalid category
    # from ever reaching the <select>.
    class = "cc-spatial-modal",
    "Select pre-defined zones by clicking on the map or table (some categories are table-only -- see below). Click selected zones again to deselect them. Alternatively, use the \"Custom\" category to drawn your own region of interest.",
    selectInput(
      "sel_places_cat",
      tags$b("Category"),
      selected = selected_cat,
      # cc_places categories (clickable map + table), then every other
      # spatial_layers.csv layer grouped by its registry group
      # (table-only -- see global.R::sample_spatial_layers), then Custom.
      places_cat_choices),
    fluidRow(
      column(6, maplibreOutput("spatial_filter_map", height = "400px")),
      column(6, DTOutput("tbl_places") )
    ),

    footer = tagList(
      modalButton("Back"),
      input_task_button("submit", "Apply") ),

    size = "xl",
    fade = FALSE
  )
}

#' Compact One-line Filter Chip Text
#'
#' The redesign's filter-chip row summary -- quarters, year range, depth and
#' area from `modal_edit_filters()`, always all four, condensed to one line
#' next to the "Edit filters" link (each value bold with a blue dot).
#' Deliberately separate from `prep_filter_summary()`
#' (which still drives the fuller sidebar "Filter Summary" accordion,
#' taxa/variable included) rather than reusing it, so this stays a small,
#' low-risk addition instead of a change to an already-relied-on function.
#'
#' @param sel_qtr integer vector of selected quarters
#' @param sel_date_range length-2 Date vector
#' @param sel_depth_range length-2 numeric vector (meters)
#' @param sel_places character vector of selected zone/layer names (or NULL)
#' @param is_custom TRUE if the active spatial filter is a drawn polygon
#' @param date_bounds length-2 Date vector, the full available date range; a
#'   date facet shows only when \code{sel_date_range} is inside these bounds
#' @param depth_default length-2 numeric, the startup depth range; a depth
#'   facet shows only when \code{sel_depth_range} differs from it
#'
#' @return a UI fragment: a \code{tagList} of \code{.cc-chip-facet} spans (one
#'   per narrowed filter), or a single \code{.cc-chip-empty} span when nothing
#'   is narrowed
#'
#' @export
chip_summary_text <- function(sel_qtr, sel_date_range, sel_depth_range,
                              sel_places = NULL, is_custom = FALSE,
                              date_bounds = NULL, depth_default = c(0, 515)) {
  # The chip always shows all four facets (quarters, years, depth, area) --
  # same content as before, just each value bold with a blue dot
  # (.cc-chip-facet). Kept deliberately simple/always-on rather than hiding
  # defaults, so the chip reads the same on every session.
  qtr_txt <- if (length(sel_qtr) >= 4 || length(sel_qtr) == 0) "Q1–Q4"
             else paste0("Q", paste(sort(as.integer(sel_qtr)), collapse = ", "))

  yr_txt <- sprintf(
    "%s–%s",
    format(as.Date(sel_date_range[1]), "%Y"),
    format(as.Date(sel_date_range[2]), "%Y"))

  depth_txt <- sprintf("%s–%s m", sel_depth_range[1], sel_depth_range[2])

  loc_txt <- if (isTRUE(is_custom)) "Custom area"
             else if (!is.null(sel_places) && length(sel_places) > 0)
               sprintf("%d location%s", length(sel_places),
                       if (length(sel_places) > 1) "s" else "")
             else "All locations"

  tagList(lapply(
    c(qtr_txt, yr_txt, depth_txt, loc_txt),
    function(f) tags$span(class = "cc-chip-facet", f)))
}


#' Depth Profile Modal Dialog
#'
#' Creates a modal dialog for defining a transect line segment and buffer
#' distance to generate environmental depth profiles.
#'
#' @param map_sp maplibre object (currently unused in implementation, retained for future enhancement)
#'
#' @return Shiny modal dialog object with transect drawing interface and buffer distance input
#'
#' @details
#' Users draw a line segment on the map to define a transect. The buffer distance
#' controls the width of the corridor around the transect for data aggregation.
#' Default buffer is 5 km.
#'
#' @examples
#' \dontrun{
#' # in server.R
#' observeEvent(input$create_profile, {
#'   showModal(modal_depth_profile(map_sp = NULL))
#' })
#' }
#'
#' @seealso \code{\link{buffer_transect}} for transect buffer generation
#'
#' @importFrom shiny modalDialog numericInput modalButton tagList
#' @importFrom maplibre maplibreOutput
#'
#' @export
modal_depth_profile <- function(map_sp) {
  modalDialog(
    title = tagList("Create Depth Profile", modal_x_btn()),
    class = "cc-transect-modal",

    div(class = "cc-transect-top",
        p(class = "small text-muted mb-0",
          "Draw a line segment on the map to define your transect."),
        numericInput(
          "modal_buffer_dist", "Buffer distance (km)",
          value = 5, width = "160px")),

    # a real px height so the widget's first render is sized; CSS
    # (#shiny-modal:has(.cc-transect-modal) #transect_map) then clamps it to the
    # viewport so the whole dialog fits with no scroll, centred vertically.
    maplibreOutput("transect_map", height = "400px"),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit_transect", "Generate Profile")
    ),

    size = "l",
    fade = FALSE
  )
}


#' Create Placeholder Message UI
#'
#' Generates a centered placeholder message for empty or loading states in the
#' Shiny UI. Useful for displaying instructions or status messages when no data
#' is available.
#'
#' @param title Character string for heading text
#' @param message Character string for body text
#'
#' @return shiny.tag div element with centered, styled placeholder content
#'
#' @examples
#' \dontrun{
#' output$map_placeholder <- renderUI({
#'   ui_placeholder("No Data Selected", "Please select species from the filter menu.")
#' })
#' }
#'
#' @importFrom shiny div h4 p
#'
#' @export
ui_placeholder <- function(title, message) {
  div(
    class = "d-flex align-items-center justify-content-center",
    style = "height: 80vh;",
    div(
      class = "text-center text-muted",
      h4(title),
      p(message)
    )
  )
}

#' Build the "Data Sources & Attribution" page content
#'
#' Reads app/data/attributions.csv -- one row per CONTRIBUTING COMPONENT, not
#' per app dataset. A dataset the app treats as one filter option (e.g.
#' Ichthyoplankton) is often stitched together from several distinct sources
#' (egg counts, egg stages, larvae counts...), each with its own provider and
#' citation, so each gets its own row here, sharing the app-level
#' `dataset_key` that groups them back under one heading. Rendered once at
#' app startup (ui.R), same pattern as `about_html`, since citations only
#' change with a release, not per request. Dataset display names come from
#' DATASET_LABELS (global.R) so this page can never drift from the names
#' shown in filters/search elsewhere in the app.
#'
#' To add a dataset or component, or update a citation: edit
#' app/data/attributions.csv (and DATASET_LABELS in global.R for a brand-new
#' dataset_key) -- no code change needed.
#'
#' @return shiny.tag div element

#' Is this CSV value present?
#'
#' `nzchar(NA_character_)` returns TRUE (R's documented backward-compatible
#' behavior), so a genuinely empty CSV cell -- read as NA by readr -- was
#' passing every `nzchar(x %||% "")` check in this file and rendering the
#' literal text "NA". This is the one place that has to get it right.
#'
#' @param x a scalar (typically one CSV cell)
#' @return TRUE only for a non-NA, non-empty value
#' @export
has_val <- function(x) !is.null(x) && length(x) > 0 && !is.na(x) && nzchar(x)

#' Vectorised has_val(), safe on a whole column (NA-safe, unlike raw nzchar())
has_val_v <- function(x) !is.na(x) & nzchar(x)

#' A short license/label ("CC-BY-4.0", "Public Domain") can sit in a pill;
#' a long one (a full ERDDAP-style disclaimer paragraph) can't -- render it
#' as its own collapsed block instead so it doesn't turn into wall-of-text.
#' @export
is_short_license <- function(x) has_val(x) && nchar(x) <= 40

#' When every row in a dataset group shares the exact same license/disclaimer
#' text, it belongs on the group once, not repeated per component.
#' @return the shared string, or NA_character_ if there's no single shared value
#' @export
shared_license_for <- function(g) {
  vals <- unique(g$license[has_val_v(g$license)])
  if (length(vals) == 1) vals else NA_character_
}

#' Collapsed "License & disclaimer" block for a long license string.
#'
#' Collapsed by default (`open = FALSE`) for the compact contexts this is
#' shared with (the cite modal, the download panel) -- pass `open = TRUE`
#' for a page like the full Data Sources & Attribution table, where there's
#' room to show everything without a click.
#' @export
license_disclaimer_block <- function(text, summary_label = "License & disclaimer", open = FALSE) {
  if (!has_val(text)) return(NULL)
  tags$details(
    class = "cc-license-block",
    open = if (open) NA else NULL,
    tags$summary(summary_label),
    p(class = "cc-license-block-text", text))
}

attribution_table_html <- function() {

  attrib <- ATTRIBUTIONS

  # keep the app's existing dataset order (DATASET_LABELS' declaration order),
  # then any dataset_key the curated map does not list, in CSV order. Ordering
  # by match() alone put those at NA; selecting `ordered_keys %in% ...` below
  # dropped them from the page entirely while n_datasets still counted them --
  # a row added to attributions.csv for a newly ingested dataset would have
  # gone missing from the one page that exists to show it.
  ordered_keys <- union(names(DATASET_LABELS), unique(attrib$dataset_key))
  attrib <- attrib[order(match(attrib$dataset_key, ordered_keys)), ]

  # one dl-style field per non-empty value; has_val() treats a true NA (an
  # empty CSV cell, read as NA_character_) as absent -- nzchar(NA) alone
  # returns TRUE, which is what used to print the literal text "NA"
  meta_dl <- function(...) {
    pairs <- list(...)
    rows <- Filter(Negate(is.null), pairs)
    if (length(rows) == 0) return(NULL)
    tags$dl(class = "cc-attrib-meta", rows)
  }
  meta_field <- function(label, value) {
    if (!has_val(value)) return(NULL)
    tagList(tags$dt(label), tags$dd(value))
  }

  # a license identical across every component of a dataset (the common case
  # for a multi-file NOAA ERDDAP dataset) is shown once per dataset row below,
  # never repeated verbatim in each component's own meta list
  component_block <- function(d, show_heading, shared_license) {
    cite_text <- d$citation %||% ""
    own_license <- if (has_val(d$license) &&
                       (is.na(shared_license) || !identical(d$license, shared_license)))
      d$license else NA_character_
    div(
      class = "cc-attrib-component",
      if (show_heading) div(class = "cc-attrib-comp-name", d$component),
      div(class = "cc-attrib-cite", citation_text_html(cite_text)),
      meta_dl(
        meta_field("PI / provider", d$pi),
        meta_field("License", if (is_short_license(own_license)) own_license),
        meta_field("Acknowledge", d$acknowledgement)),
      if (has_val(own_license) && !is_short_license(own_license))
        license_disclaimer_block(own_license, open = TRUE),
      if (has_val(cite_text))
        div(class = "cc-attrib-actions",
            citation_source_link(cite_text, d$source_url),
            tags$button(
              type = "button", class = "cc-attrib-copy",
              onclick = sprintf(
                "navigator.clipboard.writeText(%s); var b=this; b.textContent='Copied!'; setTimeout(function(){b.textContent='Copy citation';}, 1500);",
                jsonlite::toJSON(cite_text, auto_unbox = TRUE)),
              "Copy citation")))
  }

  rows <- lapply(ordered_keys[ordered_keys %in% attrib$dataset_key], function(key) {
    g <- attrib[attrib$dataset_key == key, ]
    multi  <- nrow(g) > 1
    shared <- shared_license_for(g)
    # summarize_programs(), not paste(): a blank `program` cell is NA and
    # paste() stringifies it to "NA" -- see summarize_institutions()
    programs <- summarize_programs(g$program)
    insts    <- summarize_institutions(g$institution)
    tags$details(
      class = "cc-attrib-row",
      # open on load -- this page is the one place with room to show every
      # source without a click, unlike the compact cite modal/download panel
      open = NA,
      tags$summary(
        div(class = "cc-attrib-main",
            span(class = "cc-attrib-name", dataset_full_label(key)),
            if (multi) span(class = "cc-attrib-comp-count",
                             sprintf("%d sources", nrow(g)))),
        div(class = "cc-attrib-provider",
            programs, tags$span(class = "cc-attrib-inst", insts)),
        span(class = "cc-attrib-chev", bs_icon("chevron-right"))),
      div(class = "cc-attrib-detail",
          lapply(seq_len(nrow(g)), function(i) component_block(g[i, ], multi, shared)),
          if (has_val(shared)) license_disclaimer_block(shared, open = TRUE)))
  })

  n_datasets <- length(unique(attrib$dataset_key))

  div(
    class = "cc-attrib-page",
    div(
      class = "cc-attrib-intro",
      h4("Data Sources & Attribution"),
      p(class = "cc-muted",
        "Built from the ",
        a(href = "https://calcofi.io/docs/db.html#integrated-database-ingestion-strategy",
          target = "_blank", "integrated database"),
        " -- ", n_datasets, " contributing datasets (", nrow(attrib),
        " underlying data sources) across CalCOFI, CCE-LTER, NOAA SWFSC, CDFW ",
        "and the Farallon Institute. Click a row for the citation, license and ",
        "required acknowledgement of each source.")),
    div(
      class = "cc-attrib-table",
      div(class = "cc-attrib-thead",
          span(class = "cc-attrib-lbl", "Dataset"),
          span(class = "cc-attrib-lbl", "Program & provider")),
      tagList(rows)))
}

#' Resolve one or more dataset_key values to their attribution rows
#'
#' ATTRIBUTIONS has one row per contributing SOURCE, not per dataset_key -- a
#' bundled dataset (e.g. swfsc_ichthyo) has several rows sharing one key. This
#' is the one place every other attribution touchpoint reads from.
#'
#' @param keys character vector of dataset_key values (NAs / dupes tolerated)
#' @return a data.frame slice of ATTRIBUTIONS, possibly zero rows
#' @export
attrib_rows_for <- function(keys) {
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  if (length(keys) == 0) return(ATTRIBUTIONS[0, ])
  ATTRIBUTIONS[ATTRIBUTIONS$dataset_key %in% keys, ]
}

#' Program and institution strings per dataset_key, kept separate
#'
#' Same grouping/collapsing as attrib_provider_summary() below, but returns
#' the two pieces unglued -- for spots that label them separately ("Programs:"
#' / "Providers:") instead of folding institution into a "Program (Institution)"
#' string (2026-09-07: that combined form repeated the institution a second
#' time right next to DATASET_LABELS' own "SWFSC:"-style prefix, which looked
#' redundant in the map title popover and the left-panel Data Sources box).
#'
#' @param keys character vector of dataset_key values
#' @return named list, one entry per unique dataset_key in `keys`, each entry
#'   a list(program = ..., institution = ...); institution may be "" if a
#'   source only ever states a program
#' @export
attrib_provider_parts <- function(keys) {
  rows <- attrib_rows_for(keys)
  if (nrow(rows) == 0) return(list())
  ks <- unique(rows$dataset_key)
  parts <- lapply(ks, function(k) {
    g     <- rows[rows$dataset_key == k, ]
    progs <- summarize_programs(g$program)
    insts <- summarize_institutions(g$institution)
    list(program = progs, institution = insts)
  })
  setNames(parts, ks)
}

#' One "Program(s) (Institution(s))" line per dataset_key
#'
#' For spots that need to NAME the source without the full citation: the
#' search dropdown subtitle. (The map title popover and the Data Sources box
#' show Program/Institution as separate labeled lines instead -- see
#' attrib_provider_parts() above.)
#'
#' @param keys character vector of dataset_key values
#' @return named character vector, one entry per unique dataset_key in `keys`,
#'   in the order they first appear
#' @export
attrib_provider_summary <- function(keys) {
  parts <- attrib_provider_parts(keys)
  if (length(parts) == 0) return(character(0))
  vals <- vapply(parts, function(p) {
    if (nzchar(p$institution) && p$institution != p$program)
      paste0(p$program, " (", p$institution, ")") else p$program
  }, character(1))
  vals
}

#' The curated "<Abbrev>: <description>" label, NA-safe
#'
#' What `DATASET_LABELS[[key]]` used to be at the three call sites that want
#' the prefixed form (the Data Sources page, the cite modal, and
#' \code{\link{dataset_short_label}}, which strips the prefix back off). Same
#' string as before for every key the curated map lists -- the point is only
#' the fallback: `[[` on a named atomic vector with a missing name throws
#' "subscript out of bounds" BEFORE `%||%` can supply a default, so a
#' dataset_key measured live off bio_obs or read from attributions.csv but
#' absent from the map errored the page instead of naming itself.
#'
#' Single-bracket indexing yields NA there, and global.R::dataset_label()
#' takes over: the release's own `dataset_name_short`, then its formal
#' `dataset_name`, then the raw key. The curated map stays FIRST here on
#' purpose -- these three spots show the "SWFSC: "/"CalCOFI: " prefix, which
#' the release's short form does not carry.
#'
#' @param key single dataset_key
#' @return character(1)
#' @export
dataset_full_label <- function(key) {
  lbl <- unname(DATASET_LABELS[key])
  if (is.na(lbl) || !nzchar(lbl)) dataset_label(key) else lbl
}

#' Strip DATASET_LABELS' institution-abbreviation prefix ("SWFSC: ", "CalCOFI: ")
#'
#' Every DATASET_LABELS value follows "<Abbrev>: <description>" so the search
#' dropdown optgroups read as "SWFSC > Ichthyoplankton" etc. That abbreviation
#' just repeats what attrib_provider_parts()'s institution line already spells
#' out immediately below it in the map title popover / Data Sources box, so
#' those two spots show the bare description instead (2026-09-07 feedback:
#' "the program prefix should be removed because it is just repeated on
#' bottom"). Falls back to the full label if it doesn't match that shape.
#'
#' Reads the label through \code{\link{dataset_full_label}} rather than
#' `DATASET_LABELS[[key]] %||% key`, which could not fall back at all: `[[` on
#' a named atomic vector with a missing name throws "subscript out of bounds"
#' BEFORE `%||%` is reached. The keys reaching this function are measured live
#' off bio_obs (dataset_list_picker_ui()), so a newly ingested dataset errored
#' the filter picker instead of showing its raw key. Same string as before for
#' every key the curated map lists.
#'
#' @param key single dataset_key
#' @return character(1)
#' @export
dataset_short_label <- function(key) {
  lbl <- dataset_full_label(key)
  sub("^[^:]+:\\s*", "", lbl)
}

#' Strip a curator's "confirmed ..." annotation off a license string
#'
#' `attributions.csv`'s `license` column carries editorial "how/when this was
#' verified" notes for the curator's own reference. Useful in the CSV, not part
#' of the license, and never shown to users -- see the `ATTRIBUTIONS` read in
#' global.R, which is the only caller.
#'
#' Both forms the CSV uses are handled: parenthesised ("CC-BY-4.0 (confirmed
#' via calcofi.org/..., 2026-09-07)") and dash-introduced ("CC BY 4.0 --
#' confirmed via the Farallon Institute Data Sharing Agreement, 23 July
#' 2025."). Only the parenthesised one used to be, so the dash form reached
#' users AND -- at 87 characters -- pushed a plain CC-BY row past
#' \code{\link{is_short_license}}'s 40-character pill threshold into a
#' collapsed "License & disclaimer" block.
#'
#' Anchored on the word "confirmed" so a license whose own text contains a
#' dash or a trailing parenthetical (the NOAA ERDDAP disclaimer, both EDI
#' customs, the Ohman Data Use Policy) is left exactly as written.
#'
#' @param license character vector of `license` values
#' @return the same vector, annotation removed and whitespace squished
#' @export
strip_license_annotation <- function(license) {
  stringr::str_squish(stringr::str_remove(
    license, "\\s*(\\(confirmed[^)]*\\)|(--|\u2014|\u2013)\\s*confirmed\\b.*)\\s*$"))
}

#' Join a dataset's per-component `program` strings, dropping blanks
#'
#' The `program` counterpart to \code{\link{summarize_institutions}}: an empty
#' CSV cell reads as NA_character_ and a bare
#' \code{paste(unique(x), collapse = "; ")} renders it as the literal text
#' "NA" in the map title popover, the Data Sources row and the search
#' dropdown subtitle.
#'
#' @param program character vector of one dataset's `program` values
#' @return character(1); "" when no component states a program
#' @export
summarize_programs <- function(program) {
  paste(unique(program[has_val_v(program)]), collapse = "; ")
}

#' Combine a dataset's per-component institution strings into one short label
#'
#' Some multi-component datasets (e.g. Dungeness Crab Megalopae) give each
#' component its own institution string, and a spelled-out name repeats
#' across components instead of collapsing to its abbreviation ("California
#' Department of Fish and Wildlife (CDFW)" vs. "...(CDFW) / UCSD SIO" -- two
#' different strings even though "CDFW" is the useful part of both). This
#' splits on "/", collapses each "Full Name (ABBR)" down to "ABBR", and
#' de-duplicates before rejoining, so the result is short and non-repetitive.
#' @export
summarize_institutions <- function(institution) {
  # has_val_v(), not nzchar(): an empty CSV cell reads as NA_character_,
  # strsplit() passes that through as an NA token, and nzchar(NA) is TRUE --
  # so a bare nzchar() filter let paste() render the literal text "NA" as an
  # institution. Same trap has_val() was introduced for above.
  tokens <- unlist(strsplit(institution, "\\s*/\\s*"))
  tokens <- gsub("[^()/]*\\(([A-Za-z0-9]+)\\)", "\\1", trimws(tokens))
  paste(unique(tokens[has_val_v(tokens)]), collapse = " / ")
}

#' regex for a URL embedded in citation text -- must end on a non-punctuation
#' character so a DOI followed by a sentence-ending period ("...pasta/xyz.")
#' doesn't swallow that period
.cc_url_re <- "https?://[^\\s<>\"']*[^\\s<>\"'.,;:)\\]]"

#' Citation text with any embedded DOI/URL made clickable.
#'
#' @param cite_text the citation sentence (may already contain a doi/URL)
#' @return a tagList, safe to drop straight into a div/span
#' @export
citation_text_html <- function(cite_text) {
  if (!has_val(cite_text))
    return("No formal citation has been drafted for this source yet.")

  urls <- regmatches(cite_text, gregexpr(.cc_url_re, cite_text, perl = TRUE))[[1]]
  if (length(urls) == 0) return(cite_text)

  parts <- strsplit(cite_text, .cc_url_re, perl = TRUE)[[1]]
  # strsplit drops a trailing empty piece when the string ends on a match
  if (length(parts) < length(urls) + 1) parts <- c(parts, "")
  pieces <- vector("list", length(urls) * 2 + 1)
  for (i in seq_along(urls)) {
    pieces[[2 * i - 1]] <- parts[i]
    pieces[[2 * i]] <- tags$a(href = urls[i], target = "_blank", rel = "noopener", urls[i])
  }
  pieces[[length(pieces)]] <- parts[length(urls) + 1]
  tagList(pieces)
}

#' Small "Source" link for a citation, shown as a peer action next to the
#' Copy button rather than inline in the (often long, wrapping) citation
#' text -- from `source_url` (attributions.csv), only when the citation text
#' has no URL of its own to link (most of the SWFSC/NOAA ERDDAP citations,
#' which name the dataset but don't quote a URL inline).
#'
#' @param cite_text the citation sentence (checked for an embedded URL)
#' @param source_url this source's URL on file (attributions.csv), or NA --
#'   two of the CDFW Dungeness Crab archival sources have none, since
#'   they're unpublished archive material with nothing to link to
#' @return an <a> tag, or NULL when the citation already links out on its
#'   own or there's no source_url to link to
#' @export
citation_source_link <- function(cite_text, source_url = NA_character_) {
  if (!has_val(source_url)) return(NULL)
  if (has_val(cite_text) && grepl(.cc_url_re, cite_text, perl = TRUE)) return(NULL)
  tags$a(href = source_url, target = "_blank", rel = "noopener",
         class = "cc-cite-source-link", "Source ↗")
}

#' One reading-list "Copy citation" button, shared by the modal and detail entries
cc_copy_button <- function(cite_text, label = "Copy citation") {
  if (!has_val(cite_text)) return(NULL)
  tags$button(
    type = "button", class = "cc-cite-copylink",
    onclick = sprintf(
      "navigator.clipboard.writeText(%s); var b=this; b.textContent='Copied!'; setTimeout(function(){b.textContent=%s;}, 1500);",
      jsonlite::toJSON(cite_text, auto_unbox = TRUE),
      jsonlite::toJSON(label, auto_unbox = TRUE)),
    label)
}

#' Full citation text for one or more datasets, as a plain reading list
#'
#' Drives the "Cite this data" modal: one entry per contributing source (a
#' bundled dataset like Ichthyoplankton gets one entry per component, named
#' "Dataset -- Component"), each showing the citation, then license and
#' acknowledgement folded into a single fine-print line, separated by a plain
#' divider -- no boxes, no accent color.
#'
#' @param keys character vector of dataset_key values
#' @return a tagList; a muted placeholder if `keys` resolves to nothing
#' @export
attrib_citation_html <- function(keys) {
  rows <- attrib_rows_for(keys)
  if (nrow(rows) == 0)
    return(p(class = "cc-muted",
             "Select a species or an environmental variable to see its data source."))

  ordered_keys <- names(DATASET_LABELS)
  ks <- unique(rows$dataset_key)
  ks <- ks[order(match(ks, ordered_keys))]

  # a dataset with several components (e.g. SWFSC: Ichthyoplankton's 7 ERDDAP
  # files) gets ONE list entry: the dataset name once as a group heading, then
  # each component boxed underneath -- instead of repeating the full dataset
  # name on every line, which read as a flat, ungrouped list.
  one_component <- function(d, cite_text, own_license) {
    fine <- paste(c(
      if (is_short_license(own_license)) own_license,
      if (has_val(d$acknowledgement)) d$acknowledgement), collapse = " · ")
    div(
      class = "cc-attrib-component",
      div(class = "cc-attrib-comp-name", d$component),
      div(class = "cc-cite-entry-top",
          span(),
          div(class = "cc-cite-actions",
              citation_source_link(cite_text, d$source_url), cc_copy_button(cite_text))),
      div(class = "cc-cite-mono", citation_text_html(cite_text)),
      if (nzchar(fine)) div(class = "cc-cite-fine", fine),
      if (has_val(own_license) && !is_short_license(own_license))
        license_disclaimer_block(own_license))
  }

  entries <- list()
  for (k in ks) {
    g      <- rows[rows$dataset_key == k, ]
    multi  <- nrow(g) > 1
    shared <- shared_license_for(g)

    if (!multi) {
      d         <- g[1, ]
      cite_text <- d$citation %||% ""
      own_license <- if (has_val(d$license)) d$license else NA_character_
      fine <- paste(c(
        if (is_short_license(own_license)) own_license,
        if (has_val(d$acknowledgement)) d$acknowledgement), collapse = " · ")
      entries[[length(entries) + 1]] <- div(
        class = "cc-cite-entry",
        div(class = "cc-cite-entry-top",
            span(class = "cc-cite-entry-name", dataset_full_label(k)),
            div(class = "cc-cite-actions",
                citation_source_link(cite_text, d$source_url), cc_copy_button(cite_text))),
        div(class = "cc-cite-mono", citation_text_html(cite_text)),
        if (nzchar(fine)) div(class = "cc-cite-fine", fine),
        if (has_val(own_license) && !is_short_license(own_license))
          license_disclaimer_block(own_license))
    } else {
      comps <- lapply(seq_len(nrow(g)), function(i) {
        d <- g[i, ]
        cite_text <- d$citation %||% ""
        own_license <- if (has_val(d$license) &&
                           (is.na(shared) || !identical(d$license, shared)))
          d$license else NA_character_
        one_component(d, cite_text, own_license)
      })
      entries[[length(entries) + 1]] <- div(
        class = "cc-cite-entry cc-cite-group",
        div(class = "cc-cite-entry-name cc-cite-group-name",
            dataset_full_label(k),
            span(class = "cc-attrib-comp-count", sprintf("%d sources", nrow(g)))),
        tagList(comps),
        if (has_val(shared)) license_disclaimer_block(shared))
    }
  }
  div(class = "cc-cite-list", tagList(entries))
}

#' Compact per-source citation blocks for one dataset's attribution rows
#'
#' Citation text, then a license pill and an inline "Copy" link on one row,
#' one block per contributing source -- used by the left-panel "Sources &
#' Citations" box (server.R::data_sources_box_ui()) so a dataset's citation
#' shows inline instead of behind a separate "Dataset citations & license"
#' toggle, which used to repeat the same information.
#'
#' @param g one dataset_key's slice of ATTRIBUTIONS (see attrib_rows_for())
#' @return a tagList
#' @export
dataset_citation_items <- function(g) {
  shared <- shared_license_for(g)
  tagList(
    lapply(seq_len(nrow(g)), function(i) {
      d         <- g[i, ]
      cite_text <- d$citation %||% ""
      # only show a license/disclaimer here if it's short (pill-worthy) or
      # it differs from the one already shared at the dataset level below
      own <- if (has_val(d$license) &&
                 (is.na(shared) || !identical(d$license, shared)))
        d$license else NA_character_
      div(
        class = paste("cc-dl-cite-item", if (nrow(g) > 1) "cc-dl-cite-item-multi"),
        if (nrow(g) > 1) div(class = "cc-dl-cite-comp", d$component),
        div(class = "cc-dl-cite-text", citation_text_html(cite_text)),
        div(class = "cc-dl-cite-row",
            if (is_short_license(own)) span(class = "cc-dl-cite-pill", own)
            else span(),
            div(class = "cc-dl-cite-actions",
                citation_source_link(cite_text, d$source_url), cc_copy_button(cite_text, "Copy"))),
        if (has_val(own) && !is_short_license(own)) license_disclaimer_block(own))
    }),
    if (has_val(shared)) license_disclaimer_block(shared))
}

#' "Cite this data" modal
#'
#' Full citation / license / acknowledgement text for the dataset(s) behind
#' the currently applied species + environmental variable filters, each with
#' its own Copy button, plus a link to the full Data Sources tab for anything
#' not covered by the current selection. Opened from the Download panel's
#' "Cite this data" button (server: \code{input$btn_cite_data}), the same
#' \code{attrib_citation_html()} content also drives the panel's own
#' "Dataset citations & license" details block.
#'
#' @param keys character vector of dataset_key values currently in view
#' @return a Shiny \code{modalDialog}
#'
#' @importFrom shiny modalDialog tagList tags
#' @export
modal_cite_data <- function(keys) {
  modalDialog(
    title = tagList(bs_icon("quote"), "Cite this data", modal_x_btn()),
    div(
      class = "cc-cite-modal",
      attrib_citation_html(keys),
      # names the tool this export/view came from, not just the underlying
      # dataset(s) -- app.calcofi.io/station queries the same integrated
      # database differently, so "which CalCOFI app" is itself part of what
      # makes a citation reproducible (user question 2026-09-05).
      div(
        class = "cc-cite-entry",
        div(class = "cc-cite-entry-top",
            span(class = "cc-cite-entry-name", "This view"),
            div(class = "cc-cite-actions",
                tags$a(href = APP_CITE_URL, target = "_blank", rel = "noopener",
                       class = "cc-cite-source-link", "Open ↗"),
                cc_copy_button(
                  paste0("CalCOFI Hexagon Explorer. ", APP_CITE_URL), "Copy"))),
        div(class = "cc-cite-mono",
            paste0("CalCOFI Hexagon Explorer. ", APP_CITE_URL))),
      p(class = "cc-muted cc-cite-more",
        # counted from ATTRIBUTIONS, not typed: adding a row to
        # attributions.csv is the whole workflow for a new source, and a
        # hardcoded number silently goes stale the first time it is used
        "Full citations, licenses, and acknowledgements for all ",
        length(unique(ATTRIBUTIONS$dataset_key)),
        " contributing datasets are on the ",
        actionLink("cite_modal_datasources_link", "Data Sources"), " tab.")),
    footer = tagList(
      tags$button(type = "button", class = "btn btn-secondary",
                  `data-bs-dismiss` = "modal", "Close")),
    size = "l", easyClose = TRUE, fade = FALSE)
}

#' Which dataset_key(s) a selected species/taxon belongs to
#'
#' Resolves picker labels ("Common (rank: Scientific)") to `d_taxa_ds`'s
#' dataset_key via an exact scientific_name match. Does not walk taxonomic
#' children the way \code{resolve_sp_ids()} does -- attribution only needs to
#' name the source, and a higher-rank taxon (family/genus) with no exact
#' bio_obs match simply resolves to nothing, which callers show as "select a
#' species" rather than guessing.
#'
#' @param sp_labels character vector of picker labels
#' @return character vector of dataset_key values, possibly empty
#' @export
dataset_keys_for_species <- function(sp_labels) {
  sci <- unique(vapply(sp_labels, extract_scientific_name, character(1)))
  unique(d_taxa_ds$dataset_key[d_taxa_ds$scientific_name %in% sci])
}

#' Which dataset_key an environmental variable belongs to
#'
#' @param measurement_type a single `measurement_type` value (input$sel_env_var)
#' @return character scalar dataset_key, or character(0) if unmatched
#' @export
dataset_key_for_env_var <- function(measurement_type) {
  i <- match(measurement_type, d_env_vars$measurement_type)
  if (is.na(i)) character(0) else d_env_vars$dataset_key[i]
}


# utility functions ----

#' Generate Time Aggregation Expression for Species Data
#'
#' Creates a SQL-based expression for temporal aggregation of species time series
#' using DuckDB date functions. Used internally by \code{\link{prep_ts_sp}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_sp |> mutate(time = !!expr_time_sp("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_sp}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_sp <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', time_start)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', time_start)), day(date_trunc('quarter', time_start)))")),
         "month"        = expr(sql("extract('month' FROM time_start)")),
         "day"          = expr(sql("extract('doy' FROM time_start)")),
         "year_quarter" = expr(sql("date_trunc('quarter', time_start)")),
         "year_month"   = expr(sql("date_trunc('month', time_start)")),
         "year_day"     = expr(sql("date_trunc('day', time_start)"))
  )
}


#' Generate Time Aggregation Expression for Environmental Data
#'
#' Creates a SQL-based expression for temporal aggregation of environmental
#' time series using DuckDB date functions. Used internally by \code{\link{prep_ts_env}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_env |> mutate(time = !!expr_time_env("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_env}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_env <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', dtime)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', dtime)), day(date_trunc('quarter', dtime)))")),
         "month"        = expr(sql("extract('month' FROM dtime)")),
         "day"          = expr(sql("extract('doy' FROM dtime)")),
         "year_quarter" = expr(sql("date_trunc('quarter', dtime)")),
         "year_month"   = expr(sql("date_trunc('month', dtime)")),
         "year_day"     = expr(sql("date_trunc('day', dtime)"))
  )
}


#' Create Buffer Around Line Segment with Dateline Handling
#'
#' Creates a buffered polygon around a line segment (transect), handling
#' dateline crossings and projecting to appropriate UTM zone for accurate
#' distance calculations.
#'
#' @param coords Matrix or data.frame of coordinates (longitude, latitude) defining the line segment
#' @param buffer_dist Numeric buffer distance in meters (default: 5000)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{utm_crs} - EPSG code for the UTM projection used
#'     \item \code{segment} - sf linestring object in WGS84 (EPSG:4326)
#'     \item \code{segment_utm} - sf linestring object in UTM projection
#'     \item \code{buffer} - sf polygon buffer in WGS84 (EPSG:4326)
#'     \item \code{buffer_utm} - sf polygon buffer in UTM projection
#'   }
#'
#' @details
#' The function automatically detects the appropriate UTM zone based on the
#' centroid of the input segment. Dateline crossings are handled by normalizing
#' coordinates to 0-360 range when necessary.
#'
#' @examples
#' # create transect across California Current
#' coords <- matrix(c(-120, 34, -118, 36), ncol = 2, byrow = TRUE)
#' result <- buffer_transect(coords, buffer_dist = 10000)
#' plot(result$buffer)
#'
#' @seealso \code{\link{fix_dateline_crossing}} for dateline crossing detection
#' @seealso \code{\link{modal_depth_profile}} for UI implementation
#'
#' @importFrom sf st_sf st_sfc st_linestring st_centroid st_coordinates st_transform st_buffer st_wrap_dateline
#' @importFrom units set_units
#'
#' @export
buffer_transect <- function(coords, buffer_dist = 5000) {
  # create initial segment
  segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # handle dateline crossing
  segment <- fix_dateline_crossing(segment)

  # get centroid to determine UTM zone
  centroid <- st_centroid(segment)
  cent_coords <- st_coordinates(centroid)
  lon <- cent_coords[1, "X"]
  lat <- cent_coords[1, "Y"]

  # adjust longitude for UTM if it was shifted
  lon <- ifelse(lon > 180, lon - 360, lon)

  # calculate UTM zone
  zone <- floor((lon + 180) / 6) + 1
  hemisphere <- if (lat >= 0) 32600 else 32700
  utm_crs <- hemisphere + zone

  # transform to UTM, buffer, and transform back
  segment_utm <- st_transform(segment, utm_crs)
  buffer_utm <- st_buffer(segment_utm, dist = buffer_dist, endCapStyle = "FLAT")
  buffer <- st_transform(buffer_utm, 4326)

  # ensure buffer is valid and handles dateline
  buffer <- st_wrap_dateline(buffer, options = c("WRAPDATELINE=YES"))

  return(list(
    utm_crs     = utm_crs,
    segment     = segment,
    segment_utm = segment_utm,
    buffer      = buffer,
    buffer_utm  = buffer_utm))
}


#' Detect and Handle Dateline Crossing in Line Segments
#'
#' Normalizes longitude coordinates when a line segment crosses the ±180°
#' dateline, preventing discontinuities in buffering and visualization.
#'
#' @param segment sf linestring object representing a transect or track
#'
#' @return sf linestring object with normalized coordinates (0-360° range if dateline is crossed)
#'
#' @details
#' Dateline crossings are detected by checking for longitude jumps > 180°.
#' When detected, negative longitudes are shifted to 0-360° range. The segment
#' is then segmentized to 1000m intervals for smooth buffering.
#'
#' @examples
#' \dontrun{
#' # transect crossing the dateline
#' coords <- matrix(c(175, 30, -175, 35), ncol = 2, byrow = TRUE)
#' segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))
#' normalized <- fix_dateline_crossing(segment)
#' }
#'
#' @seealso \code{\link{buffer_transect}} for usage in buffering workflow
#'
#' @importFrom sf st_coordinates st_sf st_sfc st_linestring st_segmentize
#' @importFrom units set_units
#'
#' @keywords internal
fix_dateline_crossing <- function(segment) {
  coords <- st_coordinates(segment)[, c("X", "Y")]
  lons <- coords[, "X"]

  # check for dateline crossing (large longitude jump)
  lon_diff <- diff(lons)
  crosses_dateline <- any(abs(lon_diff) > 180)

  if (!crosses_dateline) return(segment)

  # normalize longitudes to avoid discontinuity
  # shift coords to a 0-360 range if crossing +180/-180
  if (any(lons < 0)) {
    coords[, "X"] <- ifelse(lons < 0, lons + 360, lons)
  }

  # create new linestring
  new_segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # optional: split into multiple segments if needed
  # use st_segmentize to add points across dateline for smoother buffer
  new_segment <- st_segmentize(new_segment, set_units(1000, "m"))

  return(new_segment)
}


# reproducible downloads ----
# The download bundle pairs every data file with the exact, portable SQL that
# produced it. The integration query is built and executed by
# calcofi4r::cc_match_bio_env() against the public GCS release parquet, so
# anyone can re-run query/integrated_*.sql in DuckDB (CLI, Python or R) and get
# identical rows. See query/REPRODUCE.md inside any downloaded bundle.

#' Resolve the frozen-release version powering the app database
#'
#' Parses the version (e.g. \code{"v2026.05.14"}) out of the app's DuckDB file
#' name; falls back to the GCS \code{latest.txt} pointer.
#'
#' @param path Path to the app database (default: the \code{db_path} global).
#' @return Release version string.
#' @export
release_version <- function(path = db_path) {
  v <- sub(".*calcofi_(v[0-9][0-9.]*)\\.duckdb$", "\\1", basename(path))
  if (grepl("^v[0-9]", v)) return(v)
  tryCatch(
    trimws(readLines(
      "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
      warn = FALSE)[1]),
    error = function(e) "latest")
}

#' A release catalog, fetched once per version
#'
#' @param version Release version string from \code{\link{release_version}}.
#' @return The catalog as a nested list (\code{calcofi4r::cc_catalog()}).
#' @export
release_catalog <- local({
  cache <- new.env(parent = emptyenv())
  function(version) {
    version <- as.character(version)
    if (is.null(cache[[version]]))
      cache[[version]] <- calcofi4r::cc_catalog(version)
    cache[[version]]
  }
})

#' Per-table \code{read_parquet()} SQL for a release
#'
#' Every release table is resolved through the catalog by
#' \code{calcofi4r::cc_release_sources()} — the one sanctioned map from a table
#' to its parquet bytes: content-addressed objects
#' (\code{ducklake/tables/{table}/{hash}/…}) since the v2026.09 releases, the
#' per-release \code{releases/{version}/parquet/…} path before that (an
#' \code{s3://} glob for a legacy partitioned table such as \code{obs}, which
#' needs the anonymous-S3 settings of \code{\link{gcs_s3_settings_sql}}). Never
#' build that path by hand. Same shape as \code{calcofi4r:::.cc_read_parquet()}.
#'
#' @param version Release version string from \code{\link{release_version}}.
#' @return A function \code{table -> "read_parquet(...)"} SQL fragment.
#' @export
release_read_parquet <- function(version) {
  cat_ <- release_catalog(version)
  function(table)
    calcofi4r::cc_read_parquet_sql(calcofi4r::cc_release_sources(cat_, table))
}

#' Resolved parquet sources of release tables, for manifest provenance
#'
#' @param version Release version string.
#' @param tables Character vector of release table names.
#' @return Named list (one per table) of \code{urls}, \code{hive_partitioning}
#'   and, on a content-addressed release, \code{content_hash} per object.
#' @export
release_table_sources <- function(version, tables) {
  cat_ <- release_catalog(version)
  setNames(lapply(tables, function(tb) {
    s   <- calcofi4r::cc_release_sources(cat_, tb)
    out <- list(urls = as.list(as.character(s$urls)), hive_partitioning = isTRUE(s$hive))
    if (any(!is.na(s$hashes))) out$content_hash <- as.list(unname(s$hashes))
    out
  }), tables)
}

#' DuckDB settings that let an \code{s3://} release glob read GCS anonymously
#'
#' Only a legacy (pre-v2026.09) partitioned table resolves to one; the five
#' \code{SET}s are those of \code{calcofi4r:::.cc_setup_gcs_httpfs()}.
#'
#' @param sql SQL string(s); the settings are emitted only if one reads \code{s3://}.
#' @return A SQL string (possibly empty).
#' @export
gcs_s3_settings_sql <- function(sql) {
  if (!any(startsWith(extract_source_urls(sql), "s3://"))) return("")
  paste(
    "SET s3_region = 'auto';",
    "SET s3_endpoint = 'storage.googleapis.com';",
    "SET s3_url_style = 'path';",
    "SET s3_access_key_id = '';",
    "SET s3_secret_access_key = '';",
    sep = "\n")
}

# the release tables the portable bio + env match queries read
MATCH_TABLES <- c("obs", "taxon", "sample_measurement")

#' Extract a scientific name from a UI species label
#'
#' Species dropdown labels look like \code{"Common name (rank: Scientific
#' name)"} (or \code{"(Scientific name)"} when no common name). Returns the
#' scientific name.
#'
#' @param label Character species label.
#' @return Scientific name (character).
#' @export
extract_scientific_name <- function(label) {
  # "Common (rank: Scientific)" -> "Scientific" (last parenthetical, after ': ')
  sci <- sub(".*\\(.*:\\s*([^)]+)\\).*", "\\1", label)
  if (identical(sci, label))
    # fallback "Common (Scientific)" / "(Scientific)" -> last parenthetical
    sci <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", label)
  trimws(sci)
}

#' Distinct read_parquet() source URLs referenced in a SQL string
#'
#' Handles both \code{read_parquet('url')} and the explicit file-list form
#' \code{read_parquet(['url', 'url'], hive_partitioning = true)} of a
#' content-addressed partitioned table.
#'
#' @param sql One or more SQL strings.
#' @return Sorted unique character vector of parquet URLs.
#' @export
extract_source_urls <- function(sql) {
  one  <- paste(sql, collapse = "\n")
  hits <- regmatches(one, gregexpr("read_parquet\\(\\[?\\s*'[^']+'(\\s*,\\s*'[^']+')*", one))[[1]]
  urls <- unlist(regmatches(hits, gregexpr("'[^']+'", hits)))
  sort(unique(gsub("^'|'$", "", urls)))
}

#' Build the biological (ichthyoplankton) match subquery
#'
#' Emits a portable \code{SELECT} over the release's GCS parquet, shaped for
#' \code{calcofi4r::cc_match_bio_env()} (columns \code{bio_id}, \code{bio_datetime},
#' \code{bio_lon}, \code{bio_lat}, \code{bio_value} plus descriptive columns).
#' When \code{include_children} is \code{TRUE} the species filter is expanded
#' via a recursive walk of the WoRMS \code{taxon.parentNameUsageID} tree.
#'
#' @param sci_names Character vector of scientific names.
#' @param qtr Integer vector of quarters (1-4).
#' @param date_range Length-2 date vector (tow start bounds).
#' @param version Release version string.
#' @param include_children Include descendant taxa (default: TRUE).
#' @return SQL \code{SELECT} string.
#' @export
build_bio_match_sql <- function(
    sci_names, qtr, date_range, version, include_children = TRUE) {

  rp   <- release_read_parquet(version)
  nm   <- paste0("'", gsub("'", "''", sci_names), "'", collapse = ", ")
  qtrs <- paste(as.integer(qtr), collapse = ", ")
  d1   <- as.character(date_range[1])
  d2   <- as.character(date_range[2])

  prefix        <- ""
  species_where <- glue("t.scientific_name IN ({nm})")
  if (isTRUE(include_children)) {
    # unified taxon: seed by scientific_name, walk descendants via parent_taxon_key
    prefix <- glue(
      "WITH RECURSIVE taxon_tree AS (
      SELECT taxon_key
      FROM {rp('taxon')}
      WHERE scientific_name IN ({nm})
    UNION ALL
      SELECT t.taxon_key
      FROM {rp('taxon')} t
      JOIN taxon_tree tt ON t.parent_taxon_key = tt.taxon_key
  )
  ")
    species_where <- "o.taxon_key IN (SELECT taxon_key FROM taxon_tree)"
  }

  # read the consolidated core `obs` (ichthyo abundance) + effort from
  # `sample_measurement` — the per-dataset ichthyo/net/tow/site tables are retired.
  # Mirrors calcofi4r::.cc_bio_sql_ichthyo (kept 1:1 with the app matcher).
  glue(
    "  {prefix}SELECT
    o.obs_id::VARCHAR AS bio_id,
    o.datetime        AS bio_datetime,
    o.longitude       AS bio_lon,
    o.latitude        AS bio_lat,
    o.measurement_value * shf.measurement_value / nullif(ps.measurement_value, 0) AS bio_value,
    t.scientific_name,
    t.common_name,
    t.worms_id,
    o.life_stage,
    o.measurement_value AS tally,
    extract(quarter FROM o.datetime)::INTEGER AS quarter
  FROM {rp('obs')} o
  JOIN {rp('taxon')} t ON t.taxon_key = o.taxon_key
  LEFT JOIN {rp('sample_measurement')} shf ON shf.sample_key = o.sample_key AND shf.measurement_type = 'std_haul_factor'
  LEFT JOIN {rp('sample_measurement')} ps  ON ps.sample_key  = o.sample_key AND ps.measurement_type = 'prop_sorted'
  WHERE o.realm = 'bio' AND o.dataset_key = 'swfsc_ichthyo' AND o.measurement_type = 'abundance'
    AND o.measurement_value IS NOT NULL
    AND {calcofi4r::cc_qual_ok_sql('o')}
    AND o.datetime IS NOT NULL
    AND o.longitude IS NOT NULL
    AND o.latitude IS NOT NULL
    AND {species_where}
    AND extract(quarter FROM o.datetime) IN ({qtrs})
    AND o.datetime >= TIMESTAMP '{d1}'
    AND o.datetime <= TIMESTAMP '{d2}'")
}

#' Build the environmental (CTD-bottle) match subquery
#'
#' Emits a portable \code{SELECT} over the release's GCS parquet, shaped for
#' \code{calcofi4r::cc_match_bio_env()} (columns \code{env_id}, \code{env_datetime},
#' \code{env_lon}, \code{env_lat}, \code{env_value}, \code{env_depth_m},
#' \code{measurement_type}). The date window is padded by \code{pad_hours} so
#' boundary matches survive the downstream interval join.
#'
#' @param env_var Environmental \code{measurement_type}.
#' @param qtr Integer vector of quarters (1-4).
#' @param date_range Length-2 date vector (cast datetime bounds).
#' @param depth_range Length-2 numeric vector (bottle depth bounds, meters).
#' @param version Release version string.
#' @param pad_hours Hours to pad the date window (default: 6).
#' @return SQL \code{SELECT} string.
#' @export
build_env_match_sql <- function(
    env_var, qtr, date_range, depth_range, version, pad_hours = 6) {

  rp   <- release_read_parquet(version)
  qtrs <- paste(as.integer(qtr), collapse = ", ")
  d1   <- as.character(date_range[1])
  d2   <- as.character(date_range[2])
  dmin <- depth_range[1]
  dmax <- depth_range[2]

  # read the consolidated core `obs` (env realm, bottle) — the per-dataset
  # bottle_measurement/bottle/casts tables are retired. Mirrors
  # calcofi4r::.cc_env_sql.
  glue(
    "  SELECT
    o.obs_id             AS env_id,
    o.datetime           AS env_datetime,
    o.longitude          AS env_lon,
    o.latitude           AS env_lat,
    o.measurement_value  AS env_value,
    o.depth_min_m        AS env_depth_m,
    o.measurement_type   AS measurement_type
  FROM {rp('obs')} o
  WHERE o.realm = 'env' AND o.dataset_key = 'calcofi_bottle' AND o.measurement_type = '{env_var}'
    AND o.measurement_value IS NOT NULL
    AND {calcofi4r::cc_qual_ok_sql('o')}
    AND o.datetime IS NOT NULL
    AND o.longitude IS NOT NULL
    AND o.latitude IS NOT NULL
    AND o.depth_min_m >= {dmin}
    AND o.depth_min_m <= {dmax}
    AND extract(quarter FROM o.datetime) IN ({qtrs})
    AND o.datetime >= TIMESTAMP '{d1}' - INTERVAL '{pad_hours} hours'
    AND o.datetime <= TIMESTAMP '{d2}' + INTERVAL '{pad_hours} hours'")
}

#' Render the CITATION.md for a download bundle
#'
#' One dataset per file this app draws from (the ichthyoplankton bio side, the
#' bottle env side — see \code{\link{build_download_bundle}}), plus the
#' integrated release's own citation, via \code{calcofi4r::cc_cite()} (>=
#' 1.19.0) so this bundle's citations, licences, DOIs and dataset-page links
#' can never disagree with the app's other citation surfaces (the About page,
#' \code{cc_cite()} elsewhere). With an older calcofi4r installed (no
#' \code{cc_cite()} page line yet), degrades to the release/dataset page URLs
#' alone rather than erroring — a bundle must always ship a CITATION.md.
#'
#' @param version Release version the bundle was built against.
#' @param datasets `dataset_key`s this bundle draws from.
#' @return Character vector of markdown lines.
#' @export
citation_md <- function(version, datasets = c("calcofi_bottle", "swfsc_ichthyo")) {
  page_url <- function(key) sprintf("https://calcofi.io/datasets/%s/", key)
  # names the tool that produced this specific export, the way
  # app.calcofi.io/station's own citations name that portal -- a query built
  # here (this app's own species/filter selection) can't be reconstructed
  # from the underlying dataset citations alone, only from the app + its URL
  # (user question 2026-09-05: "should these citations also include the app
  # it was downloaded from").
  accessed_via <- c(
    "## Accessed via", "",
    sprintf("This export was produced by the CalCOFI Hexagon Explorer: %s", APP_CITE_URL),
    "")
  if (!requireNamespace("calcofi4r", quietly = TRUE) ||
      utils::packageVersion("calcofi4r") < "1.19.0") {
    return(c(
      "# Citing this data",
      "",
      "Cite the CalCOFI integrated database and each dataset in this bundle:",
      "",
      sprintf("- **%s**: %s", datasets, page_url(datasets)),
      "",
      sprintf("The integrated database, release %s: https://calcofi.io/db-schema/?v=%s",
             version, version),
      "",
      "(calcofi4r >= 1.19.0 is not installed here, so the formatted citation,",
      "licence and DOI from calcofi4r::cc_cite() are not available — the",
      "dataset pages above carry them.)",
      "",
      accessed_via))
  }
  lines <- calcofi4r::cc_cite(datasets, version = version, format = "text")
  c("# Citing this data",
    "",
    "Cite the CalCOFI integrated database AND every dataset in this bundle.",
    "",
    "## The integrated database", "", lines[1], "",
    unlist(lapply(seq_along(datasets), function(i)
      c(sprintf("## %s", datasets[i]), "", lines[i + 1], ""))),
    accessed_via)
}

#' Render the REPRODUCE.md walk-through for a download bundle
#'
#' @param manifest The manifest list assembled by \code{\link{build_download_bundle}}.
#' @return Character vector of markdown lines.
#' @export
reproduce_md <- function(manifest) {
  v <- manifest$release_version
  c(
    "# Reproducing this CalCOFI download",
    "",
    glue(
      "Every file under `data/` was produced by a SQL query in `query/`, run ",
      "against the **public** Parquet files of CalCOFI release `{v}` on Google ",
      "Cloud Storage. Re-run any `.sql` file in DuckDB and you get identical ",
      "rows — no credentials, no API and no app required."),
    "",
    "## What's in this bundle",
    "",
    "| data file | query | description |",
    "|---|---|---|",
    "| `data/original/bio.csv` | `query/bio.sql` | net-tow ichthyoplankton (standardized tally) |",
    "| `data/original/env.csv` | `query/env.sql` | CTD-bottle environmental measurements |",
    "| `data/integrated/integrated_<method>.csv` | `query/integrated_<method>.sql` | bio matched to env in time + space |",
    "| `query/manifest.json` | — | release version, filters, row counts, md5 checksums |",
    "",
    "## Scope of `bio.csv`",
    "",
    paste(
      "The biological query is **SWFSC ichthyoplankton** only, as a",
      "standardized tally (count per 10 m^2: raw tally x std_haul_factor /",
      "prop_sorted). Your taxa, quarters, date range and the \"include",
      "taxonomic children\" setting are all applied to it. The app's",
      "**Datasets** filter and its **Standardized as** unit pick are not --",
      "they shape what the map and plots show, not this export. Read",
      "`query/bio.sql` for the exact filter set; every other dataset in the",
      "release is queryable the same way from the same public Parquet."),
    "",
    glue(
      "`<method>` is one of `nearest_time`, `nearest_dist`, `average` — how the ",
      "environmental observations within the match window are reduced per ",
      "biological observation."),
    "",
    "## Re-run the integration query",
    "",
    "### DuckDB CLI",
    "",
    "```sh",
    "duckdb < query/integrated_nearest_time.sql",
    "```",
    "",
    "(each `.sql` file is prefixed with the `INSTALL`/`LOAD` of `httpfs` + `spatial`,",
    "plus the anonymous-S3 `SET`s when a release table is read as an `s3://` glob)",
    "",
    "### Python",
    "",
    "```python",
    "import duckdb",
    "con = duckdb.connect()",
    "df = con.sql(open('query/integrated_nearest_time.sql').read()).df()",
    "```",
    "",
    "### R",
    "",
    "```r",
    "library(DBI)",
    "con <- dbConnect(duckdb::duckdb())",
    "sql <- paste(readLines('query/integrated_nearest_time.sql'), collapse = '\\n')",
    "df  <- dbGetQuery(con, sql)",
    "```",
    "",
    "Or with the **calcofi4r** package — the same helper that generated this bundle:",
    "",
    "```r",
    "# remotes::install_github('calcofi/calcofi4r')",
    "library(calcofi4r)",
    "d <- cc_match_ichthyo_by_name(",
    "  'Sardinops sagax', env_var = 'temperature',",
    "  date_min = '2018-01-01', date_max = '2018-03-31', relax_matching = TRUE)",
    "cat(attr(d, 'sql'))   # the portable SQL behind it",
    "```",
    "",
    "## Verify integrity",
    "",
    "```sh",
    "md5sum data/integrated/integrated_nearest_time.csv",
    "# compare against query/manifest.json -> files[...].md5",
    "```")
}

#' Assemble the reproducible portion of a download bundle
#'
#' Builds the portable bio + env subqueries from the current filter
#' \code{params}, runs them (and the integration, once per join method) against
#' the public GCS release parquet via \code{calcofi4r::cc_match_bio_env()}, and
#' writes \code{data/original/}, \code{data/integrated/} and \code{query/}
#' (per-file \code{*.sql}, \code{manifest.json}, \code{REPRODUCE.md}) under
#' \code{zip_root}. The SQL that is serialized is exactly the SQL that was run.
#'
#' @param zip_root Directory to write the bundle into.
#' @param params Filter params list (from \code{rx$params}): \code{taxa},
#'   \code{env_var}, \code{sel_qtr}/\code{quarters}, \code{date_range},
#'   \code{depth_range}, \code{ck_children}/\code{include_children},
#'   \code{time_window}, \code{dist_window}, \code{zones}.
#' @param version Release version string; defaults to \code{\link{release_version}()}.
#' @return Character vector of bundle-relative paths written.
#' @export
build_download_bundle <- function(zip_root, params, version = NULL) {

  if (utils::packageVersion("calcofi4r") < "1.11.0")
    stop(
      "build_download_bundle() needs calcofi4r >= 1.11.0 ",
      "(cc_match_bio_env + cc_release_sources). ",
      "Update with: remotes::install_github('calcofi/calcofi4r')")

  version <- version %||% release_version()

  # resolve filters from params --------------------------------------------
  taxa             <- params$taxa
  sci_names        <- vapply(taxa, extract_scientific_name, character(1),
                             USE.NAMES = FALSE)
  include_children <- isTRUE(params$ck_children %||%
                             params$include_children %||% FALSE)
  qtr         <- params$sel_qtr %||% params$quarters %||% 1:4
  date_range  <- params$date_range
  depth_range <- params$depth_range %||% c(0, 5000)
  max_time_hr <- params$time_window %||% default_max_hours_diff
  max_dist_km <- (params$dist_window %||% default_max_meters_diff) / 1000

  # two portable subqueries ------------------------------------------------
  bio_sql <- build_bio_match_sql(
    sci_names, qtr, date_range, version, include_children)
  env_sql <- build_env_match_sql(
    params$env_var, qtr, date_range, depth_range, version,
    pad_hours = max_time_hr)

  # GCS-capable connection (httpfs + spatial) ------------------------------
  con_gcs <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con_gcs, shutdown = TRUE), add = TRUE)
  dbExecute(con_gcs, "INSTALL httpfs; LOAD httpfs;")
  dbExecute(con_gcs, "INSTALL spatial; LOAD spatial;")
  # a legacy (pre-v2026.09) partitioned table resolves to an s3:// glob, which
  # DuckDB expands through its S3 client pointed anonymously at GCS; the same
  # settings go into the bundle's .sql files so they stay copy-paste runnable
  s3_sql <- gcs_s3_settings_sql(c(bio_sql, env_sql))
  if (nzchar(s3_sql)) dbExecute(con_gcs, s3_sql)

  # writer helpers ---------------------------------------------------------
  paths      <- character()
  files_meta <- list()
  write_file <- function(rel, x) {
    full <- file.path(zip_root, rel)
    dir.create(dirname(full), showWarnings = FALSE, recursive = TRUE)
    if (is.data.frame(x)) {
      write.csv(x, full, row.names = FALSE, na = "")
    } else {
      writeLines(as.character(x), full)
    }
    paths <<- c(paths, rel)
    full
  }
  # .sql files are written copy-paste runnable: prefixed with the extension
  # loads they need to read GCS parquet over HTTPS + compute spatial distance
  sql_header <- paste(
    "-- Re-run in DuckDB (CLI, Python or R) against public CalCOFI release",
    "-- parquet. See REPRODUCE.md. No credentials or API required.",
    "INSTALL httpfs; LOAD httpfs;",
    "INSTALL spatial; LOAD spatial;",
    if (nzchar(s3_sql)) s3_sql,
    "", "", sep = "\n")
  write_sql <- function(rel, sql) write_file(rel, paste0(sql_header, sql, "\n"))
  add_meta <- function(rel_csv, rel_sql, df, extra = list()) {
    files_meta[[rel_csv]] <<- c(
      list(
        sql    = rel_sql,
        n_rows = nrow(df),
        md5    = unname(tools::md5sum(file.path(zip_root, rel_csv)))),
      extra)
  }

  # original bio + env -----------------------------------------------------
  write_sql("query/bio.sql", bio_sql)
  write_sql("query/env.sql", env_sql)
  # Materialize bio + env into local temp tables ONCE. The three join methods
  # below then compute against these (fast) instead of each re-embedding the
  # bio/env subqueries and re-scanning the 17.5M-row obs.parquet over HTTPS —
  # which made the bundle take ~5 min (3×~95s) and blow the proxy timeout, so
  # the browser got a truncated response ("Site wasn't available"). GCS is now
  # scanned twice total (here), not eight times, cutting the bundle to ~30s.
  dbExecute(con_gcs, glue("CREATE TEMP TABLE _bio_src AS {bio_sql}"))
  dbExecute(con_gcs, glue("CREATE TEMP TABLE _env_src AS {env_sql}"))
  d_bio <- dbGetQuery(con_gcs, "SELECT * FROM _bio_src")
  d_env <- dbGetQuery(con_gcs, "SELECT * FROM _env_src")
  write_file("data/original/bio.csv", d_bio)
  write_file("data/original/env.csv", d_env)
  add_meta("data/original/bio.csv", "query/bio.sql", d_bio)
  add_meta("data/original/env.csv", "query/env.sql", d_env)

  # integrated match, once per join method ---------------------------------
  # Compute against the local temp tables (fast). Write the PORTABLE GCS-parquet
  # SQL (built from bio_sql/env_sql via return_sql, runnable anywhere) to the
  # bundle's .sql files — the temp tables are only a local compute shortcut, so
  # the .sql re-run against GCS yields byte-identical rows to the CSV.
  methods <- c("nearest_time", "nearest_dist", "average")
  for (m in methods) {
    d <- calcofi4r::cc_match_bio_env(
      "SELECT * FROM _bio_src", "SELECT * FROM _env_src",
      max_dist_km = max_dist_km, max_time_hr = max_time_hr,
      join_method = m, con = con_gcs, version = version, collect = TRUE)
    portable_sql <- as.character(calcofi4r::cc_match_bio_env(
      bio_sql, env_sql,
      max_dist_km = max_dist_km, max_time_hr = max_time_hr,
      join_method = m, version = version, return_sql = TRUE))
    rel_csv <- glue("data/integrated/integrated_{m}.csv")
    rel_sql <- glue("query/integrated_{m}.sql")
    write_sql(rel_sql, portable_sql)
    write_file(rel_csv, as.data.frame(d))
    add_meta(rel_csv, rel_sql, d, list(join_method = m))
  }

  # manifest.json ----------------------------------------------------------
  manifest <- list(
    schema_version    = "1.0",
    generated_at      = format(Sys.time(), tz = "UTC", usetz = TRUE),
    release_version   = version,
    calcofi4r_version = as.character(utils::packageVersion("calcofi4r")),
    # per release table: the parquet object(s) the catalog resolved it to, with
    # content_hash on a content-addressed (v2026.09+) release
    release_sources   = release_table_sources(version, MATCH_TABLES),
    filters = list(
      taxa             = as.list(taxa),
      scientific_names = as.list(sci_names),
      include_children = include_children,
      env_var          = params$env_var,
      quarters         = as.list(as.integer(qtr)),
      date_range       = as.character(date_range),
      depth_range_m    = as.list(depth_range),
      zones            = if (is.null(params$zones))
        "all locations" else as.list(params$zones)),
    match_params = list(
      max_dist_km  = max_dist_km,
      max_time_hr  = max_time_hr,
      join_methods = as.list(methods)),
    gcs_source_urls = as.list(extract_source_urls(c(bio_sql, env_sql))),
    files           = files_meta)
  write_file(
    "query/manifest.json",
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"))

  # REPRODUCE.md -----------------------------------------------------------
  write_file("query/REPRODUCE.md", reproduce_md(manifest))

  # CITATION.md (plan 2026-09-05 D-4/D-6: every dataset the bundle draws
  # from — bio via ichthyo, env via bottle — gets its own citation + page) --
  write_file("query/CITATION.md", citation_md(version))

  paths
}


#' Which quantities a species selection actually contains
#'
#' `std_tally` is not one quantity. Where a net tow supports standardization it is
#' a gear-standardized density (`count/10m2` for oblique/vertical, `count/100m3`
#' for manta); everywhere else it is the value the source published, in that
#' source's own unit — an areal density for the euphausiid and ZooScan series, a
#' bare occurrence count for `cdfw_dungeness-crab`, which is a lab-examined
#' aliquot of an archived catch with no tow volume to divide by.
#'
#' Calling all of that "CPUE" is wrong in the last case: nothing was divided by
#' effort. Calling it "density" is wrong too — that was the bug this replaced. So
#' the app asks the data what it is holding, and says so.
#'
#' Deliberately keyed on `tow_type`/`std_haul_factor` presence rather than on
#' dataset or unit names: those are the same columns `prep_db.R` branches on to
#' compute `cpue_unit`, so a dataset added later is classified correctly without
#' anything here knowing its name.
#'
#' @param df_sp species table (lazy or collected) carrying `cpue_unit`,
#'   `tow_type`, `std_haul_factor`
#' @return a tibble with ONE ROW PER `cpue_unit` -- `cpue_unit`,
#'   `standardized` (logical; TRUE only when every row in that unit is), `n` --
#'   ordered by `n` descending; zero rows if nothing is summarizable
#' @export
sp_unit_summary <- function(df_sp) {
  # WARN, never swallow. An earlier version returned an empty tibble on any
  # error, which turned a scope bug (`df_sp` not visible in the render block)
  # into a plausible-looking "Avg. value" legend with no note — indistinguishable
  # from a genuinely empty selection, and invisible in the log. If this cannot
  # summarize, the reason belongs in the log.
  out <- tryCatch(
    df_sp |>
      dplyr::filter(!is.na(std_tally), !is.na(cpue_unit)) |>
      # `cpue_standardized` comes from prep_db.R, computed in the same CASE that
      # produces cpue_unit. Do NOT re-derive it here from tow_type/effort columns:
      # an earlier version did, and drifted immediately — it treated any tow
      # carrying a volume as standardized, though prep_db only standardizes by
      # volume for manta. One rule, one place.
      dplyr::mutate(standardized = as.logical(cpue_standardized)) |>
      dplyr::count(cpue_unit, standardized) |>
      dplyr::collect() |>
      # ONE ROW PER cpue_unit, which is what every caller assumes: both
      # cpue_unit_selector_ui() (its `nrow(u) <= 1L` gate and its
      # choiceValues) and sp_value_label() (`nrow(u) == 1L` vs. "(mixed
      # units)") read nrow() as "how many units are present". count() keys on
      # the (unit, flag) PAIR, so one unit carrying both cpue_standardized
      # values -- reachable through prep_db.R's `ELSE COALESCE(mt.units,
      # o.measurement_type)` fallback -- rendered a two-option radio whose
      # second option was a silent no-op, under a "(mixed units)" legend for
      # a single unit. all(), not any(): claim "effort-standardized" only
      # when every row in that unit is.
      dplyr::summarise(
        standardized = all(standardized),
        n            = sum(n),
        .by          = cpue_unit),
    error = function(e) {
      warning("sp_unit_summary(): ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(out) || !nrow(out))
    return(tibble::tibble(cpue_unit = character(), standardized = logical(), n = integer()))
  dplyr::arrange(out, dplyr::desc(n))
}

#' Which dataset_key(s) actually back a species selection's CURRENT rows
#'
#' Unlike \code{\link{dataset_keys_for_species}} (a taxonomy-table lookup:
#' every dataset a scientific name has EVER been recorded in, regardless of
#' what's filtered), this reads the dataset_key(s) present in `df_sp` AFTER
#' the Datasets filter and the "Standardized as" pick have both been
#' applied. Sources & Citations, the map title's info popover, and "Cite
#' this data" all used the taxonomy-table version, which is why picking
#' e.g. "Pacific sardine -- CUFES Fish Eggs" showed citations for SWFSC:
#' Ichthyoplankton too: sardine is also caught by that program's bongo nets,
#' and the lookup does not know which of the picker's dataset GROUPS was
#' actually chosen, or that the Datasets filter had narrowed the map to one
#' of them. Reading `df_sp` instead answers "what is actually contributing
#' to what's drawn right now" rather than "what has this taxon ever
#' appeared in."
#'
#' @param df_sp species table (lazy or collected) carrying `dataset_key`
#' @return character vector of dataset_key values, possibly empty
#' @export
sp_dataset_keys <- function(df_sp) {
  out <- tryCatch(
    df_sp |> dplyr::distinct(dataset_key) |> dplyr::collect(),
    error = function(e) {
      warning("sp_dataset_keys(): ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(out) || !nrow(out)) return(character(0))
  out$dataset_key
}

#' Legend title for a species layer, named by what the values actually are
#'
#' One unit: name it (`"Avg. count/10m2"`). Several: say so rather than picking a
#' label that is true of only some rows — the hexagon value averages across them,
#' and a mean over a mix of units is not a quantity. The sidebar note carries the
#' breakdown; see [sp_unit_summary()].
#'
#' @param u a [sp_unit_summary()] tibble
#' @return a character legend title
#' @export
sp_value_label <- function(u) {
  # "abundance" names the quantity all these rows represent, answering
  # "average of what?"; the "(mixed units)" caveat stays so a mean taken across
  # e.g. count/10m2 and count/100m3 is not read as a single physical rate --
  # the sidebar note still carries the per-unit breakdown.
  if (!nrow(u))        return("Avg. abundance")
  if (nrow(u) == 1L)   return(paste0("Avg. ", fmt_cpue_unit(u$cpue_unit[1])))
  "Avg. abundance (mixed units)"
}

#' Keep only the rows in one `cpue_unit`
#'
#' The single choke point for the "Standardized as" pick (see
#' \code{\link{cpue_unit_selector_ui}}): every map/plot consumer reads
#' \code{rx$df_sp} AFTER this runs, so filtering here -- once, upstream of
#' \code{\link{agg_sp_hex}}, \code{\link{prep_sp_poly}} and the time
#' series/scatter/depth-profile aggregations -- is what keeps all of them
#' from ever averaging across incompatible units again. Deliberately generic
#' on the exact `cpue_unit` string (not a fixed "10m2/100m3/raw" scheme):
#' which units exist is a property of the taxon/dataset, not something this
#' app hard-codes -- ichthyoplankton happens to have three, ZooDB has three
#' different ones (mgC/m2, count/m2, count/1000m3), a positive-only dataset
#' may have just one.
#'
#' @param df_sp dbplyr lazy table or data.frame carrying `cpue_unit`
#' @param unit the exact `cpue_unit` value to keep; NULL/NA is a no-op (used
#'   when nothing has been chosen yet, or only one unit exists so there is
#'   nothing to choose between)
#' @export
filter_cpue_unit <- function(df_sp, unit) {
  if (is.null(unit) || is.na(unit)) return(df_sp)
  df_sp |> dplyr::filter(cpue_unit == !!unit)
}

#' Format a `cpue_unit` string for display, with real unit exponents
#'
#' `cpue_unit` values are stored/compared as plain ASCII ("count/10m2",
#' "count/100m3", "mgC/m2") because \code{\link{filter_cpue_unit}} tests them
#' for equality and widgets use them as `value`s -- a unicode superscript
#' would just be one more way for two written forms of "the same" value to
#' fail to match. This is the one place that ASCII form becomes what a
#' person reads: m2 -> m², m3 -> m³, wherever the app NAMES a
#' `cpue_unit` (radio labels, the hexagon/polygon unit note, tooltips)
#' rather than storing or testing it.
#'
#' @param unit character vector of `cpue_unit` values (NA passes through)
#' @return character vector, same length, with m2/m3 superscripted
#' @export
fmt_cpue_unit <- function(unit) {
  unit <- gsub("m3", "m³", unit, fixed = TRUE)
  gsub("m2", "m²", unit, fixed = TRUE)
}

#' "Standardized as" control: one radio button per `cpue_unit` actually
#' present in the current selection
#'
#' Mirrors the CalCOFI Explorer's denominator picker (per 10 m^2 / per 100
#' m^3 / raw count for ichthyoplankton) but reads the choices from the data
#' rather than hard-coding them, so it degrades correctly for every other
#' dataset's own units (ZooDB's mgC/m2 + count/m2 + count/1000m3; a
#' single-unit dataset skips the control entirely -- there is nothing to
#' pick between, and \code{\link{sp_value_label}}/the poly note already say
#' what the one unit is).
#'
#' @param u a \code{\link{sp_unit_summary}} tibble (`cpue_unit`,
#'   `standardized`, `n`), already ordered by `n` descending
#' @param chosen the `cpue_unit` currently in effect (radio `selected`)
#' @return a `radioButtons` tag, or NULL when there is nothing to choose
#'   between (0 or 1 units present)
#' @export
cpue_unit_selector_ui <- function(u, chosen) {
  if (is.null(u) || nrow(u) <= 1L) return(NULL)
  n_tot <- sum(u$n)
  radioButtons(
    "sel_cpue_unit",
    tagList(
      "Standardized as",
      popover(
        bs_icon("question-circle"),
        HTML("This selection is published in more than one unit -- pick the
              one to map. The others are excluded from what's shown, not
              merged into it: a mean taken across units is not a quantity."))),
    choiceNames = lapply(seq_len(nrow(u)), function(i) tagList(
      tags$strong(fmt_cpue_unit(u$cpue_unit[i])),
      sprintf(" — %s obs (%.0f%%)",
              format(u$n[i], big.mark = ","), 100 * u$n[i] / n_tot),
      if (isTRUE(u$standardized[i]))
        tags$span(class = "cc-unit-std", " effort-standardized"))),
    choiceValues = u$cpue_unit,
    selected     = chosen,
    width        = "100%")
}

# ---- ?datasets= : the taxa-dataset selection carried in the URL --------------
# A calcofi.io dataset page links here and wants to open on one dataset (UI plan
# D-6, Decision 10). The rule is small but it has two ways to be wrong, so it
# lives here as a pure function with tests (tests/test_url_datasets.R) rather
# than inline in an observer that needs a running app and a release DB to reach.
#
#   · a key that names no dataset in THIS release is dropped — a link outlives
#     a release, and the app should open rather than error
#   · a list with none left over is NULL, not character(0): in this app an empty
#     dataset selection already means "all of them" (see the modal's own note),
#     so a stale link must not read as a deliberate empty filter
#
# @param param  the raw ?datasets= value ("a,b"), or NULL
# @param known  the dataset_keys this release actually carries
# @return the keys to select, in the URL's order, or NULL for "the URL asks for
#         nothing" — which is also what an all-unknown list returns
parse_datasets_param <- function(param, known) {
  if (is.null(param) || !length(param) || !nzchar(param[1])) return(NULL)
  want <- trimws(strsplit(param[1], ",")[[1]])
  want <- want[nzchar(want)]
  keep <- want[want %in% known]
  if (!length(keep)) return(NULL)
  unique(keep)
}

# ---- ?env= : open with an ENVIRONMENTAL dataset's first headline variable -----
# ?datasets= drives the taxa picker only, so a calcofi.io page for bottle, CTD,
# DIC, METS or picoplankton had nothing to deep-link with and said "pick the
# dataset there". ?env=<dataset_key> is that link: the app flicks Compare on
# and selects the dataset's LEADING variable -- the first entry of
# ENV_HEADLINE_TYPES (global.R, curated per dataset in the order people plot
# them: temperature for bottle, DIC for dic, SST for METS) that the release
# actually carries for it. Same two rules as parse_datasets_param(): an
# unknown key is ignored rather than erroring, and a dataset with no variable
# in this release is NULL.
#
# @param param     the raw ?env= value (one dataset_key), or NULL
# @param env_vars  data.frame with dataset_key + measurement_type (d_env_vars)
# @param headline  the curated headline types in lead order (ENV_HEADLINE_TYPES)
# @return the measurement_type to open on, or NULL
parse_env_param <- function(param, env_vars, headline) {
  if (is.null(param) || !length(param) || !nzchar(param[1])) return(NULL)
  key <- trimws(strsplit(param[1], ",")[[1]])[1]
  have <- env_vars$measurement_type[env_vars$dataset_key == key]
  if (!length(have)) return(NULL)
  lead <- headline[headline %in% have]
  # an uncurated dataset leads with whatever it has (ENV_HEADLINE_SHOWN's rule)
  if (length(lead)) lead[1] else sort(have)[1]
}
