#' Transition between data.frames containing sfc columns
#'
#' This function is equivalent to [tweenr::tween_state()] except that it
#' understands [`sf::sfc`] columns, as defined by the `sf` package. An `sfc`
#' column is a column containing simple features and can this hold both points,
#' lines polygons and more. `tween_sf` currently has support for (multi)point,
#' (multi)linestring, and (multi)polygon types and requires that the transition
#' is between compatible types (points-to-points, linestring-to-linestring,
#' polygon-to-polygon). For (multi)linestring and (multi)polygon, the behavior
#' is similar to [tween_path()] and [tween_polygon()] respectively, with each
#' feature beeing run through the respective function with `match = FALSE`. For
#' (multi)points it behaves more or less like [tweenr::tween_state()] except
#' additional points are added as needed to make the to stages contain the same
#' number of points. Points are added on top of existing points so it appears as
#' if the points are divided into more.
#'
#' @inheritParams tweenr::tween_state
#'
#' @return A data.frame containing intermediary states
#'
#' @importFrom tweenr .complete_states .max_id .has_frames
#' @importFrom rlang enquo
#' @importFrom sf st_crs st_transform
#' @export
#'
#' @examples
#' library(magrittr)
#' star_hole <- poly_star_hole(st = TRUE)
#' circles <- poly_circles(st = TRUE)
#' spiral <- path_spiral(st = TRUE)
#' waves <- path_waves(st = TRUE)
#' random <- point_random(st = TRUE)
#' grid <- point_grid(st = TRUE)
#' df1 <- data.frame(geo = sf::st_sfc(star_hole, spiral, random))
#' df2 <- data.frame(geo = sf::st_sfc(circles, waves, grid))
#'
#' tween_sf(df1, df2, 'linear', 30)
#'
tween_sf <- function(.data, to, ease, nframes, id = NULL, enter = NULL, exit = NULL) {
  if (!is.data.frame(.data)) {
    stop('`.data` must be a `data.frame`', call. = FALSE)
  }
  sf_columns <- vapply(.data, inherits, logical(1), 'sfc')
  if (!any(sf_columns)) return(tween_state(.data, to, ease, nframes, !!id, enter, exit))

  for (col in which(sf_columns)) {
    from_crs <- st_crs(.data[[col]])
    to_crs <- st_crs(to[[col]])
    if (is.na(from_crs)) {
      if (!is.na(to_crs)) {
        st_crs(.data[[col]]) <- to_crs
      }
      next
    }
    if (is.na(to_crs)) {
      st_crs(to[[col]]) <- from_crs
    } else {
      to[[col]] <- st_transform(to[[col]], from_crs)
    }
  }

  from <- .get_last_frame(.data)
  from$.phase <- rep('raw', nrow(from))
  to$.phase <- rep('raw', nrow(to))
  to$.id <- rep(NA_integer_, nrow(to))
  id <- enquo(id)
  if (.has_frames(.data)) nframes <- nframes + 1

  # recalc to make sure .phase and .id columns are included
  sf_columns <- vapply(from, inherits, logical(1), 'sfc')

  full_set <- .complete_states(from, to, id, enter, exit, .max_id(.data))
  to$.id <- full_set$orig_to
  sf_from <- full_set$from[, sf_columns, drop = FALSE]
  sf_to <- full_set$to[, sf_columns, drop = FALSE]
  full_set$from[, sf_columns] <- rep(1L, nrow(full_set$from))
  full_set$to[, sf_columns] <- rep(1L, nrow(full_set$to))
  morph <- tween_state(as.data.frame(full_set$from), as.data.frame(full_set$to), ease, nframes, id = NULL, enter, exit)
  morph[which(sf_columns)] <- tween_sf_col(sf_from, sf_to, rep(ease, length.out = ncol(from))[sf_columns], nframes)
  morph <- morph[!morph$.frame %in% c(1, nframes), , drop = FALSE]
  if (nrow(morph) == 0) morph <- NULL
  morph <- vec_rbind(
    if (nframes > 1) cbind(as.data.frame(from), .frame = rep(1, nrow(from))) else NULL,
    morph,
    cbind(as.data.frame(to), .frame = rep(nframes, nrow(to)))
  )
  .with_prior_frames(.data, morph, nframes)
}
#' @importFrom sf st_geometry_type st_sfc
#' @importFrom tweenr tween_state
tween_sf_col <- function(from, to, ease, nframes) {
  lapply(seq_along(from), function(i) {
    aligned <- align_sf(from[[i]], to[[i]])
    if (is.null(aligned)) return(st_sfc(list()))
    tweened <- tween_state(aligned$from, aligned$to, ease, nframes)
    tweened$id <- as.integer(tweened$id)
    tweened$sf_id <- as.integer(tweened$sf_id)
    tweened$.frame <- as.integer(tweened$.frame)
    st_sfc(repack_sf(tweened, aligned$type, as.integer(nframes)), crs = st_crs(from[[i]]))
  })
}

align_sf <- function(from, to) {
  if (length(from) == 0 && length(to) == 0) return(NULL)
  from_type <- as.character(unlist(lapply(from, st_geometry_type)))
  if (!all(from_type %in% supp_types)) stop('Unsupported geometry type', call. = FALSE)
  to_type <- as.character(unlist(lapply(to, st_geometry_type)))
  if (!all(to_type %in% supp_types)) stop('Unsupported geometry type', call. = FALSE)
  if (!all(sub('MULTI', '', from_type) == sub('MULTI', '', to_type))) stop('Incompatible geometry types', call. = FALSE)
  from <- unpack_sf(from, from_type)
  to <- unpack_sf(to, to_type)
  aligned <- Map(function(from, to, type) {
    switch(
      type,
      POINT =,
      MULTIPOINT = align_sf_point(from, to),
      LINESTRING =,
      MULTILINESTRING = align_sf_path(from, to, 50),
      POLYGON =,
      MULTIPOLYGON = align_sf_polygon(from, to, 50)
    )
  }, from = from, to = to, type = from_type)
  from <- lapply(aligned, `[[`, 'from')
  to <- lapply(aligned, `[[`, 'to')
  id <- rep(seq_along(from), vapply(from, nrow, integer(1)))
  from <- vec_rbind(!!!from)
  to <- vec_rbind(!!!to)
  from$sf_id <- id
  to$sf_id <- id
  list(from = from, to = to, type = from_type)
}
#' @importFrom sf st_point st_distance
#' @importFrom lpSolve lp.assign
align_sf_point <- function(from, to) {
  f_st <- lapply(seq_len(nrow(from)), function(i) st_point(c(from$x[i], from$y[i])))
  t_st <- lapply(seq_len(nrow(to)), function(i) st_point(c(to$x[i], to$y[i])))
  dist <- st_distance(st_sfc(f_st), st_sfc(t_st))
  if (nrow(dist) < ncol(dist)) {
    closest <- apply(dist, 1, mean)
    to_add <- rep(order(closest), length.out = ncol(dist) - nrow(dist))
    dist <- rbind(dist, dist[to_add, , drop = FALSE])
    from <- vec_rbind(from, from[to_add, , drop = FALSE])
  } else if (nrow(dist) > ncol(dist)) {
    closest <- apply(dist, 2, mean)
    to_add <- rep(order(closest), length.out = nrow(dist) - ncol(dist))
    dist <- cbind(dist, dist[, to_add, drop = FALSE])
    to <- vec_rbind(to, to[to_add, , drop = FALSE])
  }
  match_points <- lp.assign(dist)
  if (match_points$status == 0) {
    to <- to[apply(round(match_points$solution) == 1, 1, which), , drop = FALSE]
  }
  from$id <- seq_len(nrow(from))
  to$id <- from$id
  list(from = from, to = to)
}
align_sf_path <- function(from, to, min_n) {
  prepped <- prep_align_paths(from, to)
  polygons <- mapply(
    match_shapes,
    from = prepped$from,
    to = prepped$to,
    MoreArgs = list(
      enter = NULL, exit = NULL, min_n = min_n, closed = FALSE
    ),
    SIMPLIFY = FALSE
  )
  from <- lapply(polygons, `[[`, 'from')
  id <- rep(seq_along(from), vapply(from, nrow, integer(1)))
  from <- vec_rbind(!!!from)
  to <- vec_rbind(!!!lapply(polygons, `[[`, 'to'))
  from$id <- id
  to$id <- id
  list(from = from, to = to)
}
align_sf_polygon <- function(from ,to, min_n) {
  from <- as_clockwise(from)
  to <- as_clockwise(to)
  prepped <- prep_align_polygons(from, to)
  polygons <- mapply(
    match_shapes,
    from = prepped$from,
    to = prepped$to,
    MoreArgs = list(
      enter = NULL, exit = NULL, min_n = min_n, closed = TRUE
    ),
    SIMPLIFY = FALSE
  )
  from <- lapply(polygons, `[[`, 'from')
  id <- rep(seq_along(from), vapply(from, nrow, integer(1)))
  from <- vec_rbind(!!!from)
  to <- vec_rbind(!!!lapply(polygons, `[[`, 'to'))
  from$id <- id
  to$id <- id
  list(from = from, to = to)
}
supp_types <- c('POINT', 'LINESTRING', 'POLYGON', 'MULTIPOINT', 'MULTILINESTRING', 'MULTIPOLYGON')
