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
  stopifnot(is.data.frame(.data))
  from <- .get_last_frame(.data)
  from$.phase <- rep('raw', nrow(from))
  to$.phase <- rep('raw', nrow(to))
  to$.id <- rep(NA_integer_, nrow(to))
  id <- enquo(id)
  if (.has_frames(.data)) nframes <- nframes + 1

  sf_columns <- vapply(from, inherits, logical(1), 'sfc')
  if (!any(sf_columns)) return(tween_state(.data, to, ease, nframes, !!id, enter, exit))
  full_set <- .complete_states(from, to, id, enter, exit, .max_id(.data))
  to$.id <- full_set$orig_to
  sf_from <- full_set$from[, sf_columns, drop = FALSE]
  sf_to <- full_set$to[, sf_columns, drop = FALSE]
  full_set$from[, sf_columns] <- rep(1L, nrow(full_set$from))
  full_set$to[, sf_columns] <- rep(1L, nrow(full_set$to))
  morph <- tween_state(as.data.frame(full_set$from),as.data.frame(full_set$to), ease, nframes, id = NULL, enter, exit)
  morph[which(sf_columns)] <- tween_sf_col(sf_from, sf_to, rep(ease, length.out = ncol(from))[sf_columns], nframes)
  morph <- morph[!morph$.frame %in% c(1, nframes), , drop = FALSE]
  morph <- rbind(
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
    if (length(from[[i]]) == 0 && length(to[[i]]) == 0) return(st_sfc(list()))
    from_type <- as.character(unlist(lapply(from[[i]], st_geometry_type)))
    if (!all(from_type %in% supp_types)) stop('Unsupported geometry type', call. = FALSE)
    to_type <- as.character(unlist(lapply(to[[i]], st_geometry_type)))
    if (!all(to_type %in% supp_types)) stop('Unsupported geometry type', call. = FALSE)
    if (!all(sub('MULTI', '', from_type) == sub('MULTI', '', to_type))) stop('Incompatible geometry types', call. = FALSE)
    from <- unpack_sf(from[[i]], from_type)
    to <- unpack_sf(to[[i]], to_type)
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
    from <- do.call(rbind, from)
    to <- do.call(rbind, to)
    from$sf_id <- id
    to$sf_id <- id
    tweened <- tween_state(from, to, ease, nframes)
    st_sfc(repack_sf(tweened, from_type, nframes))
  })
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
    from <- rbind(from, from[to_add, , drop = FALSE])
  } else if (nrow(dist) > ncol(dist)) {
    closest <- apply(dist, 2, mean)
    to_add <- rep(order(closest), length.out = nrow(dist) - ncol(dist))
    dist <- cbind(dist, dist[, to_add, drop = FALSE])
    to <- rbind(to, to[to_add, , drop = FALSE])
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
  from <- do.call(rbind, from)
  to <- do.call(rbind, lapply(polygons, `[[`, 'to'))
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
  from <- do.call(rbind, from)
  to <- do.call(rbind, lapply(polygons, `[[`, 'to'))
  from$id <- id
  to$id <- id
  list(from = from, to = to)
}
supp_types <- c('POINT', 'LINESTRING', 'POLYGON', 'MULTIPOINT', 'MULTILINESTRING', 'MULTIPOLYGON')
