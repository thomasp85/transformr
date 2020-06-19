#' Transition between polygon data.frames
#'
#' This function is equivalent to [tweenr::tween_state()] except that data is
#' interpeted as encoding polygons. Data is expected to have an `x` and `y`
#' column encoding the location of corners in the polygon.
#'
#' @inheritParams tweenr::tween_state
#' @param match Should polygons be matched by id? If `FALSE` then polygons will
#' be matched by shortest distance and if any state has more polygons than the
#' other, the other states polygons will be chopped up so the numbers match.
#'
#' @return A data.frame containing intermediary states
#'
#' @section Aligning polygons:
#' `transformr` performs a lot of work to try to ensure the transition between
#' different shapes are as smooth and direct as possible. The first operation is
#' to ensure that the two end states of the polygon are both drawn clockwise, so
#' that the transition will not contain an inversion. Second, we need to make
#' sure that each end state is drawn with the same number of points. If not, the
#' less detailed polygon will get points inserted at the longest edges so that
#' the number is even between the two states. Third, we rotate the last state so
#' as to minimize the cumulative distance between all point pairs, thus ensuring
#' that the transition will involve a minimum of rotation.
#'
#' @section Cutting polygons:
#' If the transition involves changing the number of polygons, there are two
#' strategies: Making polygons appear/disappear to even out the number, or
#' cutting up the polygons in the state with the fewest in order to create the
#' same number of polygons for the transition. In the latter case, a choice have
#' to be made with regards to which polygons to cut, into how many, and where to
#' cut it. `transformr` will distribute the number of cuts among candidate
#' polygons based on their relative area, ensuring that it is not necessarily
#' the largest polygon that gets all the cuts, but that divisions are
#' distributed as fairly as possible. For deciding on where to cut the polygons
#' they are triangulated and the triangles are then reassembled into the number
#' of pieces needed by always adding to the smallest piece.
#'
#' @section Polygon with holes:
#' `transformr` support polygons with any number of holes. Holes are encoded by
#' adding an `NA` row to the main enclosing polygon and appending the hole after
#' that. Multiple holes are likewise added by simply separating them with `NA`
#' rows. A hole might get cut up and disappear during transition if the polygon
#' needs to be divided. When transitioning between polygons with holes the holes
#' are matched by position to minimize the travel distance. If there is a
#' mismatch between the number of holes in each end state then new zero-area
#' holes are inserted in the centroid of the polygon with the fewest to even out
#' the number.
#'
#' @export
#' @importFrom tweenr tween_state .get_last_frame .with_prior_frames .has_frames
#' @importFrom rlang enquo quo_is_null quo eval_tidy %||%
#'
#' @examples
#' library(magrittr)
#' star <- poly_star_hole()
#' circle <- poly_circle()
#' circles <- poly_circles()
#'
#' tween_polygon(circle, star, 'cubic-in-out', 20) %>%
#'   tween_polygon(circles, 'cubic-in-out', 20)
#'
tween_polygon <- function(.data, to, ease, nframes, id = NULL, enter = NULL, exit = NULL, match = TRUE) {
  stopifnot(is.data.frame(.data))
  from <- .get_last_frame(.data)
  id <- enquo(id)
  from$.id <- eval_tidy(id, from) %||% rep(1L, nrow(from))
  from$.phase <- rep('raw', nrow(from))
  to$.id <- eval_tidy(id, to) %||% rep(1L, nrow(to))
  to$.phase <- rep('raw', nrow(to))
  if (.has_frames(.data)) nframes <- nframes + 1
  polygons <- align_polygons(from, to, enter = enter, exit = exit, match = match)
  polygons <- tween_state(polygons$from, polygons$to, ease = ease, nframes = nframes)
  polygons <- polygons[!polygons$.frame %in% c(1, nframes), , drop = FALSE]
  morph <- rbind(
    if (nframes > 1) cbind(from, .frame = rep(1, nrow(from))) else NULL,
    polygons,
    cbind(to, .frame = rep(nframes, nrow(to)))
  )
  .with_prior_frames(.data, morph, nframes)
}

align_polygons <- function(from, to, min_n = 50, enter, exit, match = TRUE) {
  from <- lapply(make_polygons(from), function(x) as_clockwise(list(x))[[1]])
  to <- lapply(make_polygons(to), function(x) as_clockwise(list(x))[[1]])
  polygons <- if (match) {
    prep_match_polygons(from, to)
  } else {
    prep_align_polygons(from, to)
  }
  polygons <- mapply(
    match_shapes,
    from = polygons$from,
    to = polygons$to,
    MoreArgs = list(
      enter = enter, exit = exit, min_n = min_n, closed = TRUE
    ),
    SIMPLIFY = FALSE
  )
  from <- lapply(polygons, `[[`, 'from')
  id <- rep(seq_along(from), vapply(from, nrow, integer(1)))
  from <- do.call(rbind, from)
  to <- do.call(rbind, lapply(polygons, `[[`, 'to'))
  from$.id <- id
  to$.id <- id
  common_id(from = from, to = to)
}

prep_match_polygons <- function(from, to) {
  all_ids <- as.character(union(names(from), names(to)))
  from_all <- structure(rep(list(NULL), length(all_ids)), names = all_ids)
  to_all <- from_all
  from_all[names(from)] <- from
  to_all[names(to)] <- to
  list(from = from_all, to = to_all)
}
#' @importFrom sf st_sfc st_area st_distance st_centroid
#' @importFrom lpSolve lp.assign
prep_align_polygons <- function(from, to) {
  from_st <- st_sfc(lapply(from, to_polygon))
  to_st <- st_sfc(lapply(to, to_polygon))
  if (length(from) < length(to)) {
    area <- st_area(from_st)
    from <- divide_polygons(from, area, length(to) - length(from))
    from_st <- st_sfc(lapply(from, to_polygon))
  } else if (length(to) < length(from)) {
    area <- st_area(to_st)
    to <- divide_polygons(to, area, length(from) - length(to))
    to_st <- st_sfc(lapply(to, to_polygon))
  }
  from_st <- st_normalize(from_st)
  to_st <- st_normalize(to_st)
  if (length(from_st) != 1 && length(to_st) != 1) {
    distance <- st_distance(st_centroid(from_st), st_centroid(to_st))
    area_diff <- abs(outer(st_area(from_st), st_area(to_st), `-`))
    if (max(area_diff) > 0) distance <- distance * (1 + area_diff / max(area_diff))
    match_poly <- lp.assign(distance)
    if (match_poly$status == 0) {
      to <- to[apply(round(match_poly$solution) == 1, 1, which)]
    }
  }
  names(from) <- as.character(seq_along(from))
  names(to) <- as.character(seq_along(to))
  list(from = from, to = to)
}
make_polygons <- function(x) {
  lapply(split(x, x$.id), function(xx) {
    holes <- which(is.na(xx$x))
    if (length(holes) == 0) {
      xx <- list(xx)
    } else {
      holes <- c(holes, nrow(xx) + 1)
      xx <- xx[-holes, , drop = FALSE]
      xx <- split(xx, rep(seq_along(holes), diff(c(0, holes)) - 1))
    }
    lapply(xx, function(ring) {
      nr <- nrow(ring)
      if (nr > 0 && nr < 4) {
        ring[c(seq_len(nr), rep(1, 4 - nr)), ]
      } else {
        ring
      }
    })
  })
}
align_holes <- function(from, to) {
  from_st <- st_sfc(lapply(from, function(x) to_polygon(list(x))))
  to_st <- st_sfc(lapply(to, function(x) to_polygon(list(x))))
  from_centroids <- st_centroid(from_st)
  to_centroids <- st_centroid(to_st)
  n_from <- length(from_st)
  n_to <- length(to_st)
  if (n_from < n_to) {
    from_centroids <- from_centroids[c(seq_len(n_from), rep(1, n_to - n_from))]
  }
  if (n_from > n_to) {
    to_centroids <- to_centroids[c(seq_len(n_to), rep(1, n_from - n_to))]
  }
  distance <- st_distance(from_centroids[-1], to_centroids[-1])
  match_poly <- lp.assign(distance)
  from <- from[seq_along(from_centroids)]
  if (match_poly$status == 0) {
    to <- to[c(1, apply(round(match_poly$solution) == 1, 1, which) + 1)]
  } else {
    to <- to[seq_along(to_centroids)]
  }
  holes <- Map(function(from, to) {
    if (is.null(from)) {
      from <- to
      from$x <- from_centroids[[1]][1]
      from$y <- from_centroids[[1]][2]
    } else if (is.null(to)) {
      to <- from
      to$x <- to_centroids[[1]][1]
      to$y <- to_centroids[[1]][2]
    } else {
      n_points <- max(nrow(from), nrow(to))
      if (nrow(from) < n_points) from <- add_points(from, n_points - nrow(from))
      if (nrow(to) < n_points) main_to <- add_points(to, n_points - nrow(to))
      offset <- rotate(to$x, to$y, from$x, from$y)
      to_end <- seq_len(nrow(to)) < offset
      to <- rbind(to[!to_end, , drop = FALSE],
                  to[to_end, , drop = FALSE])
    }
    list(from = from, to = to)
  }, from = from[-1], to = to[-1])
  list(from = lapply(holes, `[[`, 'from'), to = lapply(holes, `[[`, 'to'))
}
as_clockwise <- function(polygon) {
  lapply(polygon, function(poly) {
    lapply(poly, function(p) {
      x <- c(p$x, p$x[1])
      y <- c(p$y, p$y[1])
      area <- (x[-1] - x[-length(x)]) * (y[-1] + y[-length(y)])
      if (sum(area) < 0) p <- p[rev(seq_len(nrow(p))), ]
      p
    })
  })
}
#' @importFrom sf st_polygon st_sample st_combine st_intersection st_cast st_voronoi st_as_sfc st_bbox st_sfc st_area st_touches st_union
split_polygon <- function(polygon, n) {
  poly <- st_polygon(lapply(polygon, function(p) cbind(p$x, p$y)[c(seq_len(nrow(p)), 1), ]))
  rand <- st_sample(poly, n)
  while (length(rand) < n) {
    rand <- c(rand, st_sample(poly, n))
  }
  rand <- st_combine(rand[sample(length(rand), n)])
  tiles <- st_intersection(st_cast(st_voronoi(rand, st_as_sfc(st_bbox(poly)))), poly)
  tiles <- unlist(lapply(tiles, function(x) {
    if (inherits(x, 'MULTIPOLYGON')) {
      lapply(x, st_polygon)
    } else {
      list(x)
    }
  }), recursive = FALSE)
  while (length(tiles) > n) {
    st_tiles <- st_sfc(tiles)
    smallest <- which.min(st_area(st_tiles))
    touches <- st_touches(st_tiles[smallest], st_tiles)
    neighbor <- touches[[1]][1]
    combined <- st_union(tiles[[smallest]], tiles[[neighbor]])
    tiles <- c(tiles[-c(smallest, neighbor)], list(combined))
  }
  all_points <- do.call(rbind, polygon)
  id <- paste0(all_points$x, '-', all_points$y)
  lapply(tiles, function(t) {
    x <- t[[1]][-nrow(t[[1]]), 1]
    y <- t[[1]][-nrow(t[[1]]), 2]
    orig_points <- match(paste0(x, '-' , y), id)
    if (all(is.na(orig_points))) {
      tile <- all_points[rep(1, length(orig_points)), , drop = FALSE]
    } else {
      orig_points <- orig_points[fill_down(is.na(orig_points))]
      tile <- all_points[orig_points, , drop = FALSE]
    }
    tile$x <- x
    tile$y <- y
    list(tile)
  })
}
divide_polygons <- function(polygons, area, n) {
  biggest <- order(area, decreasing = TRUE)
  n_splits <- find_splits(area[biggest], n)
  splits <- n_splits[match(seq_along(area), biggest)] + 1
  unlist(Map(split_polygon, polygon = polygons, n = splits), recursive = FALSE)
}
#' @importFrom sf st_polygon
to_polygon <- function(points) {
  points <- points[[1]]
  if (is.data.frame(points)) {
    points <- as.matrix(points[, c('x', 'y')])
  } else {
    points <- points[, 1:2]
  }
  if (anyNA(points)) {
    first_na <- which(is.na(points[,1]))
    points <- points[seq_len(first_na-1), , drop = FALSE]
  }
  st_polygon(list(rbind(points, points[1,])))
}
#' Normalise a geometry to fit inside a unit square
#'
#' This is a small helper function that will take an sf geometry and fit it
#' inside the unit square (a square centered on 0 and ranging from -1 to 1 in
#' both dimensions). The function will retain the aspect ratio of the geometry
#' and simply scale it down until it fits.
#'
#' @param st An sf geometry such as `sf`, `sfc`, or `sfg`
#'
#' @return An object of the same type as `st`
#'
#' @importFrom sf st_bbox st_geometry st_geometry<-
#' @export
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' st_bbox(nc)
#'
#' nc_norm <- st_normalize(nc)
#' st_bbox(nc_norm)
#'
st_normalize <- function(st) {
  if (inherits(st, 'sf')) {
    st_geometry(st) <- st_normalize(st_geometry(st))
    st
  } else {
    bbox <- st_bbox(st)
    x <- bbox$xmin + (bbox$xmax - bbox$xmin) / 2
    y <- bbox$ymin + (bbox$ymax - bbox$ymin) / 2
    st <- st - c(x, y)
    st / (0.5 * max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin))
  }
}
