#' Transition between path data frames
#'
#' This function is equivalent to [tweenr::tween_state()] but expects the data
#' to have an `x` and `y` column and encode paths.
#'
#' @inheritParams tween_polygon
#'
#' @return A data.frame containing intermediary states
#'
#' @section Aligning paths:
#' There is less work required to align paths than there is to align polygons,
#' simply because no rotation is possible/required, and the notion of clockwise
#' winding order is not meaningful in the scope of paths. Still, paths need to
#' be matched and the number of points in each pair of matched paths must be
#' equal. Paths are matched based on relative length rather than on position and
#' seek to minimize the change in length during transition. This is chosen from
#' the point of view that huge elongation or contraction are much more
#' distracting than longer travel distances.
#'
#' @section Cutting paths:
#' If the number of paths to transition between is not even, some of the paths
#' need to be cut in order to succesfully match the paths. The cuts are
#' distributed based on the same algorithm that distributes cuts in polygons and
#' seek to cut the lines into as even-length pieces as possible.
#'
#' @section Multipaths:
#' It is possible to encode multiple paths with the same id be separating them
#' with a `NA` row, much in the same way as holes are encoded in polygons. If
#' paths are not matched based on id (`match = FALSE`) then multipaths will
#' simply be split into separate paths. On the other hand, if paths are matched
#' by id all paths within a multipath will transition into the (multi)path that
#' has the same id in the other state.
#'
#' @importFrom rlang enquo quo_is_null eval_tidy %||%
#' @importFrom tweenr .has_frames tween_state
#' @export
tween_path <- function(.data, to, ease, nframes, id = NULL, enter = NULL, exit = NULL, match = TRUE) {
  stopifnot(is.data.frame(.data))
  from <- .get_last_frame(.data)
  id <- enquo(id)
  from$.id <- eval_tidy(id, from) %||% rep(1L, nrow(from))
  from$.phase <- rep('raw', nrow(from))
  to$.id <- eval_tidy(id, to) %||% rep(1L, nrow(to))
  to$.phase <- rep('raw', nrow(to))
  if (.has_frames(.data)) nframes <- nframes + 1
  paths <- align_paths(from, to, enter = enter, exit = exit, match = match)
  paths <- tween_state(paths$from, paths$to, ease = ease, nframes = nframes)
  paths <- paths[!paths$.frame %in% c(1, nframes), , drop = FALSE]
  morph <- rbind(
    if (nframes > 1) cbind(from, .frame = rep(1, nrow(from))) else NULL,
    paths,
    cbind(to, .frame = rep(nframes, nrow(to)))
  )
  .with_prior_frames(.data, morph, nframes)
}

align_paths <- function(from, to, min_n = 50, enter, exit, match) {
  from <- make_paths(from)
  to <- make_paths(to)
  paths <- if (match) {
    prep_match_paths(from, to)
  } else {
    prep_align_paths(from, to)
  }
  paths <- mapply(
    match_shapes,
    from = paths$from,
    to = paths$to,
    MoreArgs = list(
      enter = enter, exit = exit, min_n = min_n, closed = FALSE
    ),
    SIMPLIFY = FALSE
  )
  from <- lapply(paths, `[[`, 'from')
  id <- rep(seq_along(from), vapply(from, nrow, integer(1)))
  from <- do.call(rbind, from)
  to <- do.call(rbind, lapply(paths, `[[`, 'to'))
  from$.id <- id
  to$.id <- id
  common_id(from = from, to = to)
}
prep_match_paths <- function(from, to) {
  all_ids <- as.character(union(names(from), names(to)))
  from_all <- structure(rep(list(NULL), length(all_ids)), names = all_ids)
  to_all <- from_all
  from_all[names(from)] <- from
  to_all[names(to)] <- to
  paths <- Map(function(from, to) {
    if (is.null(from) || is.null(to) || (length(from) == 1 && length(to) == 1)) {
      list(from = from, to = to)
    } else {
      divide_paths(from, to)
    }
  }, from = from_all, to = to_all)
  from_all <- lapply(paths, `[[`, 'from')
  to_all <- lapply(paths, `[[`, 'to')
  list(from = from_all, to = to_all)
}
prep_align_paths <- function(from, to) {
  from <- unlist(from, recursive = FALSE)
  to <- unlist(to, recursive = FALSE)
  paths <- divide_paths(from, to)
  from <- paths$from
  to <- paths$to
  names(from) <- as.character(seq_along(from))
  names(to) <- as.character(seq_along(to))
  list(from = lapply(from, list), to = lapply(to, list))
}
make_paths <- function(x) {
  lapply(split(x, x$.id), to_path)
}
to_path <- function(path) {
  gaps <- which(is.na(path$x))
  if (length(gaps) == 0) {
    path <- list(path)
  } else {
    gaps <- c(gaps, nrow(path) + 1)
    path <- path[-gaps, , drop = FALSE]
    path <- split(path, rep(seq_along(gaps), diff(c(0, gaps)) - 1))
  }
  lapply(path, function(line) {
    if (nrow(line) == 1) {
      line[c(1,1), ]
    } else {
      line
    }
  })
}
#' @importFrom sf st_multilinestring st_linestring
to_line <- function(path) {
  if (is.data.frame(path)) {
    if (nrow(path) == 1) {
      st_linestring(cbind(path$x[c(1, 1)], path$y[c(1, 1)]))
    } else {
      st_linestring(cbind(path$x, path$y))
    }
  } else {
    st_multilinestring(lapply(path, function(x) {
      if (nrow(x) == 1) {
        cbind(x$x[c(1, 1)], x$y[c(1, 1)])
      } else {
        cbind(x$x, x$y)
      }
    }))
  }
}
cut_lines <- function(lines, lengths, n) {
  longest <- order(lengths, decreasing = TRUE)
  n_splits <- find_splits(lengths[longest], n)
  splits <- n_splits[match(seq_along(lengths), longest)] + 1
  lines <- Map(function(path, n) {
    if (n == 1) return(list(path))
    if (n > nrow(path) - 1) path <- add_points(path, n - nrow(path) + 1, FALSE)
    paths <- split(seq_len(nrow(path)), sort(seq_len(nrow(path))%%n))
    lapply(paths, function(i) {
      if (i[1] != 1) i <- c(i[1] - 1, i)
      path[i, , drop = FALSE]
    })
  }, path = lines, n = splits)
  unlist(lines, recursive = FALSE)
}
#' @importFrom sf st_length
#' @importFrom lpSolve lp.assign
divide_paths <- function(from, to) {
  from_st <- st_sfc(lapply(from, to_line))
  to_st <- st_sfc(lapply(to, to_line))
  if (length(from) < length(to)) {
    l <- st_length(from_st)
    from <- cut_lines(from, l, length(to) - length(from))
    from_st <- st_sfc(lapply(from, to_line))
  } else if (length(to) < length(from)) {
    l <- st_length(to_st)
    to <- cut_lines(to, l, length(from) - length(to))
    to_st <- st_sfc(lapply(to, to_line))
  }
  from_st <- st_normalize(from_st)
  to_st <- st_normalize(to_st)
  if (length(from_st) != 1 && length(to_st) != 1) {
    distance <- st_distance(st_centroid(from_st), st_centroid(to_st))
    length_diff <- abs(outer(st_length(from_st), st_length(to_st), `-`))
    if (max(length_diff) > 0) distance <- distance * (1 + length_diff / max(length_diff))
    match_poly <- lp.assign(distance)
    if (match_poly$status == 0) {
      to <- to[apply(round(match_poly$solution) == 1, 1, which)]
    }
  }
  list(from = from, to = to)
}
