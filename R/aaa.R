add_points <- function(polygon, n, connect = TRUE) {
  if (connect) {
    x <- diff(c(polygon$x, polygon$x[1]))
    y <- diff(c(polygon$y, polygon$y[1]))
  } else {
    x <- diff(polygon$x)
    y <- diff(polygon$y)
  }
  l <- sqrt(x*x + y*y)
  longest <- order(l, decreasing = TRUE)
  n_splits <- find_splits(l[longest], n)
  splits <- n_splits[match(seq_along(l), longest)]
  if (!connect) splits <- c(splits, 0)
  new_points <- insert_points(polygon$x, polygon$y, splits, n)
  polygon <- polygon[rep(seq_len(nrow(polygon)), splits + 1), ]
  polygon$x <- new_points$x
  polygon$y <- new_points$y
  polygon
}

match_shapes <- function(from, to, new_id, id, enter, exit, min_n, closed = TRUE) {
  if (is.null(from)) {
    if (is.null(enter)) {
      from <- list(to[[1]][0, , drop = FALSE])
      to <- from
    } else {
      from <- lapply(to, enter)
    }
  } else if (is.null(to)) {
    if (is.null(exit)) {
      to <- list(from[[1]][0, , drop = FALSE])
      from <- to
    } else {
      to <- lapply(from, exit)
    }
  } else {
    matched <- if (closed) match_polygon(from, to, min_n) else match_path(from, to, min_n)
    from <- matched$from
    to <- matched$to
  }
  from <- do.call(rbind, lapply(from, function(x) x[seq_len(nrow(x) + 1), ]))
  to <- do.call(rbind, lapply(to, function(x) x[seq_len(nrow(x) + 1), ]))
  from <- from[-nrow(from), ]
  to <- to[-nrow(to), ]
  if (!is.null(id) && nrow(from) > 0) {
    from[[id]] <- new_id
    to[[id]] <- new_id
  }
  list(from = from, to = to)
}
match_polygon <- function(from, to, min_n) {
  main_from <- from[[1]]
  main_to <- to[[1]]
  n_points <- max(c(min_n, nrow(main_from), nrow(main_to)))
  if (nrow(main_from) < n_points) main_from <- add_points(main_from, n_points - nrow(main_from), connect = TRUE)
  if (nrow(main_to) < n_points) main_to <- add_points(main_to, n_points - nrow(main_to), connect = TRUE)
  offset <- rotate(main_to$x, main_to$y, main_from$x, main_from$y)
  to_end <- seq_len(nrow(main_to)) < offset
  main_to <- rbind(main_to[!to_end, , drop = FALSE],
                   main_to[to_end, , drop = FALSE])
  from[[1]] <- main_from
  to[[1]] <- main_to
  if (length(from) > 1 || length(to) > 1) {
    holes <- align_holes(from, to)
    from <- c(from[1], holes$from)
    to <- c(to[1], holes$to)
  }
  list(from = from, to = to)
}
match_path <- function(from, to, min_n) {
  paths <- Map(function(from, to) {
    n_points <- max(c(min_n, nrow(from), nrow(to)))
    if (nrow(from) < n_points) from <- add_points(from, n_points - nrow(from), connect = FALSE)
    if (nrow(to) < n_points) to <- add_points(to, n_points - nrow(to), connect = FALSE)
    list(from = from, to = to)
  }, from = from, to = to)
  list(from = lapply(paths, `[[`, 'from'), to = lapply(paths, `[[`, 'to'))
}
