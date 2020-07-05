add_points <- function(polygon, n, connect = TRUE) {
  if (n < 1) {
    return(polygon)
  }
  if (nrow(polygon) == 1) {
    return(polygon[rep(1, n + 1), , drop = FALSE])
  }
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
  new_polygon <- polygon[rep(seq_len(nrow(polygon)), splits + 1), ]
  new_polygon$x <- new_points$x
  new_polygon$y <- new_points$y
  if ('ymin' %in% names(polygon) && length(unique(polygon$ymin)) > 1) {
    new_points <- insert_points(polygon$x, polygon$ymin, splits, n)
    new_polygon$ymin <- new_points$y
  }
  if ('ymax' %in% names(polygon) && length(unique(polygon$ymax)) > 1) {
    new_points <- insert_points(polygon$x, polygon$ymax, splits, n)
    new_polygon$ymax <- new_points$y
  }
  if ('xmin' %in% names(polygon) && length(unique(polygon$xmin)) > 1) {
    new_points <- insert_points(polygon$xmin, polygon$y, splits, n)
    new_polygon$xmin <- new_points$x
  }
  if ('xmax' %in% names(polygon) && length(unique(polygon$xmax)) > 1) {
    new_points <- insert_points(polygon$xmax, polygon$y, splits, n)
    new_polygon$xmax <- new_points$x
  }
  new_polygon
}

match_shapes <- function(from, to, enter, exit, min_n, closed = TRUE) {
  if (is.null(from)) {
    if (is.null(enter)) {
      from <- list(to[[1]][0, , drop = FALSE])
      to <- from
    } else {
      from <- lapply(to, function(x) {
        x <- enter(x)
        x$.phase <- 'enter'
        x
      })
    }
  } else if (is.null(to)) {
    if (is.null(exit)) {
      to <- list(from[[1]][0, , drop = FALSE])
      from <- to
    } else {
      to <- lapply(from, function(x) {
        x <- exit(x)
        x$.phase <- 'exit'
        x
      })
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
common_id <- function(from, to) {
  from_id <- as.character(from$.id)
  to_id <- as.character(to$.id)
  all_id <- unique(c(from_id, to_id))
  from$.id <- match(from_id, all_id)
  to$.id <- match(to_id, all_id)
  list(from = from, to = to)
}
