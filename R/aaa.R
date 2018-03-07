align <- function(from, to, min_n = 50) {
  from <- lapply(split(from, from$group), as_clockwise)
  to <- lapply(split(to, to$group), as_clockwise)
  from_group <- sapply(from, function(x) x$group[1])
  to_group <- sapply(to, function(x) x$group[1])
  all_groups <- as.character(union(from_group, to_group))
  from_all <- structure(rep(list(NULL), length(all_groups)), names = all_groups)
  to_all <- from_all
  from_all[names(from)] <- from
  to_all[names(to)] <- to
  polygons <- Map(function(from, to) {
    if (is.null(from)) {
      from <- to
      from$x <- mean(from$x)
      from$y <- mean(from$y)
    } else if (is.null(to)) {
      to <- from
      to$x <- mean(to$x)
      to$y <- mean(to$y)
    } else {
      n_points <- max(c(min_n, nrow(from), nrow(to)))
      if (nrow(from) < n_points) from <- add_points(from, n_points - nrow(from))
      if (nrow(to) < n_points) to <- add_points(to, n_points - nrow(to))
      offset <- rotate(to$x, to$y, from$x, from$y)
      to_end <- seq_len(nrow(to)) < offset
      to <- rbind(to[!to_end, , drop = FALSE],
                  to[to_end, , drop = FALSE])
    }
    list(from = from, to = to)
  }, from = from, to = to)
  from <- do.call(rbind, lapply(polygons, `[[`, 'from'))
  to <- do.call(rbind, lapply(polygons, `[[`, 'to'))
  list(from, to)
}

as_clockwise <- function(polygon) {
  x <- c(polygon$x, polygon$x[1])
  y <- c(polygon$y, polygon$y[1])
  area <- (x[-1] - x[-length(x)]) * (y[-1] + y[-length(y)])
  if (sum(area) < 0) polygon <- polygon[rev(seq_len(nrow(polygon))), ]
  polygon
}

add_points <- function(polygon, n) {
  x <- diff(c(polygon$x, polygon$x[1]))
  y <- diff(c(polygon$y, polygon$y[1]))
  l <- sqrt(x*x + y*y)
  longest <- order(l, decreasing = TRUE)
  n_splits <- find_splits(l[longest], n)
  splits <- n_splits[match(seq_along(l), longest)]
  new_points <- insert_points(polygon$x, polygon$y, splits, n)
  polygon <- polygon[rep(seq_len(nrow(polygon)), splits + 1), ]
  polygon$x <- new_points$x
  polygon$y <- new_points$y
  polygon
}
