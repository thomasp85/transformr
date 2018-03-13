align <- function(from, to, min_n = 50, match = TRUE) {
  polygons <- if (match) {
    prep_match(from, to)
  } else {
    prep_align(from, to)
  }
  polygons <- Map(function(from, to, group) {
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
    from$group <- group
    to$group <- group
    list(from = from, to = to)
  }, from = polygons$from, to = polygons$to, group = as.integer(names(polygons$from)))
  from <- do.call(rbind, lapply(polygons, `[[`, 'from'))
  to <- do.call(rbind, lapply(polygons, `[[`, 'to'))
  list(from, to)
}

prep_match <- function(from, to) {
  from <- lapply(split(from, from$group), as_clockwise)
  to <- lapply(split(to, to$group), as_clockwise)
  from_group <- sapply(from, function(x) x$group[1])
  to_group <- sapply(to, function(x) x$group[1])
  all_groups <- as.character(union(from_group, to_group))
  from_all <- structure(rep(list(NULL), length(all_groups)), names = all_groups)
  to_all <- from_all
  from_all[names(from)] <- from
  to_all[names(to)] <- to
  list(from = from_all, to = to_all)
}
#' @importFrom sf st_sfc st_area st_distance st_centroid
#' @importFrom lpSolve lp.assign
prep_align <- function(from, to) {
  from <- lapply(split(from, from$group), as_clockwise)
  to <- lapply(split(to, to$group), as_clockwise)
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
  distance <- st_distance(st_centroid(from_st), st_centroid(to_st))
  match_poly <- lp.assign(distance)
  if (match_poly$status == 0) {
    to <- to[apply(round(match_poly$solution) == 1, 1, which)]
  }
  names(from) <- as.character(seq_along(from))
  names(to) <- as.character(seq_along(to))
  list(from = from, to = to)
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
#' @importFrom sf st_union st_multipolygon
split_polygon <- function(polygon, n) {
  if (n == 1) return(list(polygon))
  if (nrow(polygon) < n + 2) polygon <- add_points(polygon, n + 2 - nrow(polygon))
  triangles <- as.data.frame(cut_polygon(as.matrix(polygon), n))
  names(triangles) <- c('x', 'y', 'groups')
  points <- lapply(split(triangles[, c('x', 'y')], triangles$groups), function(poly) {
    tri <- split(poly, rep(seq_len(nrow(poly)/3), each = 3))
    tri <- lapply(tri, function(x) list(as.matrix(x[c(1:3,1),])))
    poly <- as.matrix(st_union(st_multipolygon(tri)))
    paste0(poly[-nrow(poly),1], '-' , poly[-nrow(poly),2])
  })
  split(polygon[match(unlist(points), paste0(polygon$x, '-', polygon$y)), ], rep(seq_along(points), lengths(points)))
}
divide_polygons <- function(polygons, area, n) {
  biggest <- order(area, decreasing = TRUE)
  n_splits <- find_splits(area[biggest], n)
  splits <- n_splits[match(seq_along(area), biggest)] + 1
  unlist(Map(split_polygon, polygon = polygons, n = splits), recursive = FALSE)
}
#' @importFrom sf st_polygon
to_polygon <- function(points) {
  if (is.data.frame(points)) {
    points <- as.matrix(points[, c('x', 'y')])
  } else {
    points <- points[, 1:2]
  }
  st_polygon(list(rbind(points, points[1,])))
}
