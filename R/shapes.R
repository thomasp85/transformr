#' Some different geometries to play with
#'
#' These functions are provided to allow you to play with somee simple shapes as
#' you explore `transformr` and are also used in the examples for the different
#' tween functions. All geometries can be returned as either a standard
#' `data.frame` with `x`, `y`, and `id` column, or as an sf geometry of the
#' appropriate type.
#'
#' @param st Logical. Should the geometry be returned as an `sf` feature?
#' @param detail The number of points defining the shape
#' @param n For `poly_circles` the number of circles, for `poly_star` and
#' `poly_star_hole` the number of 'arms', and for `point_random` the number of
#' points
#' @param r,r1 The radius of the geometry. `r` gives the radius of the circles
#' in `poly_circles` and `r1` gives the inner radius for
#' `poly_star`/`poly_star_hole`, thus determining how pointy it is
#' @param windings The number of revolutions in the spiral
#' @param w1,w2 The frequency for the two sine waves
#' @param dim the number of rows and columns in the grid
#'
#' @return Either a data.frame or an sf feature depending on the value of `st`
#'
#' @name simple_shapes
#' @rdname simple_shapes
#'
#' @examples
#' # Create a 7-pointed star
#' poly_star(n = 7)
#'
NULL

#' @rdname simple_shapes
#' @export
poly_circle <- function(st = FALSE, detail = 360) {
  i <- seq(0, 2*pi, length.out = detail+1)[-detail-1]
  x <- sin(i)
  y = cos(i)
  if (st) {
    st_polygon(list(cbind(x, y)[c(seq_len(detail), 1), , drop = FALSE]))
  } else {
    data.frame(x = x, y = y, id = 1L)
  }
}
#' @rdname simple_shapes
#' @importFrom sf st_multipolygon
#' @export
poly_circles <- function(st = FALSE, n = 3, r = 0.25, detail = 360) {
  i <- seq(0, 2*pi, length.out = n+1)[-n-1]
  x <- sin(i)
  y = cos(i)
  d <- poly_circle(detail = detail)
  d_small <- d
  d_small$x <- d_small$x * r
  d_small$y <- d_small$y * r
  d1 <- d_small
  d2 <- d_small
  d3 <- d_small
  d1$x <- d1$x + x[1]
  d1$y <- d1$y + y[1]
  d2$x <- d2$x + x[2]
  d2$y <- d2$y + y[2]
  d2$id <- 2L
  d3$x <- d3$x + x[3]
  d3$y <- d3$y + y[3]
  d3$id <- 3L
  if (st) {
    st_multipolygon(list(
      list(cbind(d1$x, d1$y)[c(seq_len(360), 1), , drop = FALSE]),
      list(cbind(d2$x, d2$y)[c(seq_len(360), 1), , drop = FALSE]),
      list(cbind(d3$x, d3$y)[c(seq_len(360), 1), , drop = FALSE])
    ))
  } else {
    rbind(d1,d2,d3)
  }
}
#' @rdname simple_shapes
#' @export
poly_star <- function(st = FALSE, n = 5, r1 = 0.5) {
  d <- poly_circle(detail = n*2)
  d$x[c(FALSE, TRUE)] <- d$x[c(FALSE, TRUE)] * r1
  d$y[c(FALSE, TRUE)] <- d$y[c(FALSE, TRUE)] * r1
  if (st) {
    d <- st_polygon(list(cbind(d$x, d$y)[c(seq_len(n*2), 1), , drop = FALSE]))
  }
  d
}
#' @rdname simple_shapes
#' @export
poly_star_hole <- function(st = FALSE, n = 5, r1 = 0.5) {
  d <- poly_star(n = n, r1 = r1)
  d1 <- d
  d1$x <- d1$x * 0.5
  d1$y <- d1$y * 0.5
  if (st) {
    st_polygon(list(
      cbind(d$x, d$y)[c(seq_len(n*2), 1), , drop = FALSE],
      cbind(d1$x, d1$y)[c(seq_len(n*2), 1), , drop = FALSE]
    ))
  } else {
    rbind(
      d,
      data.frame(x = NA, y = NA, id = 1L),
      d1
    )
  }
}
#' @rdname simple_shapes
#' @export
path_spiral <- function(st = FALSE, windings = 5) {
  n = 50 * windings
  r = seq(0, 1, length.out = n)
  i <- seq(0, 2*pi*windings, length.out = n+1)[-n-1]
  x <- sin(i) * r
  y <- cos(i) * r
  if (st) {
    st_linestring(cbind(x, y))
  } else {
    data.frame(x = x, y = y, id = 1L)
  }
}
#' @rdname simple_shapes
#' @export
path_waves <- function(st = FALSE, w1 = 7, w2 = 11) {
  x <- seq(-1, 1, length.out = 150)
  y1 = 0.2*sin(w1*x) + 0.5
  y2 = 0.2*sin(w2*x) - 0.5
  if (st) {
    st_multilinestring(list(
      cbind(x, y1),
      cbind(x, y2)
    ))
  } else {
    data.frame(x = rep(x, 2), y = c(y1, y2), id = rep(c(1L, 2L), each = length(x)))
  }
}
#' @rdname simple_shapes
#' @export
#' @importFrom sf st_multipoint
#' @importFrom stats runif
point_random <- function(st = FALSE, n = 10) {
  x <- runif(10, min = -1, max = 1)
  y <- runif(10, min = -1, max = 1)
  if (st) {
    st_multipoint(cbind(x, y))
  } else {
    data.frame(x = x, y = y, i = seq_len(n))
  }
}
#' @rdname simple_shapes
#' @export
#' @importFrom sf st_multipoint
point_grid <- function(st = FALSE, dim = 5) {
  x <- rep(seq(-1, 1, length.out = dim), each = dim)
  y <- rep(seq(-1, 1, length.out = dim), dim)
  if (st) {
    st_multipoint(cbind(x, y))
  } else {
    data.frame(x = x, y = y, i = dim^2)
  }
}
