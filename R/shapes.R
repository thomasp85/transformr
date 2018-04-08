#' @export
poly_circle <- function(st = FALSE, n = 360) {
  i <- seq(0, 2*pi, length.out = n+1)[-n-1]
  x <- sin(i)
  y = cos(i)
  if (st) {
    st_polygon(list(cbind(x, y)[c(seq_len(n), 1), , drop = FALSE]))
  } else {
    data.frame(x = x, y = y, id = 1L)
  }
}
#' @export
poly_circles <- function(st = FALSE, n = 3, r = 0.25) {
  i <- seq(0, 2*pi, length.out = n+1)[-n-1]
  x <- sin(i)
  y = cos(i)
  d <- poly_circle()
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
#' @export
poly_star <- function(st = FALSE, n = 5, r1 = 0.5) {
  d <- poly_circle(n = n*2)
  d$x[c(FALSE, TRUE)] <- d$x[c(FALSE, TRUE)] * r1
  d$y[c(FALSE, TRUE)] <- d$y[c(FALSE, TRUE)] * r1
  if (st) {
    d <- st_polygon(list(cbind(d$x, d$y)[c(seq_len(n*2), 1), , drop = FALSE]))
  }
  d
}
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
#' @export
point_random <- function(st = FALSE, n = 10) {
  x <- runif(10, min = -1, max = 1)
  y <- runif(10, min = -1, max = 1)
  if (st) {
    st_multipoint(cbind(x, y))
  } else {
    data.frame(x = x, y = y, i = seq_len(n))
  }
}
#' @export
point_grid <- function(st = FALSE, dim = 5) {
  x <- rep(seq(-1, 1, length.out = dim), each = dim)
  y <- rep(seq(-1, 1, length.out = dim), dim)
  if (st) {
    st_multipoint(cbind(x, y))
  } else {
    data.frame(x = x, y = y, i = dim^2)
  }
}
