#' @importFrom tweenr tween_states
#' @importFrom rlang enquo quo_is_null quo eval_tidy
tween_polygon <- function(.data, to, x = NULL, y = NULL, group = NULL, ease = 'linear', n_frames = 2) {
  stopifnot(is.data.frame(.data))
  xquo <- enquo(x)
  if (quo_is_null(xquo)) xquo <- quo(x)
  yquo <- enquo(y)
  if (quo_is_null(yquo)) yquo <- quo(y)
  groupquo <- enquo(group)
  if (quo_is_null(groupquo)) groupquo <- quo(1)
  from <- get_last_frame(.data)
  from <- eval_tidy(quo(data.frame(x = !!xquo, y = !!yquo, group = !!groupquo)), data = from)
  to <- eval_tidy(quo(data.frame(x = !!xquo, y = !!yquo, group = !!groupquo)), data = to)
  polygons <- align(from, to)
  polygons <- tween_states(polygons, tweenlength = 1, statelength = 0, ease = ease, nframes = n_frames - 1)
  polygons <- polygons[!polygons$.frame %in% c(1, n_frames), , drop = FALSE]
  morph <- rbind(
    cbind(from, .frame = 1),
    polygons,
    cbind(to, .frame = n_frames)
  )
  with_prior_frames(.data, morph)
}

get_last_frame <- function(polygon) {
  if ('.frame' %in% names(polygon)) {
    polygon[polygon$.frame == max(polygon$.frame), names(polygon) != '.frame']
  } else {
    polygon
  }
}
with_prior_frames <- function(prior, morph) {
  if ('.frame' %in% names(prior)) {
    prior <- prior[prior$.frame != max(prior$.frame), , drop = FALSE]
    morph$.frame <- morph$.frame + max(prior$.frame)
    rbind(prior, morph)
  } else {
    morph
  }
}
