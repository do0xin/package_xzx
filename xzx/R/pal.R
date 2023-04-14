#'According to the positive and negative of the data,
#'paint background with different colours
#'
#'@param x A numeric vector

pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c("#b2de81"),
    domain = c(-10e6, 0)
  )
  f_pos <- scales::col_numeric(
    palette = c("#fae3d9"),
    domain = c(0, 10e6)
  )
  f_zero <- scales::col_numeric(
    palette = c("#ffffff"),
    domain = c(0)
  )
  fcase(x < 0, f_neg(x),
        x > 0, f_pos(x),
        x == 0, f_zero(x))
}
