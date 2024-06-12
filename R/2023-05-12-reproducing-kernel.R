
# source: https://m-clark.github.io/models-by-example/cubic-spline.html

rk <- function(x, z) {
  ((z - 0.5) ^ 2 - 1 / 12) * ((x - 0.5) ^ 2 - 1 / 12) / 4 -
    ((abs(x - z) - 0.5) ^ 4 - (abs(x - z) - 0.5) ^ 2 / 2 + 7 / 240) / 24
}