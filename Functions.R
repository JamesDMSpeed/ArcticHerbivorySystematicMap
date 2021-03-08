#' Contours of a HDP region estimate
#'
#' @param prob Numeric. Probability level of the HDP region.
#' @inheritParams ggplot2::stat_density_2d
#' @inheritParams MASS::kde2d
#' @param ...
#'
#' @return
#' @export

stat_hpd_2d <- function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, n = 100, prob = 0.95, ...) {
  ggplot2::layer(
    stat = StatHPDContour, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, prob = prob, ...)
  )
}

StatHPDContour <- ggplot2::ggproto(
  "hpd_2d"
  , Stat
  , compute_group = function (data, scales, na.rm = FALSE, h = NULL,
                              n = 100, prob = 0.95)
  {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }
    dens <- MASS::kde2d(data$x, data$y, h = h, n = n,
                        lims = c(scales$x$dimension(), scales$y$dimension()))
    df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
    df$group <- data$group[1]
    
    dx <- diff(dens$x[1:2])
    dy <- diff(dens$y[1:2])
    sz <- sort(dens$z)
    c1 <- cumsum(sz) * dx * dy
    
    breaks <- sapply(prob, function(x) {
      withCallingHandlers(
        stats::approx(c1, sz, xout = 1 - x)$y
        , warning = function(w) {
          if (grepl("collapsing to unique 'x' values", w$message))
            invokeRestart("muffleWarning")
        }
      )
    })
    
    ggplot2::StatContour$compute_panel(df, scales, breaks = breaks)
  }
  , required_aes = c("x", "y")
)