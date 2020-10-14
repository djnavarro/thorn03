#' Makes a thorns image
#'
#' @param shade A string specifying a hex colour (e.g., "#ff45ab")
#' @param file Name of the file to be created (if NULL, default is created using shade value)
#' @param cells Number of points used in the voronoi tesselation (default = 20000)
#' @param layers Number of function variants used in the iterated function system (default = 5)
#' @param threshold Maximum size for brightly coloured tiles (default = .02)
#' @param trim Threshold for discarding outlier points (default = 1)
#'
#' @export
thorn03 <- function(shade, file = NULL, cells = 20000, layers = 5, threshold = .02, trim = 1) {

  # seed
  seed <- sum(grDevices::col2rgb(shade)) + 1
  set.seed(seed)

  # palette specification ---------------------------------------------------

  pal <- sample(grDevices::colours(distinct = TRUE), 4)
  pal[1] <- shade


  # generate the data -------------------------------------------------------

  cat("generating...\n")

  # create data frame
  df <- thorn_grow(cells, layers)
  df <- as.data.frame(df)
  names(df) <- c("x","y","c")

  # filter and transform
  df <- df[-(1:100),]
  filter_x <- c(-trim, trim)
  filter_y <- c(-trim, trim)
  if(!is.null(filter_x)) {
    x_ok <- df$x > filter_x[1] & df$x < filter_x[2]
    y_ok <- df$y > filter_y[1] & df$y < filter_y[2]
    df <- df[x_ok & y_ok, ]
  }
  df$c <- rank(df$c)
  df$y <- -df$y

  # scale the co-ordinates to the image size
  px <- 5000
  xrng <- max(df[,1]) - min(df[,1])
  yrng <- max(df[,2]) - min(df[,2])
  rng <- max(c(xrng, yrng))

  # create a vector of colours
  ncol <- length(pal)
  col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * (ncol - 1)) + 1L
  df$col <- pal[col_idx]




  # generate the image ------------------------------------------------------

  cat("rendering...\n")

  sift <- function(thresh = .025) {
    function(data) {
      data <- data %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(tilesize = (max(x) - min(x)) * (max(y) - min(y))) %>%
        dplyr::ungroup()
      data$tilealpha <- .1
      data$tilealpha[data$tilesize < thresh^2] <- 1
      return(data)
    }
  }

  StatVoronoiseTile <- voronoise::StatVoronoiseTile

  bg <- pal[1]
  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      group = 1,
      fill = col,
      alpha = ggplot2::after_stat(tilealpha)
    )
  ) +
    voronoise::geom_voronoise(
      perturb = sift(threshold),
      max.radius = NULL,
      radius = 0,
      expand = 0
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg, colour = bg)) +
    ggplot2::coord_cartesian(
      xlim = filter_x * .9,
      ylim = filter_y * .9
    )

  ggplot2::ggsave(
    file = make_filename(file, shade),
    plot = p,
    width = 5000 / 300,
    height = 5000 / 300,
    dpi = 300
  )

}

make_filename <- function (file, shade) {
  if (is.null(file)) {
    file <- paste0("thorn_03_", gsub("#", "", shade), ".png")
  }
  return(file)
}

#' @import voronoise
#' @import ggforce
#' @import ggplot2
NULL

#' @useDynLib thorn03
NULL

utils::globalVariables(c("x", "y", "tilealpha", "group"))
