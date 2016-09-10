require(proto)
collidev <- function(data, height = NULL, name, strategy, check.height = TRUE) {
  if (!is.null(height)) {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data <- within(data, {
        ymin <- y - height/2
        ymax <- y + height/2
      })
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }
    heights <- unique(with(data, ymax - ymin))
    heights <- heights[!is.na(heights)]
    if (!zero_range(range(heights))) {
      warning(name, " requires constant height: output may be incorrect", 
              call. = FALSE)
    }
    height <- heights[1]
  }
  data <- data[order(data$ymin), ]
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-06)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
  }
  if (!is.null(data$xmax)) {
    ddply(data, .(ymin), strategy, height = height)
  } else if (!is.null(data$x)) {
    message("xmax not defined: adjusting position using x instead")
    transform(ddply(transform(data, xmax = x), .(ymin), strategy, height = height), 
              x = xmax)
  } else {
    stop("Neither x nor xmax defined")
  }
}

pos_dodgev <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) 
    return(df)
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }
  d_width <- max(df$ymax - df$ymin)
  diff <- height - d_width
  groupidx <- match(df$group, sort(unique(df$group)))
  df$y <- df$y + height * ((groupidx - 0.5)/n - 0.5)
  df$ymin <- df$y - d_width/n/2
  df$ymax <- df$y + d_width/n/2
  df
}

position_dodgev <- function(width = NULL, height = NULL) {
  PositionDodgev$new(width = width, height = height)
}

PositionDodgev <- proto(ggplot2:::Position, {
  objname <- "dodgev"
  
  adjust <- function(., data) {
    if (empty(data)) 
      return(data.frame())
    check_required_aesthetics("y", names(data), "position_dodgev")
    
    collidev(data, .$height, .$my_name(), pos_dodgev, check.height = TRUE)
  }
})
