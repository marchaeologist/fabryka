# Load packages used in fabryka
fabryka_packages = c("shiny","dplyr","ggplot2","plotly", "ggsci", "ggtern", "circular", "CircStats", "Ternary", "RStoolbox", "shinyjs", "rmarkdown", "shinythemes", "ggalt", "DT", "raster")
is_installed = sapply(fabryka_packages, require, character.only=T)
sapply(fabryka_packages[!is_installed], install.packages)

# Modified function from circular package to perform Rao test
#' @export
print.rao.spacing.test.modified <- function(x, digits=4, ...) {
  U <- x$statistic
  alpha <- x$alpha
  n <- x$n
  data(rao.table, package='circular', envir=sys.frame(which=sys.nframe()))
  if (n <= 30)
    table.row <- n - 3
  else if (n <= 32)
    table.row <- 27
  else if (n <= 37)
    table.row <- 28
  else if (n <= 42)
    table.row <- 29
  else if (n <= 47)
    table.row <- 30
  else if (n <= 62)
    table.row <- 31
  else if (n <= 87)
    table.row <- 32
  else if (n <= 125)
    table.row <- 33
  else if (n <= 175)
    table.row <- 34
  else if (n <= 250)
    table.row <- 35
  else if (n <= 350)
    table.row <- 36
  else if (n <= 450)
    table.row <- 37
  else if (n <= 550)
    table.row <- 38
  else if (n <= 650)
    table.row <- 39
  else if (n <= 750)
    table.row <- 40
  else if (n <= 850)
    table.row <- 41
  else if (n <= 950)
    table.row <- 42
  else table.row <- 43

  cat("\n")
  cat("       Rao's Spacing Test of Uniformity", "\n", "\n")
  cat("Test Statistic =", round(U, digits=digits), "\n")

  if (alpha == 0) {
    if (U > rao.table[table.row, 1])
      cat("P-value < 0.001", "\n", "\n")
    else if (U > rao.table[table.row, 2])
      cat("0.001 < P-value < 0.01", "\n", "\n")
    else if (U > rao.table[table.row, 3])
      cat("0.01 < P-value < 0.05", "\n", "\n")
    else if (U > rao.table[table.row, 4])
      cat("0.05 < P-value < 0.10", "\n", "\n")
    else cat("P-value > 0.10", "\n", "\n")
    x$accepted <- NA
  } else {
    table.col <- (1:4)[alpha == c(0.001, 0.01, 0.05, 0.1)]
    critical <- rao.table[table.row, table.col]
    cat("Level", alpha, "critical value =", critical, "\n")
    if (U > critical) {
      cat("Reject null hypothesis of uniformity \n\n")
      # cat("P_value < 0.05 \n\n")
      x$accepted <- "< 0.05"
      x$critical <- critical
    } else {
      cat("Do not reject null hypothesis of uniformity \n\n")
      # cat("P_value > 0.05 \n\n")
      x$accepted <- "> 0.05"
      x$critical <- critical
    }
  }
  
  invisible(unclass(x))
}

# All global functions used from McPherron 2018. 
# Please note some of them have been modified. This is particularly the case in the app_server file.

#################################################################################################################
# Compute plunge and bearing angles from XYZ 2-shot data
plunge_and_bearing <- function(xyz) {
  # Compute the plunge angle
  # Note: Angles are converted to degrees
  run <- sqrt((xyz$X2 - xyz$X1)^2 + (xyz$Y2 - xyz$Y1)^2)
  rise <- abs(xyz$Z2 - xyz$Z1)
  plunge_angle <- ifelse(run == 0, 90, deg(atan(rise / run)))
  
  # Compute Schmidt bearing (lower hemisphere)
  # Note: Angle are adjusted to be clockwise with north (0) = positive y-axis
  # Note: Angles are converted to degrees.
  run <- ifelse(xyz$Z1 >= xyz$Z2, xyz$X2 - xyz$X1, xyz$X1 - xyz$X2)
  rise <- ifelse(xyz$Z1 >= xyz$Z2, xyz$Y2 - xyz$Y1, xyz$Y1 - xyz$Y2)
  slope <- ifelse(run != 0, rise / run, 1000000)
  bearing_angle <- 90 - deg(atan(slope))
  bearing_angle[run <= 0] <- 180 + bearing_angle[run <= 0]
  
  return(data.frame(plunge = plunge_angle, bearing = bearing_angle))
}

########################################################################################################
# convert to radians
rad <- function(deg) {
  (deg * pi) / (180)
}

#################################################################################################################
# Convert radians to degrees
deg <- function(radian) {
  (radian * 180) / pi
}

#################################################################################################################
# Return vector(artifact) lengths
compute_lengths <- function(xyz) {
  return(sqrt((xyz$X1 - xyz$X2)^2 + (xyz$Y1 - xyz$Y2)^2 + (xyz$Z1 - xyz$Z2)^2))
}

#################################################################################################################
# Helping function to place points on Benn diagram
# Expects an object with columns named elongation and isotropy
benn_coords <- function(benn) {
  return(cbind(X = benn[, "elongation"] + (.5 * benn[, "isotropy"]), Y = benn[, "isotropy"] * .866))
}

#################################################################################################################
#  Normalize vectors
vector_normals <- function(xyz) {
  l <- compute_lengths(xyz)
  xnorm <- ifelse(l != 0, (xyz$X1 - xyz$X2) / l, 0)
  ynorm <- ifelse(l != 0, (xyz$Y1 - xyz$Y2) / l, 0)
  znorm <- ifelse(l != 0, (xyz$Z1 - xyz$Z2) / l, 0)
  
  return(cbind(xnorm, ynorm, znorm))
}

#################################################################################################################
#  Compute eigen values from normalized vectors
eigen_values <- function(xyz) {
  l <- xyz[, 1] # X
  m <- xyz[, 2] # Y
  n <- xyz[, 3] # Z
  
  # Build a matrix prior to computing eigen values
  M11 <- sum(l^2)
  M12 <- sum(l * m)
  M13 <- sum(l * n)
  M21 <- sum(m * l)
  M22 <- sum(m^2)
  M23 <- sum(m * n)
  M31 <- sum(n * l)
  M32 <- sum(n * m)
  M33 <- sum(n^2)
  M <- matrix(c(M11, M12, M13, M21, M22, M23, M31, M32, M33), nrow = 3, ncol = 3)
  
  # Compute eigen values on matrix normalized for sample size
  n <- nrow(xyz)
  return(eigen(M / n))
}

###############################################################################################################
# Rose.diag from CircStats package adapted to do only plunge angle from 0 to 90,
# to deal with angles that turn clockwise, and some other customizations.
rose_diagram_plunge <- function(x, bins = 10, main = "", prop = 1,
                                cex = 1, pch = 19, pts_on_edge = FALSE,
                                color_codes = NULL, color_filled = NULL,
                                pnt_col = "black", bar_col = "white", bg = "white",
                                dotsep = 40, shrink = 1, ...) {
  x <- rad(x)
  x <- x %% (2 * pi)
  plot(cos(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000)),
       sin(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000)),
       axes = FALSE, xlab = "", ylab = "", main = "", type = "l",
       xlim = shrink * c(-.15, 1.15), ylim = shrink * c(-1.15, 0.15), asp = 1
  )
  polygon(c(0, cos(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000))),
          c(0, sin(seq(3 / 4 * 2 * pi, 2 * pi, length = 1000))),
          col = bg
  )
  # text(-.88,1.05,main,cex=1.1)
  text(-.03, .1, main, cex = cex * 1.1)
  lines(c(0, 0), c(-0.9, -1))
  text(0.005, -1.075, "90", cex = cex * 1.1)
  lines(c(0.9, 1), c(0, 0))
  text(1.05, 0, "0", cex = cex * 1.1)
  lines(c(0, 0), c(-1, 0))
  n <- length(x)
  freq <- c(1:bins)
  arc <- (1 / 2 * pi) / bins
  for (i in 1:bins) {
    newi <- bins - i + 1 # This turns the angles clockwise
    freq[i] <- sum(x <= newi * arc & x > (newi - 1) * arc)
  }
  rel.freq <- freq / n
  radius <- sqrt(rel.freq) * prop
  # radius <- freq/max(freq) * prop     					  # This will bring bars to circle but with flat proportions
  radius <- radius / max(radius) * prop # This will bring bars to circle but with exponential proportions
  sector <- seq(0, 1 / 2 * pi - (1 / 2 * pi) / bins, length = bins)
  sector <- sector - (pi / 2) # This rotates them to the right spot
  mids <- seq(arc / 2, 1 / 2 * pi - (pi / 4) / bins, length = bins)
  index <- cex / dotsep
  for (i in 1:bins) {
    if (rel.freq[i] != 0) {
      xp <- c(0, radius[i] * cos(sector[i]), radius[i] * cos(sector[i] + (1 / 2 * pi) / bins))
      yp <- c(0, radius[i] * sin(sector[i]), radius[i] * sin(sector[i] + (1 / 2 * pi) / bins))
      polygon(xp, yp, col = bar_col, ...)
      
      if (pts_on_edge) {
        xp <- cos(x)
        yp <- -sin(x)
        if (!is.null(color_codes)) {
          points(xp[color_filled], yp[color_filled],
                 cex = (cex * .8),
                 col = color_codes[color_filled], pch = 19
          )
          points(xp[!color_filled], yp[!color_filled],
                 cex = (cex * .8),
                 col = color_codes[!color_filled], pch = 21
          )
        } else {
          points(xp, yp, cex = (cex * .8), pch = pch, col = pnt_col)
        }
      }
    }
  }
}

#################################################################################################################
# Function to plot points on circle in Schmidt equal area space
# Code to draw circle taken from rose.diag in CircStats package
# Angles should be in decimal degrees
schmidt_diagram <- function(bearing = NULL, plunge = NULL, angles = NULL,
                            level = "All Points", color_codes = NULL,
                            color_filled = FALSE, redraw = TRUE, col = "black",
                            pch = 19, cex = 1, main = "", ...) {
  # library(CircStats)
  
  if (is.null(angles) & (is.null(bearing) | is.null(plunge))) stop("Not enough data passed to schmidt.diagram.2shot. Bearing and plunge angles are required.")
  
  if (!is.null(angles)) {
    plunge <- angles[, 1]
    bearing <- angles[, 2]
  }
  
  level <- factor(level)
  
  for (l in levels(level)) {
    bearing_angle_level <- subset(bearing, level == l)
    plunge_angle_level <- subset(plunge, level == l)
    
    # If the circle doesn't already exist (from doing a Rose diagram), then make it
    if (length(levels(level)) > 1) {
      if (!exists("main")) draw_circle_diagram(main = l, ...) else draw_circle_diagram(main = main[which(levels(level) == l)], ...)
    } else {
      if (redraw) draw_circle_diagram(...)
    }
    
    # Shift points into Schmidt space
    d <- sin(rad((90 - plunge_angle_level) / 2)) / sin(rad(45))
    x <- d * sin(rad(bearing_angle_level))
    y <- d * cos(rad(bearing_angle_level))
    
    # Plot them using color coding or not
    if (!is.null(color_codes)) {
      points(x[color_filled], y[color_filled],
             cex = (cex * .8),
             col = color_codes[color_filled], pch = 19
      )
      points(x[!color_filled], y[!color_filled],
             cex = (cex * .8),
             col = color_codes[!color_filled], pch = 21
      )
    } else {
      points(x, y, cex = (cex * .8), pch = pch, col = col)
    }
  }
}

#################################################################################################################
# Internal function to setup a circular graph for rose or schmidt diagrams
draw_circle_diagram <- function(bg = "white", cex = 1, main = "", ...) {
  plot(cos(seq(0, 2 * pi, length = 1000)), sin(seq(0, 2 * pi, length = 1000)),
       axes = FALSE,
       xlab = "", ylab = "", main = "", type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1
  )
  polygon(cos(seq(0, 2 * pi, length = 1000)), sin(seq(0, 2 * pi, length = 1000)), col = bg)
  
  text(-.88, 1.05, main, cex = (cex * 1.1))
  lines(c(0, 0), c(0.9, 1))
  text(0.005, 1.10, "0", cex = (cex * 1.1))
  lines(c(0, 0), c(-0.9, -1))
  text(0.005, -1.11, "180", cex = (cex * 1.1))
  lines(c(-1, -0.9), c(0, 0))
  text(-1.16, 0, "270", cex = (cex * 1.1))
  lines(c(0.9, 1), c(0, 0))
  text(1.13, 0, "90", cex = (cex * 1.1))
  lines(c(-.05, .05), c(0, 0))
  lines(c(0, 0), c(-0.05, .05))
}


#################################################################################################################
