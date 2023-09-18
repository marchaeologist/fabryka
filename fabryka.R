### --- R packages ---

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsci)
library(ggtern)
library(RColorBrewer)
library(circular)
library(CircStats)
library(Ternary)
library(cowplot)
library(RStoolbox)


### --- Model data from Bertran & Lenoble 2002 ---
model <- read.csv2("C:/Users/Surface User/Documents/articles/fabriques/fabryka/model.csv")
ternary_model <- model %>%
  dplyr::filter(area != "NA")

### --- Example data : Chez Pinaud at Jonzac (Quina, dec 9, excavation Rendu et al) ---
data_example <- read.csv("C:/Users/Surface User/Documents/fab_dec9.csv",
  colClasses = c(
    "X1" = "numeric",
    "X2" = "numeric",
    "Y1" = "numeric",
    "Y2" = "numeric",
    "Z1" = "numeric",
    "Z2" = "numeric"
  ), sep = ";"
)



# ### -- function to compute spherical variance of samples --- juste ou pas ???
# varSph <- function(bearing, plunge){
#
#   bearing_rad <- bearing * (pi / 180)
#   plunge_rad <- plunge * (pi / 180)
#
#   x <- sin(plunge_rad) * cos(bearing_rad)
#   y <- sin(plunge_rad) * sin(bearing_rad)
#   z <- cos(plunge_rad)
#
#   center_x <- mean(x)
#   center_y <- mean(y)
#   center_z <- mean(z)
#
#   sum_squared_distances <- sum((x - center_x)^2 + (y - center_y)^2 + (z - center_z)^2)
#   spherical_variance <- round(sum_squared_distances / (length(bearing) - 1), 2)
#
#   spherical_variance
# }



### --- Mcpherron (2018) functions not modified ---

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
#
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
  library(CircStats)

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
#
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



### --- Application ---
### --- ui ---

ui <- tagList(
  navbarPage(
    "",
    tabPanel(
      "Presentation",
      mainPanel(
        column(12,
          align = "center",
          tags$img(src = "hex_Fabryka.png", height = 200)
        ),
        tags$br(),
        h2("Fabryka"),
        hr(),
        h5("Fabryka is a R shiny application dedicated to the fabric analysis of archaeological remains. It allows a deep exploration of the fabric data. The application is organised in four different panels."), 
        tags$br(),
        h5(strong("1) The 'Upload your data'"), "panel allows the user to upload different data sources"),
        tags$br(),
        h5(strong("2) The 'classical method'"), "panel contains panels to perform:"),
        h5("- Benn diagrams with possible integration of Bertran & Lenoble model (2002)"),
        h5("- Rose diagrams for bearing and plunge"),
        h5("- Schmidt diagrams"),
        h5("- Summary data table with orientation statistical tests"),
        tags$br(),
        h5(strong("3) The 'Spatialised method'"), "panel contains panels to perform:"),
        h5("- Benn diagrams with Benn indices computed with nearest neighbors points (McPherron 2018)"),
        h5("- Spatial projections of the points according to their fabric shape"),
        h5("- Summary data table with orientation statistical tests"),
        tags$br(),
        h5(strong("4) The 'Spatial exploration'"), "panel contains panels to perform:"),
        h5("- A spatial exploration of your data starting from a Benn diagram"),
        h5("- A spatial exploration of your data starting from the spatial projections"),
        h5("- Summary data tables with orientation statistical tests"),
        tags$br(),
        h5("An article and a tutorial are available respectively", strong("here"), "and", strong("here")),
        tags$br(),
        hr(),
        h5("Bertran P., Claud E., Detrain L., Lenoble A., Masson B., Vallin L. 2006 - Composition granulométrique des assemblages lithiques. Paléo 18, pp. 7-36."),
        h5("McPherron S.P., 2018 - Additional statistical and graphical methods for analyzing site formation processes using artifact orientations, PloS One 13, 21p.")
        )
    ),
    tabPanel(
      "Upload your data",
      sidebarPanel(
        h5("The column names must be the same as in the example datasets below."),
        fileInput("user_data", "Upload your data (.csv)", multiple = FALSE, accept = ".csv"),
        radioButtons("sep", "Column separator",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ";",
          inline = TRUE
        ),
        tags$br(),
        radioButtons("decimal", "Decimal separator",
          choices = c(Point = ".", Comma = ","),
          selected = ".",
          inline = TRUE
        ),
        hr(),
        radioButtons("data_type",
          label = "What form does your dataset take?",
          choices = c(
            "1) There are only angles",
            "2) Only angles from DistoX2",
            "3) From DistoX2 with a single set of coordinates",
            "4) With a single set of coordinates and angles",
            "5) With two sets of coordinates without angles"
          )
        ),
        hr(),
        h5(strong("Example datasets for each possible form")),
        downloadLink("angles_only", "1) There are only angles"),
        tags$br(),
        downloadLink("angles_only_disto", "2) Only angles from DistoX2"),
        tags$br(),
        downloadLink("disto_one_shot", "3) From DistoX2 with a single set of coordinates"),
        tags$br(),
        downloadLink("angles_one_shot", "4) With a single set of coordinates and angles"),
        tags$br(),
        downloadLink("two_shots", "5) With two sets of coordinates without angles"),
        tags$br()
      ),
      mainPanel(
        h5(strong("View of the first lines of your dataset")),
        tableOutput("user_data_head"),
        hr(),
        h5(strong("View of the data that will be used in the application")),
        DT::dataTableOutput("app_data_view")
      )
    ), tabPanel(
      "Classical method",
      sidebarPanel(
        h5(strong("About your data")),
        fluidRow(
          column(
            6,
            checkboxInput("plot_all_samples_c", strong("Plot all samples"), TRUE),
            uiOutput("sample_c")
          ),
          column(
            6,
            checkboxInput("plot_all_codes_c", strong("Plot all codes"), TRUE),
            uiOutput("code_c")
          )
        ),
        hr(),
        h5(strong("About the Benn diagram")),
        sliderInput("minimum_n",
          "Minimum number of measures",
          min = 5,
          max = 500,
          value = 40
        ),
        hr(),
        checkboxGroupInput("model", "Build your model",
          choices = c("none", unique(ternary_model$Type)),
          selected = "none"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Benn diagram",
            h5(strong("Benn diagram")),
            plotOutput("benn"),
            # h5("Download your plot by choosing a file format and pressing the 'Download' button"),
            radioButtons(
              inputId = "file_format_benn_c",
              label = "Select a file format",
              choices = c("pdf", "png", "jpeg"),
              inline = TRUE
            ), tags$br(),
            downloadButton("download_benn_c", "Download Benn")
          ),
          tabPanel(
            "Rose diagrams",
            h5(strong("Rose diagrams")),
            splitLayout(
              style = "border: 1px solid silver:",
              cellWidths = c(400, 400),
              plotOutput("orientation_rose_c"),
              plotOutput("plunge_rose_c")
            ),
            # h5(strong("Download your plot by choosing a file format and pressing the 'Download' button")),
            fluidRow(
              column(
                6,
                # h5(strong("Orientation Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_orientation",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_orientation_rose_c", "Download rose O.")
              ),
              column(
                6,
                # h5(strong("Plunge Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_plunge",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_plunge_rose_c", "Download rose P.")
              )
            )
          ),
          tabPanel(
            "Schmidt diagram",
            h5(strong("Schmidt diagram")),
            plotOutput("schmidt_diagram_c"),
            # h4("Download your plot by choosing a file format and pressing the 'Download' button"),
            radioButtons(
              inputId = "file_format_schmidt_c",
              label = "Select a file format",
              choices = c("pdf", "png", "jpeg"),
              inline = TRUE
            ),
            downloadButton("download_schmidt_c", "Download Schmidt")
          ),
          tabPanel(
            "Summary table",
            h4("Sample data table"), tags$br(),
            DT::dataTableOutput("summary_table_c"), tags$br(),
          )
        )
      )
    ), tabPanel(
      "Spatialised method",
      sidebarPanel(
        fluidRow(
          column(
            6,
            # checkboxInput("plot_all_samples_sm",
            #               strong("Plot all samples"), TRUE),
            uiOutput("sample_sm")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_sm",
              strong("Plot all codes"), TRUE
            ),
            uiOutput("code_sm")
          )
        ),
        hr(),
        sliderInput("n_nearest_sm",
          "Number of nearest points in series",
          min = 10,
          max = 150,
          value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_sm",
          strong("Sticks or points"), TRUE
        ),
        hr(),
        fileInput("ortho_xy_sm",
          "Upload XY ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_sm",
          "Upload XZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_sm",
          "Upload YZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_yz_ortho",
          strong("Include YZ ortho"), FALSE
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Benn diagram & projections",
            h5(strong("Benn diagram & spatial projections")),
            plotOutput("benn_sm"),
            plotOutput("projection_xy_sm", inline = T),
            plotOutput("projection_xz_sm", inline = T),
            plotOutput("projection_yz_sm", inline = T),
            # h5("Download your plot by choosing a file format and pressing the 'Download' button"),
            fluidRow(
              column(
                6,
                # h5(strong("Benn diagram")),
                hr(),
                radioButtons(
                  inputId = "file_format_benn_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_sm", "Download Benn"),
                hr(),
                # h5(strong("XY projection")),
                radioButtons(
                  inputId = "file_format_xy_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xy_sm", "Download XY proj."),
                hr()
              ),
              column(
                6,
                # h5(strong("XZ projection")),
                hr(),
                radioButtons(
                  inputId = "file_format_xz_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xz_sm", "Download XZ proj."),
                hr(),
                # h5(strong("YZ projection")),
                radioButtons(
                  inputId = "file_format_yz_sm",
                  label = "Select a file format",
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_yz_sm", "Download YZ proj."),
                hr()
              )
            )
          ),
          tabPanel(
            "Spatial series data table",
            tags$br(),
            h5(strong("Summary of data used")), tags$br(),
            DT::dataTableOutput("summary_table_sm"), tags$br(),
          ),
          tabPanel(
            "Spatial samples data table",
            tags$br(),
            h5(strong("Summary of data used")), tags$br(),
            DT::dataTableOutput("summary_table2_sm"), tags$br(),
          )
        )
      )
    ), tabPanel(
      "Spatial exploration",
      sidebarPanel(
        fluidRow(
          column(
            6,
            checkboxInput(
              "plot_all_samples_se",
              strong("No sample filter"), TRUE
            ),
            uiOutput("sample_se")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_se",
              strong("Plot all codes"), TRUE
            ),
            uiOutput("code_se")
          )
        ),
        hr(),
        sliderInput("n_nearest_se",
          "Number of nearest points in spatial series (interactive Benn diagram)",
          min = 10,
          max = 150,
          value = 40
        ),
        sliderInput("n_nearest_se2",
          "Number of nearest points in spatial series (selected points Benn diagram)",
          min = 10,
          max = 150,
          value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_se",
          strong("Sticks or points"), TRUE
        ),
        hr(),
        fileInput("ortho_xy_se",
          "Upload XY ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho_se",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_se",
          "Upload XZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho_se",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_se",
          "Upload YZ ortho",
          multiple = FALSE
        ),
        checkboxInput(
          "include_yz_ortho_se",
          strong("Include YZ ortho"), FALSE
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "From Benn diagram",
            h5(strong("Interactive Benn diagram & new plots")),
            tags$br(),
            plotlyOutput("interactive_ternary"),
            fluidRow(
              hr(),
              h5(strong("Selected points")),
              column(
                6,
                plotOutput("selected_benn")
              ),
              column(
                6,
                plotOutput("schmidt_selected")
              )
            ),
            fluidRow(
              column(
                6,
                plotOutput("orientation_rose_selected")
              ),
              column(
                6,
                plotOutput("plunge_rose_selected")
              )
            ),
            column(
              6,
              plotOutput("projection_xy_selected_se", inline = T),
              plotOutput("projection_xz_selected_se", inline = T),
              plotOutput("projection_yz_selected_se", inline = T)
            ),
            hr(),
            DT::dataTableOutput("selected_sample_summary"),
            hr()
          ),
          tabPanel(
            "Get selected data (Benn diagram)",
            h5(strong("Summary data of selected sample")),
            DT::dataTableOutput("click"),
            hr()
          ),
          tabPanel(
            "From projection",
            h5(strong("Interactive Benn diagram & new plots")),
            tags$br(),
            radioButtons("axis",
              "Choose what axis you want to plot",
              choices = c("XY", "XZ", "YZ"),
              inline = TRUE
            ),
            plotOutput("interactive_projection",
                       brush = brushOpts(
                         id = "plot1_brush")),
           # plotlyOutput("interactive_projection"),
            fluidRow(
              hr(),
              h5(strong("Selected points")),
              column(6,
                     plotOutput("selected_benn2")),
              column(6,
                    plotOutput("schmidt_selected2"))
            ),
            fluidRow(
              column(6,
                     plotOutput("orientation_rose_selected2")),
              column(6,
                     plotOutput("plunge_rose_selected2"))
            ),
            hr(),
            DT::dataTableOutput("selected_sample_summary2"),
            hr()
            # column(6,
            #        plotOutput("projection_xy_selected_se", inline = T),
            #        plotOutput("projection_xz_selected_se", inline = T),
            #        plotOutput("projection_yz_selected_se", inline = T)
            # )
          ),
          tabPanel(
            "Get selected data (projection)",
            h5(strong("Summary data of selected sample")),
            DT::dataTableOutput("click2"),
            hr()
          )
        )
      )
    )
  )
)



### --- server ---

server <- function(input, output, session) {
  # increase file upload size limit in shiny to 50MB
  options(shiny.maxRequestSize = 150 * 1024^2)

  # get the user data
  user_data <- reactive({
    req(input$user_data)
    inFile <- input$user_data
    if (input$decimal == ".") {
      data <- read.csv(inFile$datapath, sep = input$sep)
    } else {
      data <- read.csv2(inFile$datapath, sep = input$sep)
    }
    return(data)
  })

  # User can visualize his imported dataset
  output$user_data_head <- renderTable({
    req(input$user_data)
    head(user_data(), n = 5)
  })

  ######### Modifier colonne sample à l'inport, il faut que ce soit des characters sinon filter ne fonctionne pas
  # format the data to include all columns needed for the app' in a reactive function
  app_data <- reactive({
    req(input$user_data)
    req(input$data_type)
    if (input$data_type == "1) There are only angles") {
      user_data()
    } else if (input$data_type == "2) Only angles from DistoX2") {
      user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & direction < 180) ~ round(direction + 180, 0),
            (dip > 0 & direction > 180) ~ round(direction - 180, 0),
            dip < 0 ~ round(direction, 0)
          ),
          plunge = abs(round(dip, 0)),
          orientation_pi = dplyr::case_when(
            direction < 180 ~ round(direction, 0),
            direction > 180 ~ round(direction - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        )
    } else if (input$data_type == "3) From DistoX2 with a single set of coordinates") {
      user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & direction < 180) ~ round(direction + 180, 0),
            (dip > 0 & direction > 180) ~ round(direction - 180, 0),
            dip < 0 ~ round(direction, 0)
          ),
          plunge = abs(round(dip, 0)),
          orientation_pi = dplyr::case_when(
            direction < 180 ~ round(direction, 0),
            direction > 180 ~ round(direction - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = x) %>%
        dplyr::mutate(Y1 = y) %>%
        dplyr::mutate(Z1 = z) %>%
        dplyr::mutate(X2 = round(x + cos(rad(plunge)) * sin(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Y2 = round(y + cos(rad(plunge)) * cos(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Z2 = round(z - sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::select(-c(x, y, z, direction, dip))
    } else if (input$data_type == "5) With two sets of coordinates without angles") {
      user_data() %>%
        dplyr::mutate(plunge = round(plunge_and_bearing(user_data())[[1]], 0)) %>%
        dplyr::mutate(bearing = round(plunge_and_bearing(user_data())[[2]], 0)) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing < 180 ~ bearing,
            bearing > 180 ~ bearing - 180
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        )
    } else { # Single coordinate with angles
      user_data() %>%
        dplyr::rename(bearing = direction) %>%
        dplyr::rename(plunge = dip) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing < 180 ~ bearing,
            bearing > 180 ~ bearing - 180
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = x) %>%
        dplyr::mutate(Y1 = y) %>%
        dplyr::mutate(Z1 = z) %>%
        dplyr::mutate(X2 = round(x + cos(rad(plunge)) * sin(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Y2 = round(y + cos(rad(plunge)) * cos(rad(bearing)) * 0.04, 3)) %>%
        dplyr::mutate(Z2 = round(z - sin(rad(plunge)) * 0.04, 3))
    }
  })

  # The user sees the data included in the reactive function app_data() and can download it
  output$app_data_view <- DT::renderDataTable(
    DT::datatable(app_data(),
      options = list(
        lengthMenu = c(5, 10, 20, 50),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    ) %>%
      DT::formatRound(columns = sapply(colnames(app_data()), is.numeric), digits = 3)
  )

  # Creation of the data I will use in the examples files
  cpn_example <- reactive({
    data_example %>%
      dplyr::mutate(plunge = plunge_and_bearing(data_example)[, 1]) %>%
      dplyr::mutate(bearing = plunge_and_bearing(data_example)[, 2]) %>%
      dplyr::mutate(
        orientation_pi = dplyr::case_when(
          bearing < 180 ~ bearing,
          bearing > 180 ~ bearing - 180
        ),
        angle_double = dplyr::case_when(
          2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
          2 * orientation_pi <= 180 ~ 2 * orientation_pi
        )
      )
  })

  # if angles only
  angles_only <- reactive({
    cpn_example() %>%
      dplyr::select(sample, code, bearing, plunge)
  })

  # function to download the file if angle only
  output$angles_only <- downloadHandler(
    filename = function() {
      paste("There are only angles", ".csv")
    },
    content = function(file) {
      write.csv2(angles_only(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  #### tous les fichiers exemples à faire plus tard avec tous les modèles de fin ???

  ### --- Classical method ---

  # User chooses levels to display
  output$sample_c <- renderUI({
    req(input$user_data)
    radioButtons("sample_c", label = "Filter by sample", choices = unique(as.character(app_data()$sample)))
  })

  # User chooses codes to display
  output$code_c <- renderUI({
    req(input$user_data)
    if (input$plot_all_samples_c) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_c)
    }
    radioButtons("code_c", label = "Filter by code", choices = unique(app_data_temp$code))
  })

  # User chooses the model to display
  model_subset <- reactive({
    req(input$model)
    ternary_model <- ternary_model %>%
      dplyr::filter(Type %in% input$model)
  })

  # get the data selected by the user (filter by sample and code)
  subset_code_sample_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$sample_c)
    if (input$plot_all_samples_c & input$plot_all_codes_c) {
      data_sub <- app_data()
    } else if (input$plot_all_samples_c & !input$plot_all_codes_c) {
      data_sub <- subset(app_data(), code == input$code_c)
    } else if (!input$plot_all_samples_c & input$plot_all_codes_c) {
      data_sub <- subset(app_data(), sample == input$sample_c)
    } else {
      data_sub <- subset(app_data(), code == input$code_c & sample == input$sample_c)
    }
    return(data_sub)
  })

  # calculating benn indices for the user subset data
  benn_index_c <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$minimum_n) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )

      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)

        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))

          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])

          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }

      return(benn)
    }



    benn_ind <- data.frame(benn(subset_code_sample_c(), level = subset_code_sample_c()$sample)) %>%
      dplyr::mutate(PL = 1 - IS - EL)
    benn_ind$Sample <- rownames(benn_ind)
    return(benn_ind)
  })

  # benn_plot function
  benn_plot <- reactive({
    tern_model <- model_subset()

    p <- ggtern(tern_model, aes(as.numeric(PL), as.numeric(IS), as.numeric(EL))) +
      geom_polygon(aes(fill = Type, group = area, alpha = 0.1)) +
      scale_fill_jco() +
      ggplot2::labs(title = "", fill = "Process", x = "PL", y = "IS", z = "EL") +
      ggplot2::theme(
        tern.axis.line.T = element_line(color = "black", size = 1),
        tern.axis.line.L = element_line(color = "black", size = 1),
        tern.axis.line.R = element_line(color = "black", size = 1)
      ) +
      ggtern::theme_bw() +
      ggplot2::guides(alpha = "none", size = 0.5) +
      ggtern::scale_T_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_L_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_R_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::geom_Tline(Tintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Lline(Lintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Rline(Rintercept = 0.5, colour = "black", size = 0.2) +
      ggplot2::geom_point(data = benn_index_c(), aes(PL, IS, EL, colour = Sample)) +
      ggplot2::scale_color_brewer(palette = "Paired")

    return(p)
  })

  # print the benn plot
  output$benn <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(benn_plot())
  })

  # download the benn plot
  output$download_benn_c <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_c == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_c == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      print(benn_plot())
      dev.off()
    }
  )

  # orientation rose diagram
  orientation_rose_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    d <- subset_code_sample_c()$orientation_pi + 180
    e <- c(subset_code_sample_c()$orientation_pi, d)
    f <- circular(e,
      type = "angles", units = "degrees",
      template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })

  # plot orientation_rose_c
  output$orientation_rose_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(orientation_rose_c())
  })

  # plunge rose diagram
  plunge_rose_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(subset_code_sample_c()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })

  # plot plunge_rose_c
  output$plunge_rose_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(plunge_rose_c())
  })

  # download rose diagram
  output$download_orientation_rose_c <- downloadHandler(
    filename = function() {
      paste("rose_diagram", input$file_format_rose_orientation, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_orientation == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_orientation == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(orientation_rose_c())
      dev.off()
    }
  )

  # download rose plunge
  output$download_plunge_rose_c <- downloadHandler(
    filename = function() {
      paste("rose_diagram", input$file_format_rose_plunge, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_plunge == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_plunge == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(plunge_rose_c())
      dev.off()
    }
  )

  # schmidt diagram
  schmidt_diagram_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(subset_code_sample_c()$bearing,
      subset_code_sample_c()$plunge,
      pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })

  # plot schmidt_diagram_c
  output$schmidt_diagram_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(schmidt_diagram_c())
  })

  output$download_schmidt_c <- downloadHandler(
    filename = function() {
      paste("schmidt_diagram", input$file_format_schmidt_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_c == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_c == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(schmidt_diagram_c())
      dev.off()
    }
  )

  # data for summary table
  summary_data <- reactive({
    level <- unique(subset_code_sample_c()$sample)
    results <- data.frame(matrix(nrow = length(level), ncol = 5))
    for (i in level) {
      data_level <- subset(subset_code_sample_c(), sample == i)
      if (nrow(data_level >= input$minimum_n)) {
        R1 <- circular::rayleigh.test(2 * circular::circular(data_level$orientation_pi,
          type = "angles",
          units = "degrees",
          modulo = "pi"
        ))
        R2 <- circular::rayleigh.test(2 * circular::circular(data_level$angle_double,
          type = "angles",
          units = "degrees",
          modulo = "pi"
        ))
        L <- round(R1$statistic * 100, 2)
        p <- round(R1$p.value, 2)
        L.double <- round(R2$statistic * 100, 2)
        p.double <- round(R2$p.value, 2)
        # SphVar <- varSph(data_level$bearing, data_level$plunge)
        results[i, ] <- c(i, L, p, L.double, p.double)
        column_names <- c("sample", "L", "R.p", "L.double", "R.p.double")
        colnames(results) <- column_names
      }
    }

    benn_indices <- benn_index_c() %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::rename(sample = Sample)
    table <- merge(x = benn_indices, y = results, by = "sample")
    return(table)
  })

  # Summary data table with all indices and orientation tests
  output$summary_table_c <- DT::renderDataTable(
    DT::datatable(summary_data(),
      options = list(
        lengthMenu = c(10, 20, 50),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    )
  )

  ### -- spatialised method --

  # User chooses levels to display
  output$sample_sm <- renderUI({
    req(input$user_data)
    radioButtons("sample_sm", label = "Filter by sample", choices = unique(app_data()$sample))
  })

  # User chooses codes to display
  output$code_sm <- renderUI({
    req(input$user_data)
    # if(input$plot_all_samples_sm){
    #   app_data_temp <- app_data()
    # } else {
    app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_sm)
    # }
    radioButtons("code_sm", label = "Filter by code", choices = unique(app_data_temp$code))
  })

  # get the data selected by the user (filter by sample and code)
  subset_code_sample_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$sample_sm)
    # if (input$plot_all_samples_sm & input$plot_all_codes_sm) {
    #   data_sub <- app_data()
    # } else if (input$plot_all_samples_sm & !input$plot_all_codes_sm) {
    #   data_sub <- subset(app_data(), code == input$code_sm)
    # } else if (!input$plot_all_samples_sm & input$plot_all_codes_sm) {
    #   data_sub <- subset(app_data(), sample == input$sample_sm)
    # } else {
    #   data_sub <- subset(app_data(), code == input$code_sm & sample == input$sample_sm)
    # }

    if (input$plot_all_codes_sm) {
      data_sub <- subset(app_data(), sample == input$sample_sm)
    } else {
      data_sub <- subset(app_data(), sample == input$sample_sm & code == input$code_sm)
    }

    return(data_sub)
  })

  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))

  # calculating benn indices for the user subset data
  benn_index_sm <- reactive({
    req(input$n_nearest_sm)

    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_sm) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )

      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)

        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }

      return(benn)
    }


    spatial_benn <- function(xyz, nearest = input$n_nearest_sm, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )

      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }

      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]

      return(list(benn))
    }


    benn_sm <- data.frame(spatial_benn(subset_code_sample_sm())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)

    return(benn_sm)
  })

  # make a function to plot spatial benn ternary
  benn_sm_plot <- reactive({
    req(input$user_data)
    req(input$data_type)
    data_to_plot <- benn_index_sm()

    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    recordPlot()
  })

  output$benn_sm <- renderPlot({
    benn_sm_plot()
  })

  ortho_xy_sm <- reactive({
    ortho <- raster::stack(input$ortho_xy_sm$datapath)
    return(ortho)
  })

  ortho_xz_sm <- reactive({
    ortho <- raster::stack(input$ortho_xz_sm$datapath)
    return(ortho)
  })

  ortho_yz_sm <- reactive({
    ortho <- raster::stack(input$ortho_yz_sm$datapath)
    return(ortho)
  })

  # XY spatial projection with points
  projection_points_xy_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xy_sm()@extent@xmin,
          ortho_xy_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_sm()@extent@ymin,
          ortho_xy_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with points
  projection_points_xz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xz_sm()@extent@xmin,
          ortho_xz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_sm()@extent@ymin,
          ortho_xz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xz)
  })

  # YZ spatial projection with points
  projection_points_yz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_yz_sm()@extent@xmin,
          ortho_yz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_sm()@extent@ymin,
          ortho_yz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(yz)
  })

  # XY spatial projection with sticks
  projection_sticks_xy_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Y1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Y1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xy_sm()@extent@xmin,
          ortho_xy_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_sm()@extent@ymin,
          ortho_xy_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with sticks
  projection_sticks_xz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xz_sm()@extent@xmin,
          ortho_xz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_sm()@extent@ymin,
          ortho_xz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(xz)
  })

  # YZ spatial projection with sticks
  projection_sticks_yz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$Y1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$Y2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_sm(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$Y1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$Y2,
          yend = subset_code_sample_sm()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_yz_sm()@extent@xmin,
          ortho_yz_sm()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_sm()@extent@ymin,
          ortho_yz_sm()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(yz)
  })

  # xy plot with colored points or sticks
  output$projection_xy_sm <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(req(projection_points_xy_sm()))
      } else {
        print(req(projection_sticks_xy_sm()))
      }
    },
    height = 500,
    width = 700
  )

  # xz plot with colored points or sticks
  output$projection_xz_sm <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(projection_points_xz_sm())
      } else {
        print(projection_sticks_xz_sm())
      }
    },
    height = 300,
    width = 780
  )

  # yz plot with colored points or sticks
  output$projection_yz_sm <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_sm) {
        print(projection_points_yz_sm())
      } else {
        print(projection_sticks_yz_sm())
      }
    },
    height = 300,
    width = 780
  )

  # dowload benn diagram
  output$download_benn_sm <- downloadHandler(
    filename = function() {
      paste("benn_diagram", input$file_format_benn_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      replayPlot(benn_sm_plot())
      dev.off()
    }
  )

  # download xy projection
  output$download_xy_sm <- downloadHandler(
    filename = function() {
      paste("XY projection", input$file_format_xy_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xy_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xy_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_xy_sm())
        dev.off()
      } else {
        print(projection_sticks_xy_sm())
        dev.off()
      }
    }
  )

  # download xz projection
  output$download_xz_sm <- downloadHandler(
    filename = function() {
      paste("XZ projection", input$file_format_xz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xz_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xz_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_xz_sm())
        dev.off()
      } else {
        print(projection_sticks_xz_sm())
        dev.off()
      }
    }
  )

  # download xz projection
  output$download_yz_sm <- downloadHandler(
    filename = function() {
      paste("YZ projection", input$file_format_yz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_yz_sm == "png") {
        png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_yz_sm == "pdf") {
        pdf(file)
      } else {
        jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        print(projection_points_yz_sm())
        dev.off()
      } else {
        print(projection_sticks_yz_sm())
        dev.off()
      }
    }
  )

  # summary data at series scale
  summary_data_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    spatial_bearing <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }


    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }

    data <- subset_code_sample_sm()
    benn_indices <- round(benn_index_sm(), 2)
    rayleigh_test <- round(data.frame(spatial_bearing(data)), 2)
    rayleigh_double <- round(data.frame(spatial_bearing_double(data)), 2)

    summary_data <- cbind(data, benn_indices, rayleigh_test, rayleigh_double)
    return(summary_data)
  })

  # Summary data table with all indices and orientation tests for series
  output$summary_table_sm <- DT::renderDataTable(
    DT::datatable(summary_data_sm(),
      options = list(
        lengthMenu = c(10, 20, 50),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    )
  )

  # summary data at sample scale
  summary_data2_sm <- reactive({
    req(input$user_data)
    req(input$data_type)

    data <- summary_data_sm()
    level <- unique(summary_data_sm()$sample)
    results <- data.frame(matrix(nrow = length(level), ncol = 12))

    for (i in level) {
      data_level <- subset(data, sample == i)

      if (nrow(data_level >= input$minimum_n)) {
        N <- nrow(data_level)
        IS <- round(mean(data_level$IS), 2)
        EL <- round(mean(data_level$EL), 2)
        PL <- round(mean(data_level$PL), 2)
        sd.IS <- round(var(data_level$IS), 2)
        sd.EL <- round(var(data_level$EL), 2)
        sd.PL <- round(var(data_level$PL), 2)
        L <- round(mean(data_level$L), 2)
        Tr <- round(sum(data_level$R.p < 0.05) * 100 / nrow(data_level), 2)
        L.double <- round(mean(data_level$L.double), 2)
        Tr.double <- round(sum(data_level$R.p.double < 0.05) * 100 / nrow(data_level), 2)

        results[i, ] <- c(i, N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
        column_names <- c("sample", "N", "IS", "EL", "PL", "sd.IS", "sd.EL", "sd.PL", "L", "Tr", "L.double", "Tr.double")
        colnames(results) <- column_names
      }
    }
    level_data <- data.frame(sample = level)
    results_def <- merge(level_data, results, by = "sample")
    return(results_def)
  })

  # Summary data table2 with all indices and orientation tests at sample scale
  output$summary_table2_sm <- DT::renderDataTable(
    DT::datatable(summary_data2_sm(),
      options = list(
        lengthMenu = c(10, 20, 50),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    )
  )

  ### --- spatial exploration ---

  # User chooses levels to display
  output$sample_se <- renderUI({
    req(input$user_data)
    radioButtons("sample_se", label = "Filter by sample", choices = unique(app_data()$sample))
  })

  # User chooses codes to display
  output$code_se <- renderUI({
    req(input$user_data)
    if (input$plot_all_samples_se) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_se)
    }
    radioButtons("code_se", label = "Filter by code", choices = unique(app_data_temp$code))
  })

  # get the data selected by the user (filter by sample and code)
  subset_code_sample_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$sample_se)
    if (input$plot_all_samples_se & input$plot_all_codes_se) {
      data_sub <- app_data()
    } else if (input$plot_all_samples_se & !input$plot_all_codes_se) {
      data_sub <- subset(app_data(), code == input$code_se)
    } else if (!input$plot_all_samples_se & input$plot_all_codes_se) {
      data_sub <- subset(app_data(), sample == input$sample_se)
    } else {
      data_sub <- subset(app_data(), code == input$code_se & sample == input$sample_se)
    }
    return(data_sub)
  })

  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))

  # calculating benn indices for the user subset data
  benn_index_se <- reactive({
    req(input$n_nearest_se)

    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }

    spatial_benn <- function(xyz, nearest = input$n_nearest_se, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      return(list(benn))
    }
    # }

    # spatial_bearing = function(xyz, nearest = input$n_nearest_se) {
    #
    #   # Make a place to hold the computed mean bearings of nearest neighbors
    #   near_avg_bearing_p = vector(mode = "numeric", length = nrow(xyz))
    #   near_avg_bearing_Rbar = vector(mode = "numeric", length = nrow(xyz))
    #
    #   # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
    #   for (k in 1:nrow(xyz)) {
    #     centerx = xyz$X1[k]
    #     centery = xyz$Y1[k]
    #     centerz = xyz$Z1[k]
    #     d = sqrt((centerx-xyz$X1)^2 + (centery-xyz$Y1)^2 + (centerz-xyz$Z1)^2)
    #     xyz_subsample = xyz[order(d)[1:nearest],]
    #
    #     # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
    #     near_avg_bearing_p[k] = round(r.test(2*xyz_subsample$orientation_pi, degree=TRUE)$p.value, 2)
    #     near_avg_bearing_Rbar[k] = round(r.test(2*xyz_subsample$orientation_pi, degree=TRUE)$r.bar, 2)
    #   }
    #
    #   # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
    #   return(list(L = near_avg_bearing_Rbar*100, R.p = near_avg_bearing_p)) }
    #
    #
    # spatial_bearing_double = function(xyz, nearest = input$n_nearest_se) {
    #
    #   # Make a place to hold the computed mean bearings of nearest neighbors
    #   near_avg_bearing_p = vector(mode = "numeric", length = nrow(xyz))
    #   near_avg_bearing_Rbar = vector(mode = "numeric", length = nrow(xyz))
    #
    #   # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
    #   for (k in 1:nrow(xyz)) {
    #     centerx = xyz$X1[k]
    #     centery = xyz$Y1[k]
    #     centerz = xyz$Z1[k]
    #     d = sqrt((centerx-xyz$X1)^2 + (centery-xyz$Y1)^2 + (centerz-xyz$Z1)^2)
    #     xyz_subsample = xyz[order(d)[1:nearest],]
    #
    #     # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
    #     near_avg_bearing_p[k] = round(r.test(2*xyz_subsample$angle_double,degree=TRUE)$p.value, 2)
    #     near_avg_bearing_Rbar[k] = round(r.test(2*xyz_subsample$angle_double,degree=TRUE)$r.bar, 2)
    #   }

    # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
    # return(list(L.double = near_avg_bearing_Rbar*100, R.p.double = near_avg_bearing_p)) }

    # rayleigh_test <- round(data.frame(spatial_bearing(subset_code_sample_se())), 2)
    # rayleigh_double <- round(data.frame(spatial_bearing_double(subset_code_sample_se())), 2)

    benn_se <- data.frame(spatial_benn(subset_code_sample_se())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)

    data <- cbind(subset_code_sample_se(), benn_se) # , rayleigh_test, rayleigh_double)
    return(data)
  })

  # interactive ternary diagram
  interactive_ternary_plot <- reactive({
    data_to_plot <- benn_index_se()

    colors <- data_to_plot %>%
      dplyr::select(IS, EL, PL) %>%
      dplyr::mutate(color_code = rgb(IS, EL, PL)) %>%
      dplyr::select(color_code)
    colors_p <- setNames(colors$color_code, data_to_plot$id)

    fig <- plot_ly(
      data = data_to_plot,
      a = ~IS, b = ~PL, c = ~EL,
      customdata = ~id,
      type = "scatterternary",
      mode = "markers",
      marker = list(
        size = 5,
        color = colors_p
      ),
      text = ~ paste("sample:", sample, "<br>code", code)
    ) %>%
      layout(
        title = "",
        ternary = list(
          aaxis = list(title = "IS"),
          baxis = list(title = "PL"),
          caxis = list(title = "EL")
        )
      )

    event_register(fig, "plotly_selected")

    return(fig)
  })

  # plot the interactive ternary
  output$interactive_ternary <- renderPlotly({
    interactive_ternary_plot()
  })

  # download XY ortho path
  ortho_xy_se <- reactive({
    ortho <- raster::stack(input$ortho_xy_se$datapath)
    return(ortho)
  })

  # download XZ ortho path
  ortho_xz_se <- reactive({
    ortho <- raster::stack(input$ortho_xz_se$datapath)
    return(ortho)
  })

  # download YY ortho path
  ortho_yz_se <- reactive({
    ortho <- raster::stack(input$ortho_yz_se$datapath)
    return(ortho)
  })

  # get in a reactive function the selected data
  selected_data <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }

    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- matrix(NA,
        nrow = nrow(xyz), ncol = 2,
        dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )

      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }

      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]

      return(list(benn))
    }

    plotly_event_data <- event_data(event = "plotly_selected", priority = "event")
    req(plotly_event_data)

    selected <- filter(benn_index_se(), id %in% plotly_event_data$customdata) %>%
      dplyr::select(-c(EL, IS, PL))

    new_benn <- cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      mutate(PL = 1 - EL - IS)

    return(new_benn)
  })

  # plot the selected data in a new benn diagram
  output$selected_benn <- renderPlot({
    data_to_plot <- selected_data() %>%
      dplyr::select(EL, IS, PL)

    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
  })

  # orientation rose diagram for selected data
  orientation_rose_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    d <- selected_data()$orientation_pi + 180
    e <- c(selected_data()$orientation_pi, d)
    f <- circular(e,
      type = "angles", units = "degrees",
      template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })

  # plot orientation_rose_se for selected data
  output$orientation_rose_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(orientation_rose_se())
  })

  # plunge rose diagram for selected data
  plunge_rose_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(selected_data()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })

  # plot plunge_rose_c for selected data
  output$plunge_rose_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(plunge_rose_se())
  })

  # schmidt diagram for selected data
  schmidt_diagram_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(selected_data()$bearing,
      selected_data()$plunge,
      pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })

  # plot schmidt_diagram_c for selected data
  output$schmidt_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(schmidt_diagram_se())
  })

  # XY spatial projection with points
  projection_points_xy_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        data = selected_data(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xy_se()@extent@xmin,
          ortho_xy_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_se()@extent@ymin,
          ortho_xy_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with points
  projection_points_xz_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho_se) {
      xz <- ggplot2::ggplot(
        data = selected_data(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_xz_se()@extent@xmin,
          ortho_xz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_se()@extent@ymin,
          ortho_xz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xz)
  })

  # YZ spatial projection with points
  projection_points_yz_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        data = selected_data(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed(ratio = 1)
    } else {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::xlim(
          ortho_yz_se()@extent@xmin,
          ortho_yz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_se()@extent@ymin,
          ortho_yz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(yz)
  })

  # XY spatial projection with sticks
  projection_sticks_xy_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Y1,
          xend = selected_data()$X2,
          yend = selected_data()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Y1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xy_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Y1,
          xend = selected_data()$X2,
          yend = selected_data()$Y2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xy_se()@extent@xmin,
          ortho_xy_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xy_se()@extent@ymin,
          ortho_xy_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }
    return(xy)
  })

  # XZ spatial projection with sticks
  projection_sticks_xz_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_xz_ortho_se) {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Z1,
          xend = selected_data()$X2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        selected_data(),
        aes(X1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_xz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Z1,
          xend = selected_data()$X2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_xz_se()@extent@xmin,
          ortho_xz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_xz_se()@extent@ymin,
          ortho_xz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(xz)
  })

  # YZ spatial projection with sticks
  projection_sticks_yz_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data_to_plot_bis <- selected_data()
    benn_rgb <- cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )

    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$Y1,
          y = selected_data()$Z1,
          xend = selected_data()$Y2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        selected_data(),
        aes(Y1, Z1)
      ) +
        RStoolbox::ggRGB(
          img = ortho_yz_se(),
          r = 1, g = 2, b = 3,
          maxpixels = 500000,
          ggLayer = TRUE,
          ggObj = TRUE,
          coord_equal = TRUE
        ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$Y1,
          y = selected_data()$Z1,
          xend = selected_data()$Y2,
          yend = selected_data()$Z2,
          col = rgb(round(benn_rgb[, 1:3], 0),
            maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::xlim(
          ortho_yz_se()@extent@xmin,
          ortho_yz_se()@extent@xmax
        ) +
        ggplot2::ylim(
          ortho_yz_se()@extent@ymin,
          ortho_yz_se()@extent@ymax
        ) +
        ggplot2::theme_minimal()
    }

    return(yz)
  })

  # xy plot with colored points or sticks
  output$projection_xy_selected_se <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(req(projection_points_xy_se()))
      } else {
        print(req(projection_sticks_xy_se()))
      }
    },
    height = 700,
    width = 700
  )

  # xz plot with colored points or sticks
  output$projection_xz_selected_se <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(projection_points_xz_se())
      } else {
        print(projection_sticks_xz_se())
      }
    },
    height = 300,
    width = 780
  )

  # yz plot with colored points or sticks
  output$projection_yz_selected_se <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)

      if (input$plot_type_se) {
        print(projection_points_yz_se())
      } else {
        print(projection_sticks_yz_se())
      }
    },
    height = 300,
    width = 780
  )

  # get selected data and add rayleigh tests
  summary_selected_data <- reactive({
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }


    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))

      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]

        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }

      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }

    # plotly_event_data <- event_data(event = 'plotly_selected', priority = "event")
    # req(plotly_event_data)
    selected <- selected_data() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)

    rayleigh_test <- round(data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- round(data.frame(spatial_bearing_double(selected)), 2)

    summary_data <- cbind(selected, rayleigh_test, rayleigh_double)
    return(summary_data)
  })

  #

  # summary data at sample scale
  summary_sample_se <- reactive({
    req(input$user_data)
    req(input$data_type)

    data <- summary_selected_data()

    N <- nrow(data)
    IS <- round(mean(data$IS), 2)
    EL <- round(mean(data$EL), 2)
    PL <- round(mean(data$PL), 2)
    sd.IS <- round(sd(data$IS), 2)
    sd.EL <- round(sd(data$EL), 2)
    sd.PL <- round(sd(data$PL), 2)
    L <- round(mean(data$L), 2)
    Tr <- round(sum(data$R.p < 0.05) * 100 / nrow(data), 2)
    L.double <- round(mean(data$L.double), 2)
    Tr.double <- round(sum(data$R.p.double < 0.05) * 100 / nrow(data), 2)

    result <- data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
    return(result)
  })

  # summary statistics of selected data
  output$selected_sample_summary <- DT::renderDataTable(
    DT::datatable(summary_sample_se(),
      options = list(
        lengthMenu = c(1),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    )
  )

  # get selected data
  output$click <- DT::renderDataTable(
    DT::datatable(summary_selected_data(),
      options = list(
        lengthMenu = c(10, 20, 50),
        scrollX = TRUE,
        buttons = c("csv", "excel"),
        server = FALSE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      extensions = "Buttons"
    )
  )

  # reactive function for interactive projection
  # interactive_projection_plot <- reactive({
  #   data_to_plot <- benn_index_se()
  # 
  #   colors <- data_to_plot %>%
  #     dplyr::select(IS, EL, PL) %>%
  #     dplyr::mutate(color_code = rgb(IS, EL, PL)) %>%
  #     dplyr::select(color_code)
  #   colors_p <- setNames(colors$color_code, data_to_plot$id)
  # 
  #   if (input$axis == "XY") {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~X1, y = ~Y1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  #     
  #     # fig <- ggplot(data_to_plot, aes(x = X1, y = Y1)) + 
  #     #   geom_point() 
  #     # 
  #     # fig <- fig + coord_fixed(ratio = 1) + theme_minimal()
  #     # 
  #     # fig <- ggplotly(fig)
  # 
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   } else if (input$axis == "XZ") {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~X1, y = ~Z1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  # 
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   } else {
  #     fig <- plot_ly(
  #       data = data_to_plot,
  #       x = ~Y1, y = ~Z1,
  #       customdata = ~id,
  #       mode = "markers",
  #       marker = list(
  #         size = 5,
  #         color = colors_p
  #       ),
  #       text = ~ paste("sample:", sample, "<br>code", code)
  #     )
  # 
  #     plotly_event_data <- event_register(fig, "plotly_selected")
  #   }
  #   return(fig)
  # })
  
  
  #reactive function for interactive projection
  interactive_projection_plot <- reactive({
    benn_index_se()
    
    if (input$axis == "XY") {
  fig <- ggplot(benn_index_se(), aes(x = X1, y = Y1)) +
    ggplot2::geom_point(
      shape = 21, col = "black",
      fill = rgb(round(data.frame(benn_index_se()$IS*255, 
                       benn_index_se()$EL*255, 
                       benn_index_se()$PL*255), 0), 
                 maxColorValue=255
      ),
      size = 2.3,
      stroke = 0.3
    ) +
    coord_fixed(ratio=1) +
    theme_minimal()

    } else if (input$axis == "XZ") {
      fig <- ggplot(benn_index_se(), aes(x = X1, y = Z1)) +
        geom_point(
          shape = 21, col = "black",
          fill = rgb(round(data.frame(benn_index_se()$IS*255, 
                                      benn_index_se()$EL*255, 
                                      benn_index_se()$PL*255), 0), 
                     maxColorValue=255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
  coord_fixed(ratio=1) +
  theme_minimal()

    } else {
      fig <- ggplot(benn_index_se(), aes(x = Y1, y = Z1)) +
        geom_point(
                shape = 21, col = "black",
      fill = rgb(round(data.frame(benn_index_se()$IS*255, 
                       benn_index_se()$EL*255, 
                       benn_index_se()$PL*255), 0), 
                 maxColorValue=255
      ),
      size = 2.3,
      stroke = 0.3
        ) +
  coord_fixed(ratio=1) +
  theme_minimal()
      
    }
    return(fig)
})

  # render the plot
  # output$interactive_projection <- renderPlotly({
  #   interactive_projection_plot()
  # })
  output$interactive_projection <- renderPlot({
    interactive_projection_plot()
  })

  
  # get in a reactive function the selected data
  selected_data2 <- reactive({
    
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- matrix(
        nrow = length(unique(level)), ncol = 6,
        dimnames = list(unique(level), c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }
    
    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- matrix(NA,
                     nrow = nrow(xyz), ncol = 2,
                     dimnames = list(rownames(xyz), c("elongation", "isotropy"))
      )
      
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }
      
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      
      return(list(benn))
    }
    
    # plotly_event_data <- event_data(event = "plotly_selected", priority = "event")
    # req(plotly_event_data)
    
    plotly_event_data <- brushedPoints(benn_index_se(), input$plot1_brush)
    req(plotly_event_data)
    
    selected <- filter(benn_index_se(), id %in% plotly_event_data$id) %>%
      dplyr::select(-c(EL, IS, PL))
    
    new_benn <- cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      mutate(PL = 1 - EL - IS)
    
    return(new_benn)
  })
  
  # plot the selected data in a new benn diagram
  output$selected_benn2 <- renderPlot({
    data_to_plot <- selected_data2() %>%
      dplyr::select(EL, IS, PL)
    
    tst <- function() {
      TernaryPlot(
        atip = "IS", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      ColourTernary(cols(), spectrum = NULL)
      AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
  })
  
  # orientation rose diagram for selected data
  orientation_rose_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    d <- selected_data2()$orientation_pi + 180
    e <- c(selected_data2()$orientation_pi, d)
    f <- circular(e,
                  type = "angles", units = "degrees",
                  template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    recordPlot()
  })
  
  # plot orientation_rose_se for selected data
  output$orientation_rose_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(orientation_rose_se2())
  })
  
  # plunge rose diagram for selected data
  plunge_rose_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(selected_data2()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    recordPlot()
  })
  
  # plot plunge_rose_c for selected data
  output$plunge_rose_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(plunge_rose_se2())
  })
  
  # schmidt diagram for selected data
  schmidt_diagram_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(selected_data2()$bearing,
                         selected_data2()$plunge,
                         pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    recordPlot()
  })
  
  # plot schmidt_diagram_c for selected data
  output$schmidt_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    print(schmidt_diagram_se2())
  })
  
  # get selected data and add rayleigh tests
  summary_selected_data2 <- reactive({
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }
    
    
    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- vector(mode = "numeric", length = nrow(xyz))
      near_avg_bearing_Rbar <- vector(mode = "numeric", length = nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- round(r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      return(list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }
    
    selected <- selected_data2() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)
    
    rayleigh_test <- round(data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- round(data.frame(spatial_bearing_double(selected)), 2)
    
    summary_data <- cbind(selected, rayleigh_test, rayleigh_double)
    return(summary_data)
  })
  
  #
  
  # summary data at sample scale
  summary_sample_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data <- summary_selected_data2()
    
    N <- nrow(data)
    IS <- round(mean(data$IS), 2)
    EL <- round(mean(data$EL), 2)
    PL <- round(mean(data$PL), 2)
    sd.IS <- round(sd(data$IS), 2)
    sd.EL <- round(sd(data$EL), 2)
    sd.PL <- round(sd(data$PL), 2)
    L <- round(mean(data$L), 2)
    Tr <- round(sum(data$R.p < 0.05) * 100 / nrow(data), 2)
    L.double <- round(mean(data$L.double), 2)
    Tr.double <- round(sum(data$R.p.double < 0.05) * 100 / nrow(data), 2)
    
    result <- data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
    return(result)
  })
  
  # summary statistics of selected data
  output$selected_sample_summary2 <- DT::renderDataTable(
    DT::datatable(summary_sample_se(),
                  options = list(
                    lengthMenu = c(1),
                    scrollX = TRUE,
                    buttons = c("csv", "excel"),
                    server = FALSE,
                    dom = "Bfrtip"
                  ),
                  rownames = FALSE,
                  extensions = "Buttons"
    )
  )
  
  # get selected data
  output$click2 <- DT::renderDataTable(
    DT::datatable(summary_selected_data2(),
                  options = list(
                    lengthMenu = c(10, 20, 50),
                    scrollX = TRUE,
                    buttons = c("csv", "excel"),
                    server = FALSE,
                    dom = "Bfrtip"
                  ),
                  rownames = FALSE,
                  extensions = "Buttons"
    )
  )
  
  
  
} # end of server !!!

### --- Run the application ---
shinyApp(ui = ui, server = server)
