### --- ui ---
app_ui <- function() {
  
  addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "fabryka")
  )
  
  ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    p(strong("fabryka 1.1")),
    tabPanel(
      "Presentation",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("Presentation"),
               hr(),
               p("fabryka is a R shiny application dedicated to the fabric analysis of archaeological remains. The app' is developped by Marc THOMAS", a(href = "https://orcid.org/0000-0002-8160-1910", strong("orcid.")), "This is the version", strong("1.1")),
               p("An article and the source code are available via the links respectively", a(href = "https://zenodo.org/records/15236695", strong("here")), "and", a(href = "https://github.com/marchaeologist/fabryka/tree/main", strong("here."))),
               tags$br(),
               p(strong("The 'Upload data'"), "panel allows the user to upload different data sources."),
               tags$br(),
               p(strong("The 'Classical method'"), "panel contains subpanels to perform:"),
               tags$li("Benn diagrams with possible integration of Bertran & Lenoble model (2002)"),
               tags$li("Rose diagrams for axial data and dip"),
               tags$li("Schmidt diagrams"),
               tags$li("Woodcock diagrams"),
               tags$li("Summary data table with orientation statistical tests."),
               tags$br(),
               p(strong("The 'Spatialised method'"), "panel contains subpanels to perform:"),
               tags$li("Benn diagrams with Benn indices computed with nearest neighbors points (McPherron 2018)"),
               tags$li("Spatial projections of the points according to their fabric shape"),
               tags$li("Summary data tables with orientation statistical tests."),
               tags$br(),
               p(strong("The 'Spatial exploration'"), "panel contains subpanels to perform:"),
               tags$li("A spatial exploration of your data starting from a Benn diagram"),
               tags$li("A spatial exploration of your data starting from the spatial projections"),
               tags$li("Summary data tables with orientation statistical tests."),
               tags$br(),
               p(strong("The 'Markdown report'"), "panel allows to download automatic reports of your data."),
               tags$br(),
               p(strong("The 'Tutorial'"), "panel contains a summary of the mathematics, statistics and methods used in fabryka and a short tutorial."),
               tags$br(),
               p(strong("The 'New models'"), "panel is in progress. It will contain new Benn diagram models of natural processes for the spatialised method."),
               tags$br(),
               p(strong("The 'Bibliography'"), "panel contains the bibliography cited in the application."),
               tags$br(),
               p(strong("The 'Contributors'"), "section contains acknowledgements and a list of people who have contributed to the creation of the application."),
               tags$br(),
               p(strong("The 'Citation'"), "describes how to cite the application, the methods and datasets used."),
               tags$br(),
               hr(),
               tags$br()
        )
      )
    ),
    tabPanel(
      "Upload data",
      sidebarPanel(
        fileInput("user_data", 
                  strong("Upload your data file (.csv)"), 
                  multiple = FALSE, 
                  accept = ".csv"),
        tags$br(),
        radioButtons("sep", 
                     strong("Column separator"),
                     choices = base::c(Semicolon = ";", Comma = ",", Tab = "\t"),
                     selected = ";",
                     inline = TRUE
        ),
        tags$br(),
        radioButtons("decimal", strong("Decimal separator"),
                     choices = base::c(Comma = ",", Point = "."),
                     selected = ",",
                     inline = TRUE
        ),
        hr(),
        radioButtons("data_type",
                     label = strong("What form does your dataset take?"),
                     choices = base::c(
                       "Case 1: Only angles",
                       "Case 2: Only angles from DistoX2",
                       "Case 3: Angles from DistoX2 with coordinates",
                       "Case 4: Angles and coordinates",
                       "Case 5: Two shots data without angles"
                     ),
                     selected = "Case 1: Only angles",
        ),
        hr(),
        p(strong("Example files for each case via the links below"), style = "font-size: 12px;"),
        downloadLink("angles_only", "Case 1: Only angles"),
        tags$br(),
        downloadLink("angles_only_disto", "Case 2: Only angles from DistoX2"),
        tags$br(),
        downloadLink("disto_one_shot", "Case 3: Angles from DistoX2 with coordinates"),
        tags$br(),
        downloadLink("angles_one_shot", "Case 4: Angles and coordinates"),
        tags$br(),
        downloadLink("two_shots", "Case 5: Two shots data without angles"),
        tags$br(),
      ),
      mainPanel(
        p(strong("First, upload your .csv file in the left sidebar panel. Pay attention to your decimal and column delimiters."),
        p(style = "text-align: justify;", "The simplest method to upload data in the application is to name the column names directly, as shown in the example files in the side bar panel on the left. Enter your data in a .csv file, adding the columns \'id\', \'code\' and \'sample\'. If the columns don't exist, fill them in with the same letter. By doing so, you do not need to select your variables in the main panel but just to tick the right form (i.e. case) your data takes. The application correctly reads the data file directly. You will find a tutorial in the application (i.e. panel \'Tutorial\') that explains how to load your data according to format. If an error appears at the bottom of this page, that means your file is in the wrong format.")),
        tags$hr(),
        h5(strong("View of the first line of your dataset")),
        tableOutput("user_data_head"),
        tags$hr(),
        p(strong("Depending on the source and form of your data, select the appropriate columns.")),
        p(strong("In any case,"), "whatever the form of your dataset you can select your corresponding context columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("id1")
          ),
          column(
            4,
            uiOutput("code1")
          ),
          column(
            4,
            uiOutput("sample1")
          )
        ),
        tags$br(),
        p("In", strong("cases 1, 2, 3"), "and", strong("4,"), "you must select your 'orientation' and 'dip' columns. If your orientation and dip data come from a DistoX2, they will be converted to classical angles (by checking cases 2 or 3)."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("orientation1")
          ),
          column(
            4,
            uiOutput("dip1")
          ),
          column(
            4,
          )
        ),
        tags$br(),
        p("In", strong("cases 3,"), strong("4"), "and", strong("5,"), "you must select your x, y and z coordinates columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("x1")
          ),
          column(
            4,
            uiOutput("y1")
          ),
          column(
            4,
            uiOutput("z1")
          )
        ),
        tags$br(),
        p("In", strong("case 5,"), "you must select your X2, Y2 and Z2 coordinates columns."),
        tags$br(),
        fluidRow(
          column(
            4,
            uiOutput("x2")
          ),
          column(
            4,
            uiOutput("y2")
          ),
          column(
            4,
            uiOutput("z2")
          )
        ),
        hr(),
        h5(strong("View of the data that will be used in the application")),
        tags$br(),
        DT::dataTableOutput("app_data_view"),
        hr(),
        tags$br()
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
            checkboxInput("plot_all_codes_c", strong("No code distinction"), TRUE),
            uiOutput("code_c")
          )
        ),
        hr(),
        sliderInput("minimum_n",
                    strong("Minimum number of measures in samples to compute Benn diagram and summary table statistics"),
                    min = 10,
                    max = 60,
                    value = 40
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Benn diagram",
            h5(strong("Benn diagram")),
            plotOutput("benn"),
            hr(),
            checkboxGroupInput("model", strong("Build your model"),
                               choices = c("none", unique(ternary_model$Type)),
                               selected = "none",
                               inline = TRUE
            ),
            tags$br(),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the Download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_benn_c",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                )
              ),
              column(
                6,
                downloadButton("download_benn_c", "Benn diagram")
              )
            ),
            hr(),
            tags$br(),
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
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Orientation Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_orientation",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_orientation_rose_c", "Orientation rose diagram")
              ),
              column(
                6,
                # h5(strong("Dip Rose Diagram")),
                radioButtons(
                  inputId = "file_format_rose_plunge",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_plunge_rose_c", "Dip rose diagram")
              )
            ),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Schmidt diagram",
            h5(strong("Schmidt diagram")),
            fluidRow(
              column(10,
                     align = "center",
                     plotOutput("schmidt_diagram_c")
              )
            ),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_schmidt_c",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                )
              ),
              column(
                6,
                downloadButton("download_schmidt_c", "Schmidt diagram")
              )
            ),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Woodcock diagram",
            h5(strong("Woodcock diagram")),
            fluidRow(
              column(10,
                     align = "center",
                     plotOutput("woodcock_diagram_c")
              )
            ),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_woodcock_c",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                )
              ),
              column(
                6,
                downloadButton("download_woodcock_c", "Woodcock diagram")
              )
            ),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Summary table",
            h5(strong("Summary table of samples")), tags$br(),
            DT::dataTableOutput("summary_table_c"), tags$br(),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Model reference data",
            h5(strong("Model reference data")),
            tags$br(),
            p("Only the processes selected in the \'Benn diagram\' tab by the \'Build your model\' selector are displayed."),
            tags$br(),
            DT::dataTableOutput("model_reference"), tags$br(),
            hr(),
            tags$br()
          )
        )
      )
    ), tabPanel(
      "Spatialised method",
      sidebarPanel(
        h5(strong("About your data")),
        fluidRow(
          column(
            6,
            # checkboxInput("plot_all_samples_sm",
            #               strong("Plot all samples"), TRUE),
            tags$br(),
            tags$br(),
            uiOutput("sample_sm")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_sm",
              strong("No code distinction"), TRUE
            ),
            uiOutput("code_sm")
          )
        ),
        hr(),
        sliderInput("n_nearest_sm",
                    strong("Number of nearest points in spatial series"),
                    min = 10,
                    max = 60,
                    value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_sm",
          strong("Uncheck this box to make sticks in the projections"), TRUE
        ),
        p(style = "text-align: justify;","Note that by default, for the construction of the sticks, the y axis is considered to be north. If you have used a compass to measure the orientations, the sticks will be rotated by the difference between the y axis and magnetic north."), 
        hr(),
        fileInput("ortho_xy_sm",
                  strong("Upload XY ortho"),
                  multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_sm",
                  strong("Upload XZ ortho"),
                  multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_sm",
                  strong("Upload YZ ortho"),
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
            h5(strong("Benn diagram")),
            plotOutput("benn_sm"),
            h5(strong("XY projection")),
            plotOutput("projection_xy_sm", inline = T),
            h5(strong("XZ projection")),
            plotOutput("projection_xz_sm", inline = T),
            h5(strong("YZ projection")),
            plotOutput("projection_yz_sm", inline = T),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                # h5(strong("Benn diagram")),
                radioButtons(
                  inputId = "file_format_benn_sm",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_sm", "Benn diagram"),
                hr(),
                # h5(strong("XY projection")),
                radioButtons(
                  inputId = "file_format_xy_sm",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xy_sm", "XY projection"),
                hr(),
              ),
              column(
                6,
                # h5(strong("XZ projection")),
                radioButtons(
                  inputId = "file_format_xz_sm",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xz_sm", "XZ projection"),
                hr(),
                # h5(strong("YZ projection")),
                radioButtons(
                  inputId = "file_format_yz_sm",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_yz_sm", "YZ projection"),
                hr(),
              )
            )
          ),
          tabPanel(
            "Spatial series data table",
            h5(strong("Statistics table of spatial series")), tags$br(),
            DT::dataTableOutput("summary_table_sm"),
            tags$br(),
            hr(),
            tags$br()
          ),
          tabPanel(
            "Summary table of samples",
            h5(strong("Summary table of samples")), tags$br(),
            DT::dataTableOutput("summary_table2_sm"),
            tags$br(),
            hr(),
            tags$br()
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
              strong("No sample distinction"), TRUE
            ),
            uiOutput("sample_se")
          ),
          column(
            6,
            checkboxInput(
              "plot_all_codes_se",
              strong("No code distinction"), TRUE
            ),
            uiOutput("code_se")
          )
        ),
        hr(),
        sliderInput("n_nearest_se",
                    strong("Number of nearest points in spatial series in the interactive Benn diagram"),
                    min = 10,
                    max = 60,
                    value = 40
        ),
        sliderInput("n_nearest_se2",
                    strong("Number of nearest points in the spatial series for the new Benn diagram built with the selected points"),
                    min = 10,
                    max = 60,
                    value = 40
        ),
        hr(),
        checkboxInput(
          "plot_type_se",
          strong("Uncheck this box to make sticks in the projections"), TRUE
        ),
        p(style = "text-align: justify;","Note that by default, for the construction of the sticks, the y axis is considered to be north. If you have used a compass to measure the orientations, the sticks will be rotated by the difference between the y axis and magnetic north."), 
        hr(),
        fileInput("ortho_xy_se",
                  strong("Upload XY ortho"),
                  multiple = FALSE
        ),
        checkboxInput(
          "include_xy_ortho_se",
          strong("Include XY ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_xz_se",
                  strong("Upload XZ ortho"),
                  multiple = FALSE
        ),
        checkboxInput(
          "include_xz_ortho_se",
          strong("Include XZ ortho"), FALSE
        ),
        hr(),
        fileInput("ortho_yz_se",
                  strong("Upload YZ ortho"),
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
            h5("Use the lasso at the top right of the Benn diagram to sample points."),
            plotlyOutput("interactive_ternary"),
            hr(),
            h5(strong("New analysis with the selected points (i.e. spatial series)")),
            tags$br(),
            h5(strong("Summary table of selected points")),
            DT::dataTableOutput("selected_sample_summary"),
            tags$br(),
            fluidRow(
              column(
                6,
                h5(strong("Benn diagram")),
                plotOutput("selected_benn")
              ),
              column(
                6,
                h5(strong("Schmidt diagram")),
                plotOutput("schmidt_selected")
              )
            ),
            fluidRow(
              column(
                6,
                h5(strong("Orientation rose diagram")),
                plotOutput("orientation_rose_selected")
              ),
              column(
                6,
                h5(strong("Dip rose diagram")),
                plotOutput("plunge_rose_selected")
              )
            ),
            h5(strong("XY projection")),
            plotOutput("projection_xy_selected_se", inline = T),
            h5(strong("XZ projection")),
            plotOutput("projection_xz_selected_se", inline = T),
            h5(strong("YZ projection")),
            plotOutput("projection_yz_selected_se", inline = T),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_benn_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_se1", "Benn diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_schmidt_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_schmidt_se1", "Schmidt diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_o_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_o_se1", "Orientation rose diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_p_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_p_se1", "Dip rose diagram"),
                hr()
              ),
              column(
                6,
                # h5(strong("XY projection")),
                radioButtons(
                  inputId = "file_format_xy_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xy_se1", "XY projection"),
                hr(),
                # h5(strong("XZ projection")),
                radioButtons(
                  inputId = "file_format_xz_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_xz_se1", "XZ projection"),
                hr(),
                # h5(strong("YZ projection")),
                radioButtons(
                  inputId = "file_format_yz_se1",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_yz_se1", "YZ projection"),
                hr()
              )
            ),
          ),
          tabPanel(
            "Get selected data in Benn diagram",
            h5(strong("Statistics table of selected spatial series")),
            tags$br(),
            DT::dataTableOutput("click"),
            tags$br(),
            hr(),
            tags$br()
          ),
          tabPanel(
            "From projection",
            fluidRow(
              column(
                6,
                h5(strong("Interactive projections & new plots")),
                p("Use your mouse to sample points in the projection.")
              ),
              column(
                6,
                tags$br(),
                radioButtons("axis",
                             strong("Choose what axis you want to plot"),
                             choices = c("XY", "XZ", "YZ"),
                             inline = TRUE
                ),
                tags$br(),
              )
            ),
            plotOutput("interactive_projection",
                       brush = brushOpts(
                         id = "plot1_brush"
                       )
            ),
            # plotlyOutput("interactive_projection"),
            hr(),
            h5(strong("New analysis with the selected points (i.e. spatial series)")),
            tags$br(),
            h5(strong("Summary table of selected points")),
            DT::dataTableOutput("selected_sample_summary2"),
            tags$br(),
            fluidRow(
              column(
                6,
                h5(strong("Benn diagram")),
                plotOutput("selected_benn2")
              ),
              column(
                6,
                h5(strong("Schmidt diagram")),
                plotOutput("schmidt_selected2")
              )
            ),
            fluidRow(
              column(
                6,
                h5(strong("Orientation rose diagram")),
                plotOutput("orientation_rose_selected2")
              ),
              column(
                6,
                h5(strong("Dip rose diagram")),
                plotOutput("plunge_rose_selected2")
              )
            ),
            hr(),
            h5(strong("Download your plot by choosing a file format and pressing the download button")),
            tags$br(),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "file_format_benn_se2",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_benn_se2", "Download Benn diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_schmidt_se2",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_schmidt_se2", "Download Schmidt diagram"),
                hr()
              ),
              column(
                6,
                radioButtons(
                  inputId = "file_format_rose_o_se2",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_o_se2", "Download orientation rose diagram"),
                hr(),
                radioButtons(
                  inputId = "file_format_rose_p_se2",
                  label = strong("Select a file format"),
                  choices = c("pdf", "png", "jpeg"),
                  inline = TRUE
                ), tags$br(),
                downloadButton("download_rose_p_se2", "Download Dip rose diagram"),
                hr(),
              )
            )
          ),
          tabPanel(
            "Get selected data in projection",
            h5(strong("Summary data of selected sample")),
            tags$br(),
            DT::dataTableOutput("click2"),
            tags$br(),
            hr(),
            tags$br()
          )
        )
      )
    ),
    tabPanel(
      "Report",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("Markdown report"),
               hr(),
               p("Work in progress. This option will be available in the next version."),
               p("To download a rmarkdown report with figures and summary statistics of your data, choose a file format and then push the", strong("\'Generate report\'"), "button."),
               tags$br(),
               column(
                 12,
                 align = "center",
                 # sliderInput("slider", "Slider", 1, 100, 50),
                 tags$br(),
                 radioButtons("format", 
                              strong("Document format"), 
                              c("HTML", "PDF", "Word"),
                              inline = TRUE
                 ),
                 downloadButton("report", "Generate report")
               ),
               hr(),
               tags$br()
        )
      )
    ),
    tabPanel(
      "Tutorial",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("Tutorial"),
               hr(),
               h4(strong("Uploading your data")),
               tags$br(),
               p("First, upload your .csv file in the left sidebar panel. Pay attention to your decimal and column delimiters."),
               tags$br(),
               p("fabryka supports three angle measurement methods:"),
               tags$ul(
                 tags$li("angles taken with a compass and an inclinometer"),
                 tags$li("angles taken with a DistoX2"),
                 tags$li("angles taken with two shots at the total station.")
               ),
               tags$br(),
               p("Five cases (i.e. forms of data) are covered by the application:"),
               tags$ul(
                 tags$li("check", strong("\'Case 1: Only angles\'"), "if you have in your dataset the columns \'id\', \'sample\', \'code\', \'orientation\' and \'dip\'."),
                 tags$li("check", strong("\'Case 2: Only angles from DistoX2\'"), "if you have in your dataset the same columns that above but your data comes from a DistoX2 and needs to be converted."),
                 tags$li("check", strong("\'Case 3: Angles from DistoX2 with coordinates\'"), "if you have in your dataset the same columns that above (Case 2, data from DistoX2) with a single set of coordinates (x, y, z)."),
                 tags$li("check", strong("\'Case 4: Angles and coordinates\'"), "if your data has the same form that above (Case 3) but is coming from a compass and a clinometer."),
                 tags$li("check", strong("\'Case 5: Two shots data without angles\'"), "if you have the columns \'id\', \'sample\', \'code\' and the angles have been measured with a total station (i.e. two shots data).")
               ),
               tags$br(),
               p("The simplest method to upload data in the application is to name the column names directly, as shown in the example files. Enter your data in a .csv file, adding the columns \'id\', \'code\' and \'sample\'. If the columns don't exist, fill them in with the same letter. By doing so, you do not need to select your variables in the main panel. The application correctly reads the data file directly."),
               tags$br(),
               p(style = "text-align: justify;", "According to your method of angle measurement and the presence of coordinates you have to select the right case.
           Please refer to the example files in the left side bar panel. It contains a dataset coming from Le Moustier Lower shelter (Dordogne, France).
           The exmample files contain directly the good column names.
           You can use them directly in the application.
           Whatever the names of the columns in your file, you can select the appropriate columns in the column selector.
           Please keep only complete lines (i.e. no empty modalities, so no line without a fabric measurement).
           When a data table with new columns appears below the column selector, you are ready to use the application.
          "),
               tags$br(),
               p(style = "text-align: justify;", "If your data comes from a DistoX2, the columns \'orientation\' (= \'bearing\', 0 to 360\u00b0 angles) and \'dip\' (= \'plunge\') are automatically corrected in the new table.
           Whatever the form of your dataset, two new columns \'orientation_pi\' (0 to 180\u00b0 angles) and \'angle_double\' (see Krumbein 1939) appear.
           If the angles were measured using a total station (i.e. two shots data), the orientation and the dip are also calculated.
           If only one set of coordinates is provided with angles, a second set of coordinates is calculated and displayed in the new table.
           You can download the new data by pushing the \'copy\', the \'csv\' or the \'pdf\' buttons."),
               tags$br(),
               hr(),
               h4(strong("Classical method")),
               tags$br(),
               p(style = "text-align: justify;", "The classical method is available in all cases (i.e. source of data and absence or presence of coordinates).
           You can control the data you plot and the data appearing in the figures and the summary data table by changing the checkboxes filters (samples and code) and the slider (i.e. minimum number of measures) in the left sidebar panel.
           Certain geological processes can have a different impact on objects depending on their physical properties (e.g. silex versus bones in a water flow as in the example files provided where bones have a strong preferential orientation and are much more linear in the Benn diagram, see Thomas et al., 2019, Texier et al., 2020).
           The natural processes models that you can add to the Benn diagram are calculated using data from Bertran & Lenoble (2002).
           In the rose diagram, axial data is considered (and not orientations) as the Rayleigh test is performed on axial data (see Curray 1956) in the summary table.
           You can download all the figures by selecting a file format and pressing the \'download\' button.
           "),
               tags$br(),
               p("In this last table:"),
               tags$ul(
                 tags$li("'n' stands for number of measures in the samples"),
                 tags$li("'E1', 'E2' and 'E3' are the eigenvalues (see Watson 1966). They correspond to the sum of the cosines formed by the axis of the objects and the three axis of the space. They are used to compute the Benn indices."),
                 tags$li("'IS', and 'EL' are calculated thanks to the eigenvalues (Benn 1994). These are the Benn indices, ratios standing respectively for, 'isotropy' with IS = E3/E1 and 'elongation' with EL = 1-(E2/E1)."),
                 tags$li("''K' and 'C' are the Woodcock (1977) indices. They are calculated as follow: K = r1/r2 and C = ln(E1/E3) with r1 = ln(E1/E2) and r2 = ln(E2/E3)."),
                 tags$li("'L' is the Vector Magnitude (strength of the preferred artefact orientation) and 'R.p' is the p-value result of the Rayleigh test."),
                 tags$li("'L.double' and 'R.p.double' are the same statistics that above for double angles (see Krumbein 1939)."),
                 tags$li("'Rao p' is the p_value of the Rao (1967) test.")
               ),
               tags$br(),
               p("Curray's formula to calculate 'L' is", strong("(1.)")),
               p(style = "text-align: left;", withMathJax("$$L = \\frac{r*100}{n}$$")),
               p("with", strong("(2.)")),
               p(withMathJax("$$r = \\sqrt{\\sum_{i=\\alpha}^n sin{(2\\alpha)} + \\sum_{i=\\alpha}^n cos{(2\\alpha)}}$$")),
               p("where \'n\' is the number of measures and '\\(\\alpha\\)', are axial angles (0 to 180\u00b0)"),
               tags$br(),
               p("The Rayleigh test is perform thanks to the formula", strong("(3.)")),
               p(withMathJax("$$p = e^{(-(n*L^2))*10^-4}$$")),
               tags$br(),
               p("The Rao (1967) spacing test assesses the uniformity of circular data. It determines whether the data show significant directionality (*i.e.* the null hypothesis is a uniform distribution). It is widely used to detect multimodal directionality."),
               hr(),
               h4(strong("Spatialised method")),
               tags$br(),
               p(style = "text-align: justify;", "In order to distinguish different statistical populations, a spatial analysis method proposed by McPherron (2018) is used.
        Thanks to this methods it is possible to calculate the Benn indices and to test the orientation rate \'L\' (formula (3.) modified R McPherron 2018 script computed for orientations and double angles) for each remains, taking into account its n nearest neighbors in 3 dimensions (R McPherron 2018 script, modified , formula (4.)).
        It is possible to localize the statistical signal of a series by varying the number of measurements sampled to calculate the Benn indices.
        The choice of about 40 to 50 measurements (each vestige and its 40 to 50 closest neighbors) to calculate the Benn indices allows local particularities to be observed, and above all, it allows the Rayleigh test to be carried out in its field of application (stability of the test between 40 and 50 measurements).
        Several modes of representation are used in the \'Benn diagram & projections\' subpanel."),
               tags$br(),
               p(style = "text-align: justify;", "The Benn indices (1994) calculated for each series of remains are projected within a ternary diagram in which each pole (isotropic, linear and planar) is colored respectively in red, green and blue.
        The RGB color code is reused to materialize the fabric of the remains on spatial projections of the material along the axes of the excavation grid.
        Projections (xy, xz and yz axes) are performed. On the latter, the orientation of the objects can be materialized by sticks (by uncecking the corresponding box in the left sidebar panel) which color is representative of the fabric of the series formed by the vestige and its 50 closest neighbors.
        In the left sidebar panel, you can control the samples taken into account in the analyses by selecting the sample explored or by distinguishing the code used (e.g. flint versus bone).
        You can also use the slider to change the number of neighbors searched for around each point.
        Be careful, this will alter the results obtained in the Benn diagram and Rayleigh tests.
          "),
               tags$br(),
               p(style = "text-align: justify;", "At the spatial series scale (i.e. each points and n neighbors), statistics are provided in a table (Spatial series data table subpanel) with the Benn indices, the orientation rates \'L\', \'L.double\' and \'p-values\' returned by the Rayleigh tests.
        At the sample scale, mean and standard deviations values of Benn indices, mean \'L\' and its corresponding Rayleigh tests means (mean values of all spatial series in the sample) and rejections ratios \'Tr\' (formula 4.) are given in the subpanel \'Summary table of samples\'."),
               tags$br(),
               p("The rejection ratio \'Tr\' is perform thanks to the formula", strong("(4.)")),
               h5(withMathJax("$$Tr = \\frac{(n~times~p.value < 0.05)* 100}{n}$$")),
               tags$br(),
               hr(),
               h4(strong("Spatial exploration")),
               tags$br(),
               p(style = "text-align: justify;", "In this panel, you can perform a spatialised analysis of your data starting from a Benn diagram or from projections.
          In both cases, you need to select the data you want to explor more locally in a new analysis.
          From the projections, you can select the points you want to explore directly.
          In the Benn diagram, you need to select the lasso selector.
          Pay attention to the number of points selected and check that the new sample contains enough points to carry out the new analysis.
          If this is not the case, you can lower the slider in the left sidebar panel to perform the analysis with fewer neighbors.
          You can dowload the new samples you have selected from the corresponding subpanels.
          "),
               tags$br(),
               hr(),
               h4(strong("Markdown report")),
               tags$br(),
               p("This panel is in progress. It will be available in the next version. You will find only a beta test."),
               tags$br(),
               hr(),
        )
      )
    ),
    tabPanel(
      "New models",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("New models"),
               hr(),
               p("Work in progress. This panel will be available in the next version."),
               hr(),
               tags$br()
        )
      )
    ),
    tabPanel(
      "Bibliography",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("Bibliography"),
               hr(),
               p("Benn D.I. (1994) - Fabric shape and the interpretation of sedimentary fabric data. Journal of Sedimentary Research, A64(4), pp. 910-915."),
               p("Bertran P., Bordes J.G., Barr\u00e9 A., Lenoble A., Mourre V. (2006) - Fabrique d\'amas de d\u00e9bitage : donn\u00e9es exp\u00e9rimentales. Bulletin de la Soci\u00e9t\u00e9 Pr\u00e9historique Fran\u00e7aise 103, pp. 33-47"),
               p("Bertran P., Lenoble A. (2002) - Fabriques des niveaux arch\u00e9ologiques : M\u00e9thode et premier bilan des apports \u00e0 l'\u00e9tude taphonomique des sites pal\u00e9olithiques. Pal\u00e9o 14, pp. 13-28."),
               p("Bertran P., H\u00e9tu B., Texier J.P., Van Steijn H. (1997) - Fabric characteristics of slope deposits. Sedimentology 44, pp. 1-16."),
               p("Curray J.R. (1956) - Analysis of two-dimensional orientation data. Journal of Geology, 64, pp. 117-134."),
               p("Krumbein W. C. (1939) - Preferred orientation of pebbles in sedimentary deposits, The journal of Geology, v. XLVII, n\u00b07, pp. 673-706"),
               p("Lenoble A., Bertran P. (2004) - Fabric of Palaeolithic levels: methods and implications for site formation processes. Journal of Archaeological Science 31, pp. 457-469."),
               p("McPherron S.P. (2018) - Additional statistical and graphical methods for analyzing site formation processes using artifact orientations, PloS One 13, 21p."),
               p(" Rao, JS. (1967) - Large Sample Tests for the Homogeneity of Angular Data. Sankhya Ser B 28: pp. 172–74."),
               p("Thomas, M., Discamps, E., Gravina, B., Texier, J.-P. (2019). Analyse taphonomique et spatiale de palimpsestes d\'occupations moust\u00e9riennes de l\'abri inf\u00e9rieur du Moustier (Dordogne, France). Pal\u00e9o 30(1), pp. 278-299. https://doi.org/10.4000/paleo.4897"),
               p("Texier, J.-P., Discamps, E., Gravina, B., Thomas, M. (2020). Les d\u00e9p\u00f4ts de remplissage de l\'abri inf\u00e9rieur du Moustier (Dordogne, France) : lithostratigraphie, processus de formation et \u00e9volution du syst\u00e8me g\u00e9omorphologique. Pal\u00e9o 30(2), pp. 320-345. https://doi.org/10.4000/paleo.5826"),
               p(" Watson, G.S. (1966) - The Statistics of Orientation Data. The Journal of Geology 74 (5, Part 2): pp. 786-797."),
               p("Woodcock N.H., (1977) - Specification of fabric shapes using an eigenvalue method. Geological Society of America Bulletin, 88, pp. 1231-1236."),
               hr(),
               tags$br()
        )
      )
    ),
    tabPanel(
      "Contributors",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("Data contributions"),
               hr(),
               tags$ul(
                 tags$li("The reference data used to build the models of the classical method were collected by", strong("P. Bertran"), "throughout his career (e.g. Bertran and Lenoble 2002). P. Bertran provided us with all these data in order to feed the application."),
                 tags$li(strong("Pascal Bertran"), "PCI recommender and", strong("Frédéric Santos,"), strong("Nicolas Frerebeau"), "and,", strong("Alfonso Benito-Calvo"), "reviewers of", a(href = "https://zenodo.org/records/15236695", strong("fabryka: A web application for easily analysing & exploring the fabric of archaeological assemblages")), "have helped to improve this application.")
               )
        )
      )
    ),
    tabPanel(
      "Citation",
      mainPanel(
        column(12,
               align = "center",
               tags$img(src = "www/hex_Fabryka4.png", height = 200),
               tags$br()
        ),
        column(12,
               align = "justify",
               h2("How to cite fabryka 1.0"),
               tags$hr(),
               p("If you use fabryka please, specify the version (e.g. 'fabryka 1.1') and cite the dedicated article", a(href = "https://zenodo.org/records/15236695", strong("Thomas (2025)"))),
               tags$br(),
               p("fabryka uses data collected by numerous researchers. Depending on the method (e.g. McPherron 2018) and data (e.g. Bertran and Lenoble 2002) you use please cite the dedicated articles."),
        )
      )
    )
  )
)
}