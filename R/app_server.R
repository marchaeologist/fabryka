### --- server ---
app_server <- function(input, output, session) {
  
  # increase file upload size limit in shiny to 50MB
  base::options(shiny.maxRequestSize = 350 * 1024^2)
  
  # get the user data
  user_data <- reactive({
    req(!base::is.null(input$user_data))
    
    inFile <- input$user_data
    
    if (input$decimal == ".") {
      data <- utils::read.csv(inFile$datapath, sep = input$sep)
    } else {
      data <- utils::read.csv2(inFile$datapath, sep = input$sep)
    }
    base::return(data)
  })
  
  output$user_data_head <- renderTable(
    utils::head(user_data(), 1)
  )
  
  output$id1 <- renderUI({
    selectInput("id", 
                strong("unique 'ID'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$code1 <- renderUI({
    selectInput("code", 
                strong("'Code' (e.g. silex, fauna)"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$sample1 <- renderUI({
    selectInput("sample", 
                strong("'Sample' (e.g. US, UL)"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$x1 <- renderUI({
    selectInput("x", 
                strong("'X'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$y1 <- renderUI({
    selectInput("y", 
                strong("'Y'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$z1 <- renderUI({
    selectInput("z", 
                strong("'Z'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$x2 <- renderUI({
    selectInput("x2", 
                strong("'X2'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$y2 <- renderUI({
    selectInput("y2", 
                strong("'Y2'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$z2 <- renderUI({
    selectInput("z2", 
                strong("'Z2'"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$orientation1 <- renderUI({
    selectInput("Orientation", 
                strong("\'Orientation\' (i.e. 0\u00b0 to 360\u00b0 angles)"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  output$dip1 <- renderUI({
    selectInput("dip", 
                strong("\'Plunge\' (i.e. 0\u00b0 to 90\u00b0 angles)"),
                choices = c("NULL", base::names(user_data())),
                selected = "NULL"
    )
  })
  
  clean_user_data <- reactive({
    # req(input$user_data)

    dataaa <- user_data()
    DataNew1 <- dataaa
    
    id <- base::as.character(input$id)
    code <- base::as.factor(input$code)
    sample <- base::as.factor(input$sample)
    x <- input$x
    y <- input$y
    z <- input$z
    orientation <- base::as.numeric(input$orientation)
    dip <- base::as.numeric(input$dip)
    x2 <- input$x2
    y2 <- input$y2
    z2 <- input$z2
    
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == id)] <- "id"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == code)] <- "code"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == sample)] <- "sample"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == x)] <- "X1"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == y)] <- "Y1"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == z)] <- "Z1"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == orientation)] <- "orientation"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == dip)] <- "dip"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == x2)] <- "X2"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == y2)] <- "Y2"
    base::colnames(DataNew1)[base::which(base::colnames(DataNew1) == z2)] <- "Z2"
    
    base::return(DataNew1)
  })
  
  
  # format the data to include all columns needed for the app in a reactive function
  
  app_data <- reactive({

    if (input$data_type == "Case 1: Only angles") {
      clean_user_data() %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            orientation <= 180 ~ base::round(orientation, 0),
            orientation > 180 ~ base::round(orientation - 180, 0)
          )
        ) %>%
        dplyr::mutate(
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = 0) %>%
        dplyr::mutate(Y1 = 0) %>%
        dplyr::mutate(Z1 = 0) %>%
        dplyr::rename(bearing = orientation) %>%
        dplyr::rename(plunge = dip) %>%
        dplyr::mutate(X2 = base::round(X1 + base::cos(rad(plunge)) * base::sin(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Y2 = base::round(Y1 + base::cos(rad(plunge)) * base::cos(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Z2 = base::round(Z1 - base::sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    } else if (input$data_type == "Case 2: Only angles from DistoX2") {
      clean_user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & orientation < 180) ~ base::round(orientation + 180, 0),
            (dip > 0 & orientation > 180) ~ base::round(orientation - 180, 0),
            dip <= 0 ~ base::round(orientation, 0)
          ),
          plunge = base::abs(base::round(dip, 0)),
          orientation_pi = dplyr::case_when(
            orientation <= 180 ~ base::round(orientation, 0),
            orientation > 180 ~ base::round(orientation - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = 0) %>%
        dplyr::mutate(Y1 = 0) %>%
        dplyr::mutate(Z1 = 0) %>%
        # dplyr::rename(bearing = orientation) %>%
        # dplyr::rename(plunge = dip) %>%
        dplyr::mutate(X2 = base::round(X1 + base::cos(rad(plunge)) * base::sin(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Y2 = base::round(Y1 + base::cos(rad(plunge)) * base::cos(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Z2 = base::round(Z1 - base::sin(rad(plunge)) * 0.04, 3)) %>%
        dplyr::select(-c(orientation, dip)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    } else if (input$data_type == "Case 3: Angles from DistoX2 with coordinates") {
      clean_user_data() %>%
        dplyr::mutate(
          bearing = dplyr::case_when(
            (dip > 0 & orientation < 180) ~ base::round(base::as.numeric(orientation) + 180, 0),
            (dip > 0 & orientation > 180) ~ base::round(base::as.numeric(orientation) - 180, 0),
            dip <= 0 ~ base::round(base::as.numeric(orientation), 0)
          ),
          plunge = base::abs(base::round(base::as.numeric(dip), 0)),
          orientation_pi = dplyr::case_when(
            orientation <= 180 ~ base::round(orientation, 0),
            orientation > 180 ~ base::round(orientation - 180, 0)
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(X1 = base::as.numeric(x)) %>%
        dplyr::mutate(Y1 = base::as.numeric(y)) %>%
        dplyr::mutate(Z1 = base::as.numeric(z)) %>%
        dplyr::mutate(X2 = base::round(X1 + base::cos(rad(plunge)) * base::cos(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Y2 = base::round(Y1 + base::cos(rad(plunge)) * base::sin(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Z2 = base::round(Z1 - base::sin(rad(plunge)) * 0.05, 3)) %>%
        dplyr::select(-c(x, y, z, orientation, dip)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    } else if (input$data_type == "Case 5: Two shots data without angles") {
      clean_user_data() %>%
        dplyr::mutate(plunge = base::round(plunge_and_bearing(clean_user_data())[[1]], 2)) %>%
        dplyr::mutate(bearing = base::round(plunge_and_bearing(clean_user_data())[[2]], 2)) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing <= 180 ~ bearing,
            bearing > 180 ~ bearing - 180
          ),
          angle_double = dplyr::case_when(
            2 * orientation_pi > 180 ~ 2 * orientation_pi - 180,
            2 * orientation_pi <= 180 ~ 2 * orientation_pi
          )
        ) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    } else { # Case 4: Angles and coordinates
      clean_user_data() %>%
        dplyr::rename(bearing = orientation) %>%
        dplyr::rename(plunge = dip) %>%
        dplyr::mutate(
          orientation_pi = dplyr::case_when(
            bearing <= 180 ~ bearing,
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
        dplyr::mutate(X2 = base::round(x + base::cos(rad(plunge)) * base::sin(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Y2 = base::round(y + base::cos(rad(plunge)) * base::cos(rad(bearing)) * 0.05, 3)) %>%
        dplyr::mutate(Z2 = base::round(z - base::sin(rad(plunge)) * 0.06, 3)) %>%
        dplyr::select(-c(x, y, z)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    }
  })
  
  # The user sees the data included in the reactive function app_data() and can download it
  output$app_data_view <- DT::renderDataTable({
    
    req(input$user_data)
    req(input$data_type)
    
    DT::datatable(app_data(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      pageLength = base::nrow(app_data()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
      )
    )
  })
  
  # Creation of the data I will use in the examples files - disto_one_shot
  distoX2_xyz_example <- reactive({
    distoX2 <- distoX2
    base::return(distoX2)
  })
  
  # if disto angles only
  distoX2_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::select(id, code, sample, orientation, dip)
  })
  
  
  # if angles only
  angles_only_example <- reactive({
    data_to_modif <- distoX2_xyz_example()
    
    angles_data <- data_to_modif %>%
      dplyr::select(-c(x, y, z)) %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & orientation < 180) ~ base::round(base::as.numeric(orientation) + 180, 0),
          (dip > 0 & orientation > 180) ~ base::round(base::as.numeric(orientation) - 180, 0),
          dip <= 0 ~ base::round(base::as.numeric(orientation), 0)
        )
      ) %>%
      dplyr::mutate(plunge = base::abs(base::round(base::as.numeric(dip), 0))) %>%
      dplyr::select(-c(orientation, dip)) %>%
      dplyr::rename(orientation = bearing) %>%
      dplyr::rename(dip = plunge)
    
    base::return(angles_data)
  })
  
  
  # if angles and coordinates
  angles_xyz_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & orientation < 180) ~ base::round(base::as.numeric(orientation) + 180, 0),
          (dip > 0 & orientation > 180) ~ base::round(base::as.numeric(orientation) - 180, 0),
          dip <= 0 ~ base::round(base::as.numeric(orientation), 0)
        ),
        plunge = base::abs(base::round(base::as.numeric(dip), 0))
      ) %>%
      dplyr::select(-c(orientation, dip)) %>%
      dplyr::rename(orientation = bearing) %>%
      dplyr::rename(dip = plunge)
  })
  
  
  # if two shots data
  two_shots_example <- reactive({
    distoX2_xyz_example() %>%
      dplyr::mutate(
        bearing = dplyr::case_when(
          (dip > 0 & orientation < 180) ~ base::round(base::as.numeric(orientation) + 180, 0),
          (dip > 0 & orientation > 180) ~ base::round(base::as.numeric(orientation) - 180, 0),
          dip <= 0 ~ base::round(base::as.numeric(orientation), 0)
        ),
        plunge = base::abs(base::round(base::as.numeric(dip), 0))
      ) %>%
      dplyr::mutate(X1 = base::as.numeric(x)) %>%
      dplyr::mutate(Y1 = base::as.numeric(y)) %>%
      dplyr::mutate(Z1 = base::as.numeric(z)) %>%
      dplyr::mutate(X2 = base::round(X1 + base::cos(rad(plunge)) * base::sin(rad(bearing)) * 0.05, 3)) %>%
      dplyr::mutate(Y2 = base::round(Y1 + base::cos(rad(plunge)) * base::cos(rad(bearing)) * 0.05, 3)) %>%
      dplyr::mutate(Z2 = base::round(Z1 - base::sin(rad(plunge)) * 0.05, 3)) %>%
      dplyr::select(-c(x, y, z, orientation, dip, bearing, plunge)) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
  })
  
  
  # function to download the file if angle only
  output$angles_only <- downloadHandler(
    filename = function() {
      paste("Case 1: Only angles", ".csv")
    },
    content = function(file) {
      utils::write.csv2(angles_only_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  # function to download the file if angle only from disto
  output$angles_only_disto <- downloadHandler(
    filename = function() {
      paste("Case 2: Only angles from DistoX2", ".csv")
    },
    content = function(file) {
      utils::write.csv2(distoX2_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  # function to download the file if angle from disto and xyz
  output$disto_one_shot <- downloadHandler(
    filename = function() {
      base::paste("Case 3: Angles from DistoX2 with coordinates", ".csv")
    },
    content = function(file) {
      utils::write.csv2(distoX2_xyz_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  # function to download the file if angle and xyz
  output$angles_one_shot <- downloadHandler(
    filename = function() {
      paste("Case 4: Angles and coordinates", ".csv")
    },
    content = function(file) {
      utils::write.csv2(angles_xyz_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  # function to download the file if two shots data
  output$two_shots <- downloadHandler(
    filename = function() {
      paste("Case 5: Two shots data without angles", ".csv")
    },
    content = function(file) {
      utils::write.csv2(two_shots_example(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  ### --- Classical method ---
  # User chooses levels to display
  output$sample_c <- renderUI({
    req(input$user_data)
    radioButtons("sample_c",
                 label = strong("Filter by sample"),
                 choices = base::unique(as.character(app_data()$sample)))
  })

  # User chooses codes to display
  output$code_c <- renderUI({
    req(input$user_data)
    if (input$plot_all_samples_c) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_c)
    }
    radioButtons("code_c", 
                 label = strong("Filter by code"), 
                 choices = base::unique(app_data_temp$code))
  })
  
  
  # User chooses the model to display
  model_subset <- reactive({
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
      data_sub <- base::subset(app_data(), code == input$code_c)
    } else if (!input$plot_all_samples_c & input$plot_all_codes_c) {
      data_sub <- base::subset(app_data(), sample == input$sample_c)
    } else {
      data_sub <- base::subset(app_data(), code == input$code_c & sample == input$sample_c)
    }
    base::return(data_sub)
  })
  
  
  # calculating benn indices for the user subset data (from McPherron 2018, modified)
  benn_index_c <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$minimum_n) {
      benn <- base::matrix(
        nrow = base::length(base::unique(level)), ncol = 6,
        dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      
      for (l in unique(level)) {
        xyz_level <- base::subset(xyz, level == l)
        
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- base::c(nrow(xyz_level), base::rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          
          benn[l, ] <- base::c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      
      return(benn)
    }
    
    
    benn_ind <- data.frame(benn(subset_code_sample_c(), level = subset_code_sample_c()$sample)) %>%
      dplyr::mutate(PL = 1 - IS - EL)
    benn_ind$Sample <- base::rownames(benn_ind)
    return(benn_ind)
  })
  
  # benn_plot function
  benn_plot <- reactive({
    tern_model <- model_subset()
    
    p <- ggtern::ggtern(tern_model, ggtern::aes(as.numeric(PL), as.numeric(IS), as.numeric(EL), alpha = 0.1)) +
      # geom_polygon(aes(fill = Type, group = area, alpha = 0.1)) +
      ggalt::geom_encircle(aes(fill = Type, alpha = 0.1), s_shape = 0.6, expand = 0) +
      ggsci::scale_fill_jco() +
      ggplot2::labs(title = "", fill = "Process", x = "", y = "IS", z = "EL") +
      ggplot2::theme(
        tern.axis.line.T = element_line(color = "black", linewidth = 1),
        tern.axis.line.L = element_line(color = "black", linewidth = 1),
        tern.axis.line.R = element_line(color = "black", linewidth = 1)
      ) +
      ggtern::theme_bw() +
      ggplot2::guides(alpha = "none", size = 0.5) +
      ggtern::scale_T_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_L_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::scale_R_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), labels = c("", "50%", ""), minor_breaks = NULL, size = 0.2) +
      ggtern::geom_Tline(Tintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Lline(Lintercept = 0.5, colour = "black", size = 0.2) +
      ggtern::geom_Rline(Rintercept = 0.5, colour = "black", size = 0.2)
    
    r <- p +
      ggplot2::geom_point(data = benn_index_c(), aes(PL, IS, EL, color = Sample), size = 2.1) +
      ggplot2::scale_color_manual(values = c(
        "black",
        "#0073C2FF",
        "#EFC000FF",
        "#868686FF",
        "#526E2DFF",
        "#935931FF",
        "#E89242FF",
        "#CD534CFF",
        "#7AA6DCFF",
        "#003C67FF",
        "#8F7700FF",
        "#A73030FF",
        "#4A6990FF",
        "#FAE48BFF",
        "#E887CCFF"
      )) #+
    # ggplot2::scale_color_brewer(palette = "Paired")
    
    base::return(r)
  })
  
  # print the benn plot
  output$benn <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(benn_plot())
  })
  
  # download the benn plot
  output$download_benn_c <- downloadHandler(
    filename = function() {
      base::paste("benn_diagram", input$file_format_benn_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_c == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_c == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      base::print(benn_plot())
      grDevices::dev.off()
    }
  )
  
  # orientation rose diagram
  orientation_rose_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    d <- subset_code_sample_c()$orientation_pi + 180
    e <- base::c(subset_code_sample_c()$orientation_pi, d)
    f <- circular::circular(e,
                  type = "angles", units = "degrees",
                  template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    grDevices::recordPlot()
  })
  
  # plot orientation_rose_c
  output$orientation_rose_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(orientation_rose_c())
  })
  
  # plunge rose diagram
  plunge_rose_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(subset_code_sample_c()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    grDevices::recordPlot()
  })
  
  # plot plunge_rose_c
  output$plunge_rose_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(plunge_rose_c())
  })
  
  # download rose diagram
  output$download_orientation_rose_c <- downloadHandler(
    filename = function() {
      base::paste("orientation_rose", input$file_format_rose_orientation, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_orientation == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_orientation == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(orientation_rose_c())
      grDevices::dev.off()
    }
  )
  
  # download rose plunge
  output$download_plunge_rose_c <- downloadHandler(
    filename = function() {
      base::paste("plunge_rose", input$file_format_rose_plunge, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_plunge == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_plunge == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(plunge_rose_c())
      grDevices::dev.off()
    }
  )
  
  # schmidt diagram
  schmidt_diagram_c <- reactive({
    req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(subset_code_sample_c()$bearing,
                         subset_code_sample_c()$plunge,
                         pch = 19, cex = 0.8, col = "black", color_filled = TRUE)
    grDevices::recordPlot()
  })
  
  # plot schmidt_diagram_c
  output$schmidt_diagram_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(schmidt_diagram_c())
  })
  
  output$download_schmidt_c <- downloadHandler(
    filename = function() {
      base::paste("schmidt_diagram", input$file_format_schmidt_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_c == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_c == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(schmidt_diagram_c())
      grDevices::dev.off()
    }
  )
  
  # Woodcock diagram
  woodcock_diagram <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    woodcock_data <- benn_index_c() %>%
      dplyr::mutate(r1 = base::log(E1 / E2)) %>%
      dplyr::mutate(r2 = base::log(E2 / E3)) %>%
      dplyr::mutate_if(is.numeric, round, digits = 2)
    
    w <- ggplot2::ggplot(woodcock_data, ggplot2::aes(x = r2, y = r1, color = Sample)) + 
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, 1), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, 1), expand = c(0, 0)) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 7, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 0, yend = 7, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 0, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 7 * 0.2, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 7 * 0.5, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 7 * 2, linewidth = 0.1) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 7, yend = 7 * 5, linewidth = 0.1) +
      ggplot2::geom_segment(x = 2, y = 0, xend = 0, yend = 2, linewidth = 0.1) +
      ggplot2::geom_segment(x = 4, y = 0, xend = 0, yend = 4, linewidth = 0.1) +
      ggplot2::geom_segment(x = 6, y = 0, xend = 0, yend = 6, linewidth = 0.1) +
      ggplot2::annotate("text", x = 6.7, y = 1.5, label = "K = 0.2", color = "black", size = 3) +
      ggplot2::annotate("text", x = 6.7, y = 3.6, label = "K = 0.5", color = "black", size = 3) +
      ggplot2::annotate("text", x = 6.6, y = 6.9, label = "K = 1", color = "black", size = 3) +
      ggplot2::annotate("text", x = 3, y = 6.9, label = "K = 2", color = "black", size = 3) +
      ggplot2::annotate("text", x = 1.1, y = 6.9, label = "K = 5", color = "black", size = 3) +
      ggplot2::annotate("text", x = 2.2, y = 0.1, label = "C = 2", color = "black", size = 3) +
      ggplot2::annotate("text", x = 4.2, y = 0.1, label = "C = 4", color = "black", size = 3) +
      ggplot2::annotate("text", x = 6.2, y = 0.1, label = "C = 6", color = "black", size = 3) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = element_blank()) +
      ggplot2::labs(x = "r2 = ln(E2/E3)", y = "r1 = ln(E1/E2)") +
      ggplot2::scale_color_manual(values = c(
        "black",
        "#0073C2FF",
        "#EFC000FF",
        "#868686FF",
        "#526E2DFF",
        "#935931FF",
        "#E89242FF",
        "#CD534CFF",
        "#7AA6DCFF",
        "#003C67FF",
        "#8F7700FF",
        "#A73030FF",
        "#4A6990FF",
        "#FAE48BFF",
        "#E887CCFF"
      ))
    
    base::return(w)
      
  })
  
  
  # plot woodcock_diagram
  output$woodcock_diagram_c <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    
    base::print(woodcock_diagram())
  })
  
  # download Woodcock diagram
  output$download_woodcock_c <- downloadHandler(
    filename = function() {
      base::paste("Woodcok_diagram", input$file_format_woodcock_c, sep = ".")
    },
    content = function(file) {
      if (input$file_format_woodcock_c == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_woodcock_c == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      
      print(woodcock_diagram())
      grDevices::dev.off()
    }
  )
  
  
  # data for summary table
  summary_data <- reactive({
    level <- base::unique(subset_code_sample_c()$sample)
    results <- base::data.frame(base::matrix(nrow = length(level), ncol = 5))
    
    results <- data.frame(
      sample = base::numeric(),
      L = base::numeric(),
      R.p = base::numeric(),
      L.double = base::numeric(),
      R.p.double = base::numeric(),
      Rao = base::character(),
      stringsAsFactors = FALSE
    )
    
    for (i in level) {
      data_level <- base::subset(subset_code_sample_c(), sample == i)
      if (base::nrow(data_level >= input$minimum_n)) {
        R1 <- circular::rayleigh.test(2 * circular::circular(data_level$orientation_pi,
                                                             type = "angles",
                                                             units = "degrees",
                                                             modulo = "pi"))
        
        R2 <- circular::rayleigh.test(2 * circular::circular(data_level$angle_double,
                                                             type = "angles",
                                                             units = "degrees",
                                                             modulo = "pi"))
        
        Rao_test <- base::withVisible(print.rao.spacing.test.modified(circular::rao.spacing.test(circular::circular(rad(data_level$bearing)), alpha = 0.05)))
        res <- Rao_test$value
          
        L <- base::round(R1$statistic * 100, 2)
        p <- base::round(R1$p.value, 2)
        L.double <- base::round(R2$statistic * 100, 2)
        p.double <- base::round(R2$p.value, 2)
        Rao <- res$accepted
        results[i, ] <- base::c(i, L, p, L.double, p.double, Rao)
      }
      
      results[3:5][results[c(3:5)]<=0.01] <- "< 0.01"
      column_names <- base::c("sample", "L", "R.p", "L.double", "R.p.double", "Rao p")
      base::colnames(results) <- column_names
      
    }
    
    benn_indices <- benn_index_c() %>%
      dplyr::rename(sample = Sample) %>%
      # dplyr::mutate(r1 = base::log(E1 / E2)) %>%
      # dplyr::mutate(r2 = base::log(E2 / E3)) %>%
      dplyr::mutate(K = base::log(E1 / E2) / base::log(E2 / E3)) %>%
      dplyr::mutate(C = base::log(E1 / E3)) %>%
      dplyr::mutate_if(is.numeric, round, digits = 2)
    
    table <- base::merge(x = benn_indices, y = results, by = "sample")
    
    base::return(table)
  })
  
  # Summary data table with all indices and orientation tests
  output$summary_table_c <- DT::renderDataTable(
    summary_data() %>% dplyr::select(-c(PL)),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_data()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  output$model_reference <- DT::renderDataTable(
    model_subset() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3))),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(model_subset()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )
  
  ### -- spatialised method --
  
  # User chooses levels to display
  output$sample_sm <- renderUI({
    req(input$user_data)
    radioButtons("sample_sm", 
                 label = strong("Filter by sample"), 
                 choices = base::unique(app_data()$sample))
  })
  
  # User chooses codes to display
  output$code_sm <- renderUI({
    req(input$user_data)
    # if(input$plot_all_samples_sm){
    #   app_data_temp <- app_data()
    # } else {
    app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_sm)
    # }
    radioButtons("code_sm", 
                 label = strong("Filter by code"), 
                 choices = base::unique(app_data_temp$code))
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
      data_sub <- base::subset(app_data(), sample == input$sample_sm)
    } else {
      data_sub <- base::subset(app_data(), sample == input$sample_sm & code == input$code_sm)
    }
    
    base::return(data_sub)
  })
  
  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))
  
  # calculating benn indices for the user subset data
  benn_index_sm <- reactive({
    req(input$n_nearest_sm)
    
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_sm) {
      benn <- base::matrix(
        nrow = base::length(base::unique(level)), ncol = 6,
        dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      
      for (l in unique(level)) {
        xyz_level <- base::subset(xyz, level == l)
        
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- base::c(base::nrow(xyz_level), base::rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- base::c(base::nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      
      base::return(benn)
    }
    
    
    spatial_benn <- function(xyz, nearest = input$n_nearest_sm, maximum_distance = NA) {
      benn <- base::matrix(NA,
                     nrow = base::nrow(xyz), ncol = 2,
                     dimnames = base::list(base::rownames(xyz), base::c("elongation", "isotropy"))
      )
      
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- base::order(d)
        if (!base::is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, base::c("EL", "IS")]
      }
      
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(base::cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      
      base::return(list(benn))
    }
    
    
    benn_sm <- base::data.frame(spatial_benn(subset_code_sample_sm())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)
    
    base::return(benn_sm)
  })
  
  # make a function to plot spatial benn ternary
  benn_sm_plot <- reactive({
    req(input$user_data)
    req(input$data_type)
    data_to_plot <- benn_index_sm()
    
    tst <- function() {
      Ternary::TernaryPlot(
        atip = "IS", btip = "EL", ctip = "", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      Ternary::ColourTernary(cols(), spectrum = NULL)
      Ternary::AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    grDevices::recordPlot()
  })
  
  output$benn_sm <- renderPlot({
    benn_sm_plot()
  })
  
  ortho_xy_sm <- reactive({
    ortho <- raster::stack(input$ortho_xy_sm$datapath)
    base::return(ortho)
  })
  
  ortho_xz_sm <- reactive({
    ortho <- raster::stack(input$ortho_xz_sm$datapath)
    base::return(ortho)
  })
  
  ortho_yz_sm <- reactive({
    ortho <- raster::stack(input$ortho_yz_sm$datapath)
    base::return(ortho)
  })
  
  # XY spatial projection with points
  projection_points_xy_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        ggplot2::aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Y1)
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
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    base::return(xy)
  })
  
  # XZ spatial projection with points
  projection_points_xz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        ggplot2::aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Z1)
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
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    base::return(xz)
  })
  
  # YZ spatial projection with points
  projection_points_yz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        data = subset_code_sample_sm(),
        ggplot2::aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(Y1, Z1)
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
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    base::return(yz)
  })
  
  # XY spatial projection with sticks
  projection_sticks_xy_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho) {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Y1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Y2,
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(X1, Y1)
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
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    base::return(xy)
  })
  
  # XZ spatial projection with sticks
  projection_sticks_xz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_xz_ortho) {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$X1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$X2,
          yend = subset_code_sample_sm()$Z2,
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(X1, Z1)
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
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    
    base::return(xz)
  })
  
  # YZ spatial projection with sticks
  projection_sticks_yz_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- benn_index_sm()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_yz_ortho) {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = subset_code_sample_sm()$Y1,
          y = subset_code_sample_sm()$Z1,
          xend = subset_code_sample_sm()$Y2,
          yend = subset_code_sample_sm()$Z2,
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        subset_code_sample_sm(),
        ggplot2::aes(Y1, Z1)
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
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
    
    base::return(yz)
  })
  
  # xy plot with colored points or sticks
  output$projection_xy_sm <- renderPlot(
    {
      req(input$user_data)
      req(input$data_type)
      
      if (input$plot_type_sm) {
        base::print(req(projection_points_xy_sm()))
      } else {
        base::print(req(projection_sticks_xy_sm()))
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
        base::print(projection_points_xz_sm())
      } else {
        base::print(projection_sticks_xz_sm())
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
        base::print(projection_points_yz_sm())
      } else {
        base::print(projection_sticks_yz_sm())
      }
    },
    height = 300,
    width = 780
  )
  
  # dowload benn diagram
  output$download_benn_sm <- downloadHandler(
    filename = function() {
      base::paste("benn_diagram", input$file_format_benn_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_sm == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_sm == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(benn_sm_plot())
      grDevices::dev.off()
    }
  )
  
  # download xy projection
  output$download_xy_sm <- downloadHandler(
    filename = function() {
      base::paste("XY projection", input$file_format_xy_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xy_sm == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xy_sm == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        base::print(projection_points_xy_sm())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_xy_sm())
        grDevices::dev.off()
      }
    }
  )
  
  # download xz projection
  output$download_xz_sm <- downloadHandler(
    filename = function() {
      base::paste("XZ projection", input$file_format_xz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xz_sm == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xz_sm == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        base::print(projection_points_xz_sm())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_xz_sm())
        grDevices::dev.off()
      }
    }
  )
  
  # download xz projection
  output$download_yz_sm <- downloadHandler(
    filename = function() {
      base::paste("YZ projection", input$file_format_yz_sm, sep = ".")
    },
    content = function(file) {
      if (input$file_format_yz_sm == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_yz_sm == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_sm) {
        base::print(projection_points_yz_sm())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_yz_sm())
        grDevices::dev.off()
      }
    }
  )
  
  # summary data at series scale
  summary_data_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    spatial_bearing <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[base::order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }
    
    
    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_sm) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[base::order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }
    
    data <- subset_code_sample_sm()
    benn_indices <- base::round(benn_index_sm(), 2)
    rayleigh_test <- base::data.frame(spatial_bearing(data))
    rayleigh_double <- base::data.frame(spatial_bearing_double(data))
    
    summary_data <- base::cbind(data, benn_indices, rayleigh_test, rayleigh_double) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    summary_data[base::c(18, 20)][summary_data[c(18, 20)]<=0.01] <- "< 0.01"
    base::return(summary_data)
  })
  
  # Summary data table with all indices and orientation tests for series
  output$summary_table_sm <- DT::renderDataTable(
    summary_data_sm(),
    rownames = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_data_sm()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )
  
  
  
  # summary data at sample scale
  summary_data2_sm <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data <- summary_data_sm()
    level <- base::unique(summary_data_sm()$sample)
    results <- base::data.frame(base::matrix(nrow = base::length(level), ncol = 12))
    
    for (i in level) {
      data_level <- base::subset(data, sample == i)
      
      if (base::nrow(data_level >= input$minimum_n)) {
        N <- base::nrow(data_level)
        IS <- base::round(base::mean(data_level$IS), 2)
        EL <- base::round(base::mean(data_level$EL), 2)
        PL <- base::round(base::mean(data_level$PL), 2)
        sd.IS <- base::round(stats::sd(data_level$IS), 2)
        sd.EL <- base::round(stats::sd(data_level$EL), 2)
        sd.PL <- base::round(stats::sd(data_level$PL), 2)
        L <- base::round(base::mean(data_level$L), 2)
        Tr <- base::round(base::sum(data_level$R.p < 0.05) * 100 / base::nrow(data_level), 2)
        L.double <- base::round(base::mean(data_level$L.double), 2)
        Tr.double <- base::round(base::sum(data_level$R.p.double < 0.05) * 100 / base::nrow(data_level), 2)
        
        results[i, ] <- base::c(i, N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
        column_names <- base::c("sample", "N", "IS", "EL", "PL", "sd.IS", "sd.EL", "sd.PL", "L", "Tr", "L.double", "Tr.double")
        base::colnames(results) <- column_names
      }
    }
    level_data <- base::data.frame(sample = level)
    results_def <- base::merge(level_data, results, by = "sample") %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    base::return(results_def)
  })
  
  # Summary data table2 with all indices and orientation tests at sample scale
  output$summary_table2_sm <- DT::renderDataTable(
    summary_data2_sm(),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_data2_sm()),
      # lengthMenu = c(5, 10, 15, 20),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  ### --- spatial exploration ---
  
  # User chooses levels to display
  output$sample_se <- renderUI({
    req(input$user_data)
    radioButtons("sample_se", 
                 label = strong("Filter by sample"), 
                 choices = base::unique(app_data()$sample))
  })
  
  # User chooses codes to display
  output$code_se <- renderUI({
    req(input$user_data)
    if (input$plot_all_samples_se) {
      app_data_temp <- app_data()
    } else {
      app_data_temp <- app_data() %>% dplyr::filter(sample == input$sample_se)
    }
    radioButtons("code_se", 
                 label = strong("Filter by code"), 
                 choices = base::unique(app_data_temp$code))
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
    base::return(data_sub)
  })
  
  # get colors for colored ternary diagram
  cols <- reactiveVal(Ternary::TernaryPointValues(rgb))
  
  # calculating benn indices for the user subset data
  benn_index_se <- reactive({
    req(input$n_nearest_se)
    
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se) {
      benn <- matrix(
        nrow = base::length(base::unique(level)), ncol = 6,
        dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in base::unique(level)) {
        xyz_level <- base::subset(xyz, level == l)
        if (base::nrow(xyz_level) < min_sample) {
          benn[l, ] <- base::c(base::nrow(xyz_level), base::rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- base::c(base::nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      base::return(benn)
    }
    
    spatial_benn <- function(xyz, nearest = input$n_nearest_se, maximum_distance = NA) {
      benn <- base::matrix(NA,
                     nrow = base::nrow(xyz), ncol = 2,
                     dimnames = base::list(base::rownames(xyz), base::c("elongation", "isotropy"))
      )
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- base::order(d)
        if (!base::is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, c("EL", "IS")]
      }
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      base::return(base::list(benn))
    }
    
    benn_se <- base::data.frame(spatial_benn(subset_code_sample_se())) %>%
      dplyr::mutate(PL = 1 - isotropy - elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::rename(EL = elongation)
    
    data <- base::cbind(subset_code_sample_se(), benn_se) # , rayleigh_test, rayleigh_double)
    base::return(data)
  })
  
  # interactive ternary diagram
  interactive_ternary_plot <- reactive({
    data_to_plot <- benn_index_se()
    
    colors <- data_to_plot %>%
      dplyr::select(IS, EL, PL) %>%
      dplyr::mutate(color_code = grDevices::rgb(IS, EL, PL)) %>%
      dplyr::select(color_code)
    colors_p <- stats::setNames(colors$color_code, data_to_plot$id)
    
    fig <- plot_ly(
      data = data_to_plot,
      a = ~IS, b = ~PL, c = ~EL,
      customdata = ~id,
      type = "scatterternary",
      mode = "markers",
      marker = base::list(
        size = 5,
        color = colors_p
      ),
      text = ~ base::paste("sample:", sample, "<br>code", code)
    ) %>%
      plotly::layout(
        title = "",
        ternary = base::list(
          aaxis = base::list(title = ""),
          baxis = base::list(title = "PL"),
          caxis = base::list(title = "EL")
        )
      )
    
    plotly::event_register(fig, "plotly_selected")
    
    base::return(fig)
  })
  
  # plot the interactive ternary
  output$interactive_ternary <- renderPlotly({
    interactive_ternary_plot()
  })
  
  # download XY ortho path
  ortho_xy_se <- reactive({
    ortho <- raster::stack(input$ortho_xy_se$datapath)
    base::return(ortho)
  })
  
  # download XZ ortho path
  ortho_xz_se <- reactive({
    ortho <- raster::stack(input$ortho_xz_se$datapath)
    base::return(ortho)
  })
  
  # download YY ortho path
  ortho_yz_se <- reactive({
    ortho <- raster::stack(input$ortho_yz_se$datapath)
    base::return(ortho)
  })
  
  # get in a reactive function the selected data
  selected_data <- reactive({
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- base::matrix(
        nrow = base::length(base::unique(level)), ncol = 6,
        dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in base::unique(level)) {
        xyz_level <- base::subset(xyz, level == l)
        if (base::nrow(xyz_level) < min_sample) {
          benn[l, ] <- base::c(nrow(xyz_level), base::rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- base::c(base::nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      return(benn)
    }
    
    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- base::matrix(NA,
                     nrow = base::nrow(xyz), ncol = 2,
                     dimnames = base::list(base::rownames(xyz), base::c("elongation", "isotropy"))
      )
      
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- base::order(d)
        if (!base::is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, base::c("EL", "IS")]
      }
      
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(base::cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      
      base::return(base::list(benn))
    }
    
    plotly_event_data <- plotly::event_data(event = "plotly_selected", priority = "event")
    req(plotly_event_data)
    
    selected <- dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata) %>%
      dplyr::select(-c(EL, IS, PL))
    
    new_benn <- base::cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::mutate(PL = 1 - EL - IS)
    
    base::return(new_benn)
  })
  
  # plot the selected data in a new benn diagram
  selected_benn_se1 <- reactive({
    data_to_plot <- selected_data() %>%
      dplyr::select(EL, IS, PL)
    
    tst <- function() {
      Ternary::TernaryPlot(
        atip = "", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      Ternary::ColourTernary(cols(), spectrum = NULL)
      Ternary::AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    grDevices::recordPlot()
  })
  
  output$selected_benn <- renderPlot({
    selected_benn_se1()
  })
  
  # download benn diagram
  output$download_benn_se1 <- downloadHandler(
    filename = function() {
      base::paste("benn_diagram", input$file_format_benn_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(selected_benn_se1())
      grDevices::dev.off()
    }
  )
  
  # orientation rose diagram for selected data
  orientation_rose_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    d <- selected_data()$orientation_pi + 180
    e <- base::c(selected_data()$orientation_pi, d)
    f <- circular::circular(e,
                  type = "angles", units = "degrees",
                  template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    grDevices::recordPlot()
  })
  
  # plot orientation_rose_se for selected data
  output$orientation_rose_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(orientation_rose_se())
  })
  
  # download orientation rose
  output$download_rose_o_se1 <- downloadHandler(
    filename = function() {
      base::paste("orientation_rose", input$file_format_rose_o_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_o_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_o_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(orientation_rose_se())
      grDevices::dev.off()
    }
  )
  
  # plunge rose diagram for selected data
  plunge_rose_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    k <- rose_diagram_plunge(selected_data()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    grDevices::recordPlot()
  })
  
  
  # plot plunge_rose_c for selected data
  output$plunge_rose_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(plunge_rose_se())
  })
  
  
  # download plunge rose
  output$download_rose_p_se1 <- downloadHandler(
    filename = function() {
      base::paste("plunge_rose", input$file_format_rose_p_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_p_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_p_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(plunge_rose_se())
      grDevices::dev.off()
    }
  )
  
  # schmidt diagram for selected data
  schmidt_diagram_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    l <- schmidt_diagram(selected_data()$bearing,
                         selected_data()$plunge,
                         pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    grDevices::recordPlot()
  })
  
  
  # plot schmidt_diagram_c for selected data
  output$schmidt_selected <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    base::print(schmidt_diagram_se())
  })
  
  
  # download plunge rose
  output$download_schmidt_se1 <- downloadHandler(
    filename = function() {
      base::paste("schmidt_diagram", input$file_format_schmidt_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(schmidt_diagram_se())
      grDevices::dev.off()
    }
  )
  
  
  # XY spatial projection with points
  projection_points_xy_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data_to_plot_bis <- selected_data()
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        data = selected_data(),
        ggplot2::aes(X1, Y1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Y1)
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
          fill = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Z1)
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
          fill = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        data = selected_data(),
        ggplot2::aes(Y1, Z1)
      ) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(Y1, Z1)
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
          fill = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    if (!input$include_xy_ortho_se) {
      xy <- ggplot2::ggplot(
        selected_data(),
        ggplot2::aes(X1, Y1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Y1,
          xend = selected_data()$X2,
          yend = selected_data()$Y2,
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xy <- ggplot2::ggplot(
        selected_data(),
        ggplot2::aes(X1, Y1)
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
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
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
        ggplot2::aes(X1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$X1,
          y = selected_data()$Z1,
          xend = selected_data()$X2,
          yend = selected_data()$Z2,
          col = grDevices::rgb(base::round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      xz <- ggplot2::ggplot(
        selected_data(),
        ggplot2::aes(X1, Z1)
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
          col = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
    benn_rgb <- base::cbind(
      data_to_plot_bis$IS * 255,
      data_to_plot_bis$EL * 255,
      data_to_plot_bis$PL * 255
    )
    
    if (!input$include_yz_ortho_se) {
      yz <- ggplot2::ggplot(
        selected_data(),
        ggplot2::aes(Y1, Z1)
      ) +
        ggplot2::geom_point(colour = "white", alpha = 0, size = 0.1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_segment(
          x = selected_data()$Y1,
          y = selected_data()$Z1,
          xend = selected_data()$Y2,
          yend = selected_data()$Z2,
          col = grDevices::rgb(round(benn_rgb[, 1:3], 0),
                    maxColorValue = 255
          ),
          linewidth = 1.5,
          linetype = "solid"
        ) +
        ggplot2::theme_minimal()
    } else {
      yz <- ggplot2::ggplot(
        selected_data(),
        ggplot2::aes(Y1, Z1)
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
          col = grDevices::rgb(round(benn_rgb[, 1:3], 0),
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
        base::print(req(projection_points_xy_se()))
      } else {
        base::print(req(projection_sticks_xy_se()))
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
        base::print(projection_points_xz_se())
      } else {
        base::print(projection_sticks_xz_se())
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
        base::print(projection_points_yz_se())
      } else {
        base::print(projection_sticks_yz_se())
      }
    },
    height = 300,
    width = 780
  )
  
  
  # download xy projection
  output$download_xy_se1 <- downloadHandler(
    filename = function() {
      base::paste("XY projection", input$file_format_xy_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xy_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xy_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        base::print(projection_points_xy_se())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_xy_se())
        grDevices::dev.off()
      }
    }
  )
  
  
  # download xz projection
  output$download_xz_se1 <- downloadHandler(
    filename = function() {
      base::paste("XZ projection", input$file_format_xz_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_xz_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_xz_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        base::print(projection_points_xz_se())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_xz_se())
        grDevices::dev.off()
      }
    }
  )
  
  
  # download yz projection
  output$download_yz_se1 <- downloadHandler(
    filename = function() {
      base::paste("YZ projection", input$file_format_yz_se1, sep = ".")
    },
    content = function(file) {
      if (input$file_format_yz_se1 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_yz_se1 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      if (input$plot_type_se) {
        base::print(projection_points_yz_se())
        grDevices::dev.off()
      } else {
        base::print(projection_sticks_yz_se())
        grDevices::dev.off()
      }
    }
  )
  
  
  # get selected data and add rayleigh tests
  summary_selected_data <- reactive({
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }
    
    
    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }
    
    # plotly_event_data <- event_data(event = 'plotly_selected', priority = "event")
    # req(plotly_event_data)
    selected <- selected_data() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)
    
    rayleigh_test <- base::round(base::data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- base::round(base::data.frame(spatial_bearing_double(selected)), 2)
    
    summary_data <- base::cbind(selected, rayleigh_test, rayleigh_double) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    summary_data[c(18, 20)][summary_data[c(18, 20)]<=0.01] <- "< 0.01"
    base::return(summary_data)
  })
  
  #
  
  # summary data at sample scale
  summary_sample_se <- reactive({
    req(input$user_data)
    req(input$data_type)
    
    data <- summary_selected_data()
    
    N <- base::nrow(data)
    IS <- base::round(base::mean(data$IS), 2)
    EL <- base::round(base::mean(data$EL), 2)
    PL <- base::round(base::mean(data$PL), 2)
    sd.IS <- base::round(stats::sd(data$IS), 2)
    sd.EL <- base::round(stats::sd(data$EL), 2)
    sd.PL <- base::round(stats::sd(data$PL), 2)
    L <- base::round(base::mean(data$L), 2)
    Tr <- base::round(base::sum(data$R.p < 0.05) * 100 / base::nrow(data), 2)
    L.double <- base::round(base::mean(data$L.double), 2)
    Tr.double <- base::round(base::sum(data$R.p.double < 0.05) * 100 / base::nrow(data), 2)
    
    result <- base::data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    base::return(result)
  })
  
  # summary statistics of selected data
  output$selected_sample_summary <- DT::renderDataTable(
    summary_sample_se() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3))),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_sample_se()),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  # get selected data
  output$click <- DT::renderDataTable(
    summary_selected_data() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3))),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_selected_data()),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )
  
  ####################################################### from projection
  # reactive function for interactive projection
  interactive_projection_plot <- reactive({
    benn_index_se()
    
    if (input$axis == "XY") {
      fig <- ggplot(benn_index_se(), ggplot2::aes(x = X1, y = Y1)) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(
            round(data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme_minimal()
    } else if (input$axis == "XZ") {
      fig <- ggplot(benn_index_se(), ggplot2::aes(x = X1, y = Z1)) +
        geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(
            base::round(data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme_minimal()
    } else {
      fig <- ggplot2::ggplot(benn_index_se(), ggplot2::aes(x = Y1, y = Z1)) +
        ggplot2::geom_point(
          shape = 21, col = "black",
          fill = grDevices::rgb(
            base::round(base::data.frame(
              benn_index_se()$IS * 255,
              benn_index_se()$EL * 255,
              benn_index_se()$PL * 255
            ), 0),
            maxColorValue = 255
          ),
          size = 2.3,
          stroke = 0.3
        ) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme_minimal()
    }
    base::return(fig)
  })
  
  
  output$interactive_projection <- renderPlot({
    interactive_projection_plot()
  })
  
  # get in a reactive function the selected data
  selected_data2 <- reactive({
    
    benn <- function(xyz, level = "All Points", min_sample = input$n_nearest_se2) {
      benn <- base::matrix(
        nrow = base::length(base::unique(level)), ncol = 6,
        dimnames = base::list(base::unique(level), base::c("N", "E1", "E2", "E3", "IS", "EL"))
      )
      for (l in base::unique(level)) {
        xyz_level <- subset(xyz, level == l)
        if (nrow(xyz_level) < min_sample) {
          benn[l, ] <- base::c(nrow(xyz_level), rep(NA, 5))
        } else {
          # Normalize and compute eigen values
          e <- eigen_values(vector_normals(xyz_level))
          # Compute shape indices for Benn Diagram
          isotropy <- e$values[3] / e$values[1]
          elongation <- 1 - (e$values[2] / e$values[1])
          benn[l, ] <- c(nrow(xyz_level), e$values[1], e$values[2], e$values[3], isotropy, elongation)
        }
      }
      base::return(benn)
    }
    
    spatial_benn <- function(xyz, nearest = input$n_nearest_se2, maximum_distance = NA) {
      benn <- base::matrix(NA,
                     nrow = base::nrow(xyz), ncol = 2,
                     dimnames = base::list(base::rownames(xyz), c("elongation", "isotropy"))
      )
      
      # For each artifact, get the nearest artifacts and compute Benn values
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        sorted_pos <- order(d)
        if (!base::is.na(maximum_distance)) sorted_pos <- sorted_pos[d[sorted_pos] <= maximum_distance]
        xyz_subsample <- xyz[sorted_pos[1:nearest], ]
        benn[k, ] <- benn(xyz_subsample, min_sample = nearest)[, base::c("EL", "IS")]
      }
      
      # Calculate where the points would fall on Benn diagram so colors can be assigned
      b <- benn_coords(cbind(elongation = benn[, "elongation"], isotropy = benn[, "isotropy"]))
      xp <- b[, 1]
      yp <- b[, 2]
      
      base::return(base::list(benn))
    }
    
    # plotly_event_data <- event_data(event = "plotly_selected", priority = "event")
    # req(plotly_event_data)
    
    event_data <- shiny::brushedPoints(benn_index_se(), input$plot1_brush)
    req(event_data)
    
    selected <- dplyr::filter(benn_index_se(), id %in% event_data$id) %>%
      dplyr::select(-c(EL, IS, PL))
    
    new_benn <- base::cbind(
      selected,
      spatial_benn(selected)
    ) %>%
      dplyr::rename(EL = elongation) %>%
      dplyr::rename(IS = isotropy) %>%
      dplyr::mutate(PL = 1 - EL - IS)
    
    base::return(new_benn)
  })
  
  # plot the selected data in a new benn diagram
  selected_benn2_se2 <- reactive({
    req(input$plot1_brush)
    data_to_plot <- selected_data2() %>%
      dplyr::select(EL, IS, PL)
    
    tst <- function() {
      Ternary::TernaryPlot(
        atip = "", btip = "EL", ctip = "PL", grid.col = "lightgrey",
        grid.minor.lines = 0, tip.cex = 1, axis.labels = 0, grid.lines = 2
      )
      Ternary::ColourTernary(cols(), spectrum = NULL)
      Ternary::AddToTernary(points, data_to_plot[, c(2, 1, 3)], pch = 16, cex = 0.5, col = "black")
    }
    tst()
    grDevices::recordPlot()
  })
  
  # plot the selected data in a new benn diagram
  output$selected_benn2 <- renderPlot({
    req(input$plot1_brush)
    base::print(selected_benn2_se2())
  })
  
  
  # download benn diagram
  output$download_benn_se2 <- downloadHandler(
    filename = function() {
      base::paste("benn_diagram", input$file_format_benn_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_benn_se2 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_benn_se2 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(selected_benn2_se2())
      grDevices::dev.off()
    }
  )
  
  # orientation rose diagram for selected data
  orientation_rose_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    d <- selected_data2()$orientation_pi + 180
    e <- base::c(selected_data2()$orientation_pi, d)
    f <- circular::circular(e,
                  type = "angles", units = "degrees",
                  template = "geographics", modulo = "2pi"
    )
    i <- circular::rose.diag(f, bins = 18, axes = TRUE, prop = 2.1, col = "grey", border = "black", ticks = TRUE)
    grDevices::recordPlot()
  })
  
  # plot orientation_rose_se for selected data
  output$orientation_rose_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    base::print(orientation_rose_se2())
  })
  
  
  # download orientation rose
  output$download_rose_o_se2 <- downloadHandler(
    filename = function() {
      base::paste("orientation_rose", input$file_format_rose_o_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_o_se2 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_o_se2 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(orientation_rose_se2())
      grDevices::dev.off()
    }
  )
  
  
  # plunge rose diagram for selected data
  plunge_rose_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    k <- rose_diagram_plunge(selected_data2()$plunge, bins = 9, bar_col = "grey", cex = 0.7)
    k <- k + coord_fixed(ratio = 1)
    grDevices::recordPlot()
  })
  
  
  # plot plunge_rose_c for selected data
  output$plunge_rose_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    base::print(plunge_rose_se2())
  })
  
  
  # download plunge rose
  output$download_rose_p_se2 <- downloadHandler(
    filename = function() {
      base::paste("plunge_rose", input$file_format_rose_p_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_rose_p_se2 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_rose_p_se2 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(plunge_rose_se2())
      grDevices::dev.off()
    }
  )
  
  
  # schmidt diagram for selected data
  schmidt_diagram_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    l <- schmidt_diagram(selected_data2()$bearing,
                         selected_data2()$plunge,
                         pch = 19, cex = 0.8, col = "black", color_filled = TRUE
    )
    grDevices::recordPlot()
  })
  
  
  # plot Schmidt diagram for selected data
  output$schmidt_selected2 <- renderPlot({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    base::print(schmidt_diagram_se2())
  })
  
  
  # download Schmidt
  output$download_schmidt_se2 <- downloadHandler(
    filename = function() {
      base::paste("schmidt_diagram", input$file_format_schmidt_se2, sep = ".")
    },
    content = function(file) {
      if (input$file_format_schmidt_se2 == "png") {
        grDevices::png(file, width = 20, height = 20, units = "cm", res = 800)
      } else if (input$file_format_schmidt_se2 == "pdf") {
        grDevices::pdf(file)
      } else {
        grDevices::jpeg(file, width = 20, height = 20, units = "cm", res = 800)
      }
      grDevices::replayPlot(schmidt_diagram_se2())
      grDevices::dev.off()
    }
  )
  
  
  # get selected data and add rayleigh tests
  summary_selected_data2 <- reactive({
    req(input$plot1_brush)
    spatial_bearing <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[base::order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$orientation_pi, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L = near_avg_bearing_Rbar * 100, R.p = near_avg_bearing_p))
    }
    
    
    spatial_bearing_double <- function(xyz, nearest = input$n_nearest_se2) {
      # Make a place to hold the computed mean bearings of nearest neighbors
      near_avg_bearing_p <- base::vector(mode = "numeric", length = base::nrow(xyz))
      near_avg_bearing_Rbar <- base::vector(mode = "numeric", length = base::nrow(xyz))
      
      # Go through each artifact, get the nearest (default is 39), and compute mean bearing angle
      for (k in 1:base::nrow(xyz)) {
        centerx <- xyz$X1[k]
        centery <- xyz$Y1[k]
        centerz <- xyz$Z1[k]
        d <- base::sqrt((centerx - xyz$X1)^2 + (centery - xyz$Y1)^2 + (centerz - xyz$Z1)^2)
        xyz_subsample <- xyz[base::order(d)[1:nearest], ]
        
        # Get the mean bearing angle, test significance, and mean plunge angle of this subset of artifacts
        near_avg_bearing_p[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$p.value, 2)
        near_avg_bearing_Rbar[k] <- base::round(CircStats::r.test(2 * xyz_subsample$angle_double, degree = TRUE)$r.bar, 2)
      }
      
      # Color coding based on average bearing and on average plunge (higher plunge angles are more less saturated - i.e. more white)
      base::return(base::list(L.double = near_avg_bearing_Rbar * 100, R.p.double = near_avg_bearing_p))
    }
    
    selected <- selected_data2() # dplyr::filter(benn_index_se(), id %in% plotly_event_data$customdata)
    
    rayleigh_test <- base::round(base::data.frame(spatial_bearing(selected)), 2)
    rayleigh_double <- base::round(base::data.frame(spatial_bearing_double(selected)), 2)
    
    summary_data <- base::cbind(selected, rayleigh_test, rayleigh_double) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3)))
    summary_data[c(18, 20)][summary_data[c(18, 20)]<=0.01] <- "< 0.01"
    base::return(summary_data)
  })
  
  
  # summary data at sample scale
  summary_sample_se2 <- reactive({
    req(input$user_data)
    req(input$data_type)
    req(input$plot1_brush)
    
    data <- summary_selected_data2()
    
    N <- base::nrow(data)
    IS <- base::round(base::mean(data$IS), 2)
    EL <- base::round(base::mean(data$EL), 2)
    PL <- base::round(base::mean(data$PL), 2)
    sd.IS <- base::round(stats::sd(data$IS), 2)
    sd.EL <- base::round(stats::sd(data$EL), 2)
    sd.PL <- base::round(stats::sd(data$PL), 2)
    L <- base::round(base::mean(data$L), 2)
    Tr <- base::round(base::sum(data$R.p < 0.05) * 100 / base::nrow(data), 2)
    L.double <- base::round(base::mean(data$L.double), 2)
    Tr.double <- base::round(base::sum(data$R.p.double < 0.05) * 100 / base::nrow(data), 2)
    
    result <- base::data.frame(N, IS, EL, PL, sd.IS, sd.EL, sd.PL, L, Tr, L.double, Tr.double)
    base::return(result)
  })
  
  # summary statistics of selected data
  output$selected_sample_summary2 <- DT::renderDataTable(
    summary_sample_se2() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) base::round(x, 3))),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_sample_se2()),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollX = 250
    )
  )
  
  
  # get selected data
  output$click2 <- DT::renderDataTable(
    summary_selected_data2(),
    rownames = FALSE,
    extensions = base::c("Buttons", "Scroller"),
    options = base::list(
      pageLength = base::nrow(summary_selected_data2()),
      buttons = base::c("csv", "pdf", "copy"),
      dom = "Bfrtip",
      scrollY = 250,
      scrollX = 250
    )
  )
  
  ## --- Markdwon report ----
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      base::paste("report", sep = ".", switch(input$format,
                                        PDF = "pdf",
                                        HTML = "html",
                                        Word = "docx"
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      render_env <- new.env(parent = globalenv())
      render_env$benn_c <- benn_plot()
      render_env$xy_projection <- projection_points_xy_sm()
      render_env$xz_projection <- projection_points_xz_sm()
      render_env$yz_projection <- projection_points_yz_sm()
      render_env$benn_sm <- benn_sm_plot()
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = render_env
      )
    }
  )
} # end of server !!!