
### ===== ###
### SETUP ###
### ===== ###

# TO RUN LOCALLY...
# source("setup.R")
# Sys.setenv(RETICULATE_PYTHON = reticulate::virtualenv_python("r-anscombify"))
# shiny::runApp(".")

# Required packages
required <- c("shiny", "bslib", "ggplot2", "reticulate", "sysfonts", "showtext")

# Return error if packages not installed
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  stop("Install the missing R packages: ", paste(missing, collapse = ", "), ".")
}

# Load shiny and ggplot2
library(shiny)
library(ggplot2)

# transform.R is used for the Affine transformation
source(file.path("R", "transform.R"), local = TRUE)

# Set maximum file upload size
options(shiny.maxRequestSize = 20 * 1024^2)

# Use Averia Serif Libre font (same as my website)
sysfonts::font_add(
  family = "Averia Serif Libre",
  regular = "www/fonts/AveriaSerifLibre-Regular.ttf",
  bold = "www/fonts/AveriaSerifLibre-Bold.ttf",
  italic = "www/fonts/AveriaSerifLibre-Italic.ttf",
  bolditalic = "www/fonts/AveriaSerifLibre-BoldItalic.ttf"
)
showtext::showtext_auto()

# Only functions are cached across sessions, images/results belong to each session
# The hosting platform supplies Python and its dependencies
python_backend <- local({
  backend <- NULL
  function() {
    if (is.null(backend)) {
      candidate <- new.env(parent = globalenv())
      reticulate::source_python(file.path("python", "image_to_points.py"), envir = candidate)
      backend <<- candidate
    }
    backend
  }
})

# Function to plot coordinates using ggplot()
plot_coordinates <- function(data, title, point_size, alpha, target = NULL, means = TRUE, regression = FALSE, equal_axes = TRUE) {
  
  # Create initial plot
  p <- ggplot(data, aes(x, y))
  
  # Add coordinates
  p <- p + 
    geom_point(
      size = point_size,
      alpha = alpha,
      colour = "#4b5782",
      shape = 16
    )
  
  # If requested, add dashed lines for variable means
  if (!is.null(target) && means) {
    p <- p + 
      geom_vline(
        xintercept = target$mean[1],
        colour = "#378dfc",
        linetype = "dashed",
        linewidth = 0.5
      ) +
      geom_hline(
        yintercept = target$mean[2],
        colour = "#378dfc",
        linetype = "dashed",
        linewidth = 0.5
      )
  }
  
  # If requested, add regression line
  if (!is.null(target) && regression) {
    slope <- target$cov[1, 2] / target$cov[1, 1]
    p <- p + 
      geom_abline(
        slope = slope,
        intercept = target$mean[2] - slope * target$mean[1],
        colour = "#5b62f4",
        linewidth = 0.6
      )
  }
  
  # Add additional components
  p <- p +
    labs(
      # title = title, 
      x = "x", 
      y = "y"
    ) + 
    theme_minimal(
      base_size = 16,
      base_family = "Averia Serif Libre"
    ) +
    theme(
      text = element_text(colour = "#4b5782"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12, colour = "#4b5782"),
      strip.text = element_text(size = 14, face = "bold", colour = "#4b5782"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#d9e3f1"),
      plot.background = element_rect(fill = "#f0f5fa", colour = NA),
      plot.margin = margin(12, 12, 12, 12)
    )

  # If equal axes requested, provide these
  if (equal_axes) {
    p <- p + 
      coord_equal()
  }
  
  return(p)
}

### ============== ###
### USER INTERFACE ###
### ============== ###

ui <- fluidPage(
  
  title = "Anscombify",
  
  # Match my website style (Morph bootswatch theme, Averia font)
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "morph",
    base_font = "Averia Serif Libre",
    heading_font = "Averia Serif Libre",
    "body-color" = "#4b5782",
    "headings-color" = "#4b5782",
    "web-font-path" = "false"
  ),
  
  # Head
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    )
  ),
  
  # Hero (title/subtitle)
  tags$header(class = "hero",
    h1("Anscombify"),
    p("Turn an image into a scatterplot with the means, variances and covariance of Anscombe’s Quartet.")
  ),
  
  # Body
  sidebarLayout(
    
    # Sidebar (input/settings)
    sidebarPanel(width = 3,
                 
      # 1. Image input
      h3("1", span(" Choose an image")),
      radioButtons(
        "source",
        NULL,
        c("Try the Labubu example" = "example", "Upload an image" = "upload")
      ),
      conditionalPanel(
        "input.source === 'upload'",
        helpText("Maximum 20 MB and 40 megapixels."),
        fileInput(
          "image",
          "Image file",
          accept = c(".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff")
        )
      ),
      
      # 2. Settings
      h3("2", span(" Configure settings")),
      sliderInput(
        "n_points",
        "Number of data points",
        min = 100,
        max = 10000,
        value = 2000,
        step = 100,
        ticks = FALSE
      ),
      
      # 2a. Collapsible image processing settings
      tags$details(
        tags$summary("Image processing settings"),
        selectInput(
          "polarity",
          "Foreground",
          c("Auto-detect" = "auto", "Dark on light" = "dark", "Light on dark" = "light")
        ),
        selectInput(
          "threshold_mode",
          "Threshold method",
          c("Automatic (Otsu)" = "otsu", "Manual" = "manual")
        ),
        conditionalPanel(
          "input.threshold_mode === 'manual'",
          sliderInput(
            "threshold",
            "Brightness threshold",
            min = 0,
            max = 255,
            value = 127,
            step = 1,
            ticks = FALSE
          )
        ),
        selectInput(
          "blur",
          "Smoothing",
          c("None" = 0, "Light" = 3, "Medium" = 5, "Strong" = 7),
          selected = 3
        ),
        sliderInput(
          "min_length",
          "Minimum contour length (pixels)",
          min = 0,
          max = 200,
          value = 10,
          step = 1,
          ticks = FALSE
        ),
        selectInput(
          "max_dimension",
          "Maximum processing dimension",
          c("800 px" = 800, "1,600 px" = 1600, "2,400 px" = 2400),
          selected = 1600
        )
      ),
      
      # 2b. Collapsible plotting settings
      tags$details(
        class = "display-options",
        tags$summary("Plot settings"),
        sliderInput(
          "point_size",
          "Point size",
          min = 0.2,
          max = 3,
          value = 0.8,
          step = 0.1,
          ticks = FALSE
        ),
        sliderInput(
          "alpha",
          "Point opacity",
          min = 0.1,
          max = 1,
          value = 0.9,
          step = 0.05,
          ticks = FALSE
        ),
        checkboxInput(
          "regression",
          "Show regression line",
          TRUE
        ),
        checkboxInput(
          "means",
          "Show target means",
          FALSE
        ),
        checkboxInput(
          "equal_axes",
          "Equal units on both axes",
          FALSE
        ),
        textInput(
          "ggsave_width",
          "Width (inches)",
          6
        ),
        textInput(
          "ggsave_height",
          "Height (inches)",
          6
        ),
        textInput(
          "ggsave_dpi",
          "DPI",
          300
        )
      ),
      
      # Run button
      actionButton(
        "run",
        "Anscombify",
        class = "btn-outline-primary run-button"
      ),
    ),
    
    # Main section (results)
    mainPanel(width = 9,
              
      # Status field
      uiOutput("status"),
      
      # There are 5 tabs
      tabsetPanel(id = "tabs",
                  
        # Plots tab
        tabPanel("Plots",
          div(
            class = "plot-card",
            fluidRow(
              column(6, h4("Original"), plotOutput("before", height = "490px")),
              column(6, h4("Anscombified"), plotOutput("after", height = "490px"))
            )
          ),
          uiOutput("stat_cards"),
          div(
            class = "download-card",
            h4("Export results"),
            uiOutput("downloads")
          )
        ),
        
        # Image processing tab
        tabPanel("Image processing",
          div(
            class = "plot-card",
            fluidRow(
              column(6, h4("Input image"), imageOutput("original_image", height = "480px")),
              column(6, h4("Threshold mask"), imageOutput("mask_image", height = "480px"))
            ),
            h4("Settings used"),
            tableOutput("extraction_table")
          )
        ),
        
        # Statistics tab
        tabPanel("Statistics",
          div(
            class = "plot-card",
            h4("Descriptive statistics"),
            p("These are the descriptive statistics before and after applying the Anscombify transformation. Variances and covariance are calculated using the unbiased sample denominator, n−1."),
            tableOutput("statistics")
          )
        ),
        
        # Anscombe's Quartet tab
        tabPanel("Anscombe's Quartet",
          div(
            class = "plot-card",
            plotOutput("quartet", height = "500px")
          )
        ),
        
        # How it works (explanation) tab
        tabPanel("How it works",
          div(
            class = "plot-card method-copy",
            h3("Visualise your data!"),
            p("That's the message of Anscombe's Quartet. It's something that professors have drilled into the heads of generations of budding scientists. Even if you're unfamiliar with the name, chances are you've seen Anscombe's Quartet before. It really is a cornerstone of statistical pedagogy. Unfortunately, it's also quite boring and ugly. Sorry! This ShinyApp turns an image of your choosing (or a lovely Labubu) into a scatterplot with the means, variances and covariance of the original quartet. The steps taken are as follows:"),
            tags$ol(
              tags$li(strong("Process image: "), "We use the OpenCV Python library to turn the image into a binary foreground mask and derive contours. In other words, we scan the image to identify a clear outline."),
              tags$li(strong("Generate coordinates: "), "We generate coordinates evenly spaced along the derived contours, making sure that each contour has at least three coordinates. In other words, we generate data points where our image outline is."),
              tags$li(strong("Transform coordinates: "), "We apply an affine transformation to the coordinates to give them the means, variances, and covariance of the original Anscombe's Quartet."),
              tags$li(strong("Yay! "), "Anscombe's Labubu.")),
            h4("Tips for a better result"),
            p("Use an image that has a clean, high-contrast outline of an object. Overly detailed images, such as photographs of human faces, can produce many unwanted contours and messy results. You can try playing around with the image processing settings. If you upload an image resembling male genatalia, this will be detected and your mother will be informed."),
            p(tags$i("Created by Jacob Knyspel in October 2026.", tags$a(href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/anscombe.html", "Contact Me")))
          )
        )
      )
    )
  )
)

### ====== ###
### SERVER ###
### ====== ###

server <- function(input, output, session) {
  
  # Working directory
  work_dir <- tempfile("anscombify-")
  dir.create(work_dir)
  session$onSessionEnded(function() unlink(work_dir, recursive = TRUE))
  state <- reactiveValues(result = NULL, error = NULL)

  # Settings from input
  settings <- reactive({
    list(
      source = input$source,
      path = if (identical(input$source, "example")) {
        file.path("examples", "labubu.jpg")
      } else {
        input$image$datapath
      },
      filename = if (identical(input$source, "example")) {
        "labubu.jpg"
      } else {
        input$image$name
      },
      n_points = as.integer(input$n_points),
      polarity = input$polarity,
      threshold_mode = input$threshold_mode,
      threshold = as.numeric(input$threshold),
      blur = as.integer(input$blur),
      min_length = as.numeric(input$min_length),
      max_dimension = as.integer(input$max_dimension),
      target = "canonical"
    )
  })
  
  # Export settings (plot .png and .pdf)
  export_settings <- reactive({
    list(
      width = as.numeric(input$ggsave_width),
      height = as.numeric(input$ggsave_height),
      dpi = as.integer(input$ggsave_dpi)
    )
  })

  # Event handling (i.e., what to display when Anscombify is pressed)
  observeEvent(
    input$run, 
    {
      state$result <- NULL
      state$error <- NULL
      config <- settings()
      if (is.null(config$path) || !length(config$path)) {
        state$error <- "Please upload an image, or select the Labubu example."
        return()
      }
      
      # Progress bar information
      tryCatch(
        withProgress(
          message = "Anscombifying your image",
          value = 0,
          {
            incProgress(0.1, detail = "Preparing Python")
            backend <- python_backend()
            incProgress(0.15, detail = "Processing image")
            preview_path <- file.path(work_dir, "preview.png")
            mask_path <- file.path(work_dir, "mask.png")
            extracted <- backend$extract_points_for_r(
              path = config$path,
              n_points = config$n_points,
              polarity = config$polarity,
              threshold_mode = config$threshold_mode,
              threshold = config$threshold,
              blur = config$blur,
              min_length = config$min_length,
              max_dimension = config$max_dimension,
              preview_path = preview_path,
              mask_path = mask_path
            )
            raw <- as.data.frame(lapply(extracted$points, unlist, use.names = FALSE))
            names(raw) <- c("x", "y", "feature_id", "point_order")
            raw$feature_id <- as.integer(raw$feature_id)
            raw$point_order <- as.integer(raw$point_order)
            incProgress(0.55, detail = "Transforming coordinates")
            target <- anscombe_target()
            mapped <- map_to_target(raw, target$mean, target$cov)
            state$result <- list(
              raw = raw,
              transformed = mapped$data,
              transform = mapped,
              target = target,
              metadata = extracted$metadata,
              config = config,
              preview_path = preview_path,
              mask_path = mask_path
            )
            incProgress(0.2, detail = "Ready")
          }
        ),
        error = function(e) state$error <- conditionMessage(e)
      )
    },
    ignoreInit = TRUE
  )

  # Check current run status
  is_current <- reactive(!is.null(state$result) && identical(state$result$config, settings()))
  current_result <- reactive({
    validate(
      need(is.null(state$error), state$error),
      need(!is.null(state$result), "Choose an image and click Anscombify."))
    state$result
  })
  
  # Display current status message (not started, success, error)
  output$status <- renderUI({
    if (!is.null(state$error)) {
      return(div(
        class = "status-box status-error",
        role = "alert",
        state$error
      ))
    }
    if (is.null(state$result)) {
      return(div(
        class = "status-box",
        "Upload an image to start, or try the Labubu example. Then click Anscombify."
      ))
    }
    if (!is_current()) {
      return(div(
        class = "status-box",
        "Settings changed. Click Anscombify to update the result and downloads."
      ))
    }
    r <- state$result
    omitted <- r$metadata$contours_omitted
    div(
      class = "status-box status-success",
      role = "status",
      strong("Success! "), format(nrow(r$raw), big.mark = ","), " data points generated from ", r$metadata$contours_used," contours."
    )
  })

  # Generate plot before transformation
  before_plot <- reactive({
    r <- current_result()
    plot_coordinates(
      r$raw,
      "Original",
      input$point_size,
      input$alpha,
      equal_axes = input$equal_axes
    )
  })
  
  # Generate plot after transformation
  after_plot <- reactive({
    r <- current_result()
    plot_coordinates(r$transformed, "Anscombified", input$point_size, input$alpha,
                     target = r$target, means = input$means, regression = input$regression,
                     equal_axes = input$equal_axes)
  })
  
  # Scaling factor for plot DPI
  plot_res <- 110
  
  # Function to draw plot, tweaking DPI as needed
  draw_plot <- function(plot) {
    
    # Specify user pixel ratio
    ratio <- session$clientData$pixelratio
    if (is.null(ratio) || !is.finite(ratio) || ratio <= 0) {
      ratio <- 1
    }
    
    # Stop any showtext instance using the previous DPI
    try(showtext::showtext_end(), silent = TRUE)
    
    # Match the graphics device
    old <- showtext::showtext_opts(dpi = plot_res * ratio)
    on.exit(showtext::showtext_opts(old), add = TRUE)
    
    # Restart showtext using the correct DPI
    showtext::showtext_begin()
    on.exit(showtext::showtext_end(), add = TRUE, after = FALSE)
    
    print(plot)
  }
  
  # Render plot before transformation
  output$before <- renderPlot(
    draw_plot(before_plot()),
    res = plot_res,
    bg = "#f0f5fa",
    execOnResize = TRUE
  )
  
  # Render plot after transformation
  output$after <- renderPlot(
    draw_plot(after_plot()),
    res = plot_res,
    bg = "#f0f5fa",
    execOnResize = TRUE
  )
  
  # Render originally inputted image
  output$original_image <- renderImage({
    r <- current_result()
    list(
      src = r$preview_path,
      contentType = "image/png",
      alt = "Input image given to ShinyApp"
    )
  }, deleteFile = FALSE)
  
  # Render image mask used for contour extraction
  output$mask_image <- renderImage({
    r <- current_result()
    list(
      src = r$mask_path,
      contentType = "image/png",
      alt = "Generated image mask used for contour extraction"
    )
  }, deleteFile = FALSE)
  
  # Render cards with key statistics (means/variances/covariance/correlation)
  output$stat_cards <- renderUI({
    r <- current_result()
    s <- coordinate_stats(r$transformed)
    card <- function(label, value) {
      div(
        class = "stat-card",
        span(label),
        strong(value)
      )
    }
    div(
      class = "stats-grid",
      card("MEANS (x, y)", paste(formatC(s[1:2], digits = 4, format = "fg"), collapse = ", ")),
      card("VARIANCES (x, y)", paste(formatC(s[3:4], digits = 5, format = "fg"), collapse = ", ")),
      card("COVARIANCE", sprintf("%.1f", s[5])),
      card("CORRELATION", sprintf("%.3f", s[6]))
    )
  })
  
  # Render downloads buttons
  output$downloads <- renderUI({
    if (!is_current() || !is.null(state$error)) {
      return(p("Download options will appear after running."))
    }
    tagList(
      downloadButton("download_data", "Coordinates .csv"),
      downloadButton("download_png", "Plot .png"),
      downloadButton("download_pdf", "Plot .pdf")
    )
  })
  
  # Render image processing info table
  output$extraction_table <- renderTable({
    m <- current_result()$metadata
    data.frame(
      Setting = c(
        "Original dimensions",
        "Processing dimensions",
        "Foreground",
        "Brightness threshold",
        "Contours found",
        "Contours eligible",
        "Contours used",
        "Points generated"
      ),
      Value = c(
        paste(m$original_width, "×", m$original_height),
        paste(m$width, "×", m$height),
        m$polarity,
        round(m$threshold, 2),
        m$contours_found,
        m$contours_eligible,
        m$contours_used,
        m$n_points
      )
    )
    }, striped = TRUE, bordered = FALSE)
  
  # Compile statistics table
  numeric_stats <- reactive({
    r <- current_result()
    stats_table(r$raw, r$transformed, r$target)
  })
  
  # Render statistics table
  output$statistics <- renderTable({
    tbl <- numeric_stats()
    tbl[-1] <- lapply(tbl[-1], function(x) {
      rounded <- round(x, 3)
      rounded[rounded == 0] <- 0
      sprintf("%.3f", rounded)
    })
    tbl
  }, striped = TRUE, bordered = FALSE, align = "lrrr")
  
  # Extract original quartet data from datasets::anscombe
  quartet_data <- do.call(rbind, lapply(1:4, function(i) {
    data.frame(
      x = datasets::anscombe[[paste0("x", i)]],
      y = datasets::anscombe[[paste0("y", i)]],
      Dataset = paste("Quartet", as.roman(i))
    )
  }))
  
  # Render Anscombe's Quartet plots
  output$quartet <- renderPlot({
    p <- ggplot(quartet_data, aes(x, y)) +
      geom_abline(
        intercept = 3,
        slope = 0.5,
        colour = "#378dfc",
        linewidth = 0.6
      ) +
      geom_point(
        colour = "#4b5782",
        size = 2.4
      ) + facet_wrap(
        ~ Dataset,
        ncol = 2
      ) +
      coord_cartesian(
        xlim = c(3, 20),
        ylim = c(2, 14)
      ) +
      scale_x_continuous(
        breaks = c(4,8,12,16,20)
      ) +
      scale_y_continuous(
        breaks = c(4,8,12)
      ) +
      theme_minimal(
        base_size = 13,
        base_family = "Averia Serif Libre"
      ) +
      # labs(title = "Anscombe's Quartet") +
      theme(
        text = element_text(colour = "#4b5782"),
        axis.text = element_text(colour = "#4b5782"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#d9e3f1"),
        plot.background = element_rect(fill = "#f0f5fa", colour = NA),
        strip.text = element_text(face = "bold", colour = "#4b5782")
      )
    draw_plot(p)
  }, bg = "#f0f5fa", res = plot_res, execOnResize = TRUE)

  # Function to write .csv file
  write_precise_csv <- function(data, file) {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    data[numeric_cols] <- lapply(data[numeric_cols], round, digits = 5)
    utils::write.csv(data, file, row.names = FALSE)
  }
  
  # Configure downloadable coordinate .csv
  output$download_data <- downloadHandler(
    filename = function() {
      "anscombify_coordinates.csv"
    },
    content = function(file) {
      r <- current_result()
      data <- data.frame(
        original_x = r$raw$x,
        original_y = r$raw$y,
        transformed_x = r$transformed$x,
        transformed_y = r$transformed$y,
        feature_id = r$raw$feature_id,
        point_order = r$raw$point_order
      )
      write_precise_csv(data, file)
    }
  )
  
  # Function to save ggplot
  save_plot <- function(file, device) {
    
    # Extract and validate export settings (width, height, DPI)
    s <- export_settings()
    values <- unlist(s, use.names = FALSE)
    validate(need(
      length(values) == 3L && all(is.finite(values) & values > 0),
      "Enter positive numeric values for width, height and DPI."
    ))

    # Set font DPI before ggsave opens its graphics device
    old <- showtext::showtext_opts(dpi = s$dpi)
    on.exit(showtext::showtext_opts(old), add = TRUE)
    
    # Save ggplot
    ggplot2::ggsave(
      filename = file,
      plot = after_plot(),
      device = device,
      width = s$width,
      height = s$height,
      units = "in",
      dpi = s$dpi,
      bg = "#f0f5fa"
    )
  }
  
  output$download_png <- downloadHandler(
    filename = function() {
      "anscombify_plot.png"
    },
    content = function(file) {
      save_plot(file, device = "png")
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      "anscombify_plot.pdf"
    },
    content = function(file) {
      save_plot(file, device = "pdf")
    }
  )
  
}

### YAY SHINYAPP ###
shinyApp(ui, server)
