fretboardr_app <- function() {
  
  stopifnot("{shiny} is not installed" = requireNamespace("shiny", quietly = TRUE))
  
  ui <- fluidPage(
    fluidRow(
      actionButton("orientation", "Horizontal"),
      actionButton("flip_x", NULL, icon = icon("arrow-left", verify_fa = FALSE)),
      actionButton("flip_y", NULL, icon = icon("arrow-up", verify_fa = FALSE)),
      actionButton("add_marker", NULL, icon = icon("plus", verify_fa = FALSE))
    ),
    plotOutput("plot")
  )
  
  server <- function(input, output, session) {
    
    plot_args <- reactiveVal(default_args(diagram))
    rotation <- reactiveVal(1L)
    
    # Rotate fretboard
    observeEvent(input$orientation, {
      args <- plot_args()
      args$rotate <- !args$rotate
      
      updateActionButton(
        inputId = "orientation",
        label = if (args$rotate) "Vertical" else "Horizontal" 
      )
      
      plot_args(args)
    })
    
    # Flip fretboard about y axis
    observeEvent(input$flip_x, {
      args <- plot_args()
      args$flip <- switch(
        args$flip[1],
        none = "x",
        x = "none",
        y = "both",
        both = "y"
      )
      plot_args(args)
    })
    
    # Flip fretboard about x axis
    observeEvent(input$flip_y, {
      args <- plot_args()
      args$flip <- switch(
        args$flip[1],
        none = "y",
        x = "both",
        y = "none",
        both = "x"
      )
      plot_args(args)
    })
    
    # Add a point
    observeEvent(input$add_marker, {
      showModal(modalDialog(
        title = "Add a marker",
        selectInput(
          "new_marker_type", 
          "Marker type",
          choices = c(
            "Thumb" = "thmb", 
            "Index finger" = "indx", 
            "Middle finger" = "mddl", 
            "Ring finger" = "ring",
            "Pinky" = "pnky", 
            "Mute" = "mute", 
            "Barre" = "barr"
          )
        ),
        selectInput("new_marker_string", "String", 1:6),
        selectInput("new_marker_fret", "Fret", 1:12)
      ))
    })
    
    # Construct code to produce a diagram
    code <- reactive({
      args <- purrr::imap(plot_args(), ~ glue::glue("{.y} = {dput_chr(.x)}"))
      args <- paste0("  ", args, collapse = ",\n")
      glue::as_glue(paste(c("diagram(", args, ")"), collapse = "\n"))
    })
    
    output$plot <- renderPlot(
      eval(parse(text = code())),
      res = 120
    )
    
  }
  
  shinyApp(ui, server)
  
}
