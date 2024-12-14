library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinyjs)
library(curl)

ui <- page_sidebar(
  useShinyjs(),
  theme = bs_theme(
    bg = "#FFE8F7",
    fg = "#FF33A4",
    primary = "#FF69B4",
  ),
  title = "Pinktastic Data ✨",
  sidebar = sidebar(
    fileInput("file", "Upload CSV file", accept = c(".csv")),
    selectInput("plot_type", "Plot Type",
                choices = c("Scatter" = "scatter",
                            "Line" = "line",
                            "Bar" = "bar",
                            "Box" = "box")),
    selectInput("x_var", "X Variable", choices = NULL),
    selectInput("y_var", "Y Variable", choices = NULL),
    selectInput("color_var", "Color Variable", choices = NULL, multiple = FALSE),
    actionButton("pinkify", "Make it PINK! ✨", 
                 class = "btn-block", 
                 style = "background-color: pink; color: deeppink; font-weight: bold;")
  ),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Data Table"),
      DTOutput("data_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Plot"),
      plotOutput("plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()
  is_pink <- reactiveVal(FALSE)
  
  observeEvent(input$pinkify, {
    is_pink(!is_pink())
  })
  
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    data(df)
    
    updateSelectInput(session, "x_var", choices = names(df))
    updateSelectInput(session, "y_var", choices = names(df))
    updateSelectInput(session, "color_var", 
                      choices = c("None" = "none", names(df)))
  })
  
  output$data_table <- renderDT({
    req(data())
    datatable(data(), filter = "top", options = list(pageLength = 10))
  })
  
  output$plot <- renderPlot({
    req(data(), input$x_var, input$y_var)
    
    # Base plot
    if (input$color_var != "none") {
      p <- ggplot(data(), aes_string(x = input$x_var, y = input$y_var, 
                                     color = input$color_var))
    } else {
      p <- ggplot(data(), aes_string(x = input$x_var, y = input$y_var))
    }
    
    # Add geometry based on plot type
    if (is_pink()) {
      if (input$color_var == "none") {
        p <- switch(input$plot_type,
                    "scatter" = p + geom_point(size = 3, color = "deeppink"),
                    "line" = p + geom_line(linewidth = 1, color = "hotpink") + 
                      geom_point(size = 3, color = "deeppink"),
                    "bar" = p + geom_bar(stat = "identity", fill = "deeppink"),
                    "box" = p + geom_boxplot(fill = "pink", color = "deeppink"))
      } else {
        p <- switch(input$plot_type,
                    "scatter" = p + geom_point(size = 3),
                    "line" = p + geom_line(linewidth = 1) + geom_point(size = 3),
                    "bar" = p + geom_bar(stat = "identity"),
                    "box" = p + geom_boxplot()) +
          scale_color_manual(values = c("deeppink", "hotpink", "pink", "deeppink4", "magenta"))
      }
      
      sparkle <- "\u2728"  # Unicode sparkle emoji
      
      # Add sparkles ✨
      p <- p + 
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "pink", color = NA),
          panel.background = element_rect(fill = "mistyrose", color = NA),
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          text = element_text(color = "hotpink", face = "bold"),
          axis.text = element_text(color = "deeppink"),
          title = element_text(color = "deeppink4"),
          legend.key = element_rect(fill = "mistyrose"),
          legend.text = element_text(color = "deeppink"),
          legend.title = element_text(color = "deeppink4"),
          plot.title = element_text(size = 18)
        ) +
        labs(title = paste0(sparkle, " ", input$y_var, " vs ", input$x_var, " ", sparkle))
      
    } else {
      p <- switch(input$plot_type,
                  "scatter" = p + geom_point(size = 2),
                  "line" = p + geom_line(linewidth = 0.8) + geom_point(size = 2),
                  "bar" = p + geom_bar(stat = "identity"),
                  "box" = p + geom_boxplot()) +
        theme_minimal() +
        labs(title = paste(input$y_var, "vs", input$x_var))
    }
    
    p
  })
  
  # Make the pinkify button pulse when active
  observe({
    if (is_pink()) {
      runjs("
        $('#pinkify').css('animation', 'pulse 2s infinite');
        $('#pinkify').css('background-color', 'deeppink');
        $('#pinkify').css('color', 'white');
      ")
    } else {
      runjs("
        $('#pinkify').css('animation', 'none');
        $('#pinkify').css('background-color', 'pink');
        $('#pinkify').css('color', 'deeppink');
      ")
    }
  })
}

shinyApp(ui, server)
