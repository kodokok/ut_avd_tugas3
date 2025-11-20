library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ===============================
# Baca data
# ===============================
data_path <- "~/Workspace/R/avd/tugas3/Tugas 3 - weather.xlsx"

raw_data <- read_excel(data_path, skip = 1)
colnames(raw_data) <- make.names(colnames(raw_data))


# ===============================
# UI
# ===============================
ui <- fluidPage(
  titlePanel("Visualisasi Interaktif — Tugas 3 AVD - Sabdo Triatmojo"),

  sidebarLayout(
    sidebarPanel(
      helpText("Pilih jenis plot dan variabel yang ingin divisualisasikan."),

      selectInput("plottype", "Jenis Plot:",
                  choices = c("Scatter" = "scatter",
                              "Line"    = "line",
                              "Bar"     = "bar",
                              "Tabel"   = "table")),

      uiOutput("x_ui"),
      uiOutput("y_ui"),

      checkboxInput("use_group", "Gunakan kolom kategori sebagai warna", FALSE),
      uiOutput("group_ui"),

      hr(),
      downloadButton("download_data", "Download Data (CSV)")
    ),

    mainPanel(
      conditionalPanel("input.plottype == 'table'",
                       DTOutput("datatable")),

      conditionalPanel("input.plottype != 'table'",
                       plotlyOutput("plot", height = "650px"))
    )
  )
)


# ===============================
# SERVER
# ===============================
server <- function(input, output, session){

  df <- reactive({
    raw_data %>% mutate_all(~ ifelse(. == "NA", NA, .))
  })

  cols <- colnames(raw_data)

  output$x_ui <- renderUI({
    selectInput("xvar", "Variabel X:", choices = cols)
  })

  output$y_ui <- renderUI({
    selectInput("yvar", "Variabel Y:", choices = cols)
  })

  output$group_ui <- renderUI({
    if (input$use_group)
      selectInput("group", "Kelompok (warna berdasarkan):",
                  choices = cols)
  })


  # ==============  TABLE  ==============
  output$datatable <- renderDT({
    datatable(df())
  })


  # ==============  PLOT  ==============
  output$plot <- renderPlotly({
    req(input$xvar, input$yvar)

    data <- df()
    xvar <- input$xvar
    yvar <- input$yvar
    group <- if (input$use_group) input$group else NULL

    # konversi angka jika perlu
    if (!is.numeric(data[[yvar]])) {
      data[[yvar]] <- suppressWarnings(as.numeric(data[[yvar]]))
    }

    # ----- scatter -----
    if (input$plottype == "scatter") {

      p <- ggplot(data, aes_string(x = xvar, y = yvar)) +
        geom_point(aes_string(color = group), size = 2, alpha = 0.9) +
        theme_minimal() +
        labs(title = paste("Scatter:", yvar, "vs", xvar))

      return(ggplotly(p))
    }


    # ----- line -----
    if (input$plottype == "line") {

      p <- ggplot(data, aes_string(x = xvar, y = yvar, group = group)) +
        geom_line(aes_string(color = group)) +
        geom_point(aes_string(color = group)) +
        theme_minimal() +
        labs(title = paste("Line:", yvar, "vs", xvar))

      return(ggplotly(p))
    }


    # ----- bar -----
    if (input$plottype == "bar") {

      agg <- data %>%
        group_by_at(xvar) %>%
        summarise(Value = sum(as.numeric(.data[[yvar]]), na.rm = TRUE))

      p <- ggplot(agg, aes_string(x = xvar, y = "Value")) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(title = paste("Bar:", yvar, "by", xvar),
             y = paste("Total", yvar)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      return(ggplotly(p))
    }

  })


  # ============== DOWNLOAD CSV ==============
  output$download_data <- downloadHandler(
    filename = function() paste0("Tugas3_Data_", Sys.Date(), ".csv"),
    content = function(file){
      write.csv(df(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
