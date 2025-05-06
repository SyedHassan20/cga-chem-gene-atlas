# =================================================
#  Title: CGA (Chemical-Gene Atlas)
#  Description: Four donuts (Gene Effect, System Affected,
#               Lethality Mode of Function, Lethality Timing),
#               plus network visualization & data table (with column search bars),
#               clickable PubMed links, and additional hyperlinks.
# =================================================

# -----------------------------
#  1) Load Required Libraries
# -----------------------------
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(visNetwork)
library(DT)
library(memoise)
library(shinycssloaders)
library(RColorBrewer)
library(viridis)
library(future)
library(promises)

# Increase allowed future globals size limit
options(future.globals.maxSize = 600 * 1024^2)
plan(multisession)

# -----------------------------
#  2) Define UI Layout
# -----------------------------
ui <- fluidPage(
  tags$head(
    # Import Google Font
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Saira+Stencil+One&display=swap"),
    
    # Custom CSS Styles
    tags$style(HTML("
      .top-section {
        background: url('CGA%20logo6.png') no-repeat center center;
        background-size: cover;
        padding: 150px;
        border-bottom: 1px solid #dee2e6;
        text-align: center;
      }

      .top-section h1 {
        font-family: 'Saira Stencil One', sans-serif;
        font-size: 150px;
        color: white;
        /* Stronger pop effect */
        text-shadow: 0 8px 16px rgba(0, 0, 0, 0.85), 
                     0 0px 35px rgba(255, 255, 255, 0.25);
        margin-bottom: 10px;
      }
      
      .top-section p {
        font-family: 'Arial', sans-serif;
        font-size: 28px;
        color: white;
        /* Refined for clarity */
        text-shadow: 0 2px 4px rgba(0, 0, 0, 0.6);
      }

      .footer {
        background-color: #f8f9fa; 
        padding: 10px;
        border-top: 1px solid #dee2e6; 
        text-align: center;
      }
      .scrollable-network {
        overflow-y: auto;
        height: 600px;
      }
      .scrollable-table {
        overflow-x: auto;
      }
      table.dataTable tbody td {
        text-align: center !important;
        vertical-align: middle !important;
      }
      
      /* Make all buttons the same size and blue */
        .btn-custom {
          display: block;         /* make the button a block-level element */
          margin: 0 auto;         /* center it horizontally */
          background: linear-gradient(to bottom, #78d1eb, #1f5bbf);
          color: white;
          border-radius: 8px;
          font-size: 14px;
          padding: 14px 28px;
          border: none;
          transition: 0.3s;
          font-weight: bold;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.2);
        }
        
        .btn-custom:hover {
          background: linear-gradient(to bottom, #5bb6d9, #154a9b);
          transform: scale(1.05);
        }
    "))
  ),
  
  # ---- Header
  div(class = "top-section",
      h1("CGA"),
      p(strong("Chemical-Gene Atlas")),
      p(strong("Explore Chemical-Gene Interactions for prenatal and postnatal diseases"))
  ),
  
  # ---- Single row with only a Reset button
  fluidRow(
    column(12,
           actionButton("resetBtn", "Reset", class = "btn-custom")
    )
  ),
  
  # ---- Four Donuts in a wellPanel
  wellPanel(
    fluidRow(
      # 1) Gene Effect
      column(3,
             selectInput("geneEffectSelect", "Gene Effect:", choices = c("All"), selected = "All"),
             actionButton("zoomGeneEffect", "Zoom", icon = icon("search"), class = "btn-custom"),
             plotlyOutput("geneEffectChart", width = "100%", height = "300px") %>% withSpinner()
      ),
      # 2) System Affected
      column(3,
             selectInput("systemAffectedSelect", "System Affected:", choices = c("All"), selected = "All"),
             actionButton("zoomSystemAffected", "Zoom", icon = icon("search"), class = "btn-custom"),
             plotlyOutput("systemAffectedChart", width = "100%", height = "300px") %>% withSpinner()
      ),
      # 3) Lethality Mode of Function
      column(3,
             selectInput("lethalityModeSelect", "Lethality Mode of Function:", 
                         choices = c("All"), selected = "All"),
             actionButton("zoomLethalityMode", "Zoom", icon = icon("search"),class = "btn-custom"),
             plotlyOutput("lethalityModeChart", width = "100%", height = "300px") %>% withSpinner()
      ),
      # 4) Lethality Timing
      column(3,
             selectInput("lethalityTimingSelect", "Lethality Timing:", 
                         choices = c("All","1st","2nd","3rd","Postnatal"), selected = "All"),
             actionButton("zoomLethalityTiming", "Zoom", icon = icon("search"),class = "btn-custom"),
             plotlyOutput("lethalityTimingChart", width = "100%", height = "300px") %>% withSpinner()
      )
    )
  ),
  
  # ---- Display current filters
  fluidRow(
    column(12,
           h4("Selected Filters", style = "text-align:center; margin-top:20px;"),
           p(textOutput("selectedGeneEffect"), style = "text-align:center;"),
           p(textOutput("selectedSystemAffected"), style = "text-align:center;"),
           p(textOutput("selectedLethalityMode"), style = "text-align:center;"),
           p(textOutput("selectedLethalityTiming"), style = "text-align:center;")
    )
  ),
  
  # ---- Network & Legend
  fluidRow(
    column(1),
    column(10,
           h4("Gene-Chemical Interaction Network", style = "text-align:center; margin-top:20px;"),
           div(class = "scrollable-network", 
               visNetworkOutput("networkPlot", width = "100%", height = "600px") %>% withSpinner()
           )
    ),
    column(1)
  ),
  
  # ---- Data Table (with column search and global search)
  fluidRow(
    column(12,
           h4("Filtered Data Table", style = "margin-top:20px; text-align:center;"),
           downloadButton("exportButton", "Export Data", class = "btn-custom"),
           div(class = "scrollable-table", 
               dataTableOutput("dataTable", width = "100%") %>% withSpinner()
           )
    )
  ),
  
  # ---- Contributing Partners ----
  div(
    # style = "background-color: white; padding: 20px 0; margin-bottom: 60px;",
    h3("Contributing Partners", style = "text-align: center; margin-bottom: 15px;"),
    
    tags$div(
      style = "display: flex; justify-content: center; align-items: center;",
      
      # UCSF
      tags$a(
        href   = "https://www.ucsf.edu",
        target = "_blank",
        tags$img(
          src    = "UCSF_logo.png",
          height = "70px",
          style  = "margin: 0 20px;"
        )
      ),
      
      # Imperial College London
      tags$a(
        href   = "https://www.imperial.ac.uk",
        target = "_blank",
        tags$img(
          src    = "imperial_logo.png", 
          height = "90px",
          style  = "margin: 0 20px;"
        )
      ),
      
      # Stanford Medicine
      tags$a(
        href   = "https://med.stanford.edu",
        target = "_blank",
        tags$img(
          src    = "stanford_logo.svg", 
          height = "50px",
          style  = "margin: 0 20px;"
        )
      ),
      
      # HOPE
      tags$a(
        href   = "https://www.pregnancylossanswers.org/study",
        target = "_blank",
        tags$img(
          src    = "hope_logo.png",     
          height = "70px",
          style  = "margin: 0 20px;"
        )
      )    
    )
  ),
  
  # ---- Footer
  div(
    class = "footer",
    style = "margin-top: 40px; padding: 20px 0; background-color: #f9f9f9; width: 100%;",
    
    fluidRow(
      # Left: UCSF logo + Contact + GitHub
      column(
        width = 6,
        div(
          style = "text-align: center; padding-left: 40px;",
          tags$img(
            src = "ucsf_sublogo.jpg",
            height = "40px",
            style = "margin-top: 10px; margin-bottom: 10px;",
            alt = "UCSF Bakar CHSI logo"
          ),
          br(),
          tags$a(
            href = "mailto:hassanbukhari605@gmail.com",
            target = "_blank",
            "Contact Us"
          ),
          br(),
          tags$strong("Code & data: "),
          tags$a(
            href = "https://github.com/SyedHassan20/GeneChem-Atlas",
            target = "_blank",
            "GitHub"
          )
        )
      ),
      
      # Right: Supported by info
      column(
        width = 6,
        div(
          style = "text-align: center; padding-right: 40px;",
          tags$p(tags$strong("Supported by:")),
          tags$p(
            tags$a(
              href = "https://www.marchofdimes.org",
              target = "_blank",
              "March of Dimes (MOD)"
            )
          ),
          tags$p(
            tags$a(
              href = "https://www.pregnancylossanswers.org/",
              target = "_blank",
              "TRIOS"
            )
          ),
          tags$p(
            tags$a(
              href = "https://www.pregnancylossanswers.org/",
              target = "_blank",
              "National Institutes of Health (NIH) Eunice Kennedy Shriver National Institute of Child Health and Human Development (NICHD) [R01 HD105256]"
            )
          )
        )
      )
    )
  )
)
  

# -----------------------------
#  3) Server Logic
# -----------------------------
server <- function(input, output, session) {
  
  # 1) Define custom palette for first 3 donuts
  my_pastel_colors <- c(
    "#1B3768", "#174A8F", "#1C6FBD", "#2986CC", "#4A9FD9",
    "lightcoral", "#B29375", "#A68D5A",                   
    "#BDAAC6", "#D2C3E0", "#D5B7E0", "#B69DC1", "#9283A8", 
    "lightseagreen", "#94B8B8", "#A7C7C7", "#B7D4D4"  
  )
  my_colors <- c(  "#0A3D2E","#11674A", "#1CA37A", "#29C79A","#5CE6B0", "#A7F2D2" )
  # 2) Define more diverse blue palette for the 4th donut
  my_blues <- c(
    "#1B3768", "#174A8F", "#1C6FBD", "#2986CC")
  
  # 3) Load merged data
  if (file.exists("/Users/hassan/Desktop/UCSF Lab Work/CGA App/data/merged_data.rds")) {
    merged_df_all <- readRDS("/Users/hassan/Desktop/UCSF Lab Work/CGA App/data/merged_data.rds") %>%
      mutate(
        System.Affected = str_trim(System.Affected),
        Lethality.Mode.of.Function = str_trim(Lethality.Mode.of.Function)
      ) %>%
      filter(
        System.Affected != "Unknown",
        System.Affected != ":",
        Lethality.Mode.of.Function != "Unknown",
        Lethality.Mode.of.Function != ":"
      )
  } else {
    stop("data/merged_data.rds not found. Please place your dataset.")
  }
  
  # 4) Rename columns if needed
  merged_df_all <- merged_df_all %>%
    rename_with(~str_replace_all(.x, "^X1st$", "1st")) %>%
    rename_with(~str_replace_all(.x, "^X2nd$", "2nd")) %>%
    rename_with(~str_replace_all(.x, "^X3rd$", "3rd"))
  
  # 5) Replace NA in main filter columns
  merged_df_all <- merged_df_all %>%
    mutate(
      Gene.Effect               = replace_na(Gene.Effect, "Unknown"),
      System.Affected           = replace_na(System.Affected, "Unknown"),
      Lethality.Mode.of.Function= replace_na(Lethality.Mode.of.Function, "Unknown")
    )
  
  # 6) Reactive filters
  reactiveFilters <- reactiveValues(
    selectedGeneEffect     = NULL,
    selectedSystemAffected = NULL,
    selectedLethalityMode  = NULL,
    selectedLethalityTiming= NULL
  )
  
  # 7) Master filter function (no memoise)
  filteredData <- function(gf, sa, lm, lt) {
    data <- merged_df_all
    if (!is.null(gf) && gf != "All") {
      data <- data %>% filter(Gene.Effect == gf)
    }
    if (!is.null(sa) && sa != "All") {
      data <- data %>% filter(System.Affected == sa)
    }
    if (!is.null(lm) && lm != "All") {
      data <- data %>% filter(Lethality.Mode.of.Function == lm)
    }
    if (!is.null(lt) && lt != "All") {
      data <- data %>% filter(!is.na(.data[[lt]]) & .data[[lt]] != "")
    }
    data
  }
  
  # Reactive expression for filtered data (used by network plot)
  filteredDataReactive <- reactive({
    filteredData(
      reactiveFilters$selectedGeneEffect,
      reactiveFilters$selectedSystemAffected,
      reactiveFilters$selectedLethalityMode,
      reactiveFilters$selectedLethalityTiming
    )
  })
  
  # 8) Populate the four dropdowns from the data
  observe({
    gf_choices <- sort(unique(merged_df_all$Gene.Effect))
    sa_choices <- sort(unique(merged_df_all$System.Affected))
    lm_choices <- sort(unique(merged_df_all$Lethality.Mode.of.Function))
    
    updateSelectInput(session, "geneEffectSelect",
                      choices = c("All", gf_choices), selected = "All")
    updateSelectInput(session, "systemAffectedSelect",
                      choices = c("All", sa_choices), selected = "All")
    updateSelectInput(session, "lethalityModeSelect",
                      choices = c("All", lm_choices), selected = "All")
  })
  
  # 9) Watch for user changes
  observeEvent(input$geneEffectSelect, {
    reactiveFilters$selectedGeneEffect <- 
      if (input$geneEffectSelect == "All") NULL else input$geneEffectSelect
  })
  observeEvent(input$systemAffectedSelect, {
    reactiveFilters$selectedSystemAffected <- 
      if (input$systemAffectedSelect == "All") NULL else input$systemAffectedSelect
  })
  observeEvent(input$lethalityModeSelect, {
    reactiveFilters$selectedLethalityMode <- 
      if (input$lethalityModeSelect == "All") NULL else input$lethalityModeSelect
  })
  observeEvent(input$lethalityTimingSelect, {
    reactiveFilters$selectedLethalityTiming <- 
      if (input$lethalityTimingSelect == "All") NULL else input$lethalityTimingSelect
  })
  
  # 10) Donut chart generator
  generate_donut_chart <- function(count_data, label_col, title, chart_source, color_palette) {
    if (nrow(count_data) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available"))
    }
    count_data[[label_col]] <- as.character(count_data[[label_col]])
    
    plot_ly(
      count_data,
      labels    = ~get(label_col),
      values    = ~n,
      type      = "pie",
      hole      = 0.1,
      textinfo  = "none",
      hoverinfo = "text",
      text      = ~paste0(get(label_col), "<br>", n, " (", round(n/sum(n)*100, 2), "%)"),
      source    = chart_source,
      marker    = list(colors = color_palette, line = list(color = "black", width = 0.2)),
      customdata= ~get(label_col)
    ) %>%
      layout(
        title = if (title == "Lethality Mode of Function") {
          list(text = "Lethality Mode of Function", x = 0.5)
        } else {
          list(text = title, x = 0.5)
        },
        showlegend = FALSE,
        margin = list(l = 0, r = 0, t = 60, b = 30),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      event_register("plotly_click")
  }
  
  # ----- Gene Effect Donut -----
  geneEffectData <- reactive({
    filteredData(
      reactiveFilters$selectedGeneEffect,
      reactiveFilters$selectedSystemAffected,
      reactiveFilters$selectedLethalityMode,
      reactiveFilters$selectedLethalityTiming
    ) %>%
      distinct(Gene_Name, Gene.Effect, .keep_all = TRUE) %>%
      count(Gene.Effect) %>%
      arrange(desc(n))
  })
  output$geneEffectChart <- renderPlotly({
    df <- geneEffectData()
    generate_donut_chart(df, "Gene.Effect", "Gene Effect",
                         "geneEffectChart", my_colors)
  })
  observeEvent(event_data("plotly_click", source = "geneEffectChart"), {
    ed <- event_data("plotly_click", source = "geneEffectChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedGeneEffect <- ed$customdata
      updateSelectInput(session, "geneEffectSelect", selected = ed$customdata)
    }
  })
  observeEvent(input$zoomGeneEffect, {
    showModal(modalDialog(
      title = "Gene Effect",
      renderPlotly({
        df <- geneEffectData()
        generate_donut_chart(df, "Gene.Effect", "Gene Effect",
                             "geneEffectChartZoomed", my_colors) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # ----- System Affected Donut -----
  systemAffectedData <- reactive({
    filteredData(
      reactiveFilters$selectedGeneEffect,
      reactiveFilters$selectedSystemAffected,
      reactiveFilters$selectedLethalityMode,
      reactiveFilters$selectedLethalityTiming
    ) %>%
      distinct(Gene_Name, System.Affected, .keep_all = TRUE) %>%
      filter(System.Affected != "Unknown", System.Affected != ":", System.Affected != "") %>%
      count(System.Affected) %>%
      arrange(desc(n))
  })
  output$systemAffectedChart <- renderPlotly({
    df <- systemAffectedData()
    generate_donut_chart(df, "System.Affected", "System Affected",
                         "systemAffectedChart", my_pastel_colors)
  })
  observeEvent(event_data("plotly_click", source = "systemAffectedChart"), {
    ed <- event_data("plotly_click", source = "systemAffectedChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedSystemAffected <- ed$customdata
      updateSelectInput(session, "systemAffectedSelect", selected = ed$customdata)
    }
  })
  observeEvent(input$zoomSystemAffected, {
    showModal(modalDialog(
      title = "System Affected",
      renderPlotly({
        df <- systemAffectedData()
        generate_donut_chart(df, "System.Affected", "System Affected",
                             "systemAffectedChartZoomed", my_pastel_colors) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # ----- Lethality Mode of Function Donut -----
  lethalityModeData <- reactive({
    filteredData(
      reactiveFilters$selectedGeneEffect,
      reactiveFilters$selectedSystemAffected,
      reactiveFilters$selectedLethalityMode,
      reactiveFilters$selectedLethalityTiming
    ) %>%
      distinct(Gene_Name, Lethality.Mode.of.Function, .keep_all = TRUE) %>%
      filter(Lethality.Mode.of.Function != "Unknown", 
             Lethality.Mode.of.Function != ":", 
             Lethality.Mode.of.Function != "") %>%
      count(Lethality.Mode.of.Function) %>%
      arrange(desc(n))
  })
  
  output$lethalityModeChart <- renderPlotly({
    df <- lethalityModeData()
    generate_donut_chart(df, "Lethality.Mode.of.Function", 
                         "Lethality Mode of Function",
                         "lethalityModeChart", my_colors)
  })
  observeEvent(event_data("plotly_click", source = "lethalityModeChart"), {
    ed <- event_data("plotly_click", source = "lethalityModeChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedLethalityMode <- ed$customdata
      updateSelectInput(session, "lethalityModeSelect", selected = ed$customdata)
    }
  })
  observeEvent(input$zoomLethalityMode, {
    showModal(modalDialog(
      title = "Lethality Mode of Function",
      renderPlotly({
        df <- lethalityModeData()
        generate_donut_chart(df, "Lethality.Mode.of.Function",
                             "Lethality Mode of Function",
                             "lethalityModeChartZoomed", my_colors) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # ----- Lethality Timing Donut -----
  lethalityTimingData <- reactive({
    filteredData(
      reactiveFilters$selectedGeneEffect,
      reactiveFilters$selectedSystemAffected,
      reactiveFilters$selectedLethalityMode,
      reactiveFilters$selectedLethalityTiming
    ) %>%
      select(Gene_Name, `1st`, `2nd`, `3rd`, `Postnatal`) %>%
      
      # Ensure distinct genes before pivoting
      distinct(Gene_Name, .keep_all = TRUE) %>% 
      
      # Reshape data from wide to long format
      pivot_longer(
        cols = c(`1st`, `2nd`, `3rd`, `Postnatal`),
        names_to = "Timing",
        values_to = "Value"
      ) %>%
      
      # Remove empty or NA values
      filter(!is.na(Value) & Value != "") %>%
      
      # Count unique gene occurrences per lethality timing category
      count(Timing) %>%
      
      # Order by highest counts
      arrange(desc(n))
  })
  
  # ---- Render the Lethality Timing Donut Chart ----
  output$lethalityTimingChart <- renderPlotly({
    df <- lethalityTimingData()
    generate_donut_chart(df, "Timing", "Lethality Timing",
                         "lethalityTimingChart", my_blues)
  })
  
  # ---- Handle Click Events for Filtering ----
  observeEvent(event_data("plotly_click", source = "lethalityTimingChart"), {
    ed <- event_data("plotly_click", source = "lethalityTimingChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedLethalityTiming <- ed$customdata
      updateSelectInput(session, "lethalityTimingSelect", selected = ed$customdata)
    }
  })
  
  # ---- Zoom Modal for Lethality Timing ----
  observeEvent(input$zoomLethalityTiming, {
    showModal(modalDialog(
      title = "Lethality Timing",
      renderPlotly({
        df <- lethalityTimingData()
        generate_donut_chart(df, "Timing", "Lethality Timing",
                             "lethalityTimingChartZoomed", my_blues) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  
  # ----- Display selected filters -----
  output$selectedGeneEffect <- renderText({
    if (is.null(reactiveFilters$selectedGeneEffect)) "Gene Effect: None"
    else paste("Gene Effect:", reactiveFilters$selectedGeneEffect)
  })
  output$selectedSystemAffected <- renderText({
    if (is.null(reactiveFilters$selectedSystemAffected)) "System Affected: None"
    else paste("System Affected:", reactiveFilters$selectedSystemAffected)
  })
  output$selectedLethalityMode <- renderText({
    if (is.null(reactiveFilters$selectedLethalityMode)) "Lethality Mode of Function: None"
    else paste("Lethality Mode of Function:", reactiveFilters$selectedLethalityMode)
  })
  output$selectedLethalityTiming <- renderText({
    if (is.null(reactiveFilters$selectedLethalityTiming)) "Lethality Timing: None"
    else paste("Lethality Timing:", reactiveFilters$selectedLethalityTiming)
  })
  
  # ----- Reset Button -----
  observeEvent(input$resetBtn, {
    reactiveFilters$selectedGeneEffect     <- NULL
    reactiveFilters$selectedSystemAffected <- NULL
    reactiveFilters$selectedLethalityMode  <- NULL
    reactiveFilters$selectedLethalityTiming<- NULL
    
    updateSelectInput(session, "geneEffectSelect",      selected = "All")
    updateSelectInput(session, "systemAffectedSelect",  selected = "All")
    updateSelectInput(session, "lethalityModeSelect",   selected = "All")
    updateSelectInput(session, "lethalityTimingSelect", selected = "All")
  })
  
  # ----- Network Plot -----
  output$networkPlot <- renderVisNetwork({
    data <- filteredDataReactive()
    future({
      if (nrow(data) == 0) return(NULL)
      if (!"X..ChemicalName" %in% names(data)) return(NULL)
      
      # Limit to 300 random rows if large
      data_for_net <- if (nrow(data) > 300) dplyr::sample_n(data, 300) else data
      
      gene_degrees <- data_for_net %>%
        group_by(Gene_Name) %>%
        summarise(degree = n(), .groups = "drop")
      
      nodes <- data.frame(
        id    = unique(c(data_for_net$Gene_Name, data_for_net$X..ChemicalName)),
        label = unique(c(data_for_net$Gene_Name, data_for_net$X..ChemicalName)),
        group = ifelse(
          unique(c(data_for_net$Gene_Name, data_for_net$X..ChemicalName)) %in% data_for_net$Gene_Name,
          "Gene", "Chemical"
        ),
        stringsAsFactors = FALSE
      ) %>%
        left_join(gene_degrees, by = c("id" = "Gene_Name")) %>%
        mutate(
          degree = ifelse(group == "Gene", degree, NA),
          value  = ifelse(group == "Gene", degree, 10)
        ) %>%
        mutate(value = ifelse(group == "Gene",
                              10 + (value / max(value, na.rm = TRUE)) * 20,
                              15))
      
      edges <- data.frame(
        from = data_for_net$Gene_Name,
        to   = data_for_net$X..ChemicalName,
        stringsAsFactors = FALSE
      )
      
      visNetwork(nodes, edges) %>%
        visNodes(scaling = list(min = 10, max = 30)) %>%
        visEdges(width = 1, color = "gray", smooth = FALSE, arrows = "to") %>%
        visGroups(groupname = "Gene", 
                  color = list(background = "lightcoral", border = "lightcoral")) %>%
        visGroups(groupname = "Chemical", 
                  color = list(background = "lightseagreen", border = "lightseagreen")) %>%
        visLegend(
          enabled = TRUE,
          useGroups = FALSE,       # <--- Turn off automatic group shapes in legend
          addNodes = data.frame(
            label = c("Gene", "Chemical"),
            shape = c("box", "box"),
            color = c("lightcoral", "lightseagreen"),
            stringsAsFactors = FALSE
          )
        ) %>%
        visOptions(
          highlightNearest = TRUE, 
          nodesIdSelection = list(
            enabled = TRUE, 
            main = HTML("Select by Gene or Chemical Name"),
            style = "width: 250px; height: 30px;"
          )
        )
    }, seed = TRUE) %...>%
      (function(result) {
        if (!is.null(result)) result else visNetwork(nodes = data.frame(), edges = data.frame())
      })
  })
  
  # ----- Data Table (with column search & hyperlinks) -----
  output$dataTable <- renderDataTable({
    df <- filteredData(
      gf = reactiveFilters$selectedGeneEffect,
      sa = reactiveFilters$selectedSystemAffected,
      lm = reactiveFilters$selectedLethalityMode,
      lt = reactiveFilters$selectedLethalityTiming
    )
    
    # Rename columns: Updated "PMID" and "PubMed IDs" names,
    # and keep all columns so that later we can filter out unwanted ones.
    df <- setNames(df, c(
      "Gene Name", "Gene ID", "OMIM Gene", "Gene Effect", "Variants", 
      "Lethality Mode of Function", "Coordinates (hg19)", "Disease", "OMIM Phenotype", 
      "Inheritance", "System Affected", "Prenatal Phenotypes", "Fetal Sex", 
      "Human Evidence", "Intolerome PubMed IDs", "OMIM Gene (Duplicate)", "1st", "2nd", 
      "3rd", "Postnatal", "Chemical Name", "Chemical ID", "CAS RN", 
      "Gene Symbol", "Gene Forms", "Organism", "Organism ID", "Interaction", 
      "Interaction Actions", "CTD PubMed IDs"
    ))
    
    # Remove unwanted columns from display:
    # Filtering out "OMIM Gene (Duplicate)", "Gene Symbol", "CAS RN", "Gene Forms", "Organism", "Organism ID"
    df <- df %>%
      select(
        `Gene Name`, `Gene ID`, `OMIM Gene`, `Gene Effect`, `Variants`,
        `Lethality Mode of Function`, `Coordinates (hg19)`, `Disease`,
        `OMIM Phenotype`, `Inheritance`, `System Affected`,
        `Prenatal Phenotypes`, `Fetal Sex`, `Human Evidence`, `Intolerome PubMed IDs`,
        `1st`, `2nd`, `3rd`, `Postnatal`,
        `Chemical Name`, `Chemical ID`, `Interaction`, `Interaction Actions`,
        `CTD PubMed IDs`
      )
    
    # Force certain columns to character
    problematic_cols <- c("Gene Name", "Gene ID", "OMIM Gene", "Gene Effect", 
                          "Disease", "Variants", "Fetal Sex", "Intolerome PubMed IDs", 
                          "1st", "2nd", "3rd", "CTD PubMed IDs")
    
    df <- df %>%
      mutate(across(all_of(problematic_cols), as.character))
    
    # Convert Intolerome PubMed IDs & CTD PubMed IDs into hyperlinks
    make_pubmed_links <- function(x) {
      if (is.null(x) || is.na(x) || x == "") return("")
      # Split on commas or semicolons
      id_list <- unlist(strsplit(x, "[,;]\\s*"))
      links <- paste0(
        "<a href='https://pubmed.ncbi.nlm.nih.gov/", id_list,
        "' target='_blank'>", id_list, "</a>"
      )
      paste(links, collapse = ", ")
    }
    
    # Convert other columns to hyperlinks
    make_gene_links <- function(x) {
      if (is.na(x) || x == "") return("")
      paste0(
        "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=", x,
        "' target='_blank'>", x, "</a>"
      )
    }
    make_omim_links <- function(x) {
      if (is.na(x) || x == "") return("")
      paste0(
        "<a href='https://omim.org/entry/", x,
        "' target='_blank'>", x, "</a>"
      )
    }
    make_ctdbase_links <- function(x) {
      if (is.na(x) || x == "") return("")
      paste0(
        "<a href='https://ctdbase.org/search/?query=", x,
        "' target='_blank'>", x, "</a>"
      )
    }
    
    if ("Intolerome PubMed IDs" %in% colnames(df)) {
      df$`Intolerome PubMed IDs` <- sapply(df$`Intolerome PubMed IDs`, make_pubmed_links)
    }
    if ("CTD PubMed IDs" %in% colnames(df)) {
      df$`CTD PubMed IDs` <- sapply(df$`CTD PubMed IDs`, make_pubmed_links)
    }
    
    # Add hyperlinks for Gene Name, OMIM Gene, OMIM Phenotype, Chemical ID
    if ("Gene Name" %in% colnames(df)) {
      df$`Gene Name` <- sapply(df$`Gene Name`, make_gene_links)
    }
    if ("OMIM Gene" %in% colnames(df)) {
      df$`OMIM Gene` <- sapply(df$`OMIM Gene`, make_omim_links)
    }
    if ("OMIM Phenotype" %in% colnames(df)) {
      df$`OMIM Phenotype` <- sapply(df$`OMIM Phenotype`, make_omim_links)
    }
    if ("Chemical ID" %in% colnames(df)) {
      df$`Chemical ID` <- sapply(df$`Chemical ID`, make_ctdbase_links)
    }
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        autoWidth  = FALSE,
        dom        = 'lfrtip',
        initComplete = JS("function(settings, json) { $('thead input').css({'width': '150px'}); }")
      ),
      rownames = FALSE,
      filter   = "top",
      escape   = FALSE
    )
  }, server = TRUE)
  
  # ----- Export Button -----
  output$exportButton <- downloadHandler(
    filename = function() { paste("GeneChem_Data", Sys.Date(), ".csv", sep="") },
    content  = function(file) {
      write.csv(
        filteredData(
          gf = reactiveFilters$selectedGeneEffect,
          sa = reactiveFilters$selectedSystemAffected,
          lm = reactiveFilters$selectedLethalityMode,
          lt = reactiveFilters$selectedLethalityTiming
        ),
        file,
        row.names = FALSE
      )
    }
  )
}

# -----------------------------
#  4) Run the App
# -----------------------------
shinyApp(ui = ui, server = server)

