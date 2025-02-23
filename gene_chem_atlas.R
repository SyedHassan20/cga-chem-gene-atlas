# =================================================
#  Title: GeneChem Atlas (Shiny App)
#  Description: Explore gene-chemical interactions,
#               including multi-donut filtering and
#               a network visualization.
#  =================================================

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
library(future)
library(promises)

# Increase the allowed future globals size limit (for large data)
options(future.globals.maxSize = 600 * 1024^2)

# Plan for asynchronous processing
plan(multisession)

# A memoised donut generator (with optional noColon=TRUE to omit colons in hover text)
memoised_generate_donut_chart <- memoise(function(count_data, label_col, title, chart_source, color_palette, noColon = FALSE) {
  generate_donut_chart(count_data, label_col, title, chart_source, color_palette, noColon)
})

# -----------------------------
#  2) Define the UI Layout
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Basic styling for header, footer, and layout */
      .top-section {
        background-color: #f8f9fa; 
        padding: 20px; 
        border-bottom: 1px solid #dee2e6; 
        text-align: center;
      }
      .footer {
        background-color: #f8f9fa; 
        padding: 10px; 
        border-top: 1px solid #dee2e6; 
        text-align: center;
      }
      .facet-panel { margin-top: 20px; }
      /* We omit facet-title or set it to empty so we only see chart titles, not extra headings */
      .legend {
        margin-top: 10px;
        padding: 10px;
        background-color: #ffffff;
        border: 1px solid #dddddd;
        border-radius: 4px;
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }
      .legend-color {
        width: 20px;
        height: 20px;
        margin-right: 5px;
        border: 1px solid #000;
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
    "))
  ),
  
  # ---- Header (title only) ----
  div(class = "top-section",
      h1("GeneChem Atlas"),
      p("Explore gene-chemical interactions linked to genetic and prenatal conditions.")
  ),
  
  # ---- Single row with only a Reset button (no search bar)
  fluidRow(
    column(12,
           actionButton("resetBtn", "Reset", width = "100%")
    )
  ),
  
  # ---- Well panel with 3 Donut Charts (no extra headings)
  wellPanel(
    class = "facet-panel",
    fluidRow(
      # GENE EFFECT
      column(4,
             # No facet title above
             # label=NULL on selectInput to hide “Gene Effect:”
             selectInput("geneEffectSelect", label = NULL,
                         choices = c("All"), selected = "All"),
             actionButton("zoomGeneEffect", "Zoom", icon = icon("search"), width = "100%"),
             plotlyOutput("geneEffectChart", width = "100%", height = "300px") %>% withSpinner()
      ),
      # SYSTEM AFFECTED
      column(4,
             selectInput("systemAffectedSelect", label = NULL,
                         choices = c("All"), selected = "All"),
             actionButton("zoomSystemAffected", "Zoom", icon = icon("search"), width = "100%"),
             plotlyOutput("systemAffectedChart", width = "100%", height = "300px") %>% withSpinner()
      ),
      # LETHALITY
      column(4,
             selectInput("letlethalitySelect", label = NULL,
                         choices = c("All","1st","2nd","3rd","Postnatal"), selected = "All"),
             actionButton("zoomLetlethality", "Zoom", icon = icon("search"), width = "100%"),
             plotlyOutput("letlethalityChart", width = "100%", height = "300px") %>% withSpinner()
      )
    )
  ),
  
  # ---- Display current filter selections
  fluidRow(
    column(12,
           h4("Selected Filters", style = "text-align:center; margin-top:20px;"),
           p(textOutput("selectedGeneEffect"), style = "text-align:center;"),
           p(textOutput("selectedSystemAffected"), style = "text-align:center;"),
           p(textOutput("selectedLetlethality"), style = "text-align:center;")
    )
  ),
  
  # ---- Network & Legend
  fluidRow(
    column(1),
    column(10,
           h4("Gene-Chemical Interaction Network", style = "text-align:center; margin-top:20px;"),
           div(class = "scrollable-network", 
               visNetworkOutput("networkPlot", width = "100%", height = "600px") %>% withSpinner()
           ),
           tags$div(
             class = "legend",
             h5("Legend"),
             div(class = "legend-item",
                 div(class = "legend-color", style = "background-color: lightcoral; border-color: lightcoral;"),
                 span("Gene")
             ),
             div(class = "legend-item",
                 div(class = "legend-color", style = "background-color: lightseagreen; border-color: lightseagreen;"),
                 span("Chemical")
             )
           )
    ),
    column(1)
  ),
  
  # ---- Data Table & Export
  fluidRow(
    column(12,
           h4("Filtered Data Table", style = "margin-top:20px; text-align:center;"),
           downloadButton("exportButton", "Export Data", style="margin-bottom:10px;"),
           div(class = "scrollable-table", dataTableOutput("dataTable", width = "100%") %>% withSpinner())
    )
  ),
  
  # ---- Footer with Sponsor & GitHub Link
  div(class = "footer",
      p("Sponsored by UCSF Bakar Computational Health Sciences Institute"),
      p(
        strong("You can access the full code and data at: "),
        a("GitHub: SyedHassan20/GeneChem-Atlas", 
          href = "https://github.com/SyedHassan20/GeneChem-Atlas", 
          target = "_blank")
      )
  )
)

# -----------------------------
#  3) Server Logic
# -----------------------------
server <- function(input, output, session) {
  
  # A multi-color palette for the donuts
  multi_colors <- colorRampPalette(brewer.pal(8, "Set2"))(20)
  
  # Load data
  if (file.exists("data/merged_data.rds")) {
    merged_df_all <- readRDS("data/merged_data.rds")
  } else {
    stop("data/merged_data.rds not found. Please place your dataset.")
  }
  
  # Rename columns if needed (X1st -> 1st, etc.)
  merged_df_all <- merged_df_all %>%
    rename_with(~str_replace_all(.x, "^X1st$", "1st")) %>%
    rename_with(~str_replace_all(.x, "^X2nd$", "2nd")) %>%
    rename_with(~str_replace_all(.x, "^X3rd$", "3rd"))
  
  # Replace NA in key filter columns
  merged_df_all <- merged_df_all %>%
    mutate(
      Gene.Effect     = replace_na(Gene.Effect, "Unknown"),
      System.Affected = replace_na(System.Affected, "Unknown")
    )
  
  # Always load all possible categories in the dropdown
  observe({
    allGeneEffects <- sort(unique(merged_df_all$Gene.Effect))
    allSystems     <- sort(unique(merged_df_all$System.Affected))
    
    updateSelectInput(session, "geneEffectSelect", 
                      choices = c("All", allGeneEffects),
                      selected = if(is.null(input$geneEffectSelect)) "All" else input$geneEffectSelect)
    
    updateSelectInput(session, "systemAffectedSelect", 
                      choices = c("All", allSystems),
                      selected = if(is.null(input$systemAffectedSelect)) "All" else input$systemAffectedSelect)
  })
  
  # reactiveValues to store user’s chosen filters
  reactiveFilters <- reactiveValues(
    selectedGeneEffect     = NULL,
    selectedSystemAffected = NULL,
    selectedLetlethality   = NULL
  )
  
  # The main filter function (intersection of all 3 filters)
  filteredData <- memoise(function(selectedGeneEffect = NULL, 
                                   selectedSystemAffected = NULL, 
                                   selectedLetlethality = NULL) {
    data <- merged_df_all
    
    if (!is.null(selectedGeneEffect) && selectedGeneEffect != "All") {
      data <- data %>% filter(Gene.Effect == selectedGeneEffect)
    }
    if (!is.null(selectedSystemAffected) && selectedSystemAffected != "All") {
      data <- data %>% filter(System.Affected == selectedSystemAffected)
    }
    if (!is.null(selectedLetlethality) && selectedLetlethality != "All") {
      data <- data %>%
        filter(!is.na(.data[[selectedLetlethality]]) & .data[[selectedLetlethality]] != "")
    }
    data
  })
  
  # A function to build a donut chart with a big title
  generate_donut_chart <- function(count_data, label_col, title, chart_source, color_palette, noColon=FALSE) {
    if (nrow(count_data) == 0) {
      return(plot_ly() %>% layout(title = "No Data Available"))
    }
    count_data[[label_col]] <- as.character(count_data[[label_col]])
    
    # Build the hover text
    text_func <- if(noColon) {
      ~paste0(get(label_col), " ", n, " (", round(n/sum(n)*100, 2), "%)")
    } else {
      ~paste0(get(label_col), ": ", n, " (", round(n/sum(n)*100, 2), "%)")
    }
    
    plot_ly(
      count_data,
      labels    = ~get(label_col),
      values    = ~n,
      type      = "pie",
      hole      = 0.4,
      textinfo  = "none",
      hoverinfo = "text",
      text      = text_func,
      source    = chart_source,
      marker    = list(colors = color_palette, line = list(color = "white", width = 0.5)),
      customdata= ~get(label_col)
    ) %>%
      layout(
        title = list(text = title, x = 0.5),
        showlegend = FALSE,
        margin = list(l = 0, r = 0, t = 60, b = 30),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      event_register("plotly_click")
  }
  
  #  GENE EFFECT Donut
  geneEffectData <- reactive({
    filteredData(reactiveFilters$selectedGeneEffect,
                 reactiveFilters$selectedSystemAffected,
                 reactiveFilters$selectedLetlethality) %>%
      distinct(Gene_Name, Gene.Effect, .keep_all = TRUE) %>%
      count(Gene.Effect) %>%
      arrange(desc(n))
  })
  observeEvent(input$geneEffectSelect, {
    reactiveFilters$selectedGeneEffect <- 
      if (input$geneEffectSelect == "All") NULL else input$geneEffectSelect
  })
  observeEvent(event_data("plotly_click", source = "geneEffectChart"), {
    ed <- event_data("plotly_click", source = "geneEffectChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedGeneEffect <- ed$customdata
      updateSelectInput(session, "geneEffectSelect", selected = ed$customdata)
    }
  })
  output$geneEffectChart <- renderPlotly({
    df <- geneEffectData()
    generate_donut_chart(df, "Gene.Effect", "Gene Effect Distribution",
                         "geneEffectChart", multi_colors, noColon=FALSE)
  })
  observeEvent(input$zoomGeneEffect, {
    showModal(modalDialog(
      title = "Gene Effect Distribution (Zoomed)",
      renderPlotly({
        df <- geneEffectData()
        generate_donut_chart(df, "Gene.Effect", "Gene Effect Distribution (Zoomed)",
                             "geneEffectChartZoomed", multi_colors, noColon=FALSE) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # SYSTEM AFFECTED Donut
  systemAffectedData <- reactive({
    filteredData(reactiveFilters$selectedGeneEffect,
                 reactiveFilters$selectedSystemAffected,
                 reactiveFilters$selectedLetlethality) %>%
      distinct(Gene_Name, System.Affected, .keep_all = TRUE) %>%
      count(System.Affected) %>%
      arrange(desc(n))
  })
  observeEvent(input$systemAffectedSelect, {
    reactiveFilters$selectedSystemAffected <- 
      if (input$systemAffectedSelect == "All") NULL else input$systemAffectedSelect
  })
  observeEvent(event_data("plotly_click", source = "systemAffectedChart"), {
    ed <- event_data("plotly_click", source = "systemAffectedChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedSystemAffected <- ed$customdata
      updateSelectInput(session, "systemAffectedSelect", selected = ed$customdata)
    }
  })
  output$systemAffectedChart <- renderPlotly({
    df <- systemAffectedData()
    generate_donut_chart(df, "System.Affected", "System Affected Distribution",
                         "systemAffectedChart", multi_colors, noColon=TRUE)
  })
  observeEvent(input$zoomSystemAffected, {
    showModal(modalDialog(
      title = "System Affected Distribution (Zoomed)",
      renderPlotly({
        df <- systemAffectedData()
        generate_donut_chart(df, "System.Affected", "System Affected Distribution (Zoomed)",
                             "systemAffectedChartZoomed", multi_colors, noColon=TRUE) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # LETHALITY Donut
  lethalityData <- reactive({
    filteredData(reactiveFilters$selectedGeneEffect,
                 reactiveFilters$selectedSystemAffected,
                 reactiveFilters$selectedLetlethality) %>%
      select(Gene_Name, `1st`, `2nd`, `3rd`, `Postnatal`) %>%
      pivot_longer(cols = c(`1st`, `2nd`, `3rd`, `Postnatal`),
                   names_to = "Letlethality",
                   values_to = "Value") %>%
      filter(!is.na(Value) & Value != "") %>%
      count(Letlethality) %>%
      arrange(desc(n))
  })
  observeEvent(input$letlethalitySelect, {
    reactiveFilters$selectedLetlethality <- 
      if (input$letlethalitySelect == "All") NULL else input$letlethalitySelect
  })
  observeEvent(event_data("plotly_click", source = "letlethalityChart"), {
    ed <- event_data("plotly_click", source = "letlethalityChart")
    if (!is.null(ed$customdata)) {
      reactiveFilters$selectedLetlethality <- ed$customdata
      updateSelectInput(session, "letlethalitySelect", selected = ed$customdata)
    }
  })
  output$letlethalityChart <- renderPlotly({
    df <- lethalityData()
    generate_donut_chart(df, "Letlethality", "Lethality Distribution",
                         "letlethalityChart", multi_colors, noColon=FALSE)
  })
  observeEvent(input$zoomLetlethality, {
    showModal(modalDialog(
      title = "Lethality Distribution (Zoomed)",
      renderPlotly({
        df <- lethalityData()
        generate_donut_chart(df, "Letlethality", "Lethality Distribution (Zoomed)",
                             "letlethalityChartZoomed", multi_colors, noColon=FALSE) %>%
          layout(height = 600)
      }),
      easyClose = TRUE, size = "l"
    ))
  })
  
  # Display the currently chosen filters
  output$selectedGeneEffect <- renderText({
    if (is.null(reactiveFilters$selectedGeneEffect)) "Gene Effect: None"
    else paste("Gene Effect:", reactiveFilters$selectedGeneEffect)
  })
  output$selectedSystemAffected <- renderText({
    if (is.null(reactiveFilters$selectedSystemAffected)) "System Affected: None"
    else paste("System Affected:", reactiveFilters$selectedSystemAffected)
  })
  output$selectedLetlethality <- renderText({
    if (is.null(reactiveFilters$selectedLetlethality)) "Letlethality: None"
    else paste("Letlethality:", reactiveFilters$selectedLetlethality)
  })
  
  # Reset button
  observeEvent(input$resetBtn, {
    reactiveFilters$selectedGeneEffect     <- NULL
    reactiveFilters$selectedSystemAffected <- NULL
    reactiveFilters$selectedLetlethality   <- NULL
    
    updateSelectInput(session, "geneEffectSelect",     selected = "All")
    updateSelectInput(session, "systemAffectedSelect", selected = "All")
    updateSelectInput(session, "letlethalitySelect",   selected = "All")
  })
  
  # Network Plot
  output$networkPlot <- renderVisNetwork({
    dep1 <- reactiveFilters$selectedGeneEffect
    dep2 <- reactiveFilters$selectedSystemAffected
    dep3 <- reactiveFilters$selectedLetlethality
    
    currentGE <- isolate(reactiveFilters$selectedGeneEffect)
    currentSA <- isolate(reactiveFilters$selectedSystemAffected)
    currentLT <- isolate(reactiveFilters$selectedLetlethality)
    
    future({
      data <- filteredData(currentGE, currentSA, currentLT)
      if (nrow(data) == 0) return(NULL)
      if (!"X..ChemicalName" %in% names(data)) return(NULL)
      
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
        visLegend(enabled = FALSE) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    }, seed = TRUE) %...>%
      (function(result) {
        if (!is.null(result)) result else visNetwork(nodes = data.frame(), edges = data.frame())
      })
  })
  
  # Data Table
  output$dataTable <- renderDataTable({
    df <- filteredData(reactiveFilters$selectedGeneEffect,
                       reactiveFilters$selectedSystemAffected,
                       reactiveFilters$selectedLetlethality)
    
    df <- setNames(df, c(
      "Gene Name", "Gene ID", "OMIM Gene", "Gene Effect", "Variants", 
      "Lethality Mode of Function", "Coordinates (hg19)", "Disease", "OMIM Phenotype", 
      "Inheritance", "System Affected", "Prenatal Phenotypes", "Fetal Sex", 
      "Human Evidence", "PMID", "OMIM Gene (Duplicate)", "1st", "2nd", 
      "3rd", "Postnatal", "Chemical Name", "Chemical ID", "CAS RN", 
      "Gene Symbol", "Gene Forms", "Organism", "Organism ID", "Interaction", 
      "Interaction Actions", "PubMed IDs"
    ))
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE, filter = "top")
  }, server = TRUE)
  
  # Export
  output$exportButton <- downloadHandler(
    filename = function() { paste("GeneChem_Data", Sys.Date(), ".csv", sep="") },
    content  = function(file) {
      write.csv(
        filteredData(
          reactiveFilters$selectedGeneEffect,
          reactiveFilters$selectedSystemAffected,
          reactiveFilters$selectedLetlethality
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


