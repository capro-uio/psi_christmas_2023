library(shiny)
library(shinydashboard)
library(tidyverse)
source("R/scholar.R")

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "PSI research explorer!", titleWidth = 230),
  dashboardSidebar(
    uiOutput("authorSelector"),
    # Add sidebar content here
    sidebarMenu(
      menuItem("Google Scholar", tabName = "scholar", icon = icon("graduation-cap")),
      menuItem("ORCID", tabName = "orcid", icon = icon("orcid"))
    ),
    helpText("Created by CAPRO", style="padding: 10px;"),
    tags$a(href='https://capro.dev',
           tags$img(src='https://raw.githubusercontent.com/capro-uio/capro-uio.github.io/main/assets/images/capro_logo_dark.png',width='100%', style="padding: 10px;"),
    )
  ),
  dashboardBody(
    includeCSS("style.css"),
    
    # Add body content here
    tabItems(
      tabItem(
        "scholar",
        fluidRow(
          h1("Google Scholar results")
        ),
        shinycssloaders::withSpinner(
          uiOutput("scholar_results"),
        )
      ),
      tabItem(
        "orcid",
        fluidRow(
          # Add fluidRow content for Tab 2
        ))
      # Add more tabs as needed
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Get all PSI CRISTIN employees
  resp <- httr2::request("https://api.cristin.no/") |>
    httr2::req_url_path_append("v2/persons") |> 
    httr2::req_url_query(parent_unit_id="185.17.5.0",
                         per_page=1000) |> 
    httr2::req_perform()
  
  persons <- httr2::resp_body_json(resp) |> 
    lapply(as_tibble) |> 
    bind_rows() |> 
    arrange(first_name, surname) |> 
    mutate(
      name = paste(first_name, surname),
      n = row_number()
    ) |> 
    select(-url)
  
  output$authorSelector <- renderUI({
    selectizeInput(
      "author_name",
      "Select author",
      choices = setNames(
        persons$cristin_person_id,
        persons$name
      ),
      multiple = FALSE,
      options = list(
        placeholder = 'Search...',
        onInitialize = I('function() { this.setValue(""); }')
      ) 
    )
  })
  
  observeEvent(input$author_name, {
    gmatch <- multi <- ginfo <- NA
    if(input$author_name == ""){
      output$scholar_results <- renderUI({
        h3("Select an author in the sidebar to start exploring!")
      })
    }else{
      author <- persons |> 
        filter(cristin_person_id == input$author_name)
      
      ginfo <- search_gid_cached(author$name)
      orcid <- search_orcid_cached(
        str_split_i(author$name, " ", 1),
        str_split_i(author$name, " ", -1)
      )
      
      if(nrow(ginfo) == 0){
        si_name <- paste0(str_split_i(author$name, " ", 1), 
                          str_split_i(author$name, " ", -1))
        ginfo <- search_gid_cached(si_name)
      }
      
      if(nrow(ginfo) > 0){
        if(nrow(ginfo) > 1){
          ginfo <- ginfo |> 
            filter(grepl("Oslo", affiliation))
          if(nrow(ginfo) > 1){
            gmatch <- box(
              title = "Multiple matches",
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              glue::glue("Multiple google scholar records found {author$name}.")
            )
            output$result_multiple <- renderTable(ginfo)
            multi <- box(tableOutput("result_multiple"))
          }
        }
        
        
        if(nrow(ginfo) > 0){
          ginfo <- ginfo[1, ]
          gprof <- get_author_cached(ginfo$id)
          
          output$gcit <- renderTable(gprof$citations)
          gcit <- box(
            tableOutput("gcit"),
            title = "Citations",
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 5
          )
          
          print(gprof)
          print(ginfo)
          gmatch <- box(
            title = "Match found",
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(
              column(5,
                     h3(glue::glue("{author$name}")),
                     p(glue::glue("Google scholar id: {ginfo$id}")),
                     p(glue::glue("{ginfo$affiliation}"))
              ),
              column(6,
                     img(src = glue::glue("https://scholar.googleusercontent.com/citations?view_op=medium_photo&user={ginfo$id}"),
                         alt = author$name)
              ),
              style = "padding: 10px;"
            )
          )
          
          authinf <- get_author(ginfo$id)
          output$google_coauthors <- scholar::get_coauthors(ginfo$id, input$google_slider + 2) |> 
            filter(!author %in% c("Search Help", "About Scholar"),
                   !coauthors %in% c("Search Help", "About Scholar")) |> 
            scholar::plot_coauthors() |> 
            renderPlot()
          
          output$gpubs <- gprof$publications |> 
            mutate(title = sprintf('<p><a href = "%s">%s</a>', url, title),
                   title = map(title, gt::html)) |> 
            select(-url, -pubid) |> 
            select(year, authors, title, journal, citations) |> 
            gt::gt() |> 
            gt::data_color(columns = citations, 
                           method = "numeric", 
                           palette = "inferno") |> 
            gt::cols_label(year = "Year",
                           authors = "Authors",
                           title = "Title",
                           journal = "Journal",
                           citations = "Citations") |>
            gt::opt_interactive(use_compact_mode = TRUE) |> 
            gt::cols_width(
              year ~ px(80),
              citations ~px(120)
            ) |> 
            gt::render_gt()
          
        }
        
        output$scholar_results <- renderUI({
          
          tagList(
            fluidRow(
              gmatch,
              gcit,
            ),
            fluidRow(
              h2("Co-author network"),
              inputPanel(
                sliderInput("google_slider", "Number of coauthors to show:",
                            min = 0, max = 10, value = 5, width = '100%')
              ),
              plotOutput("google_coauthors"),
            ),
            fluidRow(
              h2("Recorded publications"),
              gt::gt_output(outputId = "gpubs")
            )
          )
        }) 
        
      }else{
        output$scholar_results <- renderUI({
          fluidRow(
            box(
              title = "No match found",
              status = "danger",
              solidHeader = TRUE,
              collapsible = FALSE,
              glue::glue("No google scholar records for {author$name}.")
            )
          )
        }) 
      } # end renderUI scholar
      
      
      # orcid ---
      
      # rorcid::orcid_search(
      #   author$first_name,
      #   author$surname
      # ) |> 
      #   print()
      #rorcid::orcid_works("0000-0003-2502-8774")
      #
    } # end if author_name == ""
  }) # end observeEvent
} # end server

# Create Shiny app ----
shinyApp(ui = ui, server = server)
