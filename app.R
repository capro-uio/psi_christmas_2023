library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

fluid_row <- function(...){
  fluidRow(
    ...,
    style = "padding: 10px;"
  )
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "PSI research explorer!", titleWidth = 230),
  dashboardSidebar(
    uiOutput("authorSelector"),
    div(
      p(
        "The data is retrieved from internet sources, and may be incomplete or inaccurate.",
        style = "color: #b0b3b6;"
      ),
      helpText("Created by"),
      tags$a(href='https://capro.dev',
             tags$img(
               src='https://raw.githubusercontent.com/capro-uio/capro-uio.github.io/main/assets/images/capro_logo_dark.png',
               width='100%', 
               style="padding: 10px;",
               alt="CAPRO"),
      ),
      style="padding: 10px;"
    )
  ),
  dashboardBody(
    includeCSS("style.css"),
    h1(textOutput("researcher")),
    p("Make sure to click on the tabsets too, there is more to see!",
      style = "color: white;"),
    tabsetPanel(
      tabPanel(
        title = span("Summary", style = "font-size: 20px;"),
        value = "summary_tab",
        icon = icon("user", lib = "font-awesome"),
        shinycssloaders::withSpinner(
          uiOutput("user_results"),
        )),
      tabPanel(
        title = span("Cristin", style = "font-size: 20px;"),
        value = "cristin_tab",
        icon = icon("copyright", lib = "font-awesome"),
        shinycssloaders::withSpinner(
          uiOutput("cristin_results"),
        )),
      tabPanel(
        title = span("Scholar", style = "font-size: 20px;"),
        value = "scholar_tab",
        icon = icon("graduation-cap", lib = "font-awesome"),
        shinycssloaders::withSpinner(
          uiOutput("scholar_results"),
        )
      ),
      tabPanel(
        title = span("ResearchGate", style = "font-size: 20px;"),
        value = "rg_tab",
        icon = icon("researchgate", lib = "font-awesome"),
        shinycssloaders::withSpinner(
          uiOutput("rg_results"),
        )
      ),
      tabPanel(
        title = span("ORCiD", style = "font-size: 20px;"),
        value = "orcid_tab",
        icon = icon("orcid", lib = "font-awesome"),
        shinycssloaders::withSpinner(
          uiOutput("orcid_results"),
        )
      )
      # Add more tabs as needed
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  jsf <- list.files(here::here("data/api"), "json", full.names = TRUE)
  jsd <- lapply(jsf, jsonlite::read_json, simplifyVector = TRUE)
  names(jsd) <- gsub(".json", "", basename(jsf))
  
  output$authorSelector <- renderUI({
    selectizeInput(
      "author_name",
      "Select author",
      choices =   setNames(
        names(jsd),
        sapply(jsd, function(x) x$cristin$name)
      ),
      multiple = FALSE,
      options = list(
        placeholder = 'Search...',
        onInitialize = I('function() { this.setValue(""); }')
      ) 
    )
  })
  
  observeEvent(input$author_name, {
    if(input$author_name == ""){
      output$scholar_results <- 
        output$cristin_results <- 
        output$orcid_results <- 
        output$user_results <- renderUI({
          h3("Select an author in the sidebar to start exploring!")
        })
    }else{  
      person <- jsd[[input$author_name]]
      # person <- jsd[["athanasia_monika_mowinckel"]]
      output$researcher <- renderText({
        person$cristin$name
      })
      
      # summary ----------------------------------------------
      for(el in c("cristin", "orcid", "gscolar", "rg")){
        assign(
          glue::glue("{el}_box"),
          value = box(
            width = 4,
            title = switch(el,
                           cristin = "Cristin",
                           orcid = "ORCiD",
                           gscolar = "Google scholar",
                           rg = "ResearchGate"
            ),
            status = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            fluid_row(
              p("No matches found")
            )
          ),
        )
      }
      
      if(length(person$cristin) > 0){
        cristin_box <- box(
          width = 4,
          title = "Cristin",
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("ID: {person$cristin$id}")),
            p(glue::glue("Position: {paste(person$cristin$position, collapse = ', ')}")),
            p(glue::glue("Keywords: {paste(person$cristin$keywords, collapse = ', ')}")),
            p(glue::glue("Publications: {nrow(person$cristin$works)}"))
          )
        )
      }
      if(length(person$orcid) > 0){
        orcid_box <- box(
          width = 4,
          title = "ORCiD",
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("ID: {person$orcid$orcid}")),
            p(glue::glue("Position: {person$orcid$employment$position[1]}, {person$orcid$employment$organisation[1]}")),
            p(glue::glue("Publications: {nrow(person$orcid$works)}")),
            p(glue::glue("Memberships: {nrow(person$orcid$membership)}")),
            p(glue::glue("Services: {nrow(person$orcid$services)}"))
          )
        )
      }
      if(length(person$gscolar) > 0){
        gscolar_box <- box(
          width = 4,
          title = "Google scholar",
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("ID: {person$gscolar$id}")),
            p(glue::glue("Position: {person$gscolar$affiliation}")),
            p(glue::glue("Publications: {nrow(person$gscolar$data$publications)}")),
            p(glue::glue("Citations: {person$gscolar$data$citations$all[[1]]}"))
          )
        )
      }
      if(length(person$rg) > 0){
        rg_box <- box(
          width = 4,
          title = "ResearchGate",
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("ID: {person$rg$id}")),
            p(glue::glue("Position: {person$rg$affiliation}")),
            p(glue::glue("Publications: {person$rg$publications}"))
          )
        )
      }
      

      
      user_results <- tagList(
        fluid_row(
          h2("User summary")
        ),
        fluid_row(
          cristin_box,
          orcid_box,
          rg_box,
          gscolar_box
        )
      )
      
      output$user_results <- renderUI({user_results})
      
      
      # scholar ----------------------------------------------
      if(length(person$gscolar) > 0){
        output$gcit <- renderTable(person$gscolar$data$citations)
        gcit <- box(
          tableOutput("gcit"),
          title = "Citations",
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 5
        )
        
        gprof <- box(
          width = 4,
          title = glue::glue("Google scholar id: {person$gscolar$id}"),
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("{person$gscolar$affiliation}")),
            a(tags$button(
              id = "btn",
              class = "shiny-bound-input action-button btn btn-primary btn-block",
              "Go to Google Scholar page"
            ),
            href = glue::glue("https://scholar.google.com/citations?hl=en&user={person$gscolar$id}
")
            )
          )
        )
        
        gimg <- box(
          align="center",
          img(src = glue::glue("https://scholar.googleusercontent.com/citations?view_op=medium_photo&user={person$gscolar$id}"),
              alt = person$cristin$name,
              style = "max-height: 300px;"),
          width = 3
        )
        
        output$gpubs <- person$gscolar$data$publications |>
          mutate(title = sprintf('<p><a href = "%s">%s</a>', url, title),
                 title = lapply(title, gt::html)) |>
          select(-url, -pubid) |>
          rename_all(tools::toTitleCase) |>
          gt::gt() |>
          gt::cols_align(
            "left", Title
          ) |> 
          gt::data_color(columns = Citations,
                         method = "numeric",
                         palette = "inferno") |>
          gt::opt_interactive(use_compact_mode = TRUE) |>
          gt::cols_width(
            Year ~ px(80),
            Citations ~ px(120)
          ) |>
          gt::render_gt()
        
        if(!is.data.frame(person$gscolar$coauthors)){
          gnetwork <- ggplot() + 
            geom_text(aes(x = 1, y = 1, label = "No network to plot"), size = 10) +
            theme_void()
        }else{
          gnetwork <- person$gscolar$coauthors |> 
            group_by(author) |> 
            scholar::plot_coauthors()
        }
        output$google_coauthors <-renderPlot(gnetwork)
        
        scholar_results <- tagList(
          fluid_row(
            h2("Google Scholar results")
          ),
          fluid_row(
            gprof,
            gimg,
            gcit
          ),
          fluid_row(
            h3("Co-author network"),
            # inputPanel(
            #   sliderInput("google_slider", "Network depth:",
            #               min = 0, max = 10, value = 5, width = '100%')
            # ),
            plotOutput("google_coauthors")
          ),
          fluid_row(
            h3("Recorded publications"),
            gt::gt_output(outputId = "gpubs")
          )
        )
      }else{
        scholar_results <- fluid_row(
          box(
            width = 4,
            title = "No google scholar match found!",
            status = "danger",
            solidHeader = TRUE,
            background = "red",
            collapsible = FALSE
          )
        )
      }
      output$scholar_results <- renderUI({scholar_results})
      
      # cristin ----------------------------------------------
      if(length(person$cristin) > 0){
        cprof <- box(
          width = 4,
          title = glue::glue("Cristin id: {person$cristin$id}"),
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("Position: {paste(person$cristin$position, collapse = ', ')}")),
            p(glue::glue("Keywords: {paste(person$cristin$keywords, collapse = ', ')}")),
            a(tags$button(
              id = "btn",
              class = "shiny-bound-input action-button btn btn-primary btn-block",
              "Go to Cristin page"
            ),
            href = glue::glue("https://app.cristin.no/persons/show.jsf?id={person$cristin$id}")
            )
          )
        )
        
        cimg <- box(
          align = "center",
          img(src = person$cristin$img,
              alt = person$cristin$name,
              style = "max-height: 300px;"),
          width = 3
        )
        
        output$cpubs <- person$cristin$works |>
          as_tibble() |> 
          select(-url) |> 
          rename_all(tools::toTitleCase) |>
          gt::gt() |>
          gt::opt_interactive(use_compact_mode = TRUE) |>
          gt::cols_align(
            "left", Title
          ) |>
          gt::cols_width(
            Year ~ px(80),
            Type ~ px(200),
            Journal ~ px(200),
          ) |>
          gt::render_gt()
        
        cristin_results <- tagList(
          fluid_row(
            h2("Cristin results")
          ),
          fluid_row(
            cprof,
            cimg
          ),
          fluid_row(
            h3("Recorded publications"),
            gt::gt_output(outputId = "cpubs")
          )
        )
      }else{
        cristin_results <- fluid_row(
          box(
            width = 4,
            title = "No cristin match found!",
            status = "danger",
            solidHeader = TRUE,
            background = "red",
            collapsible = FALSE
          )
        )
      }
      output$cristin_results <- renderUI({cristin_results})
      
      # orcid ----------------------------------------------
      if(length(person$orcid) > 0){
        oprof <- box(
          width = 4,
          title = glue::glue("orcid: {person$orcid$orcid}"),
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("Position: {person$orcid$employment$position[1]}, {person$orcid$employment$organisation[1]}")),
            a(tags$button(
              id = "btn",
              class = "shiny-bound-input action-button btn btn-primary btn-block",
              "Go to ORCiD page"
            ),
            href = glue::glue("https://orcid.org/{person$orcid$orcid}")
            )
          )
        )
        
        if(is.data.frame(person$orcid$works)){
          opubs <- person$orcid$works |>
            as_tibble()
          
          if(!"url" %in% names(opubs)){
            opubs$url <- NA
          }
          
          if(!"year" %in% names(opubs)){
            opubs$year <- NA
          }
          
          output$opubs <- opubs |> 
            mutate(
              title = ifelse(!is.na(url),
                             sprintf('<p><a href = "%s">%s</a></p>', url, title),
                             sprintf('<p>%s</p>', title)
              ),
              title = lapply(title, gt::html)
            ) |>
            select(-url) |>
            rename_all(tools::toTitleCase) |>
            distinct() |> 
            gt::gt() |>
            gt::opt_interactive(use_compact_mode = TRUE) |>
            gt::cols_align(
              "left", Title
            ) |>
            gt::cols_width(
              Year ~ px(80),
              Type ~ px(200),
            ) |> 
            gt::render_gt()
        }else{
          NULL
        }
        
        
        otabs <- c("employment", "membership", "distinctions",
                   "services", "qualifications") |> 
          lapply(\(x){
            tmp <- person$orcid[[x]]
            gid <- glue::glue("orcid_{x}")
            if(length(tmp) == 0)
              return(NULL)
            output[[gid]] <- tmp |>
              rename_all(tools::toTitleCase) |>
              gt::gt() |>
              gt::opt_interactive(use_compact_mode = TRUE)  |>
              gt::render_gt()
            fluid_row(
              h3(tools::toTitleCase(x)),
              gt::gt_output(outputId = gid)
            )
          })
        
        orcid_results <- tagList(
          fluid_row(
            h2("ORCID results")
          ),
          fluid_row(
            oprof
          ),
          fluid_row(
            h3("Publications"),
            gt::gt_output(outputId = "opubs")
          ),
          otabs
        )
      }else{
        orcid_results <- fluid_row(
          box(
            width = 4,
            title = "No orcid match found!",
            status = "danger",
            solidHeader = TRUE,
            background = "red",
            collapsible = FALSE
          )
        )
      }
      output$orcid_results <- renderUI({orcid_results})
      
      # rg ----------------------------------------------
      if(length(person$rg) > 0){
        rprof <- box(
          width = 4,
          title = glue::glue("id: {person$rg$id}"),
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          fluid_row(
            p(glue::glue("Position: {person$rg$affiliation}")),
            p(glue::glue("Reads: {person$rg$read}")),
            p(glue::glue("Citations: {person$rg$itations}")),
            p(glue::glue("Degrees: {paste(person$rg$degree, collapse = ', ')}")),
            a(tags$button(
              id = "btn",
              class = "shiny-bound-input action-button btn btn-primary btn-block",
              "Go to rg page"
            ),
            href = glue::glue("https://www.researchgate.net/profile{person$rg$id}")
            )
          )
        )
        
        rgimg <- box(
          align="center",
          img(src = person$rg$img,
              alt = person$rg$name,
              style = "max-height: 300px;"),
          width = 3
        )
        
        if(is.data.frame(person$rg$works)){
          rgpubs <- person$rg$works
          
          output$rgpubs <- rgpubs |> 
            select(-abstract) |> 
            distinct() |> 
            as_tibble() |> 
            rename_all(tools::toTitleCase) |>
            gt::gt() |>
            gt::opt_interactive(use_compact_mode = TRUE) |>
            gt::cols_align(
              "left", Title
            ) |>
            # gtExtras::gt_badge(Tags) |> doesn't work with list columns
            gt::cols_width(
              Date ~ px(80),
              Tags ~ px(200),
            ) |> 
            gt::render_gt()
        }else{
          NULL
        }
        
        
        rg_results <- tagList(
          fluid_row(
            h2("ResearchGate results")
          ),
          fluid_row(
            rprof,
            rgimg
          ),
          fluid_row(
            h3("Publications"),
            gt::gt_output(outputId = "rgpubs")
          )
        )
      }else{
        rg_results <- fluid_row(
          box(
            width = 4,
            title = "No rg match found!",
            status = "danger",
            solidHeader = TRUE,
            background = "red",
            collapsible = FALSE
          )
        )
      }
      output$rg_results <- renderUI({rg_results})
    } # end observeEvent
  })
  
  
  # orcid ---
  
  # rorcid::orcid_search(
  #   author$first_name,
  #   author$surname
  # ) |> 
  #   print()
  #rorcid::orcid_works("0000-0003-2502-8774")
  #
} # end server

# Create Shiny app ----
shinyApp(ui = ui, server = server)
