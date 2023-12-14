source(here::here("R/scholar.R"))
library(jsonlite)

# Get all PSI CRISTIN employees
dt <- httr2::request("https://api.cristin.no/") |>
  httr2::req_url_path_append("v2/persons") |> 
  httr2::req_url_query(parent_unit_id="185.17.5.0",
                       per_page=1000) |> 
  httr2::req_perform() |> 
  httr2::resp_body_json() |> 
  lapply(as_tibble) |> 
  bind_rows() |> 
  arrange(first_name, surname) |> 
  mutate(
    name = paste(first_name, surname),
    n = row_number()
  ) |> 
  select(-url)

for(person in unique(dt$name)){
  
  filenm <- str_replace_all(person, " ", "_") |> 
    tolower() |> 
    paste0(".json")
  
  filenm <- here::here("data", 
                       "api",
                       filenm)
  
  if(file.exists(filenm)){
    next
  }
  
  cli::cli_h1("Searching for {person}")
  author <- filter(dt, name == person) |> 
    slice(1)
  api <- list(
    gscolar = NULL,
    orcid   = NULL,
    cristin = NULL
  )
  cli::cli_alert("google scholar") # scholar----
  ginfo <- search_gid(person)
  if(nrow(ginfo) == 0){
    si_name <- paste0(str_split_i(person, " ", 1), 
                      str_split_i(person, " ", -1))
    ginfo <- search_gid(si_name)
    ginfo <- filter(ginfo, 
                    grepl(glue::glue("^{str_split_i(person, ' ', 1)}"),
                          name),
                    grepl(glue::glue("{str_split_i(person, ' ', -1)}$"),
                          name)
    )
  }
  
  if(nrow(ginfo) > 0){
    ginfo <- ginfo[1, ]
    api$gscolar <- list(
      name = unbox(ginfo$name),
      id = unbox(ginfo$id),
      affiliation = unbox(ginfo$affiliation),
      data = get_author(ginfo$id),
      coauthors = scholar::get_coauthors(ginfo$id, 10) |> 
        filter(!author %in% c("Search Help", "About Scholar"),
               !coauthors %in% c("Search Help", "About Scholar"))
    )
  }
  
  cli::cli_alert("orcid") # orcid ----
  
  orcid <- rorcid::orcid_search(
    author$first_name,
    author$surname
  ) 
  if(nrow(orcid) == 0){
    orcid <- rorcid::orcid_search(
      str_split_i(person, " ", 1),
      str_split_i(person, " ", -1)
    ) 
    
    if(nrow(orcid) > 0){
      orcid <- filter(orcid, 
                      grepl(glue::glue("^{str_split_i(person, ' ', 1)}"),
                            first),
                      grepl(glue::glue("{str_split_i(person, ' ', -1)}$"),
                            last)
      )
    }
  }
  
  
  if(nrow(orcid) > 0){
    orcid <- orcid[1, ]
    orcid_all <- rorcid::orcid_activities(orcid$orcid)[[1]] 
    pubs <- orcid_all$works$group$`work-summary` |> 
      bind_rows()
    
    if(nrow(pubs) > 0){
      pubs <- transmute(
        pubs,
        type = type,
        year = fill_orcid_value(pubs, "publication-date", "year.value"),
        title = title.title.value,
        journal = fill_orcid_value(pubs, "journal-title", "value"),
        url = fill_orcid_value(pubs, "url", "value")
      )    |> 
        as_tibble()
    }else(
      pubs <- NULL
    )
    
    api$orcid <- list(
      orcid = unbox(orcid$orcid),
      name = unbox(paste(orcid$first, orcid$last, sep = " ")),
      distinctions = exctract_orcid_affili(orcid_all, 
                                           "distinctions"),
      education = exctract_orcid_affili(orcid_all, "employments"),
      employment = exctract_orcid_affili(orcid_all, "employments"),
      membership = exctract_orcid_affili(orcid_all, "memberships"),
      works = pubs,
      services = exctract_orcid_affili(orcid_all, "services"),
      invited = exctract_orcid_affili(orcid_all, "invited-positions"),
      qualifications = exctract_orcid_affili(orcid_all, "qualifications")
    )
    
  }
  
  cli::cli_alert("cristin") # cristin ----
  resp <- httr2::request("https://api.cristin.no/") |>
    httr2::req_url_path_append("v2/persons", 
                               author$cristin_person_id[1]) |> 
    httr2::req_url_query(
      lang = "en"
    ) |>
    httr2::req_perform()|> 
    httr2::resp_body_json() 
  
  crpubs <- httr2::request("https://api.cristin.no/") |>
    httr2::req_url_path_append("v2/persons", 
                               author$cristin_person_id[1],
                               "results") |> 
    httr2::req_url_query(
      lang = "en"
    ) |>
    httr2::req_perform()|> 
    httr2::resp_body_json() %>% {
      tibble(
        type = map_chr(., c("category", "name", "en"), .default = NA_character_),
        year = map_chr(., c("year_published"), .default = NA_character_),
        title = map_chr(., c("title", "en"), .default = NA_character_),
        journal = map_chr(., c("journal", "name"), .default = NA_character_),
        url = map_chr(., c("url"), .default = NA_character_),
      )
    }
  
  api$cristin <- list(
    name = unbox(paste(resp$first_name, resp$surname)),
    id = unbox(resp$cristin_person_id),
    keywords = map_chr(resp$keywords, c("name", "en"), .default = NA_character_),
    position = map_chr(resp$affiliations, c("position", "en"), .default = NA_character_),
    img = unbox(resp$picture_url),
    works = crpubs
  )
  
  jsonlite::write_json(
    api,
    pretty = TRUE,
    path = filenm
  )
  
  Sys.sleep(20)
}


for(person in unique(dt$name)){
  
  filenm <- str_replace_all(person, " ", "_") |> 
    tolower() |> 
    paste0(".json")
  
  filenm <- here::here("data", 
                       "api",
                       filenm)
  
  api <- jsonlite::read_json(filenm, simplifyVector = TRUE)
  
  if("rg" %in% names(api)){
    next
  }
  
  cli::cli_h1("Searching for {person}")
  
  author <- filter(dt, name == person) |> 
    slice(1)
  
  # rg ----
  rg_s <- httr2::request("https://www.researchgate.net/") |> 
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36") |>
    httr2::req_url_path_append("search/researcher") |>
    httr2::req_url_query(
      q = URLencode(author$name)
    ) |>
    httr2::req_perform() |> 
    httr2::resp_body_html()
  
  rg <- list(
    id = NULL,
    name = NULL,
    img = NULL,
    affiliation = NULL,
    keywords = NULL,
    works = NULL
  )
  
  success <- rvest::html_element(rg_s, ".nova-legacy-v-entity-item__body") |> 
    rvest::html_element("a") |> 
    rvest::html_attr("href") |> 
    str_split_i("\\?", 1) |> 
    basename()
  
  
  if(is.na(success)){ 
    cli::cli_alert_danger("ResearchGate")
  }else{
    cli::cli_alert_success("ResearchGate") 
    
    rg$id <- jsonlite::unbox(success)
    
    rg_p <- httr2::request("https://www.researchgate.net/") |> 
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.64 Safari/537.36") |>
      httr2::req_url_path_append("profile", rg$id) |>
      httr2::req_perform() |> 
      httr2::resp_body_html()
    
    main <- rvest::html_elements(rg_p, ".nova-legacy-o-pack__item")
    
    rg$img <- rvest::html_element(main, ".nova-legacy-e-avatar") |> 
      rvest::html_element("img") |>
      rvest::html_attr("src") |> 
      na.omit() |> 
      as.character()
    if(length(rg$img) > 0)
      rg$img <- jsonlite::unbox(rg$img)
    
    rg$affiliation <- rvest::html_elements(main, "h1") |> 
      rvest::html_elements("span") |>
      rvest::html_text() |> 
      str_remove_all("[:punct:]") |> 
      str_trim() |> 
      unique() |> 
      paste(collapse = ", ") |> 
      str_remove(", $")
    if(length(rg$affiliation) > 0)
      rg$affiliation <- jsonlite::unbox(rg$affiliation)
    
    nms <- rvest::html_elements(main, ".nova-legacy-o-pack__item") |> 
      rvest::html_text()
    rg$name <- jsonlite::unbox(nms[1])
    rg$degree <- str_split(nms[3], " ")[[1]]
    
    rg$bio <- rvest::html_elements(rg_p, ".nova-legacy-c-card") |>
      rvest::html_elements(".nova-legacy-c-card__body") |>
      rvest::html_elements(".Linkify") |>
      rvest::html_text()
    if(length(rg$bio) > 0)
      rg$bio <- jsonlite::unbox(paste(rg$bio, collapse=" "))
    
    rg$keywords <- rvest::html_elements(rg_p, ".nova-legacy-c-card") |> 
      rvest::html_elements(".nova-legacy-c-card__body") |> 
      rvest::html_elements(".nova-legacy-l-flex__item") |> 
      rvest::html_elements(".nova-legacy-e-badge") |> 
      rvest::html_text()
    
    pubs <- rvest::html_elements(rg_p, ".nova-legacy-v-publication-item__stack")
    
    
    rg$works <- tibble(
      title = rvest::html_elements(pubs, ".nova-legacy-v-publication-item__title") |> 
        rvest::html_text(),
      tags = rvest::html_elements(pubs, ".nova-legacy-v-publication-item__meta") |> 
        lapply(\(x){
          rvest::html_elements(x, ".nova-legacy-v-publication-item__meta-left") |>
            rvest::html_element(".nova-legacy-e-badge") |> 
            rvest::html_text()
        }),
      date = rvest::html_elements(pubs, ".nova-legacy-v-publication-item__meta") |> 
        rvest::html_elements(".nova-legacy-v-publication-item__meta-right") |>
        rvest::html_element("span") |> 
        rvest::html_text(),
      authors = rvest::html_elements(pubs, ".nova-legacy-v-publication-item__person-list") |> 
        lapply(\(x){ 
          rvest::html_elements(x, ".nova-legacy-v-person-inline-item__fullname") |> 
            rvest::html_text()
        }),
      abstract = sapply(pubs, \(x){
        rvest::html_elements(x, ".nova-legacy-v-publication-item__description") |> 
          rvest::html_text()
      })
      
    )
    
    tab <- rvest::html_elements(rg_p, ".nova-legacy-c-card__body") |> 
      rvest::html_elements(".nova-legacy-o-grid__column")
    
    tab_box <-   rvest::html_element(tab, ".nova-legacy-e-text--family-display") |> 
      rvest::html_text() |> 
      str_split_i(" ", 1) |> 
      str_replace_all(",", "") |>
      as.integer() |> 
      as.list()
    
    names(tab_box) <- rvest::html_element(tab, ".nova-legacy-e-text--family-sans-serif") |> 
      rvest::html_text() |> 
      str_split_i(" ", 1) |> 
      tolower()
    
    rg <- c(rg, tab_box)
    
    
    api$rg <- rg
    
    jsonlite::write_json(
      api,
      pretty = TRUE,
      path = filenm
    )
  }
  Sys.sleep(5)
}
