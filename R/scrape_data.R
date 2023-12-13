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
  author <- filter(dt, name == person)
  api <- list(
    gscolar = NULL,
    orcid   = NULL,
    cristin = NULL
  )
  cli::cli_alert("google scholar") #----
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
  
  cli::cli_alert("orcid") #----
 
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
  
  cli::cli_alert("cristin") #----
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
