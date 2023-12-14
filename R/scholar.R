library(dplyr)
library(stringr)

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}


# scholar ----
search_gid <- function(name){
  resp <- httr2::request("https://scholar.google.com/") |>
    httr2::req_url_path_append("/citations") |> 
    httr2::req_url_query(
      view_op="search_authors",
      mauthors=gsub(" ", "+", name),
      hl="en") |> 
    httr2::req_user_agent("CAPRO@capro.dev") |>
    httr2::req_perform()
  
  res <- httr2::resp_body_html(resp)
  
  tibble(
    name = rvest::html_elements(res, ".gs_ai_name") |> 
      rvest::html_text(),
    cit = rvest::html_elements(res, ".gs_ai_cby") |> 
      rvest::html_text(),
    affiliation = rvest::html_elements(res, ".gs_ai_aff") |> 
      rvest::html_text(),
    id = rvest::html_elements(res, ".gs_ai_pho") |> 
      rvest::html_attr("href") |> 
      stringr::str_split_i("=", i = -1)
  )
}

get_author <- function(gid, page_size = 1000){
  resp <- httr2::request("https://scholar.google.com/") |>
    httr2::req_url_path_append("/citations") |> 
    httr2::req_url_query(
      user=gid,
      hl="en",
      pagesize=page_size) |> 
    httr2::req_user_agent("CAPRO@capro.dev") |>
    httr2::req_perform()
  
  res <- httr2::resp_body_html(resp)
  
  list(
    publications = get_pubs(res),
    citations = get_cit_stats(res)
  )
}

get_pubs <- function(res){
  art <- rvest::html_element(res, "#gsc_a_b") |> 
    rvest::html_elements(".gsc_a_t")
  inf <- lapply(art, function(x){
    el <- rvest::html_elements(x, ".gs_gray")
    tibble(
      authors = rvest::html_text(el[1]),
      journal = rvest::html_text(el[2])
    )
  }) |> 
    bind_rows()
  
  tibble(
    year = rvest::html_elements(res, ".gsc_a_h.gsc_a_hc.gs_ibl") |> 
      rvest::html_text() |> 
      as.integer(),
    citations = rvest::html_elements(res, ".gsc_a_ac.gs_ibl") |> 
      rvest::html_text() |> 
      as.integer(),
    title = rvest::html_elements(res, ".gsc_a_at") |> 
      rvest::html_text(),
    pubid = NA,
  ) |> 
    bind_cols(inf) |> 
    mutate(
      url = paste0("https://scholar.google.com",
                   art |> 
                     rvest::html_elements(".gsc_a_at") |> 
                     rvest::html_attr("href")),
      pubid = str_split_i(url, ":", -1)
    )
  
}

get_paper_authors <- function(url){
  httr2::request(url) |> 
    httr2::req_perform() |> 
    httr2::resp_body_html() |> 
    rvest::html_element(".gsc_oci_value") |> 
    rvest::html_text()
}

get_cit_stats <- function(res){
  cstats <- rvest::html_element(res, "#gsc_rsb_st") |> 
    rvest::html_element("tbody")
  nms <- rvest::html_elements(res, ".gsc_rsb_sth") |> 
    rvest::html_text()
  nms <- nms[nms != ""]
  dt <- rvest::html_elements(cstats, ".gsc_rsb_std") |> 
    rvest::html_text() |> 
    matrix(
      ncol = 2, nrow = 3, byrow = TRUE,
      dimnames = list(1:3,nms)
    ) |> 
    as_tibble()
  
  if(all(apply(dt, 1, is.na)))
    return(NULL)
  
  dt |> 
    mutate(
      type = rvest::html_elements(cstats, ".gsc_rsb_sc1") |> 
        rvest::html_text() |> 
        tolower()
    ) |> 
    select(type, everything()) |> 
    rename_all(str_replace_all, pattern = " ", "_") |> 
    rename_all(tolower) 
}

# orcid -----


fill_orcid_value <- function(dt, type, el = "value"){
  if(!type %in% names(dt))
    return(NA_character_)
  if_else(!is.na(dt[[type]]),
          NA_character_,
          dt[[glue::glue("{type}.{el}")]] %||% NA_character_
  )
}

exctract_orcid_affili <- function(orcid_record, 
                                  type = c("employments",
                                           "educations",
                                           "distinctions",
                                           "memberships",
                                           "qualifications",
                                           "services",
                                           "invited-positions")){
  type <- match.arg(type)
  spec <- switch(type,
                 "employments" = "position",
                 "educations" = "degree",
                 "distinctions" = "distinction",
                 "memberships" = "membership",
                 "qualifications" = "qualification",
                 "services" = "service",
                 "invited-positions" = "position")
  type_s <- gsub("s$", "", type)
  tmp <- orcid_all[[type]]$`affiliation-group`$summaries
  tibble(
    start = map_chr(
      tmp, 
      glue::glue("{type_s}-summary.start-date.year.value"), 
      .default = NA_character_
    ),
    end = map_chr(
      tmp, 
      glue::glue("{type_s}-summary.end-date.year.value"), 
      .default = NA_character_
    ),
    !!spec := map_chr(
      tmp, 
      glue::glue("{type_s}-summary.role-title"), 
      .default = NA_character_
    ),
    organisation = map_chr(
      tmp, 
      glue::glue("{type_s}-summary.organization.name"), 
      .default = NA_character_
    ),
    country = map_chr(
      tmp, 
      glue::glue("{type_s}-summary.organization.address.country"), 
      .default = NA_character_
    )
  )
}

# cache it ----
search_orcid_cached <- memoise::memoise(
  rorcid::orcid_search
)

search_gid_cached <- memoise::memoise(
  search_gid
)

search_gid_cached <- memoise::memoise(
  search_gid
)

get_author_cached <- memoise::memoise(
  get_author
)





