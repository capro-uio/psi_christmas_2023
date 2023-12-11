search_gid <- function(name){
  resp <- httr2::request("https://scholar.google.com/") |>
    httr2::req_url_path_append("/citations") |> 
    httr2::req_url_query(
      view_op="search_authors",
      mauthors=str_replace_all(name, " ", "+"),
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
  
  rvest::html_elements(cstats, ".gsc_rsb_std") |> 
    rvest::html_text() |> 
    matrix(
      ncol = 2, nrow = 3, byrow = TRUE,
      dimnames = list(1:3,nms)
    ) |> 
    as_tibble() |> 
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





