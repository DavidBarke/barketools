#' Small DFS Search
#'
#' Search for up to 100 keywords in DataForSEO's Google Organic Search.
#'
#' @param keywords Character vector of keywords.
#' @param postback_url Postback URL to which DataForSEO sends the search results.
#' @param language_code Language code.
#' @param location_code Location code, see https://api.dataforseo.com/v3/serp/google/locations.
#'
#' @returns Output of [httr2::req_perform].
#'
#' @export
dfs_search <- function(
  keywords,
  postback_url = Sys.getenv("POSTBACK_URL"),
  language_code = "en",
  location_code = 2840,
  username = Sys.getenv("DFS_USERNAME"),
  password = Sys.getenv("DFS_PASSWORD")
) {
  stopifnot(length(keywords) <= 100)

  task <- keywords_to_tasks(
    keywords = keywords,
    postback_url = postback_url,
    language_code = language_code,
    location_code = location_code
  )

  dfs_req(
    username = username,
    password = password
  ) |>
    httr2::req_url_path("/v3/serp/google/organic/task_post") |>
    httr2::req_body_json(task) |>
    httr2::req_perform()
}



#' Large DFS Search
#'
#' Search for more than 100 keywords in DataForSEO's Google Organic Search. This
#' function constructs a list of requests. When submitting these requests, the
#' calling function has to ensure that the rate limit from less than 2,000
#' requests with 100 keywords each is met.
#'
#' @inheritParams dfs_search
#' @param task_size Number of keywords that should be put into a single
#' request. Maximum 100.
#'
#' @export
dfs_search_reqs <- function(
    x,
    task_size = 100,
    postback_url = Sys.getenv("POSTBACK_URL"),
    language_code = "en",
    location_code = 2840,
    username = Sys.getenv("DFS_USERNAME"),
    password = Sys.getenv("DFS_PASSWORD")
) {
  stopifnot(task_size <= 100)
  n <- length(x)
  batch_starts <- seq(1, n, by = task_size)

  purrr::map(batch_starts, \(batch_start) {
    batch_end <- min(batch_start + task_size - 1, n)
    batch_keywords <- x[batch_start:batch_end] |>
      urltools::url_encode()

    tasks <- keywords_to_tasks(
      keywords = batch_keywords,
      postback_url = postback_url,
      language_code = language_code,
      location_code = location_code
    )

    req <- dfs_req(
      username = username,
      password = password
    ) |>
      httr2::req_url_path("/v3/serp/google/organic/task_post") |>
      httr2::req_body_json(tasks)
  }, .progress = TRUE)
}



dfs_req <- function(
    username = Sys.getenv("DFS_USERNAME"),
    password = Sys.getenv("DFS_PASSWORD")
) {
  httr2::request("https://api.dataforseo.com") |>
    httr2::req_auth_basic(
      username = username,
      password = password
    ) |>
    httr2::req_headers(
      "Content-Encoding" = "gzip"
    )
}



keywords_to_tasks <- function(
    keywords,
    postback_url = Sys.getenv("POSTBACK_URL"),
    language_code = "en",
    location_code = 2840
) {
  purrr::map(keywords, \(keyword) {
    list(
      keyword = keyword,
      language_code = language_code,
      location_code = location_code,
      tag = uuid::UUIDgenerate(),
      postback_url = postback_url,
      postback_data = "advanced"
    )
  })
}

