#' @export
pitchbook_ellen_to_s3 <- function(
  table = c(
    "company", "companybuysiderelation", "companycompetitorrel",
    "companyemployeehistrel", "companyindustryrelation", "companyinvestorrelation",
    "companysiccoderelation", "companyverticalrelation", "deal",
    "dealdebtlenderrelation", "dealdistribbenefrel", "dealinvestorrelation",
    "dealsellerrelation", "dealtrancherelation", "entityaffiliaterelation",
    "entitylocationrelation", "fund", "investor", "investorfundrelation"
  ),
  release = "2021"
) {
  table <- match.arg(table)
  dotenv::load_dot_env(env_file())
  DROPBOX_ELLEN_PATH <- Sys.getenv("DROPBOX_ELLEN_PATH")

  folders <- c(
    "other_northam", "other_row", "pe_northam", "pe_row", "vc_northam",
    "vc_row"
  )
  df <- purrr::map(folders, \(folder) {
    folder_short <- pitchbook_folder_to_abbreviation(folder)
    file <- glue::glue("{folder_short}_{table}.sas7bdat")
    path <- file.path(DROPBOX_ELLEN_PATH, folder, file)
    x <- haven::read_sas(path)
    x <- if (table == "entityaffiliaterelation") {
      x |>
        dplyr::mutate(
          key_entity = to_pitchbook_key(EntityID),
          key_affiliate = to_pitchbook_key(AffiliateID)
        )
    } else if (table %in% c("entitylocationrelation")) {
      x |>
        dplyr::mutate(
          key_entity = to_pitchbook_key(EntityID)
        )
    } else if (table %in% c("investor", "dealinvestorrelation")) {
      x |>
        dplyr::mutate(
          key_investor = to_pitchbook_key(InvestorID)
        )
    } else if (table %in% c("fund")) {
      x |>
        dplyr::mutate(
          key_fund = to_pitchbook_key(FundID)
        )
    } else if (table %in% c("investorfundrelation")) {
      x |>
        dplyr::mutate(
          key_investor = to_pitchbook_key(InvestorID),
          key_fund = to_pitchbook_key(FundID)
        )
    } else {
      x |>
        dplyr::mutate(
          key = to_pitchbook_key(CompanyID)
        )
    }
    # drop LastUpdated column because this might differ for otherwise
    # identical rows
    x |>
      dplyr::select(-LastUpdated) |>
      dplyr::mutate(pb_file_src = folder)
  }, .progress = TRUE) |> purrr::list_rbind()

  arrow::write_dataset(
    dataset = df |>
      dplyr::arrange(pb_file_src) |>
      dplyr::group_by(dplyr::pick(tidyselect::starts_with("key"))) |>
      dplyr::mutate(pb_file_src = paste(unique(pb_file_src), collapse = ":")) |>
      dplyr::ungroup() |>
      dplyr::distinct(),
    path = pitchbook_s3_path(
      release = release,
      type = "raw",
      table = table
    )
  )
}



to_pitchbook_key <- function(x) {
  paste0("pitchbook/", x)
}



pitchbook_folder_to_abbreviation <- function(
    folder = c(
      "other_northam", "other_row", "pe_northam", "pe_row", "vc_northam",
      "vc_row"
    )
) {
  folder <- match.arg(folder)
  switch (folder,
    "other_northam" = "ot_na",
    "other_row" = "ot_row",
    "pe_northam" = "pe_na",
    "pe_row" = "pe_row",
    "vc_northam" = "vc_na",
    "vc_row" = "vc_row"
  )
}


