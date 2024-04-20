arrow_dataset_files <- function(sources) {
  arrow::open_dataset(sources)$files |>
    purrr::map(\(file) {
      file |>
        stringr::str_split_1("/") |>
        purrr::keep(\(x) stringr::str_detect(x, "=")) |>
        purrr::map(\(kv_string) {
          tibble::as_tibble_col(
            column_name = kv_string |> stringr::str_extract("^([^=]+)=", group = 1),
            x = kv_string |> stringr::str_extract("=([^=]+)$", group = 1)
          )
        }) |>
        purrr::list_cbind()
    }) |>
    purrr::list_rbind()
}
