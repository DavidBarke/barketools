execute_on_cluster <- function(
    renv_directory,
    rmd_file,
    iterable,
    iter_to_params,
    image_id = "ami-0438747454de030f3",
    instance_type = "t2.small",
    upload_s3_prefix = "upload-rstudio-server-ubuntu-task",
    upload_s3_bucket = "gcpd"
) {
  desc <- cluster_desc(
    renv_directory = renv_directory,
    rmd_file = rmd_file,
    iterable = iterable,
    iter_to_params = iter_to_params,
    image_id = image_id,
    instance_type = instance_type
  )

  upload_s3_key <- glue::glue(
    "{prefix}/{file}.json",
    prefix = upload_s3_prefix,
    file = uuid::UUIDgenerate()
  )
  write_json_to_s3(
    desc,
    s3_key = upload_s3_key,
    bucket = upload_s3_bucket
  )
}

#' Cluster description
cluster_desc <- function(
    renv_directory,
    rmd_file,
    iterable,
    iter_to_params,
    image_id = "ami-0438747454de030f3",
    instance_type = "t2.small"
) {
  tasks <- purrr::map(iterable, \(el) {
    params_str <- iter_to_params(el) |>
      list_to_str()

    user_data <- glue::glue(
      '
      #! /bin/bash
      cd {renv_directory}
      Rscript -e "rmarkdown::render(\'{rmd_file}\', params={params_str})"
      shutdown -h now
      ',
      renv_directory = renv_directory,
      rmd_file = rmd_file,
      params_str = params_str
    )

    list(
      user_data = user_data
    )
  })

  desc <- list(
    instance_type = instance_type,
    image_id = image_id,
    tasks = tasks
  )
}

#' Serialize a list into a string representation
#'
#' Only works for primitive types and for one level of nesting
list_to_str <- function(l, quote = "'") {
  kv_pairs <- purrr::imap_chr(l, \(el, name) {
    glue::glue(name, "={quote}", el, "{quote}")
  }) |> paste(collapse = ",")
  glue::glue("list(", kv_pairs, ")")
}
