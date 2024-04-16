#' Execute a parameterized RMD on cluster
#'
#' @param renv_directory Path to a directory on the host machine that is
#' managed by renv and where all dependencies have been installed into the
#' AMI image.
#' @param rmd_file Path to a RMD script relative to `renv_directory`.
#' @param iterable Vector or list. A separate instance is launched for each
#' element.
#' @param iter_to_params Function that transforms an element of `iterable`
#' into a list of parameters that are passed to `rmarkdown::render` in the
#' `params` argument.
#' @param image_id AMI image id. Each instance will be based on this image.
#' @param instance_type EC2 instance type.
#' @param terminate If `TRUE` terminate the instance after script completes.
#' @param upload_s3_prefix Path to which the JSON file describing the cluster
#' is uploaded. The default will upload it to a preconfigured folder in S3
#' that will trigger a lambda which will eventually create the requested
#' instances.
#' @param upload_s3_bucket S3 bucket.
#'
#' @export
execute_on_cluster <- function(
    renv_directory,
    rmd_file,
    iterable,
    iter_to_params,
    image_id = "ami-0438747454de030f3",
    instance_type = "t2.small",
    terminate = TRUE,
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
    instance_type = "t2.small",
    terminate = TRUE
) {
  tasks <- purrr::map(iterable, \(el) {
    params_str <- iter_to_params(el) |>
      list_to_str()

    user_data <- glue::glue(
      '
      #! /bin/bash
      cd {renv_directory}
      Rscript -e "rmarkdown::render(\'{rmd_file}\', params={params_str})"
      ',
      renv_directory = renv_directory,
      rmd_file = rmd_file,
      params_str = params_str
    )

    if (terminate) user_data <- glue::glue(
      "{user_data}
      shutdown -h now",
      user_data = user_data
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
