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
    output_file = "output.log",
    image_id = "ami-0438747454de030f3",
    security_group_ids = c(
      "sg-01f269087c271cf61", # RStudio Server
      "sg-0f0e8b61aa72dbdef" # SSH
    ),
    instance_type = "t2.small",
    terminate = TRUE,
    upload_s3_prefix = "upload-rstudio-server-ubuntu-task",
    upload_s3_bucket = "gcpd",
    renv_directory_cmds = NULL,
    renv_directory_cmds_post = NULL
) {
  desc <- cluster_desc(
    renv_directory = renv_directory,
    rmd_file = rmd_file,
    iterable = iterable,
    iter_to_params = iter_to_params,
    image_id = image_id,
    instance_type = instance_type,
    terminate = terminate,
    security_group_ids = security_group_ids,
    output_file = output_file,
    renv_directory_cmds = renv_directory_cmds,
    renv_directory_cmds_post = renv_directory_cmds_post
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

# Cluster description
cluster_desc <- function(
    renv_directory,
    rmd_file,
    iterable,
    iter_to_params,
    output_file = "output.log",
    image_id = "ami-0438747454de030f3",
    instance_type = "t2.small",
    terminate = TRUE,
    security_group_ids = c(
      "sg-01f269087c271cf61", # RStudio Server
      "sg-0f0e8b61aa72dbdef" # SSH
    ),
    renv_directory_cmds = NULL,
    renv_directory_cmds_post = NULL
) {
  tasks <- purrr::map(iterable, \(el) {
    params_str <- iter_to_params(el) |>
      list_to_str()

    user_data <- glue::glue(
      "#! /bin/bash",
      'export HOME="/root"',
      'cd {renv_directory}',
      paste(renv_directory_cmds, collapse = "\n"),
      'Rscript -e "rmarkdown::render(\'{rmd_file}\', params={params_str})" > {output_file} 2>&1',
      paste(renv_directory_cmds_post, collapse = "\n"),
      if (terminate) 'shutdown -h now',
      renv_directory = renv_directory,
      rmd_file = rmd_file,
      params_str = params_str,
      output_file = output_file,
      .null = NULL,
      .sep = "\n"
    )

    list(
      user_data = user_data
    )
  })

  desc <- list(
    instance_type = instance_type,
    image_id = image_id,
    security_group_ids = security_group_ids,
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



#' EC2 vCPU Usage
#'
#' Compute vCPU usage of all non-terminated instances.
#'
#' @returns vCPU usage (an integer).
#'
#' @export
ec2_vcpu_usage <- function() {
  out <- shell("aws ec2 describe-instances", wait = FALSE, intern = TRUE) |>
    fromJSON()

  out$Reservations$Instances |>
    purrr::keep(\(instance) instance$State$Name != "terminated") |>
    purrr::map_dbl(ec2_instance_to_vcpu) |>
    sum()
}

ec2_instance_to_vcpu <- function(instance) {
  instance$CpuOptions$CoreCount
}

#' EC2 On-Demand Limit
#'
#' Returns the service quota for "Running On-Demand Standard (A, C, D, H, I, M,
#' R, T, Z) instances".
#'
#' @export
ec2_on_demand_limit <- function() {
  out <- shell(
    "aws service-quotas list-service-quotas --service-code ec2 --quota-code L-1216C47A",
    wait = FALSE, intern = TRUE
  ) |>
    fromJSON()

  out$Quotas$Value
}

