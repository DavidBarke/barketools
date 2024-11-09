devtools::load_all()
patents_view_schema <- compute_patents_view_schema()

usethis::use_data(patents_view_schema, overwrite = TRUE)
