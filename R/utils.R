# utility functions

# write_parquet_target ------------------------------------------------------------------------

write_parquet_target <- function(data, data_path = NULL, overwrite, ...) {

#' Write a parquet inside a target
#'
#' @description `write_parquet_target()` scales a common routine in targets with `file` format:
#' checking if a file exists, saving `data` in a `parquet` file in a given `data_path`, and
#' returning that path as the target result.
#'
#' @param data A `data.frame`, [RecordBatch][arrow::class-RecordBatch], or
#'  [Table][arrow::class-Table] passed to [arrow::write_parquet()]. If `NULL`, the function won't
#'  write anything at all and simply return `data`.
#' @param data_path Formally, the `sink` argument of [arrow::write_parquet()]

  if(is.null(data_path)) {
    return(data)
  } else {
    if(!file.exists(data_path) | overwrite) {
      arrow::write_parquet(x = data, sink = data_path, ...)
    }
    return(data_path)
  }
}



# sf_from_parquet -----------------------------------------------------------------------------


sf_from_parquet <- function(data_path, drop_geometry = F) {
  #' Open a `geoarrow` parquet inside a target
  #'
  #' @description since `targets` is allergic to geoarrow, this wrapper reads it from a path and
  #' optionally drops the geometry column if wanted. Just another wrapper of a common routine.
  #'
  #' @param data_path Formally, the `sources` argument of [arrow::open_dataset()]
  #' @param drop_geometry When `TRUE`, drops the geometry column.

  data <- arrow::open_dataset(data_path) |>
    sf::st_as_sf()
  if(drop_geometry) {
    data <- sf::st_drop_geometry(data)
  }
  return(data)
}
