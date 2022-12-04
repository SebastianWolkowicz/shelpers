
#' generate_multiple_vars
#'
#' @param col - vectoor to transform
#' @param .fun - R function which takes as argument a col and one of values provided in vec
#' @param vec - vector with arguments to use in
#' @param name - new column column name
#' @param sep - separator to be useed in new column names
#' @param ... - additional arguments to be passed to .fun
#'
#' @return data.frame
#' @export
#'
#' @examples #generate_new_vars(c(1,10),lag,c(2,3,4),"lagged_columns","_")
generate_multiple_vars <- function(col, .fun, vec, name, sep = "_", ...) {

  res_list <- list()
  names_vec = c()

  for (i in vec) {
    res_list[[paste(name, i, sep = sep)]] <- .fun(col, i, ...)
  }

  df = as.data.frame(res_list)
  return(df)
}


#' Title fix colnames of data frame
#'
#' @param df data frame
#' @param pattern a pattern to be removed from column names
#'
#' @return data.frame with corrected column names
#' @export
#'
#' @examples # df %>% fix_colnames("prefix")
fix_colnames<- function(df,pattern){

  colnames(df) <- stringr::str_remove(colnames(df),pattern)

  return(df)
}
