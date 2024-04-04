#' singlerow_df_compile
#' @description
#' An addon to function "xml_to_singlerow" which compiles each observation(xml file) in to an easy to read data frame format.
#'
#' @param xml_folder_file_path
#'
#' @return A dataframe with each XML file as an observation(row)
#' @export
#'
#'
singlerow_df_compile <- function(xml_folder_file_path) {
  # List all XML files in the target directory
  files <- list.files(path = xml_folder_file_path, pattern = "\\.xml$", full.names = TRUE)

  # Apply your function to each file and combine the results
  compiled_df <- map_dfr(files, process_xml_to_df)

  return(compiled_df)
}
