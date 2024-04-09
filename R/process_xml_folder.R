#' process xml folder to one tibble data frame
#'
#' @param folder_path takes path to folder containing xml files
#'
#' @return returns a tibble of all data in folder
#' @export
#'
process_xml_folder <- function(folder_path) {
  # Get a list of all XML files in the folder
  xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)

  # Initialize an empty list to store each combined tibble
  combined_tibbles_list <- list()

  # Loop through each XML file
  for (xml_file in xml_files) {
    # Generate the four tibbles from the XML
    tibbles_list <- xml_to_tibbles(xml_file) # Ensure xml_to_tibbles returns a list of tibbles

    # Combine the four tibbles into a single row
    combined_tibble <- tibbles_to_singlerow(tibbles_list) # Adjusted to pass a list

    # Store the combined tibble in the list
    combined_tibbles_list[[length(combined_tibbles_list) + 1]] <- combined_tibble
  }

  # Bind all single-row tibbles into one tibble by row
  final_combined_tibble <- bind_rows(combined_tibbles_list)

  return(final_combined_tibble)
}

#' process xml file to one tibble data frame
#'
#' @param xml_path takes path to folder containing xml files
#'
#' @return returns a tibble of onw row
#' @export
#'
process_xml_single <- function(xml_path) {
singlerow_df <- tibbles_to_singlerow(xml_to_tibbles(xml_path))

  return(singlerow_df)
}
