
#' xml_to_singlerow
#'
#' @description
#' *Applicable to microvasc research group, Danderyds Sjukhus workflow*. Takes a PimSOFT report in XML format and generates a single row of variables suited for implementation into a data frame. It is suited for reports generated with "Recording info" and "Mean perfusion" boxes ticked.
#'
#'
#' @param xml_file_path file path to xml file in working directory
#'
#' @return returns a single row of variables of xml file
#' @export
#'
xml_to_singlerow <- function(xml_file_path) {
  # Define the namespace to handle prefixed tags
  ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")

  # Read the XML file
  xml_data <- read_xml(xml_file_path)

  # Find all rows in the document
  rows <- xml_find_all(xml_data, ".//ss:Row", ns = ns)

  # Extract data from each row
  data <- map_df(rows, ~{
    cells <- xml_find_all(.x, ".//ss:Cell/ss:Data", ns = ns)
    cell_data <- map_chr(cells, xml_text)
    tibble(cell_data)
  })

  # Removing first row from 'data'
  data <- data[-1, ]

  # Extract the column as a vector
  original_data <- data$cell_data
  original_data_clean <- original_data[-(1:12)]

  # Removing first row from 'data' again
  original_data_clean <- original_data_clean[-1]

  # Calculate the number of rows the new data frame will have
  num_rows <- length(original_data_clean) / (sum(grepl("[a-zA-Z]", original_data_clean)) - 4)

  # Convert the vector into a matrix with the specified number of columns, filling by row, convert back to tibble.
  matrix_data <- matrix(original_data_clean, nrow = num_rows, byrow = TRUE)
  data <- as_tibble(matrix_data)

  # Set the first row as the column names and remove.
  names(data) <- as.character(unlist(data[1,]))
  data <- data[-1,]

  # Remove numbers, punctiation and spaces from first category and change name to "ROI"
  data$PU <- str_replace_all(data$PU, "[0-9]|[[:punct:]]|\\s", "")
  names(data)[1] <- 'ROI'

  # Remove last row
  data <- data[-4, ]

  # Selecting relevant variables
  data <- data %>%
    select(
      matches("ROI"),
      matches("^(Vila|vila|Rest|rest)$"),
      matches("[Aa].*max"),
      matches("[Ss].*max")
    )

  # Renaming restperiods
  data <- data %>%
    rename_with(.fn = ~str_replace_all(.x, "^(Vila|vila|Rest|rest)$", "rest"),
                .cols = matches("^(Vila|vila|Rest|rest)$"))

  # Renaming ACH and SNP provocations
  data <- data %>%
    rename_with(.fn = ~ifelse(str_detect(., regex("AC|ac", ignore_case = TRUE)), "ACHmax", .),
                .cols = everything())
  data <- data %>%
    rename_with(.fn = ~ifelse(str_detect(., regex("SN|sn", ignore_case = TRUE)), "SNPmax", .),
                .cols = everything())

  # Remove spaces from column names
  names(data) <- names(data) %>%
    str_replace_all(" ", "") # Remove spaces

  #  Clean category string variables
  data <- data %>%
    mutate(ROI = str_replace(ROI, "ACH", "ACHsensor")) %>% # Replace ACH with sensACH in "ROI"
    mutate(ROI = str_replace(ROI, "REF|ref|Ref", "refsensor")) %>% # Replace REF with sensref in "ROI"
    mutate(ROI = str_replace(ROI, "SNP", "SNPsensor"))     # Replace SNP with sensSNP in "ROI"


  # Pivot wider
  data <- data %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = matches("ROI"), values_from = c(matches("^(Vila|vila|Rest|rest)$"),
                                                             matches("[Aa].*max"),
                                                             matches("[Ss].*max")), names_sep = "")

  # Summarize to a single row
  data <- data %>%
    summarize(across(everything(), ~ .[!is.na(.)][1]))

  # Delete first column
  data <- data[,-1]

  # Add date column
  data$datetime <- as_datetime(original_data[6])

  # Add observation id
  data$observation <- original_data[2]

  # Re-organize column order
  data <- data %>%
    select(observation, datetime, everything())

  # Convert data to numeric in a forloop
  for (i in 3:11) {
    data[[i]] <- as.numeric(as.character(data[[i]]))
  }

  # Return the new data frame
  return(data)
}
