#' Importing and splitting
#'
#' @param xml_file_path file path to a single xml file generated på PimSOFT. Mandatory recinfo, calc, meanperf and roi info.
#'
#' @return returns a list of data frames. One long column of raw data.
#' @export
#'
import_split <- function(xml_file_path) {
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

# Remove empty strings
data <- data %>%
  filter(!if_any(everything(), ~ .x == ""))

# Split up data to make it generic
# Split for rec info
start_row_rec <- which(data$cell_data == "Recording info")
end_row_rec <- (which(data$cell_data == "Image Count") + 3)
recinfodata <- data[start_row_rec:end_row_rec,]

# Split for calculation data
start_row_perf <- which(data$cell_data == "Calculations")
end_row_perf <- (which(data$cell_data == "Mean Perfusion") - 1)
calcdata <- data[start_row_perf:end_row_perf,]

# Split for mean perfusion data
start_row_perf <- which(data$cell_data == "Mean Perfusion")
end_row_perf <- (which(data$cell_data == "Percent Change Per ROI") - 1)
meanperfdata <- data[start_row_perf:end_row_perf,]

#Split for ROI data
start_row_perf <- which(data$cell_data == "Percent Change Per ROI")
end_row_perf <- nrow(data)
roidata <- data[start_row_perf:end_row_perf,]

return(list(recinfo = recinfodata, calc = calcdata, meanperf = meanperfdata, roi = roidata))
}

#' Tibble converting
#'
#' @param list a list generated from "import_split" function
#'
#' @return returns a list of tibbles and necessary variable data.
#' @export
#'
tibbleconvert <- function(list) {

recinfodata <- list$recinfo
calcdata <- list$calc
meanperfdata <- list$meanperf
roidata <- list$roi

# Counting how many rows calcdata has letters (variables)
count_rows_with_letters <- 0
for (i in 1:nrow(calcdata)) {
  # Check if the current row contains any letter
  if (grepl("[a-zA-Z]", calcdata[i, 1])) {
    # Increment the counter if a letter is found
    count_rows_with_letters <- count_rows_with_letters + 1
  }
}
# Starting variables are 6, divide by 5 to get nr of variables in calcdata
num_var_calcdata <- (count_rows_with_letters - 6) / 5

# Specific cleaning
# Recording info
recinfodata <- recinfodata[-c(1:2, 4:6, 8:13),]
recinfodata$cell_data <- gsub(" ", "", recinfodata$cell_data)
vec_recinfodata <- recinfodata$cell_data
num_rows_recinfo <- 2


# Calculations
saved_calcdata_vars <- calcdata[c(1:6),]
stripped_calcdata <- calcdata[-c(1:6),]
# Function for variable position
var_position <- function(n) {
  return(1 + (n - 1) * 25)
}
# Generate the row indices using var_position
row_indices <- sapply(1:(num_var_calcdata), var_position)
# Extract variable names
var_calcdata <- data.frame(cell_data = stripped_calcdata[row_indices, "cell_data"])
# Now remove all rows containing variable names
stripped_calcdata <- stripped_calcdata[-c(row_indices),]
# Recombine the saved variables
calcdata <- rbind(saved_calcdata_vars, stripped_calcdata)
# Remove spaces
calcdata$cell_data <- gsub(" ", "", calcdata$cell_data)
vec_calcdata <- calcdata$cell_data
num_rows_calcdata <- nrow(calcdata) / 6


# Mean perfusion
meanperfdata <- meanperfdata[-c(1),]
meanperfdata$cell_data <- gsub(" ", "", meanperfdata$cell_data)
vec_meanperfdata <- meanperfdata$cell_data
num_rows_meanperf <- 5

# ROI data
roidata <- roidata[-c(1),]
roidata$cell_data <- gsub(" ", "", roidata$cell_data)
vec_percentchangedata <- roidata$cell_data
num_rows_roidata <- 5

# Convert the vector into a matrix with the specified number of columns, filling by row, convert back to tibble.
all_vectors <- list(vec_recinfodata, vec_calcdata, vec_meanperfdata, vec_percentchangedata)
all_num_rows <- list(num_rows_recinfo, num_rows_calcdata, num_rows_meanperf, num_rows_roidata)
all_names <- c("recinfo_", "calc_", "meanperf_", "roi_")

for (i in 1:length(all_vectors)) {
  matrix_data <- matrix(all_vectors[[i]], nrow = all_num_rows[[i]], byrow = TRUE)
  data <- as_tibble(matrix_data)
  assign(paste0(all_names[i], "data"), data, envir = .GlobalEnv)
}


return(list(recinfo_tibble = recinfo_data,
            calc_tibble = calc_data,
            meanperf_tibble = meanperf_data,
            roi_tibble = roi_data,
            var_calc = var_calcdata,
            num_var = num_var_calcdata))
}

#' Tibble cleaning
#'
#' @param tibble_list a list generated from "tibbleconvert" function.
#'
#' @return returns a list of tibbles.
#' @export
#'
cleantibble <- function(tibble_list) {

  # Importing list
  var_calcdata <- tibble_list$var_calc
  num_var_calcdata <- tibble_list$num_var

  # Removing said elements
  tibble_list$var_calc <- NULL
  tibble_list$num_var <- NULL

  for (i in 2:length(tibble_list)) {
  tibble_list[[i]][[1]] <- gsub("[0-9]|[[:punct:]]", "", as.character(tibble_list[[i]][[1]]))
}

  # Make first row labels and then remove it.
  for (i in 2:length(tibble_list)) {
    names(tibble_list[[i]]) <- as.character(unlist(tibble_list[[i]][1, ]))
    tibble_list[[i]] <- tibble_list[[i]][-1, ]
  }

  # Importing another part of list
  recinfo_tibble <- tibble_list$recinfo
  calc_tibble <- tibble_list$calc
  meanperf_tibble <- tibble_list$meanperf
  roi_tibble <- tibble_list$roi

  # Removing spaces in var_calcdata
  var_calcdata[, 1] <- gsub(" ", "", var_calcdata[, 1])

  # Prepend variables names in calcdata
  for (i in 1:num_var_calcdata) {
    start_index <- 1 + (i - 1) * 4
    end_index <- i * 4

    # Loop through each individual row in the block
    for (j in start_index:end_index) {
      # Check if j is within the bounds of calc_tibble
      if (j <= nrow(calc_tibble)) {
        calc_tibble[j, 1] <- paste(var_calcdata[i, 1], calc_tibble[j, 1], sep = "|")
      }
    }
  }

  ## Back to a list
  all_tibbles <- list(recinfo_tibble, calc_tibble, meanperf_tibble, roi_tibble)
  list_names <- c("recinfo_data", "calc_data", "meanperf_data", "roi_data")
  names(all_tibbles) <- list_names

  # Renaming
  for (i in 2:length(all_tibbles)) {
    first_col_name <- names(all_tibbles[[i]])[1]  # Get the name of the first column

    all_tibbles[[i]] <- all_tibbles[[i]] %>%
      rename_with(.fn = ~ifelse(str_detect(., regex("ac", ignore_case = TRUE)), "ACHmax", .),
                  .cols = everything()) %>%
      rename_with(.fn = ~ifelse(str_detect(., regex("sn", ignore_case = TRUE)), "SNPmax", .),
                  .cols = everything()) %>%
      rename_with(~ str_replace_all(.x, regex("vila|rest", ignore_case = TRUE), "rest")) %>%
      rename_with(~ str_replace_all(.x, regex("ref", ignore_case = TRUE), "ref")) %>%
      mutate(across(all_of(first_col_name), ~ str_replace_all(.x, regex("vila|rest", ignore_case = TRUE), "rest"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace(.x, "ACNmax", "ACHmax"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace_all(.x, " ", ""))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace_all(.x, regex("ref", ignore_case = TRUE), "refsens"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace_all(.x, "\\|ACH", "|ACHsens"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace_all(.x, "\\|SNP", "|SNPsens"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace(.x, ".*?(SNPmax)", "SNPmax"))) %>%
      mutate(across(all_of(first_col_name), ~ str_replace(.x, ".*?(ACHmax)", "ACHmax")))
  }

  recinfo_tibble <- all_tibbles$recinfo_data
  calc_tibble <- all_tibbles$calc_data
  meanperf_tibble <- all_tibbles$meanperf_data
  roi_tibble <- all_tibbles$roi_data

  meanperf_tibble <- meanperf_tibble %>%
    mutate(across(PU, ~ str_replace_all(.x, "ACH", "ACHsens"))) %>%
    mutate(across(PU, ~ str_replace_all(.x, "SNP", "SNPsens")))

  roi_tibble <- roi_tibble %>%
    mutate(across(Change, ~ str_replace_all(.x, "ACH", "ACHsens"))) %>%
    mutate(across(Change, ~ str_replace_all(.x, "SNP", "SNPsens")))

  #selecting relevant variables and single row transform # recinfo
  recinfo_tibble <- recinfo_tibble %>%
    mutate(row_id = c("id", "datetime")) %>%
    pivot_wider(names_from = 2, values_from = 1) %>%
    mutate(datetime = ymd_hms(datetime))

  return(list(recinfo = recinfo_tibble, calc = calc_tibble, meanperf = meanperf_tibble, roi = roi_tibble))
  }

#' Selection of variables
#'
#' @param clean_tibble takes a list of clean tibbles from "cleantibble" function.
#'
#' @return returns a a list of tibbles.
#' @export
#'
compact <- function(clean_tibble) {

  recinfo_start <- clean_tibble$recinfo
  calc_start <- clean_tibble$calc
  meanperf_start <- clean_tibble$meanperf
  roi_start <- clean_tibble$roi

  # Selecting relevant variables
  calc_start <- calc_start %>%
    filter(grepl("rest|ACHmax|SNPmax", Calculations)) %>%
    filter(!grepl("Entireimg", Calculations))

  meanperf_start <- meanperf_start %>%
    select(1, matches("rest|ACHmax|SNPmax")) %>%
    filter(!grepl("Entireimg", PU))

  roi_start <- roi_start %>%
    select(1, matches("rest|ACHmax|SNPmax")) %>%
  filter(!grepl("Entireimg", Change))

  return(list(recinfo = recinfo_start,
              calc = calc_start,
              meanperf = meanperf_start,
              roi = roi_start))
}

#' Transforms to singlerow
#'
#' @param compact_tibble takes a list of compact tibbles from "compact" function.
#'
#' @return returns a single tibble of one row per XML input file.
#' @export
#'
singlerow <- function(compact_tibble) {

  recinfo_start <- compact_tibble$recinfo
  calc_start <- compact_tibble$calc
  meanperf_start <- compact_tibble$meanperf
  roi_start <- compact_tibble$roi

  calc_start <- calc_start %>%
    pivot_wider(names_from = Calculations, values_from = c(2:6)) %>%
    summarize(across(everything(), ~ .[!is.na(.)][1])) %>%
    rename_with(~ paste0("calc_", .), .cols = everything())

  meanperf_start <- meanperf_start %>%
    pivot_wider(names_from = 1, values_from = c(2:4)) %>%
    summarize(across(everything(), ~ .[!is.na(.)][1])) %>%
    rename_with(~ paste0("meanperf_", .), .cols = everything())

  roi_start <- roi_start %>%
    pivot_wider(names_from = Change, values_from = c(2:4)) %>%
    summarize(across(everything(), ~ .[!is.na(.)][1])) %>%
    rename_with(~ paste0("roi_", .), .cols = everything())

  combined_tibble <- bind_cols(recinfo_start, calc_start, meanperf_start, roi_start)

  return(combined_tibble)
}

#' Entire transform
#'
#' @param xml_file_path takes a XML file path
#'
#' @return returns a single tibble of one row per XML input file.
#' @export
#'
xml_to_df <- function(xml_file_path) {

  import_list <- import_split(xml_file_path)
  tibble_list <- tibbleconvert(import_list)
  clean_tibbles <- cleantibble(tibble_list)
  compact_tibbles <- compact(clean_tibbles)
  singlerow_tibble <- singlerow(compact_tibbles)

  return(singlerow_tibble)
}

#' Entire transform, folder edition
#'
#' @param folder_path takes a folder file path. Transforms all of the XML files there.
#'
#' @return returns a single tibble of one row per XML input file in folder.
#' @export
#'
folder_to_df <- function(folder_path) {

    xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)
    combined_tibbles_list <- list()

  # Loop through each XML file
  for (xml_file in xml_files) {
    # Generate the four tibbles from the XML
    # Combine the four tibbles into a single row

    tibbles_list <- xml_to_df(xml_file) # Ensure xml_to_tibbles returns a list of tibbles

    # Store the combined tibble in the list
    combined_tibbles_list[[length(combined_tibbles_list) + 1]] <- tibbles_list
  }

  # Bind all single-row tibbles into one tibble by row
  final_combined_tibble <- bind_rows(combined_tibbles_list)

  return(final_combined_tibble)
}
