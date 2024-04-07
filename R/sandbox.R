#Attach everything
library(tidyverse)
library(xml2)
library(devtools)
library(PimXMLtoDf)

# Examples
meanperf <- "sandbox/CV01_dag1_meanperf.xml"
meanperf_recinfo <- "sandbox/CV01_dag1_meanperf_recinfo.xml"
meanperf_recinfo_calculations <- "sandbox/CV01_dag1_meanperf_recinfo_calculations.xml"
meanperf_recinfo_calculations_percentchangeroi <- "sandbox/CV01_dag1_meanperf_recinfo_calculations_percentchangeroi.xml"

# Function parameters
recordinginfo = TRUE
calculations = TRUE
meanperfusion = TRUE
changeperROI = TRUE

ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")

# Read the XML file
xml_data <- read_xml(meanperf_recinfo_calculations_percentchangeroi)

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
if (identical(meanperfusion & recordinginfo & calculations & changeperROI, TRUE)) {
  start_row_perf <- which(data$cell_data == "Calculations")
  end_row_perf <- (which(data$cell_data == "Mean Perfusion") - 1)
  calcdata <- data[start_row_perf:end_row_perf,]
} else if (identical(meanperfusion & recordinginfo & calculations, TRUE) &
           identical(changeperROI, FALSE)) {
  start_row_perf <- which(data$cell_data == "Mean Perfusion")
  end_row_perf <- (which(data$cell_data == "Mean Perfusion") - 1)
  calcdata <- data[start_row_perf:end_row_perf,]
}

# Split for mean perfusion data
if (identical(meanperfusion & recordinginfo & calculations & changeperROI, TRUE)) {
start_row_perf <- which(data$cell_data == "Mean Perfusion")
end_row_perf <- (which(data$cell_data == "Percent Change Per ROI") - 1)
meanperfdata <- data[start_row_perf:end_row_perf,]
} else if (identical(meanperfusion & recordinginfo & calculations, TRUE) &
           identical(changeperROI, FALSE)) {
  start_row_perf <- which(data$cell_data == "Mean Perfusion")
  end_row_perf <- nrow(data)
  meanperfdata <- data[start_row_perf:end_row_perf,]
} else if (identical(meanperfusion & recordinginfo, TRUE) &
           identical(calculations | changeperROI, FALSE)) {
  start_row_perf <- which(data$cell_data == "Mean Perfusion")
  end_row_perf <- nrow(data)
  meanperfdata <- data[start_row_perf:end_row_perf,]
} else if (identical(meanperfusion, TRUE) &
           identical(recordinginfo | calculations | changeperROI, FALSE)) {
  start_row_perf <- which(data$cell_data == "Mean Perfusion")
  end_row_perf <- nrow(data)
  meanperfdata <- data[start_row_perf:end_row_perf,]
}

#Split for ROI data
if (identical(changeperROI, TRUE)) {
  start_row_perf <- which(data$cell_data == "Percent Change Per ROI")
  end_row_perf <- nrow(data)
  roidata <- data[start_row_perf:end_row_perf,]
}

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

# Mean perfusion
meanperfdata <- meanperfdata[-c(1),]
meanperfdata$cell_data <- gsub(" ", "", meanperfdata$cell_data)

# ROI data
roidata <- roidata[-c(1),]
roidata$cell_data <- gsub(" ", "", roidata$cell_data)

# Extract the column as a vectors
vec_recinfodata <- recinfodata$cell_data
vec_calcdata <- calcdata$cell_data
vec_meanperfdata <- meanperfdata$cell_data
vec_percentchangedata <- roidata$cell_data

# Define number of rows
num_rows_meanperf <- 5
num_rows_roidata <- 5
num_rows_recinfo <- 2
num_rows_calcdata <- nrow(calcdata) / 6

# Convert the vector into a matrix with the specified number of columns, filling by row, convert back to tibble.
all_vectors <- list(vec_recinfodata, vec_calcdata, vec_meanperfdata, vec_percentchangedata)
all_num_rows <- list(num_rows_recinfo, num_rows_calcdata, num_rows_meanperf, num_rows_roidata)
all_names <- c("recinfo_", "calc_", "meanperf_", "roi_")

for (i in 1:length(all_vectors)) {
  matrix_data <- matrix(all_vectors[[i]], nrow = all_num_rows[[i]], byrow = TRUE)
  data <- as_tibble(matrix_data)
  assign(paste0(all_names[i], "data"), data, envir = .GlobalEnv)
}

all_tibbles <- list(recinfo_data, calc_data, meanperf_data, roi_data)
list_names <- c("recinfo_data", "calc_data", "meanperf_data", "roi_data")
names(all_tibbles) <- list_names

# More cleaning
# Remove numbers and punctuations from calc, meanperf, roi first columns
for (i in 2:length(all_tibbles)) {
  all_tibbles[[i]][[1]] <- gsub("[0-9]|[[:punct:]]", "", as.character(all_tibbles[[i]][[1]]))
}

# Make first row labels and then remove it.
for (i in 2:length(all_tibbles)) {
  names(all_tibbles[[i]]) <- as.character(unlist(all_tibbles[[i]][1, ]))
  all_tibbles[[i]] <- all_tibbles[[i]][-1, ]
}

# Back to environment
list2env(all_tibbles, envir = .GlobalEnv)

# Prepend variables names in calcdata
for (i in 1:num_var_calcdata) {
  start_index <- 1 + (i - 1) * 4
  end_index <- i * 4
  calc_data$Calculations[start_index:end_index] <- paste(
    var_calcdata$cell_data[i], calc_data$Calculations[start_index:end_index], sep = "")
}

