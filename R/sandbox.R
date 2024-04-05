#Attach everything
library(tidyverse)
library(xml2)
library(devtools)
library(PimXMLtoDf)

meanperf <- "sandbox/CV01_dag1_meanperf.xml"
meanperf_recinfo <- "sandbox/CV01_dag1_meanperf_recinfo.xml"
meanperf_recinfo_calculations <- "sandbox/CV01_dag1_meanperf_recinfo_calculations.xml"
meanperf_recinfo_calculations_percentchangeroi <- "sandbox/CV01_dag1_meanperf_recinfo_calculations_percentchangeroi.xml"
# Enter perameters #
recordinginfo = TRUE
calculations = TRUE
meanperfusion = TRUE
changeperROI = TRUE


#######evaluating code
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
recinfodata <- data[start_row:end_row,]
# Split for mean perfusion data
start_row_perf <- which(data$cell_data == "Mean Perfusion")
end_row_perf <- (which(data$cell_data == "Mean Perfusion") + 65)
meanperfdata <- data[start_row_perf:end_row_perf,]
# Split for calculation data
start_row_calc <- which(data$cell_data == "Calculations")
end_row_calc <- (which(data$cell_data == "Calculations") + 305)
calcdata <- data[start_row_calc:end_row_calc,]
# Split for percent change data
start_row_perc <- which(data$cell_data == "Percent Change Per ROI")
end_row_perc <- (which(data$cell_data == "Percent Change Per ROI") + 65)
percentchangedata <- data[start_row_perc:end_row_perc,]


# Counting how many rows in meanperfdata has letters (variables)
# Initialize a counter
count_rows_with_letters <- 0

# Loop through each row of the dataframe
for (i in 1:nrow(meanperfdata)) {
  # Check if the current row contains any letter
  if (grepl("[a-zA-Z]", meanperfdata[i, 1])) {
    # Increment the counter if a letter is found
    count_rows_with_letters <- count_rows_with_letters + 1
  }
}
# Meanperfdata has 6 locked letters in its data structure. Every letter above 6 signifies a new variable.
# If you take above number, subtract 6 and add 1 -> you get the number of columns in data frame.
# If i want to subset the meanperfdata (split) then i take the number of columns in the dataframe, + 2. This is starting number.
# Then take column number and multitply by 4. Add this to starting number and add another 3.
# WORKS.


# Specific cleaning ####### Invalid
if (identical(meanperfusion, TRUE) &
    identical(recordinginfo | calculations | changeperROI, FALSE)) {
  data <- data[-1, ]

} else if (identical(meanperfusion & recordinginfo, TRUE) &
           identical(calculations | changeperROI, FALSE)) {
  data <- data[-c(1:2, 4:6, 8:14), ]

} else if (identical(meanperfusion & recordinginfo & calculations, TRUE) &
           identical(changeperROI, FALSE)) {
  data <- data %>%
    filter(!if_any(everything(), ~grepl("Mean Perfusion", .x)))
  data <- data[-c(1:2, 4:6, 8:14), ]

} else if (identical(meanperfusion & recordinginfo & calculations & changeperROI, TRUE)) {
  data <- data %>%
    filter(!if_any(everything(), ~grepl("Percent Change Per ROI", .x)))
  data <- data %>%
    filter(!if_any(everything(), ~grepl("Mean Perfusion", .x)))
  data <- data[-c(1:2, 4:6, 8:14), ]
}


# Extract the columns as a vectors
vec_recinfodata <- recinfodata$cell_data
vec_meanperfdata <- meanperfdata$cell_data
vec_calcdata <- calcdata$cell_data
vec_percentchangedata <- percentchangedata$cell_data

num_rows_recinfo <- 1
num_rows_meanperfdata <- 5
num_rows_percentchangedata <- 5

# Calculate the number of rows the new data frame will have
num_rows <- length(original_data_clean) / (sum(grepl("[a-zA-Z]", original_data_clean)))

# Convert the vector into a matrix with the specified number of columns, filling by row, convert back to tibble.
matrix_data <- matrix(vec_meanperfdata, nrow = num_rows_meanperfdata, byrow = TRUE)
data <- as_tibble(matrix_data)

