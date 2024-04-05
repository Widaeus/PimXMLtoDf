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
recordinginfo = FALSE
calculations = FALSE
meanperfusion = FALSE
changeperROI = FALSE


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


# Cleaningprocess based on XML output### in progress
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
}

# Extract the column as a vector
original_data <- filtered_data$cell_data

