#Attach everything
library(tidyverse)
library(xml2)
library(devtools)
library(PimXMLtoDf)

meanperf <- "sandbox/CV01_dag1_meanperf.xml"
meanperf_recinfo <- "sandbox/CV01_dag1_meanperf_recinfo.xml"
meanperf_recinfo_calculations <- "sandbox/CV01_dag1_meanperf_recinfo_calculations.xml"
meanperf_recinfo_calculations_percentchangeroi <- "sandbox/CV01_dag1_meanperf_recinfo_calculations_percentchangeroi.xml"
recordinginfo = TRUE
calculations = TRUE
meanperfusion = TRUE
changeperROI = FALSE

#testing with meanperf + recinfo
test <- xml_to_singlerow(meanperf_recinfo)
view(test)




#######evaluating code
ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")

# Read the XML file
xml_data <- read_xml(meanperf_recinfo_calculations)

# Find all rows in the document
rows <- xml_find_all(xml_data, ".//ss:Row", ns = ns)

# Extract data from each row
data <- map_df(rows, ~{
  cells <- xml_find_all(.x, ".//ss:Cell/ss:Data", ns = ns)
  cell_data <- map_chr(cells, xml_text)
  tibble(cell_data)
})

# Cleaningprocess based on XML output### in progress
if (identical(meanperfusion, TRUE)) {
  data <- data[-1, ]
} else if (recordinginfo) {

}

#######recinfo what to remove
data <- data[-c(1:2, 4:6, 8:13),]

######## meanperf what to remove
data <- data[-1, ]

####### calculations what to remove
data <- data[-c(3:5, 12:18, 25, 32, 39, 47:53, 60, 67, 74, 82:88, 95, 102, 109, 117:123),]

### ORDER
# RECINFO -> Calculations -> meanperfusion

# Extract the column as a vector
original_data <- data$cell_data

original_data_clean <- original_data[-(1:12)]


data

