#' Convert TSV File
#'
#' This function cleans column names and converts units in a TSV file.
#'
#' @param tsv_file A data frame representing the TSV file.
#'
#' @return A list with data frame with cleaned column names and converted units.
#'
#' @examples
#' tsv_convert(my_tsv_file)
#'
#' @export
tsv_convert <- function(tsv_file) {
# Clean column names
names(tsv_file) <- gsub(" ", "", names(tsv_file))

# Convert units
tsv_file$Timems <- tsv_file$Timems / 1000 / 60

# Rename column
names(tsv_file)[names(tsv_file) == "Timems"] <- "time"

rest_begin <- tsv_file %>%
  filter(TOIs == "rest begin") %>%
  pull(time) %>%
  first()

rest_end <- tsv_file %>%
  filter(TOIs == "rest end") %>%
  pull(time) %>%
  first()

rest_midpoint <- mean(c(rest_begin, rest_end))

# Calculate buffer for plot
buffer <- (max(tsv_file$Value) - min(tsv_file$Value, na.rm = TRUE)) * 0.03

# Replace the values in the ROI column
tsv_file$ROI[grepl("snp", tsv_file$ROI, ignore.case = TRUE)] <- "SNP"
tsv_file$ROI[grepl("ach", tsv_file$ROI, ignore.case = TRUE)] <- "ACH"
tsv_file$ROI[grepl("ref", tsv_file$ROI, ignore.case = TRUE)] <- "ref"

# Reorder factor levels
tsv_file$ROI <- factor(tsv_file$ROI,
                       levels = c("ref",
                                  "SNP",
                                  "ACH"))

####### SNP
# Filter the data for SNP
tsv_file_snp <- tsv_file %>% filter(ROI == "SNP")

# Fit a loess model to the SNP data
model_snp <- loess(Value ~ time, data = tsv_file_snp, span = 0.2)

# Generate a sequence of time values for SNP
time_values_snp <- seq(min(tsv_file_snp$time), max(tsv_file_snp$time), length.out = 1000)

# Predict the values at these time points for SNP
predicted_values_snp <- predict(model_snp, newdata = data.frame(time = time_values_snp))

# Find the maximum predicted value for SNP
max_predicted_value_snp <- max(predicted_values_snp)

# Find the time value corresponding to the maximum predicted value for SNP
time_at_max_predicted_value_snp <- time_values_snp[which.max(predicted_values_snp)]

# Calculate the value of the smoothed regression at the highest point for SNP
value_at_max_predicted_value_snp <- predict(model_snp, newdata = data.frame(time = time_at_max_predicted_value_snp))

# Subset
time_values_snp_rest <- time_values_snp[time_values_snp >= rest_begin & time_values_snp <= rest_end]
predicted_values_snp_rest <- predicted_values_snp[time_values_snp >= rest_begin & time_values_snp <= rest_end]

# Find the minimum predicted value within the rest interval for SNP
min_predicted_value_snp_rest <- min(predicted_values_snp_rest)

# Calculate snpmax_begin and snpmax_end
snpmax_begin <- time_at_max_predicted_value_snp - 0.5
snpmax_end <- time_at_max_predicted_value_snp + 0.5

# Calculate the time between rest_end and time_at_max_predicted_value_snp
time_to_max_snp <- time_at_max_predicted_value_snp - rest_end

##### ACH
# Filter the data for ACH
tsv_file_ach <- tsv_file %>% filter(ROI == "ACH")

# Fit a loess model to the ACH data
model_ach <- loess(Value ~ time, data = tsv_file_ach, span = 0.2)

# Generate a sequence of time values for ACH
time_values_ach <- seq(min(tsv_file_ach$time), max(tsv_file_ach$time), length.out = 1000)

# Predict the values at these time points for ACH
predicted_values_ach <- predict(model_ach, newdata = data.frame(time = time_values_ach))

# Find the maximum predicted value for ACH
max_predicted_value_ach <- max(predicted_values_ach)

# Find the time value corresponding to the maximum predicted value for ACH
time_at_max_predicted_value_ach <- time_values_ach[which.max(predicted_values_ach)]

# Calculate the value of the smoothed regression at the highest point for ACH
value_at_max_predicted_value_ach <- predict(model_ach, newdata = data.frame(time = time_at_max_predicted_value_ach))

# Time interval for ach
achmax_begin <- time_at_max_predicted_value_ach - 0.5
achmax_end <- time_at_max_predicted_value_ach + 0.5

# Subset
time_values_ach_rest <- time_values_ach[time_values_ach >= rest_begin & time_values_ach <= rest_end]
predicted_values_ach_rest <- predicted_values_ach[time_values_ach >= rest_begin & time_values_ach <= rest_end]

# Find the minimum predicted value within the rest interval for ACH
min_predicted_value_ach_rest <- min(predicted_values_ach_rest)

# Calculate the time between rest_end and time_at_max_predicted_value_ach
time_to_max_ach <- time_at_max_predicted_value_ach - rest_end

# Filter tsv_file for "SNP" and calculate the difference from the rest interval median
tsv_file_snp <- tsv_file %>%
  filter(ROI == "SNP")

# Using loess regression to find point where SNP goes below rest level
# Fit a loess model to the data
model <- loess(Value ~ time, data = tsv_file_snp)

# Generate a sequence of time values starting at snpmax time
time_values_snp <- seq(min(time_at_max_predicted_value_snp), max(tsv_file_snp$time), length.out = 1000)

# Predict the values at these time points
predicted_values_snp <- predict(model, newdata = data.frame(time = time_values_snp))

if (min(predicted_values_snp) <= min_predicted_value_snp_rest) {
  # Find the first time point where the predicted value is less than or equal to rest_level of SNP
  recovery_time_snp <- time_values_snp[min(which(predicted_values_snp <= min_predicted_value_snp_rest))]
} else {
  recovery_time_snp <- NA
  print("SNP does not tangent rest value")
}

# Now for ACH
# Filter the data for ACH
tsv_file_ach <- tsv_file %>% filter(ROI == "ACH")

# Fit a loess model to the ACH data
model_ach <- loess(Value ~ time, data = tsv_file_ach)

# Generate a sequence of time values starting at ach max time
time_values_ach <- seq(min(time_at_max_predicted_value_ach), max(tsv_file_ach$time), length.out = 1000)

# Predict the values at these time points
predicted_values_ach <- predict(model_ach, newdata = data.frame(time = time_values_ach))

if (min(predicted_values_ach) <= min_predicted_value_ach_rest) {
  # Find the first time point where the predicted value is less than or equal to rest level
  recovery_time_ach <- time_values_ach[min(which(predicted_values_ach <= min_predicted_value_ach_rest))]
} else {
  recovery_time_ach <- NA
  print("ACH does not tangent rest value")
}

# all relevant outputs
achmax <- value_at_max_predicted_value_ach
achtimemax <- time_at_max_predicted_value_ach
snpmax <- value_at_max_predicted_value_snp
snptimemax <- time_at_max_predicted_value_snp
achmaxbegin <- achmax_begin
achmaxend <- achmax_end
snpmaxbegin <- snpmax_begin
snpmaxend <- snpmax_end
restach <- min_predicted_value_ach_rest
restsnp <- min_predicted_value_snp_rest
deltaach <- achmax - restach
deltasnp <- snpmax - restsnp
ttresponse_snp <- time_to_max_snp
ttresponse_ach <- time_to_max_ach
ttrecov_snp <- recovery_time_snp
ttrecov_ach <- recovery_time_ach
rest_begin <- rest_begin
rest_end <- rest_end
buffer <- buffer


# Create a tibble with one row and multiple columns
relevant_variables <- tibble(
  achmax = achmax,
  achtimemax = achtimemax,
  snpmax = snpmax,
  snptimemax = snptimemax,
  achmaxbegin = achmaxbegin,
  achmaxend = achmaxend,
  snpmaxbegin = snpmaxbegin,
  snpmaxend = snpmaxend,
  restach = restach,
  restsnp = restsnp,
  deltaach = deltaach,
  deltasnp = deltasnp,
  ttresponse_snp = ttresponse_snp,
  ttresponse_ach = ttresponse_ach,
  ttrecov_snp = ttrecov_snp,
  ttrecov_ach = ttrecov_ach,
  rest_begin = rest_begin,
  rest_end = rest_end,
  buffer = buffer
)

return(list(tsv_file, relevant_variables))

}
