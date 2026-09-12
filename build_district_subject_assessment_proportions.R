options(scipen = 999)

historical_path <- file.path(
  "data",
  "SCUC_Snapshots_1995_to_2023_LONG_CANONICAL.csv"
)

staar_path <- file.path(
  "..",
  "SCUC STAAR Longitudinal Data",
  "data",
  "canonical",
  "SCUC_STAAR_Grades_3_8_Annual_2012_2026_LONG_CANONICAL.csv"
)

output_path <- file.path(
  "outputs",
  "scuc_district_subject_assessment_proportions_1995_2026_policycraft.csv"
)
validation_path <- file.path(
  "outputs",
  "scuc_district_subject_assessment_proportions_1995_2026_validation.csv"
)
coverage_path <- file.path(
  "outputs",
  "scuc_district_subject_assessment_proportions_1995_2026_subject_coverage.csv"
)
lineage_path <- file.path(
  "outputs",
  "scuc_district_subject_assessment_proportions_1995_2026_lineage.csv"
)

required_files <- c(historical_path, staar_path)
if (!all(file.exists(required_files))) {
  stop("Missing required source file(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

historical <- read.csv(
  historical_path,
  check.names = FALSE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

staar <- read.csv(
  staar_path,
  check.names = FALSE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

historical_required <- c(
  "date", "value", "section", "grouping", "units", "grade",
  "level_achieved"
)
staar_required <- c(
  "organization_name", "district_id", "academic_year_start",
  "academic_year_end", "academic_year", "tested_grade", "student_group",
  "subject", "metric", "performance_level", "measure_type",
  "canonical_unit", "value", "reporting_status"
)

stopifnot(
  length(setdiff(historical_required, names(historical))) == 0L,
  length(setdiff(staar_required, names(staar))) == 0L
)

subjects <- c("Reading", "Writing", "Mathematics", "Science", "Social Studies")
subject_sort <- setNames(seq_along(subjects), subjects)

# Published district-wide subject results before STAAR. Counts are unavailable in
# this source, so these proportions must not be represented as count-derived.
historical_subject_map <- c(
  "Reading-ELA" = "Reading",
  "Writing" = "Writing",
  "Mathematics" = "Mathematics",
  "Science" = "Science",
  "Social Studies" = "Social Studies"
)

historical_keep <- historical[
  historical$section == "Standardized_Scores" &
    historical$level_achieved == "Approaches_or_Above" &
    historical$date >= 1995 & historical$date <= 2011 &
    historical$grouping %in% names(historical_subject_map),
  ,
  drop = FALSE
]

historical_key <- paste(historical_keep$date, historical_keep$grouping, sep = "|")
stopifnot(!anyDuplicated(historical_key))
stopifnot(all(is.na(historical_keep$value) | historical_keep$value >= 0 & historical_keep$value <= 100))

historical_out <- data.frame(
  date = as.integer(historical_keep$date),
  academic_year = sprintf("%d-%d", historical_keep$date - 1L, historical_keep$date),
  subject = unname(historical_subject_map[historical_keep$grouping]),
  value = historical_keep$value / 100,
  percent = historical_keep$value,
  tests_passed = NA_real_,
  tests_taken = NA_real_,
  grades_included = "District-reported tested grades",
  grade_count = NA_integer_,
  exam_type = ifelse(historical_keep$date <= 2002, "TAAS", "TAKS"),
  exam_name = ifelse(
    historical_keep$date <= 2002,
    "Texas Assessment of Academic Skills",
    "Texas Assessment of Knowledge and Skills"
  ),
  performance_standard = "Met standard or above (source-harmonized)",
  calculation_method = "reported_district_subject_proportion",
  series_type = "reported",
  reporting_status = ifelse(is.na(historical_keep$value), "not_available", "reported"),
  source_file = basename(historical_path),
  stringsAsFactors = FALSE
)

# Modern STAAR: construct numerator and denominator independently at grade grain,
# validate the one-to-one join, then form the ratio of sums by subject and year.
modern_counts <- staar[
  as.character(staar$district_id) %in% c("94902", "094902") &
    staar$student_group == "all_students" &
    staar$subject %in% subjects &
    staar$measure_type == "count" &
    staar$metric %in% c("performance_level", "tests_taken"),
  ,
  drop = FALSE
]

grade_key_fields <- c(
  "academic_year_start", "academic_year_end", "academic_year",
  "tested_grade", "student_group", "subject"
)
grade_key <- function(data) do.call(paste, c(data[grade_key_fields], sep = "|"))

numerator_rows <- modern_counts[
  modern_counts$metric == "performance_level" &
    modern_counts$performance_level %in% c("Satisfactory", "Approaches and Above"),
  ,
  drop = FALSE
]
denominator_rows <- modern_counts[
  modern_counts$metric == "tests_taken",
  ,
  drop = FALSE
]

stopifnot(!anyDuplicated(grade_key(numerator_rows)))
stopifnot(!anyDuplicated(grade_key(denominator_rows)))
stopifnot(setequal(grade_key(numerator_rows), grade_key(denominator_rows)))

numerator_rows <- numerator_rows[order(grade_key(numerator_rows)), , drop = FALSE]
denominator_rows <- denominator_rows[order(grade_key(denominator_rows)), , drop = FALSE]
stopifnot(identical(grade_key(numerator_rows), grade_key(denominator_rows)))

grade_counts <- numerator_rows[grade_key_fields]
grade_counts$tests_passed <- numerator_rows$value
grade_counts$tests_taken <- denominator_rows$value
grade_counts$source_performance_level <- numerator_rows$performance_level

stopifnot(
  all(!is.na(grade_counts$tests_passed)),
  all(!is.na(grade_counts$tests_taken)),
  all(grade_counts$tests_taken > 0),
  all(grade_counts$tests_passed >= 0),
  all(grade_counts$tests_passed <= grade_counts$tests_taken)
)

subject_year_key <- paste(
  grade_counts$academic_year_end,
  grade_counts$subject,
  sep = "|"
)
subject_year_split <- split(grade_counts, subject_year_key)

modern_out <- do.call(
  rbind,
  lapply(subject_year_split, function(x) {
    passed <- sum(x$tests_passed)
    taken <- sum(x$tests_taken)
    source_levels <- sort(unique(x$source_performance_level))
    stopifnot(length(source_levels) == 1L)
    data.frame(
      date = as.integer(x$academic_year_end[1]),
      academic_year = x$academic_year[1],
      subject = x$subject[1],
      value = passed / taken,
      percent = 100 * passed / taken,
      tests_passed = passed,
      tests_taken = taken,
      grades_included = paste(sort(unique(x$tested_grade)), collapse = ","),
      grade_count = length(unique(x$tested_grade)),
      exam_type = "STAAR",
      exam_name = "State of Texas Assessments of Academic Readiness",
      performance_standard = source_levels,
      calculation_method = "ratio_of_sums_tests_passed_divided_by_tests_taken",
      series_type = "derived_from_counts",
      reporting_status = "reported_or_derived",
      source_file = basename(staar_path),
      stringsAsFactors = FALSE
    )
  })
)
rownames(modern_out) <- NULL

# Retain subjects administered/reported in more than five years across the
# available source span. A subject-year counts once when it has a usable value.
observed <- rbind(historical_out, modern_out)
coverage <- aggregate(
  !is.na(observed$value),
  by = list(subject = observed$subject),
  FUN = sum
)
names(coverage)[2] <- "years_with_results"
coverage$include_more_than_five_years <- coverage$years_with_results > 5L
included_subjects <- coverage$subject[coverage$include_more_than_five_years]
stopifnot(length(included_subjects) > 0L)

# A complete year-subject grid keeps known gaps explicit for longitudinal use.
grid <- expand.grid(
  date = 1995:2026,
  subject = subjects[subjects %in% included_subjects],
  stringsAsFactors = FALSE
)
grid <- grid[order(grid$date, subject_sort[grid$subject]), , drop = FALSE]

observed_key <- paste(observed$date, observed$subject, sep = "|")
stopifnot(!anyDuplicated(observed_key))
grid_key <- paste(grid$date, grid$subject, sep = "|")
match_index <- match(grid_key, observed_key)
result <- observed[match_index, , drop = FALSE]
result$date <- grid$date
result$subject <- grid$subject
result$academic_year <- ifelse(
  is.na(result$academic_year),
  sprintf("%d-%d", result$date - 1L, result$date),
  result$academic_year
)

result$exam_type <- ifelse(
  is.na(result$exam_type),
  ifelse(result$date <= 2002, "TAAS", ifelse(result$date <= 2011, "TAKS", "STAAR")),
  result$exam_type
)
result$exam_name <- ifelse(
  is.na(result$exam_name),
  ifelse(
    result$exam_type == "TAAS",
    "Texas Assessment of Academic Skills",
    ifelse(
      result$exam_type == "TAKS",
      "Texas Assessment of Knowledge and Skills",
      "State of Texas Assessments of Academic Readiness"
    )
  ),
  result$exam_name
)

is_2020 <- result$date == 2020
is_writing_after_2021 <- result$subject == "Writing" & result$date >= 2022
missing_other <- is.na(result$value) & !is_2020 & !is_writing_after_2021
result$reporting_status[is_2020] <- "not_administered_statewide"
result$reporting_status[is_writing_after_2021] <- "not_separately_reported"
result$reporting_status[missing_other] <- "not_available"
result$calculation_method[is.na(result$calculation_method)] <- "not_calculated"
result$series_type[is.na(result$series_type)] <- "missing"
result$source_file[is.na(result$source_file)] <- NA_character_
result$grades_included[is.na(result$grades_included)] <- NA_character_
result$performance_standard[is.na(result$performance_standard)] <- NA_character_

result$series_label <- result$subject
result$subject_sort <- unname(subject_sort[result$subject])
result$exam_sort <- match(result$exam_type, c("TAAS", "TAKS", "STAAR"))
result$unit <- "proportion"
result$direction <- "higher_is_better"
result$district_id <- "094902"
result$organization_name <- "Schertz-Cibolo-Universal City ISD"

result <- result[c(
  "date", "academic_year", "value", "percent", "series_label", "subject",
  "subject_sort", "exam_type", "exam_name", "exam_sort",
  "performance_standard", "tests_passed", "tests_taken", "grades_included",
  "grade_count", "calculation_method", "series_type", "reporting_status",
  "unit", "direction", "district_id", "organization_name", "source_file"
)]

result <- result[order(result$date, result$subject_sort), , drop = FALSE]
row.names(result) <- NULL

result_key <- paste(result$date, result$subject, sep = "|")
validation <- data.frame(
  check = c(
    "unique_year_subject_key",
    "date_range_is_1995_to_2026",
    "all_values_are_proportions",
    "modern_count_ratios_reconcile",
    "subjects_have_more_than_five_results",
    "2025_and_2026_present",
    "2020_is_explicitly_missing"
  ),
  passed = c(
    !anyDuplicated(result_key),
    identical(range(result$date), c(1995L, 2026L)),
    all(is.na(result$value) | result$value >= 0 & result$value <= 1),
    all(abs(modern_out$value - modern_out$tests_passed / modern_out$tests_taken) < 1e-12),
    all(coverage$years_with_results[coverage$subject %in% included_subjects] > 5L),
    all(c(2025L, 2026L) %in% result$date[result$reporting_status == "reported_or_derived"]),
    all(is.na(result$value[result$date == 2020]))
  ),
  stringsAsFactors = FALSE
)

stopifnot(all(validation$passed))

source_info <- file.info(required_files)
lineage <- data.frame(
  source_path = normalizePath(required_files, mustWork = TRUE),
  md5 = unname(tools::md5sum(required_files)),
  modified = format(source_info$mtime, "%Y-%m-%d %H:%M:%S %Z"),
  bytes = source_info$size,
  source_rows = c(nrow(historical), nrow(staar)),
  source_columns = c(ncol(historical), ncol(staar)),
  role = c(
    "Reported district subject proportions, 1995-2011",
    "STAAR grade-level counts, 2012-2026"
  ),
  stringsAsFactors = FALSE
)

write.csv(result, output_path, row.names = FALSE, na = "")
write.csv(validation, validation_path, row.names = FALSE, na = "")
write.csv(coverage[order(subject_sort[coverage$subject]), ], coverage_path, row.names = FALSE, na = "")
write.csv(lineage, lineage_path, row.names = FALSE, na = "")

message("Wrote ", nrow(result), " rows to ", output_path)
message("Observed values: ", sum(!is.na(result$value)), "; explicit missing rows: ", sum(is.na(result$value)))
