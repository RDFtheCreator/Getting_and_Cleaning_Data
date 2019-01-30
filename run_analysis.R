require("tidyverse")

set.seed(2)

# Reading in the Raw Data from the Test and Training Sets

## After reviewing the datasets using a text editor, I noticed the datasets are space delimited.

## read_space_delim - reads a space delimited dataset and returns it as a tibble(df).
### Arguments
#### path_to_txt - the path to your space delimited .txt file
#### ... any additional options to pass to readr::read_delim()
### Results
#### df_new <- the tibble created by reading the space delimited .txt file
read_space_delim <- function(path_to_txt, ...) {
      read_delim(path_to_txt,
                 delim = " ",
                 ...)
}

## Below are the relative paths to the following files and directories:
### features.txt - A space delimited text file containing descriptive names for each feature of the measurements dataset
### activity_labesl.txt - A space delimited text file containing descriptive labels for each activity
### Test Directory - Directory containing test subjects' data
### Train Directory - Directory containing training subjects' data

path_to_UCI_features_txt <- "data/raw_data/UCI HAR Dataset/features.txt"
path_to_UCI_activity_labels_txt <- "data/raw_data/UCI HAR Dataset/activity_labels.txt"
path_to_UCI_test_dataset <- "data/raw_data/UCI HAR Dataset/test/"
path_to_UCI_train_dataset <- "data/raw_data/UCI HAR Dataset/train/"

## Using the above paths, the following code reads in the features names for the measurement datasets

df_feature_names <- read_space_delim(
      path_to_UCI_features_txt,
      col_names = c("Column.Number", "Feature"),
      col_types = cols(col_integer(), col_character())
)

## Using the above paths, the following code reads in the raw measurements, activities, and test subjects for the
## test group

### Test Group Measurements DF
df_raw_test_measurements <- read_space_delim(
      paste0(path_to_UCI_test_dataset, "X_test.txt"),
      col_types = cols(.default = col_number()),
      col_names = df_feature_names$Feature #
)

### Test Group Activities DF
df_raw_test_activities <- read_space_delim(
      paste0(path_to_UCI_test_dataset, "y_test.txt"),
      col_types = cols(col_integer()),
      col_names = c("Activity")
)

### Test Group Subject IDs DF
df_raw_test_subjects <- read_space_delim(
      paste0(path_to_UCI_test_dataset, "subject_test.txt"),
      col_types = cols(col_integer()),
      col_names = c("Subject.ID")
)

## Using the above paths, the following code reads in the raw measurements, activities, and test subjects for the
## training group

### Training Group Measurements DF
df_raw_train_measurements <- read_space_delim(
      paste0(path_to_UCI_train_dataset, "X_train.txt"),
      col_types = cols(.default = col_number()),
      col_names = df_feature_names$Feature
)

### Training Group Activities DF
df_raw_train_activities <- read_space_delim(
      paste0(path_to_UCI_train_dataset, "y_train.txt"),
      col_types = cols(col_integer()),
      col_names = c("Activity")
)

### Training Group Subject IDs DF
df_raw_train_subjects <- read_space_delim(
      paste0(path_to_UCI_train_dataset, "subject_train.txt"),
      col_types = cols(col_integer()),
      col_names = c("Subject.ID")
)


# Combining the Measurements, Activities, and Subject IDs DFs for the Test and Training Groups Respectively

## Test Group Combination
df_test <- combined <- cbind(df_raw_test_subjects, df_raw_test_activities, df_raw_test_measurements)

## Training Group Combination
df_train <- combined <- cbind(df_raw_train_subjects, df_raw_train_activities, df_raw_train_measurements)


# Extracting only the Variables Representing Means and STDs from the Raw Test and Training DFs

## Test Group Extraction
df_test <- extracted <- df_test %>%
      select(Subject.ID,
             Activity,
             contains("mean()", ignore.case = FALSE),
             contains("std()", ignore.case = TRUE)
      )

## Training Group Extraction
df_train <- extracted <- df_train %>%
      select(Subject.ID,
             Activity,
             contains("mean()", ignore.case = FALSE),
             contains("std()", ignore.case = TRUE)
      )


#Decoding Activities to be Descriptive

## Reading in the Activity Labels from activity_labels.txt
df_activity_labels <- read_space_delim(
      path_to_UCI_activity_labels_txt,
      col_types = cols(col_integer(), col_character()),
      col_names = c("Activity.ID", "Activity.Label")
)

## A helper function was created to decode values using a lookup table
## decoder - looks up coded values in a lookup table and decodes them
### Arguments
#### v_to_decode - an integer vector of coded values
#### df_lookup - a lookup dataframe
### Results
#### v_decoded - a character vector of decded values

decoder <- function(v_to_decode, df_lookup) {
      v_decoded <- map_chr(v_to_decode,~ df_lookup[[., 2]])
      return(v_decoded)
}

## Using the decoder helper function to decode the Activity values in the Test and Training DFs

### Test Activities Decoding
df_test <- decoded <- df_test %>%
      mutate_at(vars("Activity"), decoder, df_lookup = df_activity_labels)

### Training Activities Decoding
df_train <- decoded <- df_train %>%
      mutate_at(vars("Activity"), decoder, df_lookup = df_activity_labels)

# Renaming Variables to be Descriptive

## Removing the unecessary parentheses from the variable names

### Test Variables Renaming
df_test <- var_renamed <- df_test %>%
      rename_at(vars(contains("()")), str_remove_all, pattern = "[()]")

### Train Variables Renaming
df_train <- var_renamed <- df_train %>%
      rename_at(vars(contains("()")), str_remove_all, pattern = "[()]")
