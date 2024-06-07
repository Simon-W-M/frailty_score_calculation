library(tidyverse)

# test data
diagnosis <- c('||T838 ,Z960 ,Z940 ,Z905 ,Z874 ,I10X ,R91X ,Z861',
               '||R104 ,R11X ,E119 ,E668 ,F444 ,F445 ,Z874 ||R104 ,R11X ,E119 ,E668 ,F444 ,F445 ,Z874 ||N201 ,E119 ,E668 ,F444 ,F445 ,Z874',
               '||R31X ,Z874 ,I480 ,Z921 ,E039 ,Z966',
               '||N390 ,I10X ,Y409 ,J450 ,Z881 ||N390 ,I10X ,Y409 ,J450 ,Z881')

data <- data.frame(diagnosis)

# the actual function

add_frailty_metrics <- function(data, diagnosis_column) {
  #' @name add_frailty_metrics
  #' @title Add Frailty Metrics to a Data Frame
  #'
  #' @description
  #' This function adds new variables to a data frame that indicate the presence of 
  #' specific ICD-10 codes and a frailty score and group based on those codes. ICD-10 
  #' codes are used in healthcare to classify diagnoses and procedures.
  #' The frailty score is based on Hospital Frailty Risk score Gilbert et al.
  #' Lancet 2018, 391: 1775-82
  #'   #'
  #' @arguments
  #'   * data: A data frame containing a column with ICD-10 diagnosis codes (character).
  #'   * diagnosis_column: The name of the column in `data` that contains ICD-10 diagnosis codes (character).
  #'
  #' @returns
  #'   The modified data frame with the following new variables:
  #'     * Columns named `icd_` followed by a three-character ICD-10 code (e.g., `icd_F00`): 
  #'       These columns contain a frailty value of the corresponding ICD-10 
  #'       code in the diagnosis column, and 0 otherwise. For example, `icd_F00` indicates the 
  #'       presence of Dementia and return a score of 7.1.
  #'     * `frailty_score`: The sum of the values in the `icd_` columns (numeric). This score 
  #'       represents the cumulative burden of health conditions based on the identified ICD-10 codes.
  #'     * `frailty_group`: A categorical variable indicating the frailty level based on the 
  #'       `frailty_score` ("high", "medium", "low", or "null"). This categorisation helps assess 
  #'       a patient's overall health risk.
  #'
  #' @details
  #'   * The function iterates through a predefined list of ICD-10 codes and checks for their 
  #'     presence in the diagnosis column using the `str_detect` function.
  #'   * The `if_else` function assigns values to the new `icd_` columns based on 
  #'     whether the corresponding ICD-10 code is found.
  #'   * The `rowSums` function calculates the sum of the values in the `icd_` columns, excluding 
  #'     missing values (NA).
  #'   * The `case_when` function categorizes the `frailty_score` into groups based on predefined 
  #'     thresholds ("high", "medium", "low", or "null").
  #'     
  data <- data |>
    mutate(
      icd_F00 = if_else(str_detect(!!sym(diagnosis_column), "F00"), 7.1, 0),
      icd_G81 = if_else(str_detect(!!sym(diagnosis_column), "G81"), 4.4, 0),
      icd_G30 = if_else(str_detect(!!sym(diagnosis_column), "G30"), 4.0, 0),
      icd_I69 = if_else(str_detect(!!sym(diagnosis_column), "I69"), 3.7, 0),
      icd_R29 = if_else(str_detect(!!sym(diagnosis_column), "R29"), 3.6, 0),
      icd_N39 = if_else(str_detect(!!sym(diagnosis_column), "N39"), 3.2, 0),
      icd_N39 = if_else(str_detect(!!sym(diagnosis_column), "F05"), 3.2, 0),
      icd_W19 = if_else(str_detect(!!sym(diagnosis_column), "W19"), 3.2, 0),
      icd_S00 = if_else(str_detect(!!sym(diagnosis_column), "S00"), 3.2, 0),
      icd_R31 = if_else(str_detect(!!sym(diagnosis_column), "R31"), 3.0, 0),
      icd_B96 = if_else(str_detect(!!sym(diagnosis_column), "B96"), 2.9, 0),
      icd_R41 = if_else(str_detect(!!sym(diagnosis_column), "R41"), 2.7, 0),
      icd_R26 = if_else(str_detect(!!sym(diagnosis_column), "R26"), 2.6, 0),
      icd_I67 = if_else(str_detect(!!sym(diagnosis_column), "I67"), 2.6, 0),
      icd_R56 = if_else(str_detect(!!sym(diagnosis_column), "R56"), 2.6, 0),
      icd_R40 = if_else(str_detect(!!sym(diagnosis_column), "R40"), 2.5, 0),
      icd_T83 = if_else(str_detect(!!sym(diagnosis_column), "T83"), 2.4, 0),
      icd_S06 = if_else(str_detect(!!sym(diagnosis_column), "S06"), 2.4, 0),
      icd_S42 = if_else(str_detect(!!sym(diagnosis_column), "S42"), 2.3, 0),
      icd_E87 = if_else(str_detect(!!sym(diagnosis_column), "E87"), 2.3, 0),
      icd_M25 = if_else(str_detect(!!sym(diagnosis_column), "M25"), 2.3, 0),
      icd_E86 = if_else(str_detect(!!sym(diagnosis_column), "E86"), 2.3, 0),
      icd_R54 = if_else(str_detect(!!sym(diagnosis_column), "R54"), 2.2, 0),
      icd_Z50 = if_else(str_detect(!!sym(diagnosis_column), "Z50"), 2.1, 0),
      icd_F03 = if_else(str_detect(!!sym(diagnosis_column), "F03"), 2.1, 0),
      icd_W18 = if_else(str_detect(!!sym(diagnosis_column), "W18"), 2.1, 0),
      icd_Z75 = if_else(str_detect(!!sym(diagnosis_column), "Z75"), 2.0, 0),
      icd_F01 = if_else(str_detect(!!sym(diagnosis_column), "F01"), 2.0, 0),
      icd_S80 = if_else(str_detect(!!sym(diagnosis_column), "S80"), 2.0, 0),
      icd_L03 = if_else(str_detect(!!sym(diagnosis_column), "L03"), 2.0, 0),
      icd_H54 = if_else(str_detect(!!sym(diagnosis_column), "H54"), 1.9, 0),
      icd_E53 = if_else(str_detect(!!sym(diagnosis_column), "E53"), 1.9, 0),
      icd_Z60 = if_else(str_detect(!!sym(diagnosis_column), "Z60"), 1.8, 0),
      icd_G20 = if_else(str_detect(!!sym(diagnosis_column), "G20"), 1.8, 0),
      icd_R55 = if_else(str_detect(!!sym(diagnosis_column), "R55"), 1.8, 0),
      icd_S22 = if_else(str_detect(!!sym(diagnosis_column), "S22"), 1.8, 0),
      icd_K59 = if_else(str_detect(!!sym(diagnosis_column), "K59"), 1.8, 0),
      icd_N17 = if_else(str_detect(!!sym(diagnosis_column), "N17"), 1.8, 0),
      icd_L89 = if_else(str_detect(!!sym(diagnosis_column), "L89"), 1.7, 0),
      icd_Z22 = if_else(str_detect(!!sym(diagnosis_column), "Z22"), 1.7, 0),
      icd_B95 = if_else(str_detect(!!sym(diagnosis_column), "B95"), 1.7, 0),
      icd_N19 = if_else(str_detect(!!sym(diagnosis_column), "N19"), 1.6, 0),
      icd_A41 = if_else(str_detect(!!sym(diagnosis_column), "A41"), 1.6, 0),
      icd_L97 = if_else(str_detect(!!sym(diagnosis_column), "L97"), 1.6, 0),
      icd_R44 = if_else(str_detect(!!sym(diagnosis_column), "R44"), 1.6, 0),
      icd_K26 = if_else(str_detect(!!sym(diagnosis_column), "K26"), 1.6, 0),
      icd_I95 = if_else(str_detect(!!sym(diagnosis_column), "I95"), 1.6, 0),
      icd_Z87 = if_else(str_detect(!!sym(diagnosis_column), "Z87"), 1.5, 0),
      icd_J96 = if_else(str_detect(!!sym(diagnosis_column), "J96"), 1.5, 0),
      icd_X59 = if_else(str_detect(!!sym(diagnosis_column), "X59"), 1.5, 0),
      icd_M19 = if_else(str_detect(!!sym(diagnosis_column), "M19"), 1.5, 0),
      icd_G40 = if_else(str_detect(!!sym(diagnosis_column), "G40"), 1.5, 0),
      icd_M81 = if_else(str_detect(!!sym(diagnosis_column), "M81"), 1.4, 0),
      icd_S72 = if_else(str_detect(!!sym(diagnosis_column), "S72"), 1.4, 0),
      icd_S32 = if_else(str_detect(!!sym(diagnosis_column), "S32"), 1.4, 0),
      icd_E16 = if_else(str_detect(!!sym(diagnosis_column), "E16"), 1.4, 0),
      icd_R94 = if_else(str_detect(!!sym(diagnosis_column), "R94"), 1.4, 0),
      icd_N18 = if_else(str_detect(!!sym(diagnosis_column), "N18"), 1.4, 0),
      icd_R33 = if_else(str_detect(!!sym(diagnosis_column), "R33"), 1.3, 0),
      icd_R69 = if_else(str_detect(!!sym(diagnosis_column), "R69"), 1.3, 0),
      icd_N28 = if_else(str_detect(!!sym(diagnosis_column), "N28"), 1.3, 0),
      icd_R32 = if_else(str_detect(!!sym(diagnosis_column), "R32"), 1.2, 0),
      icd_G31 = if_else(str_detect(!!sym(diagnosis_column), "G31"), 1.2, 0),
      icd_Y95 = if_else(str_detect(!!sym(diagnosis_column), "Y95"), 1.2, 0),
      icd_S09 = if_else(str_detect(!!sym(diagnosis_column), "S09"), 1.2, 0),
      icd_R45 = if_else(str_detect(!!sym(diagnosis_column), "R45"), 1.2, 0),
      icd_G45 = if_else(str_detect(!!sym(diagnosis_column), "G45"), 1.2, 0),
      icd_S01 = if_else(str_detect(!!sym(diagnosis_column), "S01"), 1.1, 0),
      icd_A04 = if_else(str_detect(!!sym(diagnosis_column), "A04"), 1.1, 0),
      icd_A09 = if_else(str_detect(!!sym(diagnosis_column), "A09"), 1.1, 0),
      icd_J18 = if_else(str_detect(!!sym(diagnosis_column), "J18"), 1.1, 0),
      icd_Z74 = if_else(str_detect(!!sym(diagnosis_column), "Z74"), 1.1, 0),
      icd_M79 = if_else(str_detect(!!sym(diagnosis_column), "M79"), 1.1, 0),
      icd_W06 = if_else(str_detect(!!sym(diagnosis_column), "W06"), 1.1, 0),
      icd_J69 = if_else(str_detect(!!sym(diagnosis_column), "J69"), 1.0, 0),
      icd_R47 = if_else(str_detect(!!sym(diagnosis_column), "R47"), 1.0, 0),
      icd_E55 = if_else(str_detect(!!sym(diagnosis_column), "E55"), 1.0, 0),
      icd_Z93 = if_else(str_detect(!!sym(diagnosis_column), "Z93"), 1.0, 0),
      icd_R02 = if_else(str_detect(!!sym(diagnosis_column), "R02"), 1.0, 0),
      icd_R63 = if_else(str_detect(!!sym(diagnosis_column), "R63"), 1.0, 0),
      icd_H91 = if_else(str_detect(!!sym(diagnosis_column), "H91"), 0.9, 0),
      icd_W10 = if_else(str_detect(!!sym(diagnosis_column), "W10"), 0.9, 0),
      icd_W01 = if_else(str_detect(!!sym(diagnosis_column), "W01"), 0.9, 0),
      icd_E05 = if_else(str_detect(!!sym(diagnosis_column), "E05"), 0.9, 0),
      icd_M41 = if_else(str_detect(!!sym(diagnosis_column), "M41"), 0.9, 0),
      icd_R13 = if_else(str_detect(!!sym(diagnosis_column), "R13"), 0.8, 0),
      icd_Z99 = if_else(str_detect(!!sym(diagnosis_column), "Z99"), 0.8, 0),
      icd_U80 = if_else(str_detect(!!sym(diagnosis_column), "U80"), 0.8, 0),
      icd_M80 = if_else(str_detect(!!sym(diagnosis_column), "M80"), 0.8, 0),
      icd_K92 = if_else(str_detect(!!sym(diagnosis_column), "K92"), 0.8, 0),
      icd_I63 = if_else(str_detect(!!sym(diagnosis_column), "I63"), 0.8, 0),
      icd_J22 = if_else(str_detect(!!sym(diagnosis_column), "J22"), 0.7, 0),
      icd_N20 = if_else(str_detect(!!sym(diagnosis_column), "N20"), 0.7, 0),
      icd_F10 = if_else(str_detect(!!sym(diagnosis_column), "F10"), 0.7, 0),
      icd_Y84 = if_else(str_detect(!!sym(diagnosis_column), "Y84"), 0.7, 0),
      icd_R00 = if_else(str_detect(!!sym(diagnosis_column), "R00"), 0.7, 0),
      icd_Z73 = if_else(str_detect(!!sym(diagnosis_column), "Z73"), 0.6, 0),
      icd_R79 = if_else(str_detect(!!sym(diagnosis_column), "R79"), 0.6, 0),
      icd_Z91 = if_else(str_detect(!!sym(diagnosis_column), "Z91"), 0.5, 0),
      icd_S51 = if_else(str_detect(!!sym(diagnosis_column), "S51"), 0.5, 0),
      icd_F32 = if_else(str_detect(!!sym(diagnosis_column), "F32"), 0.5, 0),
      icd_M48 = if_else(str_detect(!!sym(diagnosis_column), "M48"), 0.5, 0),
      icd_E83 = if_else(str_detect(!!sym(diagnosis_column), "E83"), 0.4, 0),
      icd_M15 = if_else(str_detect(!!sym(diagnosis_column), "M15"), 0.4, 0),
      icd_D64 = if_else(str_detect(!!sym(diagnosis_column), "D64"), 0.4, 0),
      icd_L08 = if_else(str_detect(!!sym(diagnosis_column), "L08"), 0.4, 0),
      icd_R11 = if_else(str_detect(!!sym(diagnosis_column), "R11"), 0.3, 0),
      icd_K52 = if_else(str_detect(!!sym(diagnosis_column), "K52"), 0.3, 0),
      icd_R50 = if_else(str_detect(!!sym(diagnosis_column), "R50"), 0.1, 0),
      frailty_score = rowSums(across(starts_with("icd")), na.rm = TRUE),
      frailty_group = case_when(frailty_score >= 15 ~ "high",
        frailty_score >= 5 ~ "medium",
        frailty_score >= 1 ~ "low",
        .default = "null"
      )
    )
  data
}

# demo on test data

data <- add_frailty_metrics(data, 'diagnosis')
 
    