
install.packages("janitor")
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(MASS)     # polr()
library(broom)

# Read in the files

usdoe_raw   <- read_excel("USDOE.xlsx") |> clean_names()
email_raw   <- read_excel("Email Distro.xlsx") |> clean_names()
frl_raw     <- read_csv("Free and Reduced for princip data.csv", show_col_types = FALSE) |> clean_names()
princip_raw <- read_csv("Principal Data Sept 2025.csv", show_col_types = FALSE)



# Email Distro cleaning to ensure marching

email_distro <- email_raw |>
  transmute(
    instn_name = str_squish(as.character(instn_name)),
    verified_email = str_to_lower(str_squish(as.character(verified_email)))
  ) |>
  filter(!is.na(verified_email), verified_email != "")

# View duplicated Emails

email_dupes <- email_distro |>
  add_count(verified_email) |>
  filter(n > 1) |>
  arrange(verified_email, instn_name)

# View single emails

email_single <- email_distro |>
  distinct(verified_email, .keep_all = TRUE)

# Clean FRL 2024 Reduced File

frl <- frl_raw |>
  transmute(
    instn_name = str_squish(as.character(instn_name)),
    ses_level_label = str_squish(as.character(ses_level)),
    frl_pct = suppressWarnings(as.numeric(kk_12_percent_frl_recode))
  ) |>
  filter(!is.na(instn_name), instn_name != "") |>
  mutate(
    povst4 = case_when(
      is.na(frl_pct)                ~ 1,
      frl_pct < 35                  ~ 1,
      frl_pct >= 35 & frl_pct < 50  ~ 2,
      frl_pct >= 50 & frl_pct < 75  ~ 3,
      frl_pct >= 75                 ~ 4
    )
  ) |>
  distinct(instn_name, .keep_all = TRUE)


# Clean Principal Data 

princip <- princip_raw |>
  slice(-(1:2)) |> # First 2 rows contain unusable metadata
  filter(Finished == "True" | Finished == TRUE | Progress == "100") |>
  mutate(
    recipientemail = str_to_lower(str_squish(as.character(RecipientEmail)))
  )

# Join school name and SES level to principal

princip_cov <- princip |>
  left_join(email_single, by = c("recipientemail" = "verified_email")) |>
  left_join(frl, by = "instn_name")

# Rows that do not have school name or SES 

principal_missing_school <- princip_cov |> filter(is.na(instn_name))
principal_missing_ses    <- princip_cov |> filter(is.na(povst4))

# Derive school level/ level 3 for pricnipal data 

# Elementary school = 1
# Middle school = 2
# High and other school = 3

derive_level3 <- function(x) {
  x <- str_to_lower(str_squish(x))
  
  case_when(
    str_detect(x, "elementary|primary|intermediate|4th & 5th|4th and 5th|prek|pre-k|pre kindergarten|prekindergarten") ~ 1,
    str_detect(x, "middle|junior high") ~ 2,
    str_detect(x, "high|academy|charter|k12|k-12|school|institute|education center") ~ 3,
    TRUE ~ NA_real_
  )
}

princip_cov <- princip_cov |>
  mutate(
    level3 = derive_level3(instn_name)
  )

# Optional manual review list for ambiguous or missing level3
principal_level3_review <- princip_cov |>
  filter(is.na(level3)) |>
  dplyr::select(RecipientEmail, instn_name) |>
  distinct()



# Crosswalk between principal and national data

crosswalk <- tribble(
  ~principal_var, ~national_var, ~question_id,
  "Q6", "q1", "q1",
  "Q7", "q2", "q2",
  "Q8", "q3", "q3",
  "Q9_1", "p_q4a", "q4a",
  "Q9_2", "p_q4b", "q4b",
  "Q9_3", "p_q4c", "q4c",
  "Q9_4", "p_q4d", "q4d",
  "Q9_5", "p_q4e", "q4e",
  "Q10", "q5", "q5",
  "Q11", "q6", "q6",
  "Q12", "q7", "q7",
  "Q13", "q8", "q8",
  "Q14", "q9", "q9",
  "Q15", "q10", "q10",
  "Q16", "q11", "q11",
  "Q17", "q12", "q12",
  "Q18", "q13", "q13",
  "Q19", "q14", "q14",
  "Q17_1", "q15a", "q15a",
  "Q17_2", "q15b", "q15b",
  "Q17_3", "q15c", "q15c",
  "Q17_4", "q15d", "q15d",
  "Q17_5", "q15e", "q15e",
  "Q17_6", "q15f", "q15f",
  "Q17_7", "q15g", "q15g",
  "Q18_1", "q16a", "q16a",
  "Q18_2", "q16b", "q16b",
  "Q18_3", "q16c", "q16c",
  "Q18_4", "q16d", "q16d",
  "Q19_1", "q17a", "q17a",
  "Q19_2", "q17b", "q17b",
  "Q19_3", "q17c", "q17c",
  "Q19_4", "q17d", "q17d",
  "Q20_1", "q18a", "q18a",
  "Q20_3", "q18b", "q18b",
  "Q20_4", "q18c", "q18c",
  "Q20_5", "q18d", "q18d",
  "Q20_6", "q18e", "q18e",
  "Q21_1", "q19a", "q19a",
  "Q21_3", "q19b", "q19b",
  "Q21_4", "q19c", "q19c",
  "Q21_5", "q19d", "q19d",
  "Q21_6", "q19e", "q19e",
  "Q25_1", "q20a", "q20a",
  "Q25_3", "q20b", "q20b",
  "Q25_6", "q20c", "q20c",
  "Q25_7", "q20d", "q20d",
  "Q25_9", "q20e", "q20e",
  "Q25_10", "q20f", "q20f",
  "Q25_11", "q20g", "q20g",
  "Q25_12", "q20h", "q20h",
  "Q25_13", "q20i", "q20i",
  "Q25_14", "q20j", "q20j",
  "Q25_18", "q20k", "q20k"
)

national_keep <- c("povst4", "level3", crosswalk$national_var)

usdoe <- usdoe_raw |>
  clean_names() |>
  dplyr::select(any_of(national_keep)) |>
  mutate(source = "National")


# Recode principal to match national data

# Helper recodes
recode_yes_no <- c(
  "Yes" = 1,
  "No"  = 2
)

recode_yes_no_na_q13 <- c(
  "Yes, students can borrow computers on a short term basis" = 1,
  "No, students cannot borrow computers on a short term basis" = 2,
  "Not applicable, all students take a district- or school-provided computer home with them" = -8,
  
  # Principal wording version:
  "Yes" = 1,
  "No" = 2
)

recode_q3 <- c(
  "Yes, in all grade levels." = 1,
  "Yes, but only in some grade levels." = 2,
  "No" = 3
)

recode_quality <- c(
  "Poor" = 1,
  "Fair" = 2,
  "Good" = 3,
  "Very Good" = 4,
  "Very good" = 4
)

recode_extent <- c(
  "Not at All" = 1,
  "Small Extent" = 2,
  "Moderate Extent" = 3,
  "Large Extent" = 4
)

recode_easy <- c(
  "Always Difficult" = 1,
  "Usually Difficult" = 2,
  "Usually Easy" = 3,
  "Always Easy" = 4
)

recode_reliable <- c(
  "Not Reliable" = 1,
  "Slightly Reliable" = 2,
  "Somewhat Reliable" = 3,
  "Very Reliable" = 4
)

recode_flex <- c(
  "None" = 1,
  "Minimal" = 2,
  "Moderate" = 3,
  "A Lot" = 4,
  "A lot" = 4
)

recode_agree <- c(
  "Strongly Agree" = 1,
  "Somewhat Agree" = 2,
  "Somewhat Disagree" = 3,
  "Strongly Disagree" = 4
)

recode_challenge <- c(
  "Not a Challenge" = 1,
  "Small Challenge" = 2,
  "Moderate Challenge" = 3,
  "Large Challenge" = 4
)

# Bin principal counts to match the corresponding bin national counts

bin_q4a <- function(x) case_when(
  is.na(x)          ~ NA_real_,
  x == 0            ~ 0,
  x >= 1   & x <= 199 ~ 1,
  x >= 200 & x <= 499 ~ 2,
  x >= 500          ~ 3
)

bin_q4b <- function(x) case_when(
  is.na(x)           ~ NA_real_,
  x == 0             ~ 0,
  x >= 1   & x <= 49  ~ 1,
  x >= 50  & x <= 199 ~ 2,
  x >= 200 & x <= 499 ~ 3,
  x >= 500           ~ 4
)

bin_q4c <- function(x) case_when(
  is.na(x)           ~ NA_real_,
  x == 0             ~ 0,
  x >= 1   & x <= 49  ~ 1,
  x >= 50  & x <= 99  ~ 2,
  x >= 100 & x <= 299 ~ 3,
  x >= 300           ~ 4
)

bin_q4d <- function(x) case_when(
  is.na(x)           ~ NA_real_,
  x == 0             ~ 0,
  x >= 1   & x <= 24  ~ 1,
  x >= 25  & x <= 49  ~ 2,
  x >= 50  & x <= 99  ~ 3,
  x >= 100           ~ 4
)

bin_q4e <- function(x) case_when(
  is.na(x)          ~ NA_real_,
  x == 0            ~ 0,
  x >= 1  & x <= 24 ~ 1,
  x >= 25           ~ 2
)

princip_num <- princip_cov |>
  mutate(
    source = "Principal",
    
    Q6     = unname(recode_yes_no[Q6]),
    Q7     = unname(recode_yes_no[Q7]),
    Q8     = unname(recode_q3[Q8]),
    
    Q9_1   = bin_q4a(suppressWarnings(as.numeric(Q9_1))),
    Q9_2   = bin_q4b(suppressWarnings(as.numeric(Q9_2))),
    Q9_3   = bin_q4c(suppressWarnings(as.numeric(Q9_3))),
    Q9_4   = bin_q4d(suppressWarnings(as.numeric(Q9_4))),
    Q9_5   = bin_q4e(suppressWarnings(as.numeric(Q9_5))),
    
    Q10    = unname(recode_quality[Q10]),
    Q11    = unname(recode_quality[Q11]),
    Q12    = unname(recode_extent[Q12]),
    Q13    = unname(recode_easy[Q13]),
    Q14    = unname(recode_reliable[Q14]),
    Q15    = unname(recode_extent[Q15]),
    Q16    = unname(recode_flex[Q16]),
    Q17    = unname(recode_flex[Q17]),
    Q18    = case_when(
      Q18 == "Yes" ~ 1,
      Q18 == "No"  ~ 2,
      TRUE         ~ NA_real_
    ),
    Q19    = unname(recode_yes_no[Q19]),
    
    across(c(Q17_1, Q17_2, Q17_3, Q17_4, Q17_5, Q17_6, Q17_7),
           ~ unname(recode_extent[.x])),
    
    across(c(Q18_1, Q18_2, Q18_3, Q18_4),
           ~ unname(recode_extent[.x])),
    
    across(c(Q19_1, Q19_2, Q19_3, Q19_4),
           ~ unname(recode_yes_no[.x])),
    
    across(c(Q20_1, Q20_3, Q20_4, Q20_5, Q20_6),
           ~ unname(recode_agree[.x])),
    
    across(c(Q21_1, Q21_3, Q21_4, Q21_5, Q21_6),
           ~ unname(recode_agree[.x])),
    
    across(c(Q25_1, Q25_3, Q25_6, Q25_7, Q25_9, Q25_10, Q25_11,
             Q25_12, Q25_13, Q25_14, Q25_18),
           ~ unname(recode_challenge[.x]))
  )


# Rename both sets to a common set

# National -> common names
national_common <- usdoe |>
  rename(!!!setNames(crosswalk$national_var, crosswalk$question_id)) |>
  dplyr::select(source, povst4, level3, all_of(crosswalk$question_id))

# Principal -> common names
principal_common <- princip_num |>
  rename(!!!setNames(crosswalk$principal_var, crosswalk$question_id)) |>
  dplyr::select(source, povst4, level3, all_of(crosswalk$question_id))



# Combine the 2 datasets with questions that match in the crosswalk

combined <- bind_rows(principal_common, national_common) |>
  mutate(
    source = factor(source, levels = c("National", "Principal")),
    povst4 = factor(povst4, levels = c(1,2,3,4), ordered = TRUE),
    level3 = factor(level3, levels = c(1,2,3), ordered = FALSE)
  )


# Sanity checks to see how many were matched

# How many principal rows got SES and level?
combined |>
  filter(source == "Principal") |>
  summarise(
    n = n(),
    missing_povst4 = sum(is.na(povst4)),
    missing_level3 = sum(is.na(level3))
  )

# Distribution check
combined |>
  count(source, povst4, level3)

# Build a function that will model through each question using an appropriate glm

# Control for SES Level and school level

question_vars <- crosswalk$question_id

# Create function

fit_one_question <- function(dat, q) {
  d <- dat |>
    dplyr::select(source, povst4, level3, response = all_of(q)) |> # Keep only hese columns
    dplyr::filter(!is.na(response), !is.na(source), !is.na(povst4), !is.na(level3)) # Remove rows where missing
  
  ncat <- dplyr::n_distinct(d$response) # Count how many unique response catergories
  
  # if fewer than 20 usable rows & 2 unique response categories, cannot fit model
  if (nrow(d) < 20 || ncat < 2) {
    return(tibble(
      question = q,
      model_type = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      odds_ratio = NA_real_,
      n = nrow(d)
    ))
  }
  # if response has 2 categories = binary model
  
  if (ncat == 2) {
    vals <- sort(unique(d$response))
    
    d <- d |>
      dplyr::mutate(response_bin = if_else(response == max(vals), 1, 0))
    
    fit <- glm(response_bin ~ source + povst4 + level3,
               data = d, family = binomial())
    
    out <- broom::tidy(fit) |>
      dplyr::filter(term == "sourcePrincipal") |>
      dplyr::transmute(
        question = q,
        model_type = "binary_logit",
        estimate = estimate,
        std_error = std.error,
        statistic = statistic,
        p_value = p.value,
        odds_ratio = exp(estimate),
        n = nrow(d)
      )
    # If resposnse > 2 categories = ordinal logistic regression
  } else {
    d <- d |>
      dplyr::mutate(response_ord = ordered(response, levels = sort(unique(response))))
    
    fit <- MASS::polr(response_ord ~ source + povst4 + level3,
                      data = d, Hess = TRUE)
    
    coefs <- coef(summary(fit))
    pvals <- 2 * pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)
    
    out <- tibble(
      term = rownames(coefs),
      estimate = coefs[, "Value"],
      std_error = coefs[, "Std. Error"],
      statistic = coefs[, "t value"],
      p_value = pvals
    ) |>
      dplyr::filter(term == "sourcePrincipal") |>
      dplyr::transmute(
        question = q,
        model_type = "ordinal_logit",
        estimate = estimate,
        std_error = std_error,
        statistic = statistic,
        p_value = p_value,
        odds_ratio = exp(estimate),
        n = nrow(d)
      )
  }
  
  out
}

model_results <- map_dfr(question_vars, ~ fit_one_question(combined, .x)) |>
  arrange(p_value)

model_results
