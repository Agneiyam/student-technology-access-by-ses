# Student Technology Access by SES

A look at Georgia's "digital divide" — how student access to computers in the classroom
varies by socioeconomic status (SES), school locale, and school level, and whether what
Georgia principals report about tech conditions in their schools lines up with a national
NCES benchmark.

## Data sources

- **NCES / GA DOE Free & Reduced Lunch (FRL) fiscal year reports** — school-level FRL
  percentage, used to bucket each school into a Low / Middle / High SES category per year.
- **GA DOE Technology Inventory ("GA Tech Survey")** — annual per-school counts of
  desktops, laptops, tablets/netbooks, and computers meeting instructional specs
  (FY2015–FY2023).
- **NCES Public School Characteristics file** — school locale (city/suburb/town/rural)
  and school level (elementary/middle/high), joined in to add geographic and grade-band
  context.
- **A national NCES principal survey + a parallel Georgia principal survey** (plus an
  email/school-name crosswalk and FRL data for the principal-survey schools) — used to
  compare GA-reported technology conditions against the national sample.

Raw data files aren't committed to this repo (school records / survey responses); the
scripts expect them in local subfolders matching the names referenced in the code
(`Free Reduced Lunch Fiscal/`, `GA Tech Survey Data/`, `Public School Characteristics (1)/`,
etc.).

## Scripts

### `Updated Cleaning Code.R`
Builds the core school-year dataset:
1. Parses each year's FRL report and derives an `SES_Level` (Low/Middle/High) from the
   percent-FRL cutoffs.
2. Cleans eight years of technology inventory spreadsheets — each with slightly different
   column names — into a consistent set of device counts (desktops, laptops,
   tablets/netbooks, computers meeting specs) per school per year.
3. Joins device counts to SES level (by school code + year) and to school
   characteristics (by school ID), then filters to Elementary/Middle/High schools.
4. Computes students-per-device ratios and plots them over time, broken out by SES level,
   locale, and school level.
5. Exports the cleaned combined dataset and summary tables (mean/SD/n by group) to Excel.

### `Principal compared to USDOE data.R`
Tests whether Georgia principals' reported technology conditions differ from the national
NCES survey:
1. Matches the GA principal survey to school names/SES level (via an email crosswalk and
   FRL data) and derives a school level (elementary/middle/high) from the school name.
2. Builds a question-by-question crosswalk between the GA survey and the equivalent
   national NCES survey questions, and recodes GA responses onto the national survey's
   response scale.
3. Combines both sources under common question IDs, then fits one regression per question
   — binary logistic for yes/no questions, ordinal logistic for multi-category responses —
   controlling for SES level and school level, to test whether the "source" (GA vs.
   national) predicts the response.
4. Outputs a results table (estimate, odds ratio, p-value, n) per question, sorted by
   significance.

## Requirements

R with `tidyverse`, `readxl`, `janitor`, `MASS`, `broom`, `writexl`
