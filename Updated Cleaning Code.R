## NCES File Clean Up ##

library(tidyverse)
library(readxl)

##### Free and Reduced Lunch Files #####

data_dir <- "Free Reduced Lunch Fiscal"

lunch_files <- list.files(
  path = data_dir,
  pattern = "^Free Reduced Lunch \\(FRL\\) Fiscal Year[0-9]{4} Data Report.*\\.csv$",
  full.names = TRUE
)

low_cutoff <- 70
high_cutoff <- 30

df_lunch_files <- map_dfr(lunch_files, function(file_path) {
  read_csv(file_path, skip = 4, show_col_types = FALSE) |>
    mutate(
      FiscalYear = str_extract(basename(file_path), "[0-9]{4}"),
      FRL_Char = `KK-12 % FRL`,
      FRL_Numeric = suppressWarnings(as.numeric(FRL_Char)),
      SES_Level = case_when(
        FRL_Char == "*" ~ "Low SES",
        FRL_Char == "#" ~ "High SES",
        !is.na(FRL_Numeric) & FRL_Numeric < high_cutoff ~ "High SES",
        !is.na(FRL_Numeric) & FRL_Numeric >= high_cutoff & FRL_Numeric < low_cutoff ~ "Middle SES",
        !is.na(FRL_Numeric) & FRL_Numeric >= low_cutoff ~ "Low SES",
        TRUE ~ NA_character_
      )
    )
})

df_lunch_files |>
  count(SES_Level, sort = TRUE) |>
  print(n = Inf)


df_lunch_files2 <- df_lunch_files |>
  mutate(FiscalYear = as.character(FiscalYear))

df_lunch_files2 <- df_lunch_files2 |>
  mutate(
    FiscalYear = paste0(as.integer(FiscalYear) - 1, "-", FiscalYear)
  )

## Read in GA Tech Survey Data. Read in the sheet
## which contains the phrase "School Survey" ##

tech_files <- list.files(path = "GA Tech Survey Data", 
                         pattern = "*.xls*", full.names = TRUE)[-c(1:2)]

tech_names <- substr(tech_files,
                     start=str_locate(tech_files,"Inventory")[1,1] +
                       nchar("Inventory - "),
                     stop=str_locate(tech_files,"Inventory")[1,1] +
                       nchar("Inventory - ") + 6)

ga_tech_list <- lapply(tech_files, function(x) {
  if(x == "FY 2014"){
    read_excel(x,sheet=2)
  } else{
  read_excel(x, sheet = 3)
  }
  })

names(ga_tech_list) <- tech_names

## Hard Code School Year & Create Copy of List ##

ga_tech_list2 <- ga_tech_list

## Remove FY 2013 & FY 2014 ##

ga_tech_list2 <- ga_tech_list2[!(names(ga_tech_list2) %in% c("FY 2013", "FY 2014"))]

## 2015 ##

ga_tech_list2[["FY 2015"]] <- ga_tech_list[["FY 2015"]] |>
  mutate(SCHOOL_YEAR = "2014-2015") |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, ends_with("Grand Total")) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  select(-`Host PC Grand Total`, -`Clients Served by Host PC Grand Total`,
         -`Clients Served by Host Server Grand Total`, -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `LaptopsLess Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Netbooks Less Than 5 Grand Total`,
    TN = `Tablets Less Than 5 Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

View(ga_tech_list2[["FY 2015"]])

## 2015-2016 ##

ga_tech_list2[["FY 2016"]] <- ga_tech_list[["FY 2016"]] |>
  mutate(SCHOOL_YEAR = "2015-2016") |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  select(-`Host PC Grand Total`, -`Clients Served By Host PC Grand Total`,
         -`Clients Served By Host Server Grand Total`, -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `Laptops Less Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Netbooks Less Than 5 Grand Total` + `Netbooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

View(ga_tech_list2[["FY 2016"]])

## 2016-2017 ##

ga_tech_list2[["FY 2017"]] <- ga_tech_list[["FY 2017"]] |>
  mutate(SCHOOL_YEAR = "2016-2017") |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  mutate(`LaptopsLess Than 5 Grand Total` = as.numeric(`LaptopsLess Than 5 Grand Total`)) |>
  select(-`Host PC Grand Total`, -`Clients Served by Host PC Grand Total`,
         -`Clients Served by Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `LaptopsLess Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Netbooks Less Than 5 Grand Total` + `Netbooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

View(ga_tech_list2[["FY 2017"]])

## 2017-2018 ##

ga_tech_list2[["FY 2018"]] <- ga_tech_list[["FY 2018"]] |>
  mutate(SCHOOL_YEAR = "2017-2018") |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  mutate(`LaptopsLess Than 5 Grand Total` = as.numeric(`LaptopsLess Than 5 Grand Total`)) |>
  select(-`Host PC Grand Total`, -`Clients Served by Host PC Grand Total`,
         -`Clients Served by Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `LaptopsLess Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Netbooks Less Than 5 Grand Total` + `Netbooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)
  
print(colnames(ga_tech_list2[["FY 2018"]]))

## 2018-2019 ##

ga_tech_list2[["FY 2019"]] <- ga_tech_list[["FY 2019"]] |>
  mutate(SCHOOL_YEAR = "2018-2019") |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  select(-`Host PC Grand Total`, -`Clients Served By Host PC Grand Total`,
         -`Clients Served By Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `Laptops Less Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Netbooks Less Than 5 Grand Total` + `Netbooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

print(colnames(ga_tech_list2[["FY 2019"]]))

## 2020-2021 ##

ga_tech_list2[["FY 2021"]] <- ga_tech_list[["FY 2021"]] |>
  mutate(SCHOOL_YEAR = "2020-2021") |>
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  rename(`School Code` = OrgCode) |>
  select(-`Host PC Grand Total`, -`Clients Served By Host PC Grand Total`,
         -`Clients Served By Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `Laptops Less Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Chromebooks Less Than 5 Grand Total` + `Chromebooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

print(colnames(ga_tech_list2[["FY 2021"]]))

## 2021-2022 ##

ga_tech_list2[["FY 2022"]] <- ga_tech_list[["FY 2022"]] |>
  mutate(SCHOOL_YEAR = "2021-2022") |>
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  rename(`School Code` = OrgCode) |>
  select(-`Host PC Grand Total`, -`Clients Served By Host PC Grand Total`,
         -`Clients Served By Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `Laptops Less Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Chromebooks Less Than 5 Grand Total` + `Chromebooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

View(ga_tech_list2[["FY 2022"]])

## 2022 - 2023 ##

ga_tech_list2[["FY 2023"]] <- ga_tech_list[["FY 2023"]] |>
  mutate(SCHOOL_YEAR = "2022-2023") |>
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`, ends_with("Grand Total")) |> 
  select(SCHOOL_YEAR, OrgCode, District,
         School, `Student Population`,
         `Desktops Less Than 5 Grand Total`:`Student Instructional Computers Meeting Specs Grand Total`) |>
  rename(`School Code` = OrgCode) |>
  select(-`Host PC Grand Total`, -`Clients Served By Host PC Grand Total`,
         -`Clients Served By Host Server Grand Total`,
         -`Student Instructional Computers Grand Total`) |>
  mutate(
    DT = `Desktops 5 Plus Grand Total` + `Desktops Less Than 5 Grand Total`,
    LT = `Laptops Less Than 5 Grand Total` + `Laptops 5 Plus Grand Total` +
      `Chromebooks Less Than 5 Grand Total` + `Chromebooks 5 Plus Grand Total`,
    TN = `Tablets Less Than 5 Grand Total` + `Tablets 5 Plus Grand Total`,
    CMS = `Student Instructional Computers Meeting Specs Grand Total`
  ) |>
  select(SCHOOL_YEAR, `School Code`, District,
         School, `Student Population`, DT, LT, TN, CMS)

print(colnames(ga_tech_list2[["FY 2023"]]))

## Join ga_tech_list2 into one dataframe ##

ga_tech <- bind_rows(ga_tech_list2)[,1:9]

## left_join df_ga to ga_tech ##

# 1) Make sure the primary keys are standardized

ga_tech_clean <- ga_tech|>
  mutate(
    SCHOOL_YEAR = as.character(SCHOOL_YEAR),
    `School Code` = trimws(`School Code`)
  )

df_lunch_clean <- df_lunch_files2 |>
  mutate(
    FiscalYear = as.character(FiscalYear),
    ST_SCHID   = trimws(ST_SCHID)
  )

# 2) Join on School + Year
df_ga_2 <- ga_tech_clean |>
  inner_join(
    df_lunch_clean,
    by = c(
      "School Code" = "ST_SCHID",
      "SCHOOL_YEAR" = "FiscalYear"
    )
  )



## Now, let's clean up the Public School Characteristics file to join 
## everything together ##

char_fp <- list.files(path = "Public School Characteristics (1)", 
                      pattern = "*.csv", full.names = TRUE)

characteristics <- read_csv(char_fp) |>
  filter(STABR == "GA") |>
  select(ST_LEAID,LEA_NAME,SCH_NAME,SCHOOL_LEVEL,ULOCALE)

## Left_join ##

df_ga3 <- df_ga_2|>
  mutate(ID = as.numeric(str_sub(`School Code`, 1, 3))) |>
  left_join(characteristics |>
              mutate(ID = as.numeric(str_sub(ST_LEAID,4,nchar(ST_LEAID)))),
            by = c("ID" = "ID", "School" = "SCH_NAME")
  ) # Lots of NAs in school name

## Create Locale Variable. In ULOCALE column, if the first digit is 1, then
## it is a city school, if it is 2, then it is a suburb, if it is 3, then it is
## a town, and if it is 4, then it is a rural school ##

df_ga3 <- df_ga3 |>
  mutate(LOCALE = case_when(
    str_starts(ULOCALE, "1") ~ "City",
    str_starts(ULOCALE, "2") ~ "Suburb",
    str_starts(ULOCALE, "3") ~ "Town",
    str_starts(ULOCALE, "4") ~ "Rural",
    TRUE ~ NA_character_
  ))

## Remove schools whose level is Other, Secondary, or Prekindergarten ##

df_ga3 <- df_ga3 |>
  filter(SCHOOL_LEVEL %in% c("Elementary","Middle","High"))

## How many unique schools? ##

df_ga3 |>
  group_by(ID,School) |>
  count() |>
  nrow()

## If we remove NA...##

df_ga3 |>
  na.omit() |>
  group_by(ID,School) |>
  count() |>
  nrow()

## Exploratory Analyses ##

## Let's look at Ratios overall across time ##

## Students to Desktops ##

df_ga3 |>
  mutate(Stud_to_DT = if_else(DT == 0,0,`Student Population` / DT)) |>
  group_by(SCHOOL_YEAR) |>
  summarize(M = mean(Stud_to_DT,na.rm=T),
            SD = sd(Stud_to_DT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Desktop Ratio",
      title = "Students Per Desktop Mean Ratio") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## By SES_Level ##

df_ga3 |>
  mutate(Stud_to_DT = if_else(DT == 0,0,`Student Population` / DT)) |>
  group_by(SCHOOL_YEAR,SES_Level) |>
  summarize(M = mean(Stud_to_DT,na.rm=T),
            SD = sd(Stud_to_DT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SES_Level)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Desktop Ratio",
      title = "Students Per Desktop Mean Ratio",
      subtitle = "by SES Level",
      color = "SES Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By Locale ##

df_ga3 |>
  mutate(Stud_to_DT = if_else(DT == 0,0,`Student Population` / DT)) |>
  group_by(SCHOOL_YEAR,LOCALE) |>
  summarize(M = mean(Stud_to_DT,na.rm=T),
            SD = sd(Stud_to_DT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = LOCALE)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Desktop Ratio",
      title = "Students Per Desktop Mean Ratio",
      subtitle = "by Locale",
      color = "Locale") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By School Level ##

df_ga3 |>
  mutate(Stud_to_DT = if_else(DT == 0,0,`Student Population` / DT)) |>
  group_by(SCHOOL_YEAR,SCHOOL_LEVEL) |>
  summarize(M = mean(Stud_to_DT,na.rm=T),
            SD = sd(Stud_to_DT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SCHOOL_LEVEL)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Desktop Ratio",
      title = "Students Per Desktop Mean Ratio",
      subtitle = "by School Level",
      color = "School Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

### Now Same thing with Laptops ###

## Overall ##

df_ga3 |>
  mutate(Stud_to_LT = if_else(LT == 0,0,`Student Population` / LT)) |>
  group_by(SCHOOL_YEAR) |>
  summarize(M = mean(Stud_to_LT,na.rm=T),
            SD = sd(Stud_to_LT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Laptop Ratio",
      title = "Students Per Laptop Mean Ratio") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## By SES_Level ##

df_ga3 |>
  mutate(Stud_to_LT = if_else(LT == 0,0,`Student Population` / LT)) |>
  group_by(SCHOOL_YEAR,SES_Level) |>
  summarize(M = mean(Stud_to_LT,na.rm=T),
            SD = sd(Stud_to_LT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SES_Level)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Laptop Ratio",
      title = "Students Per Laptop Mean Ratio",
      subtitle = "by SES Level",
      color = "SES Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By Locale ##

df_ga3 |>
  mutate(Stud_to_LT = if_else(LT == 0,0,`Student Population` / LT)) |>
  group_by(SCHOOL_YEAR,LOCALE) |>
  summarize(M = mean(Stud_to_LT,na.rm=T),
            SD = sd(Stud_to_LT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = LOCALE)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Laptop Ratio",
      title = "Students Per Laptop Mean Ratio",
      subtitle = "by Locale",
      color = "Locale") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By School Level ##

df_ga3 |>
  mutate(Stud_to_LT = if_else(LT == 0,0,`Student Population` / LT)) |>
  group_by(SCHOOL_YEAR,SCHOOL_LEVEL) |>
  summarize(M = mean(Stud_to_LT,na.rm=T),
            SD = sd(Stud_to_LT,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SCHOOL_LEVEL)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Laptop Ratio",
      title = "Students Per Laptop Mean Ratio",
      subtitle = "by School Level",
      color = "School Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

### Tablets/Netbooks ###

## Overall ##

df_ga3 |>
  mutate(Stud_to_TN = if_else(TN == 0,0,`Student Population` / TN)) |>
  group_by(SCHOOL_YEAR) |>
  summarize(M = mean(Stud_to_TN,na.rm=T),
            SD = sd(Stud_to_TN,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Tablet/Netbook Ratio",
      title = "Students Per Tablet/Netbook Mean Ratio") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## By SES_Level ##

df_ga3 |>
  mutate(Stud_to_TN = if_else(TN == 0,0,`Student Population` / TN)) |>
  group_by(SCHOOL_YEAR,SES_Level) |>
  summarize(M = mean(Stud_to_TN,na.rm=T),
            SD = sd(Stud_to_TN,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SES_Level)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Tablet/Netbook Ratio",
      title = "Students Per Tablet/Netbook Mean Ratio",
      subtitle = "by SES Level",
      color = "SES Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By Locale ##

df_ga3 |>
  mutate(Stud_to_TN = if_else(TN == 0,0,`Student Population` / TN)) |>
  group_by(SCHOOL_YEAR,LOCALE) |>
  summarize(M = mean(Stud_to_TN,na.rm=T),
            SD = sd(Stud_to_TN,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = LOCALE)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Tablet/Netbook Ratio",
      title = "Students Per Tablet/Netbook Mean Ratio",
      subtitle = "by Locale",
      color = "Locale") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## By School Level ##

df_ga3 |>
  mutate(Stud_to_TN = if_else(TN == 0,0,`Student Population` / TN)) |>
  group_by(SCHOOL_YEAR,SCHOOL_LEVEL) |>
  summarize(M = mean(Stud_to_TN,na.rm=T),
            SD = sd(Stud_to_TN,na.rm=T),
            n = n()) |>
  mutate(SE = SD/sqrt(n)) |>
  ggplot(aes(x = SCHOOL_YEAR, y = M, color = SCHOOL_LEVEL)) +
  geom_point() + 
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),width=0.25) +
  labs(x = "School Year",
       y = "Students Per Tablet/Netbook Ratio",
      title = "Students Per Tablet/Netbook Mean Ratio",
      subtitle = "by School Level",
      color = "School Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50))

## Now, let's see how these trends behave in a GEE model ##

writexl::write_xlsx(df_ga3,"Final Cleaned Digital Divide Data.xlsx")

## Produce Tables of the Same Info ##

outcomes <- c("DT1", "LT1", "TN1", "CMS1")

summary_school_year <- df_ga3 %>%
  mutate(DT1 = if_else(DT == 0,0,`Student Population` / DT),
           LT1 = if_else(LT == 0,0,`Student Population` / LT),
           TN1 = if_else(TN == 0,0,`Student Population` / TN),
           CMS1 = if_else(CMS == 0,0,`Student Population` / CMS)) |>
  pivot_longer(
    cols = all_of(outcomes),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  group_by(SCHOOL_YEAR, Outcome) %>%
  summarise(
    n    = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean (SD), n` = sprintf("%.2f (%.2f), n = %d", Mean, SD, n)
  ) %>%
  select(SCHOOL_YEAR, Outcome, `Mean (SD), n`) %>%
  pivot_wider(
    names_from = Outcome,
    values_from = `Mean (SD), n`
  )

make_summary_table <- function(data, group_var) {

  group_var <- rlang::ensym(group_var)

  data %>%
    pivot_longer(
      cols = all_of(outcomes),
      names_to = "Outcome",
      values_to = "Value"
    ) %>%
    group_by(SCHOOL_YEAR, !!group_var, Outcome) %>%
    summarise(
      n    = sum(!is.na(Value)),
      Mean = mean(Value, na.rm = TRUE),
      SD   = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Mean (SD), n` = sprintf("%.2f (%.2f), n = %d", Mean, SD, n)
    ) %>%
    select(
      SCHOOL_YEAR,
      !!group_var,
      Outcome,
      `Mean (SD), n`
    ) %>%
    pivot_wider(
      names_from = Outcome,
      values_from = `Mean (SD), n`
    )
}

summary_marginal_ses <- make_summary_table(
  df_ga3 |>
    mutate(DT1 = if_else(DT == 0,0,`Student Population` / DT),
           LT1 = if_else(LT == 0,0,`Student Population` / LT),
           TN1 = if_else(TN == 0,0,`Student Population` / TN),
           CMS1 = if_else(CMS == 0,0,`Student Population` / CMS)),
  group_var=SES_Level)

summary_marginal_locale <- make_summary_table(
  df_ga3 |>
    mutate(DT1 = if_else(DT == 0,0,`Student Population` / DT),
           LT1 = if_else(LT == 0,0,`Student Population` / LT),
           TN1 = if_else(TN == 0,0,`Student Population` / TN),
           CMS1 = if_else(CMS == 0,0,`Student Population` / CMS)),
  group_var=LOCALE)

summary_marginal_level <- make_summary_table(
  df_ga3 |>
    mutate(DT1 = if_else(DT == 0,0,`Student Population` / DT),
           LT1 = if_else(LT == 0,0,`Student Population` / LT),
           TN1 = if_else(TN == 0,0,`Student Population` / TN),
           CMS1 = if_else(CMS == 0,0,`Student Population` / CMS)),
  group_var=SCHOOL_LEVEL)


writexl::write_xlsx(
  list(
    "By School Year" = summary_school_year,
    "By SES Level"   = summary_marginal_ses,
    "By Locale"     = summary_marginal_locale,
    "By School Level" = summary_marginal_level
  ),
  path = "APA_Outcome_Summaries.xlsx"
)
