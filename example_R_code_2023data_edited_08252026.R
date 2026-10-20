#### FILE NAME: example_R_code_2023data.R
#### AUTHOR: Graham Crawbuck, MS
#### EMAIL: graham.crawbuck@doh.wa.gov
#### Purpose: Example code for reading in 2023 BRFSS data, recoding variable of
####          interest, and estimating weighted and unweighted prevalence.

#### ## PLEASE DO NOT REPRODUCE OR SHARE WITHOUT PERMISSION ## ####

## Steps to follow BEFORE the running code
## 1. Download the zip file for the data package from the MFT
## 2. Move the zip file to a designated folder
## 3. Unzip/extract the zip file
## 4. Change line 34 to point to the folder where the data package was unzipped,
##    using forward slashes in the file path.


#### //// SECTION I: Initial Set Up \\\\ ####

## Install and load packages
list.of.packages <- c("dplyr",
                      "haven",
                      "survey")
new.packages <- 
  list.of.packages[!(list.of.packages %in% 
                       installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
}
library(dplyr)
library(haven)
library(survey)

## Set your data directory: this is where you've unzipped the data package
dataDir <- "C:/BRFSS Dataset/"

## Read in the SAS data file
data24 <- haven::read_sas(paste0(dataDir, 
                                  "wa_brfss_2011_2024_v1.sas7bdat"))



## Rename variables to no longer have a leading underscore
names(data24) <-
  ifelse(substr(names(data24), 1, 1)=="_",
         sub("_", "x", names(data24)),
         names(data24))

## Set up the survey settings
options(survey.lonely.psu = "adjust")


#### \\\\ END SECTION I //// ###




#### //// SECTION II: Examine the data and create the survey study design \\\\ ####

## The number of rows in the dataset 
nrow(data24)

## The number of variables in the dataset
ncol(data24)

## Variable names in alphabetical order
names(data24)[order(names(data24))]

## Recode any variables that are of interest
## Changing these to a factor will allow you to label individual values
data24 <- data24 %>% 
  mutate(
    firearm_f = case_when(
      firearm4 == 1 ~ "Yes",     
      firearm4 == 2 ~ "No",       
      firearm4 %in% c(7, 9) ~ NA_character_  
    ) %>%
      factor(levels = c("Yes", "No"))  
  )



## Set the survey design object
design24 <- svydesign(id = ~1,
                      strata = ~xststr,
                      weights = ~xllcpwt, ## For single-year analyses, always
                      ## use xllcpwt
                      data = data24)


#### \\\\ END SECTION II //// ###




#### //// SECTION III: Estimate statewide weighted and unweighted prevalence \\\\ ####


## Look at the number of observations that are missing a value
table(is.na(data24$firearm_f))

## Estimate unweighted prevalence
prop.table(table(data24$firearm_f)) * 100

(prev <- svymean(~firearm_f,
                 design = design24,
                 method = "likelihood",
                 na.rm = TRUE))

confint(prev)




#SECTION IV: Calculate prevalence by year


library(survey)
library(ggplot2)
library(dplyr)

exclude_years <- c(2014, 2017, 2019, 2021, 2023)

filtered_data <- data24 %>% filter(!year %in% exclude_years)

weighted_df <- lapply(split(filtered_data, filtered_data$year), function(subdata) {
  sub_design <- subset(design24, year == unique(subdata$year))
  prev <- svymean(~firearm4, design = sub_design, method = "likelihood", na.rm = TRUE)
  ci <- confint(prev)
  
  data.frame(
    year = unique(subdata$year),  # keep actual year values here
    prevalence = as.numeric(prev),
    lower_CI = ci[, 1],
    upper_CI = ci[, 2]
  )
}) %>%
  bind_rows() %>%
  arrange(year)


weighted_df <- weighted_df %>%
  mutate(
    label = sprintf("%.3f\n(%.3f–%.3f)", prevalence, lower_CI, upper_CI)
  )

all_years <- c(2013, 2015, 2016, 2018, 2020, 2022, 2024)


#don't want all years, start from 2013, 

#Section V: Plot the Prevalence
install.packages("ggbreak")
library(ggplot2)
library(ggbreak)


# Example plot with y-axis break
p <- ggplot(weighted_df, aes(x = year, y = prevalence)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI),
                width = 0.2, color = "steelblue") +
  scale_x_continuous(breaks = all_years) +
  
  # Set y-axis range and add a break
  # scale_y_break(c(0, 1.5), scales = 0.5) + 
 # coord_cartesian(ylim = c(1.5, 2.5)) +    
  
  labs(
    title = "Prevalence of Firearm Ownership by Year",
    x = "Year",
    y = "Prevalence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )


p + scale_y_break(c(10, 60))




## Estimate weighted prevalences with confidence intervals (one category at a time)
#svyciprop(~I(genhlth_f == "Excellent"),
#          design = design23, 
#          method = "likelihood")
## Very Good
#svyciprop(~I(genhlth_f == "Very Good"),
#          design = design23, 
#          method = "likelihood")
## Good
#svyciprop(~I(genhlth_f == "Good"),
#          design = design23, 
#          method = "likelihood")
## Fair
#svyciprop(~I(genhlth_f == "Fair"),
#          design = design23, 
#          method = "likelihood")
## Poor
#svyciprop(~I(genhlth_f == "Poor"),
#          design = design23, 
#          method = "likelihood") 


#### \\\\ END SECTION III //// ###


