install.packages("haven")   # Only once
library(haven)
library(dplyr) # For data manipulation
library(tidyr)
library(readr)  # For reading CSV files
library(readxl)
library(WDI)
library(fixest)
library(AER)
install.packages("stringr")
library(stringr)
library(countrycode)  # For country name to ISO3 code conversion (if needed)
rm(list=ls())

corruption<-read_csv("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/corruption/wgi data.csv")
# Filter the relevant rows and select specific columns
corruption <- corruption %>%
  filter(`Series Name` == "Control of Corruption: Estimate") %>%
  select(`Country Name`, `Country Code`, `Series Name`, `Series Code`,
         `2000 [YR2000]`, `2005 [YR2005]`, `2010 [YR2010]`, `2015 [YR2015]`, `2020 [YR2020]`)
# Step 2: Pivot to long format
corruption<- corruption %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "corruption_estimate"
  ) %>%
  mutate(year = str_extract(year, "\\d{4}"))  # Extract year as 2000, 2005, etc.

library(tidyverse)
library(ggrepel)


# Step 1: Load the Excel file (adjust path if needed)
migration_raw <- read_excel("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/MIGRANTS STOCK.xlsx", sheet = "Table 1", col_names = FALSE)

# Step 2: Extract only the relevant columns:
# Column A (1): destination
# Column E (5): origin
# Columns G to M (7 to 13): stock for 1990 to 2020
migration_clean <- migration_raw %>%
  select(
    destination = 1,
    origin = 5,
    stock_1990 = 7,
    stock_1995 = 8,
    stock_2000 = 9,
    stock_2005 = 10,
    stock_2010 = 11,
    stock_2015 = 12,
    stock_2020 = 13
  ) %>%
  filter(!is.na(destination) & !is.na(origin))  # remove empty rows
# Step 1: Remove first row (which is a duplicate header)
migration_clean <- migration_clean[-1, ]

# Step 2: Clean and convert stock columns to numeric
migration_clean <- migration_clean %>%
  mutate(across(starts_with("stock_"), ~ as.numeric(gsub("[^0-9.]", "", .))))

# Step 3: Reshape to long format
migration_long <- migration_clean %>%
  pivot_longer(
    cols = starts_with("stock_"),
    names_to = "year",
    names_prefix = "stock_",
    values_to = "migrant_stock"
  ) %>%
  mutate(year = as.numeric(year))

# Step 4: Filter only years you want
migration_long <- migration_long %>%
  filter(year %in% c(1995, 2000, 2005, 2010, 2015, 2020))

# Optional: View the result
View(migration_long)
install.packages("countrycode")  # only once
library(countrycode)

developing_countries <- data.frame(
  country = c(
    "Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", "Azerbaijan", "Bahamas", 
    "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belize", "Benin", "Bhutan", "Bolivia", 
    "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", 
    "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "Chile", "China", 
    "Colombia", "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica", 
    "Côte d’Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Djibouti", "Dominica", 
    "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", 
    "Estonia", "Eswatini", "Ethiopia", "Fiji", "Gabon", "Gambia", "Georgia", "Ghana", "Grenada", 
    "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "India", 
    "Indonesia", "Iran", "Iraq", "Israel", "Jamaica", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
    "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", 
    "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", 
    "Mauritania", "Mauritius", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
    "Morocco", "Mozambique", "Namibia", "Nauru", "Nepal", "Nicaragua", "Niger", "Nigeria", 
    "North Korea", "North Macedonia", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", 
    "Paraguay", "Peru", "Philippines", "Poland", "Qatar", "Romania", "Russia", "Rwanda", 
    "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", 
    "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", 
    "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", 
    "South Korea", "South Sudan", "Sri Lanka", "Sudan", "Suriname", "Syria", "Tajikistan", 
    "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", 
    "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "Uruguay", "Uzbekistan", "Vanuatu", 
    "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"
  ),
  iso3 = c(
    "AFG", "ALB", "DZA", "AGO", "ARG", "ARM", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BLZ", "BEN", "BTN", "BOL",
    "BIH", "BWA", "BRA", "BRN", "BGR", "BFA", "BDI", "CPV", "KHM", "CMR", "CAF", "TCD", "CHL", "CHN", "COL", "COM",
    "COD", "COG", "CRI", "CIV", "HRV", "CUB", "CYP", "CZE", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI",
    "EST", "SWZ", "ETH", "FJI", "GAB", "GMB", "GEO", "GHA", "GRD", "GTM", "GIN", "GNB", "GUY", "HTI", "HND", "HUN",
    "IND", "IDN", "IRN", "IRQ", "ISR", "JAM", "JOR", "KAZ", "KEN", "KIR", "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO",
    "LBR", "LBY", "LTU", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MRT", "MUS", "FSM", "MDA", "MCO", "MNG",
    "MNE", "MAR", "MOZ", "NAM", "NRU", "NPL", "NIC", "NER", "NGA", "PRK", "MKD", "OMN", "PAK", "PLW", "PAN", "PNG",
    "PRY", "PER", "PHL", "POL", "QAT", "ROU", "RUS", "RWA", "KNA", "LCA", "VCT", "WSM", "STP", "SAU", "SEN", "SRB",
    "SYC", "SLE", "SGP", "SVK", "SVN", "SLB", "SOM", "ZAF", "KOR", "SSD", "LKA", "SDN", "SUR", "SYR", "TJK", "TZA",
    "THA", "TGO", "TON", "TTO", "TUN", "TKM", "TUV", "UGA", "UKR", "ARE", "URY", "UZB", "VUT", "VAT", "VEN", "VNM",
    "YEM", "ZMB", "ZWE"
  )
)

# Rename developed country ISO3 dataframe to avoid name collision
developed_countries <- data.frame(
  country = c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", 
    "Greece", "Iceland", "Ireland", "Italy", "Japan", "Luxembourg", "Mexico", "Netherlands", 
    "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "Turkey", 
    "United Kingdom", "United States"
  ),
  iso3 = c(
    "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", 
    "GRC", "ISL", "IRL", "ITA", "JPN", "LUX", "MEX", "NLD", 
    "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "TUR", 
    "GBR", "USA"
  )
)



# Add ISO3 codes to your migration dataset using countrycode
migration_long$origin_iso3 <- countrycode(migration_long$origin, origin = "country.name", destination = "iso3c")
migration_long$destination_iso3 <- countrycode(migration_long$destination, origin = "country.name", destination = "iso3c")

# Extract ISO3 codes for filtering
developing_iso3 <- developing_countries$iso3
developed_iso3 <- developed_countries$iso3

# Filter only flows from developing to developed countries
migration_filtered <- migration_long %>%
  filter(origin_iso3 %in% developing_iso3,
         destination_iso3 %in% developed_iso3)

# Remove non-country aggregates (e.g., regional groupings)
migration_filtered <- migration_filtered %>%
  filter(origin != "Less developed regions, excluding China")

# View the final filtered data
View(migration_filtered)


library(countrycode)

# Rename 'Country Code' to 'iso3'
colnames(corruption)[colnames(corruption) == "Country Code"] <- "origin"
# Drop the 'origin' column
migration_filtered$origin <- NULL

# Rename 'origin_iso3' to 'origin'
colnames(migration_filtered)[colnames(migration_filtered) == "origin_iso3"] <- "origin"

# Merge by iso3 and year
final_panel <- merge(
  migration_filtered,
  corruption,
  by = c("origin", "year"),
  all.x = TRUE
)

final_panel$destination <- NULL
colnames(final_panel)[colnames(final_panel) == "destination_iso3"] <- "destination"
final_panel$"Country Name" <- NULL
final_panel$"Series Name" <- NULL
final_panel$"Series Code"<- NULL

# Get population data from WDI
population_data <- WDI(
  country = "all",
  indicator = "SP.POP.TOTL",  # Total population
  start = 2000,
  end = 2020,
  extra = FALSE
)

library(dplyr)


population_data <- population_data %>%
  filter(!is.na(iso3c)) %>%            # remove aggregate regions
  rename(origin = iso3c,               # match your migration data
         population = SP.POP.TOTL) %>%
  select(origin, year, population)

# Make sure year is numeric in final_panel
final_panel$year <- as.numeric(final_panel$year)

# Now create total emigrants per origin and year
emigrant_totals <- final_panel %>%
  group_by(origin, year) %>%
  summarise(total_emigrants = sum(migrant_stock, na.rm = TRUE), .groups = "drop")

# Now merge population data
final_panel <- final_panel %>%
  left_join(population_data, by = c("origin", "year"))

# Merge total emigrants
final_panel <- final_panel %>%
  left_join(emigrant_totals, by = c("origin", "year"))

# Compute corrected emigration rate
final_panel$emigration_rate <- final_panel$total_emigrants / 
  (final_panel$population + final_panel$total_emigrants)

# Optional: log transformation
final_panel$log_emigration_rate <- log1p(final_panel$emigration_rate)

# Check the result
summary(final_panel$emigration_rate)
#-------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ADDING CONTROLS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------------------------------------------------------------------------------

library(WDI)
library(dplyr)

years_needed <- c(2000, 2005, 2010, 2015, 2020)

indicators <- c(
  "SP.POP.TOTL",           # Total population
  "SP.URB.TOTL.IN.ZS",     # Urbanization %
  "NY.GDP.PCAP.KD",        # GDP per capita (constant 2010 US$)
  "BX.TRF.PWKR.DT.GD.ZS",  # Personal remittances (% of GDP)
  "SE.SCH.LIFE",           # School life expectancy
  "SL.UEM.TOTL.ZS",        # Unemployment rate (%)
  "NE.TRD.GNFS.ZS",        # Trade (% of GDP)
  "GC.TAX.TOTL.GD.ZS"      # Tax revenue (% of GDP)
)

controls_data <- WDI(
  country = "all",
  indicator = indicators,
  start = 2000,
  end = 2020,
  extra = FALSE
) %>%
  filter(year %in% years_needed) %>%
  rename(
    origin = iso3c,
    population = SP.POP.TOTL,
    urbanization = SP.URB.TOTL.IN.ZS,
    gdp_pc = NY.GDP.PCAP.KD,
    remittances = BX.TRF.PWKR.DT.GD.ZS,
    education = SE.SCH.LIFE,
    unemployment = SL.UEM.TOTL.ZS,
    trade = NE.TRD.GNFS.ZS,
    tax_gdp = GC.TAX.TOTL.GD.ZS
  ) %>%
  mutate(
    log_gdp_pc = log1p(gdp_pc),
    log_remit = log1p(remittances),
    log_trade = log1p(trade)
  ) %>%
  select(origin, year, urbanization, gdp_pc, log_gdp_pc, education,
         unemployment, remittances, log_remit, trade, log_trade, tax_gdp)

final_panel <- final_panel %>%
  left_join(controls_data, by = c("origin", "year"))


vdem <- read.csv("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/corruption/V-Dem-CY-Full+Others-v15.csv")

# Democracy
democracy_data <- vdem %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  select(origin = country_text_id, year, democracy = v2x_polyarchy)

# Rule of law
rule_data <- vdem %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  select(origin = country_text_id, year, rule_of_law = v2x_rule)

# Merge into final panel
final_panel <- final_panel %>%
  left_join(democracy_data, by = c("origin", "year")) %>%
  left_join(rule_data, by = c("origin", "year"))


# Define the years you're interested in
years_needed <- c(2000, 2005, 2010, 2015, 2020)

# Download ODA as % of GNI
oda_data <- WDI(
  country = "all",
  indicator = "DT.ODA.ODAT.GN.ZS",
  start = min(years_needed),
  end = max(years_needed),
  extra = FALSE
)

# Clean and rename
oda_data <- oda_data %>%
  filter(year %in% years_needed) %>%
  rename(
    origin = iso3c,
    oda_gni = DT.ODA.ODAT.GN.ZS
  ) %>%
  select(origin, year, oda_gni)
final_panel <- final_panel %>%
  left_join(oda_data, by = c("origin", "year"))

library(haven)
library(dplyr)


# Load the CEPII geo data
geovar <- read_dta("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/geo_cepii.dta")

geo_clean <- geovar %>%
  select(origin = iso3, lat, landlocked) %>%
  group_by(origin) %>%
  summarise(
    lat = mean(lat, na.rm = TRUE),
    landlocked = mean(landlocked, na.rm = TRUE),
    .groups = "drop"
  )

# Merge with final_panel using ISO3 country code
final_panel <- final_panel %>%
  left_join(geo_clean, by = "origin")


########LEGAL ORIGIN

# Read the "large" sheet from the Excel file
law_data <- read_excel("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/CONTROLS/lawfinance.xls", sheet = "large")
names(law_data)

# Step 2: Select only needed columns
law_subset <- law_data[, c("countryc", "legor_uk", "legor_fr", "legor_ge", "legor_so", "legor_sc")]

# Step 3: Merge with final_panel using origin and countryc
final_panel <- merge(final_panel, law_subset, by.x = "origin", by.y = "countryc", all.x = TRUE)


################################. BILATERAL VARIABLES .#########################
library(haven)
library(dplyr)
bilateral_variables <- read_dta ("/Users/ambrarustemi/Desktop/RESEARCH SEMINAR DEVELOPMENT ECONOMICS/CONTROLS/dist_cepii.dta") %>%
  rename(
    origin = iso_o,
    destination = iso_d
  )
final_panel <- final_panel %>%
  left_join(bilateral_variables, by = c("origin", "destination"))

################################# LAGGED VARIABLES   ###########################
# Make sure year is numeric
final_panel$year <- as.numeric(final_panel$year)

# Make sure origin is character
final_panel$origin <- as.character(final_panel$origin)

library(dplyr)


final_panel_old <- final_panel
final_panel <- final_panel_old
# Step 1: Create lag year column in your panel
final_panel <- final_panel %>%
  mutate(lag_year = year - 5)

# Step 2: Extract unique country-year values with lagged variables
lag_vars <- final_panel %>%
  group_by(origin, year) %>%
  summarise(
    lag_log_gdp_pc = first(log_gdp_pc),
    lag_education = first(education),
    lag_urbanization = first(urbanization),
    lag_log_trade = first(log_trade),
    lag_tax_gdp = first(tax_gdp),
    lag_oda_gni = first(oda_gni),
    lag_lat = first(lat),  # optional if static
    lag_democracy = first(democracy),
    lag_rule_of_law = first(rule_of_law),
    lag_migration_rate = first(emigration_rate),
    lag_corruption_estimate = first(corruption_estimate),
    .groups = "drop"
  )

# Step 3: Merge using origin and lag_year
final_panel <- final_panel %>%
  left_join(lag_vars, by = c("origin", "lag_year" = "year")) %>%
  select(-lag_year)  # clean up


#####SUMMMARY TABLE OF VARIABLES








##CLUSTER AT COUNTRY PAIR LEVEL

library(dplyr)

final_panel <- final_panel %>%
  mutate(pair_id = paste(pmin(origin, destination), pmax(origin, destination), sep = "_"))
library(fixest)





############################IV STRATEGY
#PSEUDO-GRAVITY INSTRUMENT


#-------------------#
# ZERO-STAGE SETUP
#-------------------#

# Filter for 2010 cross-section
gravity_cross <- final_panel %>%
  filter(year == 2010) %>%
  mutate(
    log_mig_stock = log1p(migrant_stock),
    log_dist = log(distw),
    log_pop_origin = log1p(population),
    comlang_ethno = as.numeric(comlang_ethno),
    colony = as.numeric(colony),
    contig = as.numeric(contig)
  )

# Remove outliers 
gravity_cross_cleaned <- gravity_cross %>%
  filter(migrant_stock <= quantile(migrant_stock, 0.975, na.rm = TRUE))

#-----------------------------#
# Gravity Model (PPML-FE)
#-----------------------------#

gravity_model <- fepois(
  migrant_stock ~ log_dist + comlang_ethno + contig + log_pop_origin |
    destination,
  data = gravity_cross_cleaned,
  cluster = ~ interaction(origin, destination)
)

summary(gravity_model)

#--------------------------------------------#
# Predict bilateral migrant stocks (fitted)
#--------------------------------------------#

gravity_cross_data <- gravity_cross_cleaned %>%
  filter(
    !is.na(log_dist),
    !is.na(log_pop_origin),
    !is.na(comlang_ethno),
    !is.na(colony),
    !is.na(contig),
    !is.na(migrant_stock)
  )

gravity_cross_data$predicted_stock <- fitted(gravity_model)

# Merge predicted values into original cleaned dataset
gravity_cross_cleaned <- gravity_cross_cleaned %>%
  left_join(
    gravity_cross_data %>% select(origin, destination, predicted_stock),
    by = c("origin", "destination")
  )

#---------------------------------------------------------#
# STEP A: Compute observed emigration rate (by origin)
#---------------------------------------------------------#

# Sum actual emigrants by origin
observed_emigration <- gravity_cross_cleaned %>%
  group_by(origin) %>%
  summarise(
    total_emigrants = sum(migrant_stock, na.rm = TRUE),
    .groups = "drop"
  )

# Compute predicted totals by origin
predicted_totals <- gravity_cross_cleaned %>%
  group_by(origin) %>%
  summarise(
    predicted_total_emigrants = sum(predicted_stock, na.rm = TRUE),
    population = first(population),
    .groups = "drop"
  )

# Merge both and compute observed emigration rate
emigration_rate_2010 <- observed_emigration %>%
  left_join(predicted_totals, by = "origin") %>%
  mutate(
    emigration_rate = total_emigrants / (population + predicted_total_emigrants)
  ) %>%
  select(origin, emigration_rate, predicted_total_emigrants, population)

#---------------------------------------------------------#
# STEP B: Compute predicted emigration rate (same denominator)
#---------------------------------------------------------#

gravity_iv <- predicted_totals %>%
  mutate(
    predicted_emigration_rate = predicted_total_emigrants / (population + predicted_total_emigrants)
  ) %>%
  select(origin, predicted_emigration_rate)

#---------------------------------------------------------#
# STEP C: Merge and run first-stage regression
#---------------------------------------------------------#

first_stage_data <- gravity_iv %>%
  left_join(emigration_rate_2010 %>% select(origin, emigration_rate), by = "origin")

# Run first-stage regression
first_stage_model <- lm(emigration_rate ~ predicted_emigration_rate, data = first_stage_data)
summary(first_stage_model)

#---------------------------------------------------------#
# Optional: Prepare dataset for second stage (add region FE)
#---------------------------------------------------------#

gravity_cross_cleaned <- gravity_cross_cleaned %>%
  mutate(region = countrycode(origin, origin = "iso3c", destination = "region")) %>%
  mutate(corruption_estimate = as.numeric(corruption_estimate)) %>%
  left_join(gravity_iv, by = "origin")



#-------------------------#
# 5. Additional Variables
#-------------------------#

# Add region fixed effects if needed for 2nd stage later
gravity_cross_cleaned <- gravity_cross_cleaned %>%
  mutate(region = countrycode(origin, origin = "iso3c", destination = "region"))

# Ensure corruption variable is numeric
gravity_cross_cleaned$corruption_estimate <- as.numeric(gravity_cross_cleaned$corruption_estimate)
# Merge predicted_emigration_rate into your main dataset
gravity_cross_cleaned <- gravity_cross_cleaned %>%
  left_join(gravity_iv, by = "origin")

#_____________________________RESCALING CONTROL OF CORRUPTION ________________________________________
gravity_cross_cleaned <- gravity_cross_cleaned %>%
  mutate(corruption_rescaled = (corruption_estimate + 2.5) / 5)

#..................SIMPLE OLS....................................................
cross_ols_1 <- lm(corruption_rescaled ~ emigration_rate, data = gravity_cross_cleaned )
summary(cross_ols_1) 



#_________________________________2SLS__________________________________________

gravity_cross_cleaned <- gravity_cross_cleaned %>%
  rename(predicted_emigration_rate = predicted_emigration_rate.x) %>%
  select(-predicted_emigration_rate.y)





library(AER)
library(modelsummary)

#------------------ Model 1: OLS (No Controls, No FE) ------------------#
model1 <- lm(corruption_estimate ~ emigration_rate,
             data = gravity_cross_cleaned)

summary(model1)
#------------------ Model 2: IV (No Controls, No FE) ------------------#
model2 <- ivreg(
  corruption_estimate ~ emigration_rate |
    predicted_emigration_rate,
  data = gravity_cross_cleaned
)

#------------------ Model 3: IV + log GDP ------------------#
model3 <- ivreg(
  corruption_rescaled ~ emigration_rate + log_gdp_pc |
    predicted_emigration_rate + log_gdp_pc,
  data = gravity_cross_cleaned
)

#------------------ Model 4: IV + Education ------------------#
model4 <- ivreg(
  corruption_rescaled ~ emigration_rate + education |
    predicted_emigration_rate + education,
  data = gravity_cross_cleaned
)

#------------------ Model 5: IV + Trade ------------------#
model5 <- ivreg(
  corruption_rescaled ~ emigration_rate + log_trade |
    predicted_emigration_rate + log_trade,
  data = gravity_cross_cleaned
)

#------------------ Model 6: IV + landlocked ------------------#
model6 <- ivreg(
  corruption_rescaled ~ emigration_rate + landlocked |
    predicted_emigration_rate + landlocked,
  data = gravity_cross_cleaned
)

model7 <- ivreg(
  corruption_rescaled ~ emigration_rate + lat |
    predicted_emigration_rate + lat,
  data = gravity_cross_cleaned
)

model8 <- ivreg(
  corruption_rescaled~ emigration_rate + legor_uk |
    predicted_emigration_rate + legor_uk,
  data = gravity_cross_cleaned
)

model9 <- ivreg(
  corruption_rescaled ~ emigration_rate + legor_fr |
    predicted_emigration_rate + legor_fr,
  data = gravity_cross_cleaned
)
summary(model9)

#NO CONTROLS, ONLY FE
model10 <- ivreg(
  corruption_rescaled ~ emigration_rate + factor(region) |
    predicted_emigration_rate + factor(region),
  data = gravity_cross_cleaned
)
summary(model10)


model11 <- ivreg(
  corruption_rescaled ~ emigration_rate + log_gdp_pc + education + log_trade +
    landlocked + legor_uk + legor_fr + lat + factor(region) |
    predicted_emigration_rate + log_gdp_pc + education + log_trade +
    landlocked + legor_uk + legor_fr + lat + factor(region),
  data = gravity_cross_cleaned
)

summary(model11)
library(fixest)







#------------------ Modelsummary LaTeX Table ------------------#

library(modelsummary)
install.packages("ivreg")
library(ivreg)
library(fixest)

# List all models explicitly
models_named <- list(
  "(1)" = model1,
  "(2)" = model2,
  "(3)" = model3,
  "(4)" = model4,
  "(5)" = model5,
  "(6)" = model6,
  "(7)" = model7,
  "(8)" = model8,
  "(9)" = model9,
  "(10)" = model10,
  "(11)" = model11
)

# Ensure all IV models are also of class 'lm' for modelsummary compatibility
models_named <- lapply(models_named, function(m) {
  if ("ivreg" %in% class(m)) {
    class(m) <- c("ivreg", "lm")
  }
  return(m)
})

# Extract diagnostics: Observations + First-stage F
extract_iv_diagnostics <- function(model) {
  obs <- nobs(model)
  fstat <- tryCatch({
    d <- summary(model, diagnostics = TRUE)$diagnostics
    round(d["Weak instruments", "statistic"], 2)
  }, error = function(e) NA)
  return(c("Observations" = obs, "First-stage F" = fstat))
}


# Apply to all models and reformat
diagnostics_df <- as.data.frame(sapply(models_named, extract_iv_diagnostics))
diagnostics_df$stat_name <- rownames(diagnostics_df)
diagnostics_df <- diagnostics_df[, c("stat_name", names(models_named))]

# Generate LaTeX table
modelsummary(
  models_named,
  coef_map = c(
    "emigration_rate" = "Migration Rate",
    "log_gdp_pc" = "Log GDP per Capita",
    "education" = "Education",
    "log_trade" = "Trade Openness",
    "landlocked" = "Landlocked",
    "legor_uk" = "UK Legal Origin",
    "legor_fr" = "FR Legal Origin",
    "lat" = "Latitude"
  ),
  stars = TRUE,
  gof_omit = "IC|F|Log|Adj|AIC|BIC",
  add_rows = diagnostics_df,
  output = "latex"
)




#-------------------------#
# PANEL 0 STAGE
#-------------------------#
#-------------------------#
# STEP 0: Rescale Corruption Estimate in Final Panel
#-------------------------#

final_panel <- final_panel %>%
  mutate(
    corruption_estimate = as.numeric(corruption_estimate),
    corruption_rescaled = (corruption_estimate + 2.5) / 5
  )


# STEP 1: Prepare gravity model data
gravity_panel <- final_panel %>%
  filter(year %in% c(1995, 2000, 2005, 2010, 2015, 2020)) %>%
  mutate(
    log_dist = log(distw),
    log_pop_origin = log1p(population),
    comlang_ethno = as.numeric(comlang_ethno),
    contig = as.numeric(contig)
  )

# STEP 2: Estimate gravity model (PPML)
gravity_ppml <- fepois(
  migrant_stock ~ i(year, log_dist, ref = 1995) + comlang_ethno + contig + curcol + log_pop_origin |
    destination + year,
  data = gravity_panel,
  cluster = ~origin + destination
)
summary(gravity_ppml)

# STEP 3: Keep only valid rows for prediction
gravity_model_data <- gravity_panel %>%
  filter(
    !is.na(log_dist),
    !is.na(log_pop_origin),
    !is.na(comlang_ethno),
    !is.na(migrant_stock),
    !is.na(contig)
  )

# STEP 4: Predict bilateral migrant stocks
gravity_model_data$predicted_stock <- fitted(gravity_ppml)

# STEP 5: Merge predicted stock into full panel
gravity_panel <- gravity_panel %>%
  left_join(
    gravity_model_data %>% select(origin, destination, year, predicted_stock),
    by = c("origin", "destination", "year")
  )

# STEP 6: Construct predicted & actual emigration rates
actual_emigrants_panel <- gravity_panel %>%
  group_by(origin, year) %>%
  summarise(total_emigrants = sum(migrant_stock, na.rm = TRUE), .groups = "drop")

gravity_iv_panel <- gravity_panel %>%
  group_by(origin, year) %>%
  summarise(predicted_migrants = sum(predicted_stock, na.rm = TRUE), .groups = "drop") %>%
  left_join(actual_emigrants_panel, by = c("origin", "year")) %>%
  left_join(final_panel %>% distinct(origin, year, population), by = c("origin", "year")) %>%
  mutate(
    gravity_iv = predicted_migrants / (population + predicted_migrants),
    emigration_rate = total_emigrants / (population + total_emigrants)
  )

# STEP 7: Merge predicted and actual migration rates
final_panel <- final_panel %>%
  left_join(gravity_iv_panel %>% select(origin, year, gravity_iv, emigration_rate),
            by = c("origin", "year"))

#-------------------------#
# STEP 8: Lagged Variables for IV
#-------------------------#
lagged_iv <- gravity_iv_panel %>%
  mutate(year = year + 5) %>%
  rename(
    L.gravity_iv = gravity_iv,
    lag_population = population
  )
# STEP 1: Build clean panel with properly typed variables
panel_data_clean <- final_panel %>%
  left_join(lagged_iv, by = c("origin", "year")) %>%
  group_by(origin) %>%
  arrange(year) %>%
  mutate(
    corruption_estimate = as.numeric(corruption_estimate),
    lag_corruption_estimate = lag(corruption_estimate),
    corruption_rescaled = (corruption_estimate + 2.5) / 5,
    lag_corruption_rescaled = (lag_corruption_estimate + 2.5) / 5,
    lag_migration_rate = lag(emigration_rate.x),
    log_lag_population = log(lag_population),
    region = countrycode(origin, origin = "iso3c", destination = "region")
  ) %>%
  ungroup()

# STEP 2: OLS model
model1 <- feols(
  corruption_rescaled ~ lag_migration_rate + lag_corruption_rescaled | year,
  data = panel_data_clean,
  cluster = ~origin
)

# STEP 3: Prepare for IV first-stage estimation — filter NA rows
panel_data_clean <- panel_data_clean %>%
  filter(
    !is.na(lag_migration_rate),
    !is.na(L.gravity_iv),
    !is.na(lag_corruption_rescaled),
    !is.na(lag_log_gdp_pc),
    !is.na(lag_education),
    !is.na(lag_log_trade),
    !is.na(corruption_estimate)
  )

first_stage_model <- feols(
  lag_migration_rate ~ L.gravity_iv + lag_corruption_rescaled + lag_log_gdp_pc +
    lag_education + lag_log_trade + legor_uk + legor_fr + landlocked + lat | year,
  data = panel_data_clean,
  cluster = ~origin
)

summary(first_stage_model)




###ONLY ONE AT TIME
# Column 1: OLS
model1 <- feols(
  corruption_rescaled ~ lag_migration_rate + lag_corruption_rescaled | year,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model1)

# Column 2: 2SLS, no controls
model2 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model2)

# Column 3: Only GDP
model3 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_log_gdp_pc | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)

# Column 4: Only Education
model4 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_education | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)

# Column 5: Only Trade
model5 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_log_trade | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)

# Column 6: Only Legor UK
model6 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + legor_uk | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)

# Column 7: Only Legor FR
model7 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + legor_fr | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)

# Column 8: Only Landlocked
model8 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + landlocked | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)


# Column 9: Only Latitude
model9 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lat | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model9)

#Column 10: Only ODA
model10 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_oda_gni | year | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model10)

# Column 11: All controls + year and region FE
model11 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled | year + origin | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model11)

# Column 11: All controls + year and region FE
model12 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_log_gdp_pc + lag_education + lag_log_trade + lag_oda_gni +
    legor_uk + legor_fr + landlocked + lat| year + origin | lag_migration_rate ~ L.gravity_iv,
  data = panel_data_clean,
  cluster = ~origin
)
summary(model12)

etable(
  model1, model2, model3, model4, model5,
  model6, model7, model8, model9, model10, model11, model12,
  se = "hetero",
  fitstat = c("n", "r2", "ar2", "f.stat", "ivf.stat"),
  dict = c(
    lag_migration_rate = "Migration Rate",
    lag_corruption_estimate = "Lagged Corruption",
    lag_log_gdp_pc = "Log GDP per Capita",
    lag_education = "Education",
    lag_log_trade = "Log Trade Openness",
    legor_uk = "UK Legal Origin",
    legor_fr = "French Legal Origin",
    landlocked = "Landlocked",
    lat = "Latitude",
    oda_gni = "ODA / GNI"
  ),
  title = "Table 3.2. Dynamic panel model with gravity-based instruments (lagged dep. var and migration rate)",
  tex = TRUE  # ← this gives you LaTeX code as a string
)

##########################Geographyxglobaltrend insturment
# -------------------------------
# STEP 0: Prepare geographic data
# -------------------------------
geovar_clean <- geovar %>%
  mutate(origin=iso3)%>%
  distinct(origin, area, .keep_all = TRUE)  # Ensure unique 'origin'

# Merge AREA and log DISTANCE into final panel
final_panel <- final_panel %>%
  left_join(geovar_clean %>% select(origin, area), by = "origin") 

final_panel <- final_panel %>%
  mutate(
    log_dist = log(distw),
    log_area = log(area))


# -------------------------------
# STEP 1: Get World Trade (WDI)
# -------------------------------

install.packages("WDI")
library(WDI)

trade_vars <- c(
  expo = "NE.EXP.GNFS.ZS",  # exports % GDP
  impo = "NE.IMP.GNFS.ZS"   # imports % GDP
)

# Create world_trade data frame
world_trade <- WDI(country = "WLD", indicator = c(
  expo = "NE.EXP.GNFS.ZS",
  impo = "NE.IMP.GNFS.ZS"
), start = 1990, end = 2023) %>%
  mutate(world_trade = (expo + impo) / 100) %>%
  select(year, world_trade)

final_panel <- final_panel %>%
  left_join(world_trade, by = "year")

# -------------------------------
# STEP 2: Create Interaction IVs
# -------------------------------
final_panel <- final_panel %>%
  mutate(
    trade_iv_dist = log_dist * world_trade,
    trade_iv_area = log_area * world_trade
  )


# -------------------------------
# STEP 3: Lag Instruments by 5 Years
# -------------------------------
geo_iv <- final_panel %>%
  select(origin, year, trade_iv_dist, trade_iv_area) %>%
  mutate(year = year + 5) %>%
  group_by(origin, year) %>%
  summarise(
    L.trade_iv_dist = mean(trade_iv_dist, na.rm = TRUE),
    L.trade_iv_area = mean(trade_iv_area, na.rm = TRUE),
    .groups = "drop"
  )

final_panel <- final_panel %>%
  left_join(geo_iv, by = c("origin", "year"))

# -------------------------------
# STEP 4: Create Lagged Variables
# -------------------------------
final_panel <- final_panel %>%
  mutate(corruption_estimate = as.numeric(corruption_estimate)) %>%  # ensure numeric
  group_by(origin) %>%
  arrange(year) %>%
  mutate(
    lag_corruption_estimate = lag(corruption_estimate),
    lag_corruption_rescaled = (lag_corruption_estimate + 2.5) / 5
  ) %>%
  ungroup()


# -------------------------------
# STEP 5: Define Regression Sample
# -------------------------------
panel_data <- final_panel %>%
  filter(
    !is.na(lag_migration_rate),
    !is.na(L.trade_iv_dist),
    !is.na(L.trade_iv_area),
    !is.na(lag_corruption_rescaled),
    !is.na(lag_log_gdp_pc),
    !is.na(lag_education),
    !is.na(lag_log_trade),
    !is.na(corruption_estimate)
  )

# -------------------------------
# STEP 6: First-Stage Regression
# -------------------------------
library(fixest)

first_stage <- feols(
  lag_migration_rate ~ L.trade_iv_area +
    lag_corruption_rescaled + lag_log_gdp_pc + lag_education + lag_log_trade +
    legor_uk + legor_fr + landlocked + lat | year,
  data = panel_data,
  cluster = ~origin
)

summary(first_stage)

# --------------------------
# STEP 5: Prepare estimation sample
# --------------------------
panel_data <- panel_data %>%
  mutate(
    corruption_estimate = as.numeric(corruption_estimate),
    corruption_rescaled = (corruption_estimate + 2.5) / 5
  )

library(countrycode)

panel_data <- panel_data %>%
  mutate(region = countrycode(origin, origin = "iso3c", destination = "region"))

iv_2sls <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_log_gdp_pc + lag_education + lag_log_trade + lag_oda_gni + legor_uk +
    legor_fr + landlocked + lat |
    year + origin + region + destination | lag_migration_rate ~ L.trade_iv_area,
  data = panel_data,
  cluster = ~origin
)

summary(iv_2sls)

panel_data <- panel_data %>%
  mutate(
    L.trade_iv_area_scaled = (L.trade_iv_area - min(L.trade_iv_area, na.rm = TRUE)) /
      (max(L.trade_iv_area, na.rm = TRUE) - min(L.trade_iv_area, na.rm = TRUE))
  )


# MODEL 7: Add legal origin and geography
model7 <- feols(
  corruption_rescaled ~ lag_corruption_rescaled + lag_log_gdp_pc + lag_education + lag_log_trade +
    lag_oda_gni + legor_uk + legor_fr + landlocked + lat |
    year + origin + region | lag_migration_rate ~ L.trade_iv_area_scaled,
  data = panel_data,
  cluster = ~origin
)
summary(model7)



install.packages("modelsummary")  # if not already installed
library(modelsummary)

iv1 <- feols(corruption_rescaled ~ lag_corruption_rescaled  | year | 
               lag_migration_rate ~ L.trade_iv_area_scaled, 
             data = panel_data, cluster = ~origin)

iv2 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + lag_log_gdp_pc | year + origin| 
               lag_migration_rate ~ L.trade_iv_area_scaled, 
             data = panel_data, cluster = ~origin)

iv3 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + lag_education | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, 
             data = panel_data, cluster = ~origin)

iv4 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + lag_log_trade | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, 
             data = panel_data, cluster = ~origin)

iv5 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + lag_oda_gni | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, 
             data = panel_data, cluster = ~origin)

iv6 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + legor_uk | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, data = panel_data, cluster = ~origin)

iv7 <- feols(corruption_rescaled ~ lag_corruption_rescaled + legor_fr | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, data = panel_data, cluster = ~origin)

iv8 <- feols(corruption_rescaled ~ lag_corruption_rescaled  + landlocked | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, data = panel_data, cluster = ~origin)

iv9 <- feols(corruption_rescaled ~ lag_corruption_rescaled + lat | year + origin | 
               lag_migration_rate ~ L.trade_iv_area_scaled, data = panel_data, cluster = ~origin)


iv10 <- feols(corruption_rescaled ~ lag_corruption_rescaled + lag_log_gdp_pc + lag_education +
                lag_log_trade + lag_oda_gni + legor_uk + legor_fr + landlocked + lat |
                year + origin | lag_migration_rate ~ L.trade_iv_area_scaled,
              data = panel_data, cluster=~origin)
summary(iv10)

library(modelsummary)


etable(
  iv1, iv2, iv3, iv4, iv5, iv6, iv7, iv8, iv9, iv10,
  fitstat = c("n", "r2", "ar2", "f.stat", "ivf.stat"),
  dict = c(
    lag_migration_rate = "Migration Rate",
    lag_corruption_rescaled = "Lagged CC",
    lag_log_gdp_pc = "L. Log GDP per Capita",
    lag_education = "L. Education",
    lag_log_trade = "L. Log Trade Openness",
    lag_oda_gni = "L. ODA / GNI",
    legor_uk = "UK Legal Origin",
    legor_fr = "French Legal Origin",
    landlocked = "Landlocked",
    lat = "Latitude"
  ),
  title = "Table: Dynamic panel model with geography × trade instruments",
  tex = TRUE
)




# ------------------------ #
# SYSTEM GMM ESTIMATION
# ------------------------ #

library(dplyr)
library(plm)


# Step 1: Clean and summarise — DROP 1995 HERE
panel_data_origin_year <- panel_data %>%
  filter(year != 1995) %>%
  group_by(origin, year) %>%
  summarise(
    corruption_rescaled = first(corruption_rescaled),
    migration_rate=first(emigration_rate.x),
    lag_corruption_rescaled = first(lag_corruption_rescaled),
    lag_migration_rate = mean(lag_migration_rate, na.rm = TRUE),
    lag_log_gdp_pc = mean(lag_log_gdp_pc, na.rm = TRUE),
    lag_education = mean(lag_education, na.rm = TRUE),
    lag_log_trade = mean(lag_log_trade, na.rm = TRUE),
    lag_oda_gni = mean(lag_oda_gni, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origin, year) %>%
  mutate(origin = as.factor(origin), year = as.integer(year))

# Step 2: Create proper pdata.frame
pdata_gmm <- pdata.frame(panel_data_origin_year, index = c("origin", "year"))


# Model 1: No controls

gmm_col1 <- pgmm(
  formula = corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate |
    lag(corruption_rescaled, 2:3) + lag(lag_migration_rate, 2:3),
  data = pdata_gmm,
  effect = "individual",
  model = "twosteps",      # TWO-STEP is key here
  transformation = "ld",
  collapse = TRUE
)
summary(gmm_col1)

# Model 2: + Education
gmm_col2 <- pgmm(
  corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate + lag_education |
    lag(corruption_rescaled,2:3) + lag(lag_migration_rate, 2:3) + lag(lag_education, 2:3),
  data = pdata_gmm, effect = "individual", model = "twosteps",
  transformation = "ld", collapse= TRUE
)
summary(gmm_col2)

# Model 3: + log GDP per capita
gmm_col3 <- pgmm(
  corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate + lag_log_gdp_pc |
    lag(corruption_rescaled, 2:3) + lag(lag_migration_rate, 2:3) + lag(lag_log_gdp_pc, 2:3),
  data = pdata_gmm, effect = "individual", model = "twosteps",
  transformation = "ld", collapse= TRUE
)
summary(gmm_col3)

# Model 4: + log trade
gmm_col4 <- pgmm(
  corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate + lag_log_trade |
    lag(corruption_rescaled,2:3) + lag(lag_migration_rate, 2:3) + lag(lag_log_trade, 2:3),
  data = pdata_gmm, effect = "individual", model = "twosteps",
  transformation = "ld", collapse=TRUE
)
summary(gmm_col4)

# Model 5: All controls
gmm_col5 <- pgmm(
  corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate + lag_education +
    lag_log_gdp_pc + lag_log_trade|
    lag(corruption_rescaled, 2:3) + lag(lag_migration_rate, 2:3) +
    lag(lag_education, 2:3) + lag(lag_log_gdp_pc, 2:3) +
    lag(lag_log_trade, 2:3),
  data = pdata_gmm, effect = "individual", model = "twosteps",
  transformation = "ld", collapse=TRUE
)
summary(gmm_col5)



models <- list(
  "(1)" = gmm_col1,
  "(2)" = gmm_col2,
  "(3)" = gmm_col3,
  "(4)" = gmm_col4,
  "(5)" = gmm_col5
)

extract_pgmm_diagnostics <- function(mod) {
  s <- summary(mod, robust = TRUE)
  
  obs <- nobs(mod)
  
  # Sargan (one-step)
  sargan_test <- tryCatch(sargan(mod, weights = "onestep"), error = function(e) NA)
  sargan_p <- if (inherits(sargan_test, "htest")) sargan_test$p.value else NA
  
  # Hansen (two-step)
  hansen_test <- tryCatch(sargan(mod, weights = "twosteps"), error = function(e) NA)
  hansen_p <- if (inherits(hansen_test, "htest")) hansen_test$p.value else NA
  
  ar1 <- tryCatch(mtest(mod, 1)$p.value, error = function(e) NA)
  ar2 <- tryCatch(mtest(mod, 2)$p.value, error = function(e) NA)
  
  c(
    Observations = obs,
    Hansen_p = hansen_p,
    Sargan_p = sargan_p,
    AR1_p = ar1,
    AR2_p = ar2
  )
}

# Apply to all models
stats_df <- t(sapply(models, extract_pgmm_diagnostics))
stats_df <- round(as.data.frame(stats_df), 3)
stats_df

library(modelsummary)

modelsummary(
  list(
    "(1)" = gmm_col1,
    "(2)" = gmm_col2,
    "(3)" = gmm_col3,
    "(4)" = gmm_col4,
    "(5)" = gmm_col5
  ),
  stars = TRUE,
  coef_map = c(
    "lag(corruption_rescaled, 1)" = "L. Control of Corruption",
    "lag_migration_rate" = "L. Migration Rate",
    "lag_education" = "L.Education",
    "lag_log_gdp_pc" = "L.Log GDP per Capita",
    "lag_log_trade" = "L.Trade Openness"
  ),
  gof_omit = ".*",           # hides default fit stats
  output = "latex"
)


##################TABLE OF COUNTRIES
# Sort alphabetically
developing_countries <- sort(developing_countries)
# Sort the country names, not the entire data frame
sorted_countries <- sort(developing_countries$country)

install.packages("kableExtra")
library(knitr)
library(kableExtra)


# Your full list (already saved as `developing_countries`)
all_countries <- sort(developing_countries$country)  # alphabetical order

# Split alphabetically into 3 columns, top-down
n <- length(all_countries)
n_col <- 3
n_row <- ceiling(n / n_col)

# Fill matrix column-wise
country_matrix <- matrix(
  c(all_countries, rep("", n_row * n_col - n)), 
  nrow = n_row, ncol = n_col, byrow = FALSE
)

# Convert to data frame for kable
df <- as.data.frame(country_matrix)
colnames(df) <- rep("", n_col)

# Create LaTeX table
kbl(df, booktabs = TRUE, caption = "List of Developing Countries") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  column_spec(1:3, width = "0.3\\textwidth", border_left = FALSE, border_right = FALSE)
sink("developing_countries_table.tex")
kbl(df, booktabs = TRUE, caption = "List of Developing Countries") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  column_spec(1:3, width = "0.3\\textwidth", border_left = FALSE, border_right = FALSE)
sink()




#########!!!!!!!!!!!!!!ROBUSTNESS CHECK: POST-COMMUNIST COUNTRIES
# Socialist countries: defined via 'legor_so' dummy
socialist_countries <- panel_data %>%
  filter(legor_so == 1) %>%
  pull(origin) %>%
  unique()

# Sub-Saharan Africa
ssa_countries <- panel_data %>%
  filter(region == "Sub-Saharan Africa") %>%
  pull(origin) %>%
  unique()

# MENA (Middle East and North Africa)
mena_countries <- panel_data %>%
  filter(region == "Middle East & North Africa" | region == "MENA") %>%
  pull(origin) %>%
  unique()

oil_exporters_list <- c(
  "Algeria", "Iran", "Iraq", "Kuwait", "Libya", "Oman", "Saudi Arabia", "Syria", "United Arab Emirates", "Yemen",
  "Angola", "Nigeria", "Gabon", "Republic of Congo", "Sudan", "Chad",
  "Venezuela", "Ecuador",
  "Russia", "Kazakhstan", "Azerbaijan"
)

panel_data <- panel_data %>%
  mutate(oil_exporter = ifelse(origin %in% oil_exporters_list, 1, 0))

oil_countries <- panel_data %>%
  filter(oil_exporter == 1) %>%
  pull(origin) %>%
  unique()

final_panel <- final_panel %>%
  arrange(origin, year) %>%
  group_by(origin) %>%
  mutate(lag_corruption_rescaled = lag(corruption_rescaled)) %>%
  ungroup()

# Create robustness subset excluding socialist countries
panel_data_no_soc <- final_panel %>%
  filter(!(origin %in% socialist_countries), year != 1995) %>%
  group_by(origin, year) %>%
  summarise(
    corruption_rescaled = first(corruption_rescaled),
    migration_rate = first(emigration_rate.x),
    lag_corruption_rescaled = first(lag_corruption_rescaled),
    lag_migration_rate = mean(lag_migration_rate, na.rm = TRUE),
    lag_log_gdp_pc = mean(lag_log_gdp_pc, na.rm = TRUE),
    lag_education = mean(lag_education, na.rm = TRUE),
    lag_log_trade = mean(lag_log_trade, na.rm = TRUE),
    lag_oda_gni = mean(lag_oda_gni, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origin, year) %>%
  mutate(origin = as.factor(origin), year = as.integer(year))


# Step 1: Exclude Sub-Saharan African countries
panel_data_no_ssa <- final_panel %>%
  filter(!(origin %in% ssa_countries), year != 1995) %>%
  group_by(origin, year) %>%
  summarise(
    corruption_rescaled = first(corruption_rescaled),
    migration_rate = first(emigration_rate.x),
    lag_corruption_rescaled = first(lag_corruption_rescaled),
    lag_migration_rate = mean(lag_migration_rate, na.rm = TRUE),
    lag_log_gdp_pc = mean(lag_log_gdp_pc, na.rm = TRUE),
    lag_education = mean(lag_education, na.rm = TRUE),
    lag_log_trade = mean(lag_log_trade, na.rm = TRUE),
    lag_oda_gni = mean(lag_oda_gni, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origin, year) %>%
  mutate(origin = as.factor(origin), year = as.integer(year))

# Step 2: Exclude MENA countries
panel_data_no_mena <- final_panel %>%
  filter(!(origin %in% mena_countries), year != 1995) %>%
  group_by(origin, year) %>%
  summarise(
    corruption_rescaled = first(corruption_rescaled),
    migration_rate = first(emigration_rate.x),
    lag_corruption_rescaled = first(lag_corruption_rescaled),
    lag_migration_rate = mean(lag_migration_rate, na.rm = TRUE),
    lag_log_gdp_pc = mean(lag_log_gdp_pc, na.rm = TRUE),
    lag_education = mean(lag_education, na.rm = TRUE),
    lag_log_trade = mean(lag_log_trade, na.rm = TRUE),
    lag_oda_gni = mean(lag_oda_gni, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origin, year) %>%
  mutate(origin = as.factor(origin), year = as.integer(year))



# Step 3: Exclude oil exporting countries
panel_data_no_oil <- final_panel %>%
  filter(!(origin %in% oil_countries), year != 1995) %>%
  group_by(origin, year) %>%
  summarise(
    corruption_rescaled = first(corruption_rescaled),
    migration_rate = first(emigration_rate.x),
    lag_corruption_rescaled = first(lag_corruption_rescaled),
    lag_migration_rate = mean(lag_migration_rate, na.rm = TRUE),
    lag_log_gdp_pc = mean(lag_log_gdp_pc, na.rm = TRUE),
    lag_education = mean(lag_education, na.rm = TRUE),
    lag_log_trade = mean(lag_log_trade, na.rm = TRUE),
    lag_oda_gni = mean(lag_oda_gni, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origin, year) %>%
  mutate(origin = as.factor(origin), year = as.integer(year))

library(plm)

pdata_no_soc  <- pdata.frame(panel_data_no_soc, index = c("origin", "year"))
pdata_no_ssa  <- pdata.frame(panel_data_no_ssa, index = c("origin", "year"))
pdata_no_mena <- pdata.frame(panel_data_no_mena, index = c("origin", "year"))
pdata_no_oil  <- pdata.frame(panel_data_no_oil, index = c("origin", "year"))

library(plm)

gmm_no_soc <- pgmm(
  corruption_rescaled ~ lag(corruption_rescaled, 1) + lag_migration_rate + lag_education +
    lag_log_gdp_pc + lag_log_trade + lag_oda_gni |
    lag(corruption_rescaled, 2:3) + lag(lag_migration_rate, 2:3) +
    lag(lag_education, 2:3) + lag(lag_log_gdp_pc, 2:3) +
    lag(lag_log_trade, 2:3) + lag(lag_oda_gni, 2:3),
  data = pdata_no_soc, effect = "individual", model = "twosteps",
  transformation = "ld", collapse = TRUE
)

gmm_no_ssa <- update(gmm_no_soc, data = pdata_no_ssa)
gmm_no_mena <- update(gmm_no_soc, data = pdata_no_mena)
gmm_no_oil <- update(gmm_no_soc, data = pdata_no_oil)

library(modelsummary)

# Combine models
models_robust <- list(
  "No Post-Communist" = gmm_no_soc,
  "No SSA" = gmm_no_ssa,
  "No MENA" = gmm_no_mena,
  "No Oil Exporters" = gmm_no_oil
)

# Create diagnostics rows
diagnostics_robust <- tribble(
  ~term,                ~`No Post-Communist`, ~`No SSA`, ~`No MENA`, ~`No Oil Exporters`,
  "Observations",       nobs(gmm_no_soc), nobs(gmm_no_ssa), nobs(gmm_no_mena), nobs(gmm_no_oil),
  "Hansen p-value",     formatC(gmm_no_soc$test$hansen$p.value, digits = 3),
  formatC(gmm_no_ssa$test$hansen$p.value, digits = 3),
  formatC(gmm_no_mena$test$hansen$p.value, digits = 3),
  formatC(gmm_no_oil$test$hansen$p.value, digits = 3),
  "Sargan p-value",     formatC(gmm_no_soc$test$sargan$p.value, digits = 3),
  formatC(gmm_no_ssa$test$sargan$p.value, digits = 3),
  formatC(gmm_no_mena$test$sargan$p.value, digits = 3),
  formatC(gmm_no_oil$test$sargan$p.value, digits = 3),
  "AR(1) p-value",      formatC(gmm_no_soc$test$ar1$p.value, digits = 3),
  formatC(gmm_no_ssa$test$ar1$p.value, digits = 3),
  formatC(gmm_no_mena$test$ar1$p.value, digits = 3),
  formatC(gmm_no_oil$test$ar1$p.value, digits = 3),
  "AR(2) p-value",      formatC(gmm_no_soc$test$ar2$p.value, digits = 3),
  formatC(gmm_no_ssa$test$ar2$p.value, digits = 3),
  formatC(gmm_no_mena$test$ar2$p.value, digits = 3),
  formatC(gmm_no_oil$test$ar2$p.value, digits = 3)
)

modelsummary(
  models_robust,
  stars = TRUE,
  gof_omit = ".*",  # omit default GOF
  add_rows = diagnostics_robust,
  output = "latex",
  title = "Robustness Check: System GMM Estimates Excluding Country Groups",
  escape = FALSE
)


extract_pgmm_diagnostics <- function(mod) {
  s <- summary(mod, robust = TRUE)
  
  obs <- nobs(mod)
  
  # Sargan (one-step)
  sargan_test <- tryCatch(sargan(mod, weights = "onestep"), error = function(e) NA)
  sargan_p <- if (inherits(sargan_test, "htest")) sargan_test$p.value else NA
  
  # Hansen (two-step)
  hansen_test <- tryCatch(sargan(mod, weights = "twosteps"), error = function(e) NA)
  hansen_p <- if (inherits(hansen_test, "htest")) hansen_test$p.value else NA
  
  ar1 <- tryCatch(mtest(mod, 1)$p.value, error = function(e) NA)
  ar2 <- tryCatch(mtest(mod, 2)$p.value, error = function(e) NA)
  
  c(
    Observations = obs,
    Hansen_p = hansen_p,
    Sargan_p = sargan_p,
    AR1_p = ar1,
    AR2_p = ar2
  )
}

# Apply to all models
stats_df <- t(sapply(models_robust, extract_pgmm_diagnostics))
stats_df <- round(as.data.frame(stats_df), 3)
stats_df

library(dplyr)
library(ggplot2)

# Filter for 5-year intervals
panel_subset <- panel_data %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020))

# Compute the mean emigration rate across all countries per year
emigration_avg <- panel_subset %>%
  group_by(year) %>%
  summarise(mean_emigration_rate = mean(emigration_rate.x, na.rm = TRUE))

library(ggplot2)
library(scales)
ggplot(emigration_avg_clean, aes(x = year, y = mean_emigration_rate)) +
  geom_line(size = 0.6, color = "black") +
  geom_point(size = 1.8, color = "black") +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +  # X-axis baseline
  geom_vline(xintercept = min(emigration_avg_clean$year), color = "black", size = 0.3) +  # Y-axis baseline
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    limits = c(0, 0.05)  # y-axis from 0% to 5%
  ) +
  labs(
    x = "Year",
    y = "Emigration Rate (%)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 9, face = "plain"),
    axis.text = element_text(size = 8, color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("emigration_rate_plot.pdf", width = 5.5, height = 4)




#PPLOT CORRUPTION

# Load necessary libraries


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)

# Step 1: Wide format (years as columns)
corruption_wide <- corruption %>%
  filter(year %in% c("2000", "2020")) %>%
  pivot_wider(names_from = year, values_from = corruption_estimate, names_prefix = "index_")

# Step 2: Filter only developing countries
corruption_wide <- corruption_wide %>%
  filter(origin %in% developing_countries$iso3)

# Step 3: Sample 50 countries
set.seed(123)
corruption_sample <- corruption_wide %>%
  sample_n(162) %>%
  mutate(
    index_2000 = as.numeric(index_2000),
    index_2020 = as.numeric(index_2020),
    
    # Step 4: Rescale from [-2.5, 2.5] to [0, 1]
    index_2000_rescaled = rescale(index_2000, to = c(0, 1), from = c(-2.5, 2.5)),
    index_2020_rescaled = rescale(index_2020, to = c(0, 1), from = c(-2.5, 2.5))
  )

ggplot(corruption_sample, aes(x = index_2000_rescaled, y = index_2020_rescaled)) +
  geom_point(color = "black", size = 1.0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_text_repel(aes(label = `Country Name`), size = 3) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(
    x = "Index 2000",
    y = "Index 2020",
    caption = "Source: Own calculation on the basis of WGI data"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 8),  # Make "Index 2000/2020" smaller
    axis.text = element_text(size = 9),    # Tick labels smaller too
    plot.caption = element_text(size = 8)
  )

# Save as PDF (recommended for Overleaf)
ggsave("control_corruption45.pdf", width = 5.5, height = 4)



