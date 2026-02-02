library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Import Excel files and add year column
mainreport <- read_excel("C:/Users/Jiahui/Documents/Loop/BillingsClinic/Pfizer/PFIZER report ONC patients Dec 1 2023 to Nov 30 2025.xlsx", sheet = "Main Report")

## Create new columns based on analysis requirement
# Create the date column for admit, dx and death
mainreport <- mainreport %>%
  mutate(
    admit_dt = as.POSIXct(`Admit Date & Time`,
                          origin = "1899-12-30",
                          tz = "America/Denver"), # Mountain time
    dx_dt = as.POSIXct(`Diagnosis Date & Time`,
                       origin = "1899-12-30",
                       tz = "America/Denver"),
    die_dt = as.POSIXct(`External Deceased Date & Time`,
                        origin = "1899-12-30",
                        tz = "America/Denver")
  )
mainreport <- mainreport %>% # only keep the date
  mutate(
    admit_date = as.Date(admit_dt),
    dx_date    = as.Date(dx_dt),
    die_date    = as.Date(die_dt),
  )

# Create the visit order by the admit time and patient identity
mainreport <- mainreport %>%
  arrange(`MRN- Community`, admit_date) %>%   
  group_by(`MRN- Community`) %>%
  mutate(
    Visit_Order = row_number()
  ) %>%
  ungroup() %>%
  arrange(`MRN- Community`, admit_date)

# Create the first diagnosis time by the dx time and patient identity
mainreport <- mainreport %>%
  arrange(`MRN- Community`, dx_date) %>%   # or admit_dt if you kept POSIXct
  group_by(`MRN- Community`) %>%
  mutate(
    DX_Order = row_number()
  ) %>%
  ungroup() %>%
  arrange(`MRN- Community`, admit_date)

mainreport <- mainreport %>%
  group_by(`MRN- Community`) %>%
  mutate(
    First_dx_date = dx_date[DX_Order == 1][1]
  ) %>%
  ungroup()

## Age Interval: 25-50; 51-65; 66-75; 76+
mainreport <- mainreport %>%
  mutate(
    Age_interval = case_when(
      `Age- Years (Visit)` >= 25 & `Age- Years (Visit)` <= 50 ~ "25–50",
      `Age- Years (Visit)` >= 51 & `Age- Years (Visit)` <= 65 ~ "51–65",
      `Age- Years (Visit)` >= 66 & `Age- Years (Visit)` <= 75 ~ "66–75",
      `Age- Years (Visit)` >= 76                         ~ "76+",
      TRUE ~ NA_character_
    )
  )

## State and County 
mainreport <- mainreport %>%
  mutate(
    State = str_trim(str_extract(`Person Address- State & County`, "^[^-]+")),
    County_Name = str_trim(str_extract(`Person Address- State & County`, "(?<=-).*"))
  )


#### Calculate the time difference between dignosis time and admitted time
mainreport <- mainreport %>%
  mutate(
    days_dx_to_admit = as.numeric(admit_date - dx_date),
    days_dx_to_die = as.numeric(die_date - First_dx_date) # Days from death to first dx date
  )
mainreport <- mainreport %>%
  mutate(
    admit_dx = ifelse(days_dx_to_admit>0,"Admit After DX", "Same day"),
    admit_dx = ifelse(days_dx_to_admit<0,"Admit Before DX", admit_dx)
  )

#################################### Subset the data for the American Indian 
AIAN <- mainreport %>%
  filter(Race == 'American Indian or Alaska Native')

# Create new insurance & Encounter types
AIAN <- AIAN %>%
  mutate(
    Primary_Regrouped = fct_collapse(`Primary Insurance Name from encntr_plan_reltn`,
                                     "Medicaid/Medicare" = c("MEDICARE TRADITIONAL", "AARP", "MEDICAID MT"),
                                     "Federal"  = c("MAIL HANDLERS"),
                                     "Private"  = c("BCBS MT", "HUMANA", "UNITED HEALTHCARE", "AETNA", 
                                                    "ALLEGIANCE", "UMR", "BOON CHAPMAN", "CIGNA", 
                                                    "BCBS MT BLC BLUE", "EBMS", "UNITED HEALTHCARE OONWD")
    ),
    Secondary_Regrouped = fct_collapse(`Secondary Insurance Name from encntr_plan_reltn`,
                                       "Medicaid/Medicare" = c("MEDICARE TRADITIONAL","MEDICAID MT", "MEDICAID WY"),
                                       "Federal"  = c("MAIL HANDLERS", "APWU"),
                                       "Indian Health Service" = c("INDIAN HEALTH SERVICES MT"),
                                       "Senior Supplement" = c("AETNA SENIOR SUPPLEMENT"),
                                       "Charity" = c("CHARITY"),
                                       "Private/SELF PAY"  = c("BCBS MT", "MUTUAL OF OMAHA", "AETNA", 
                                                      "EBMS", "COMMERCIAL", "GEHA BILL TO UNITEDHEALTH INTGRD", 
                                                      "HUMANA", "UMR", "AMERICO FINANCIAL","SELF PAY")
    ),
    Appointment_Regrouped = fct_collapse(`Appointment Type- Short`,
                                         "Exam/Visit/Consult" = c("HEM ONC Brf or Ext Returns", "Return Appointment Oncology","Referral to HEM/ONC",
                                                                  "RAD ONC New","RAD ONC TX Brief", "Return Appointment Oncology - GYN/ONC",
                                                                  "RAD ONC Patient Ed w/ Nursing", "RAD ONC Brief", "GYN ONC New", "GYN ONC Return",
                                                                  "RAD ONC Brief Virtual Visit","Gyn Onc Phone Call"),
                                         "Procedure/Lab" = c("RAD ONC CT Simulation", "HEM ONC Bone Marrow Procedure","RAD ONC HDR CT Simulation","HEM ONC Oncology Lab"),
                                         "Treatment" = c("RAD ONC SBR Treatment", "RAD ONC Treatment w/ Physician","RAD ONC Treatment","HEM ONC","RAD ONC HDR Treatment")
                                       )
  )

AIAN <- AIAN %>%
  mutate(
    Rural = ifelse(`RUCA Codes Primary` %in% c(10, 2), "Rural", "Urban")
  )

# Descriptive Analysis
# Patient level 
AIAN_patient <- AIAN %>%
  group_by(`MRN- Community`) %>%
  slice(1) %>%
  ungroup()

patient_descriptive <- list(
  Sex = AIAN_patient %>% count(Sex),
  Race = AIAN_patient %>% count(Race),
  Ethnic_Group = AIAN_patient %>% count(`Ethnic Group`),
  RUCA = AIAN_patient %>% count(`RUCA Codes Primary`)
)

# Hospital appointment visit level 
Diagnosis_description <- list(
  dx = AIAN %>% count(`Diagnosis Code`)
)

visit_descriptive <- list(
  encounter = AIAN %>% count(`Encounter Type`),
  Pri_insurance = AIAN %>% count(`Primary_Regrouped`),
  Sec_insurance = AIAN %>% count(`Secondary_Regrouped`),
  Age_interval = AIAN %>% count(Age_interval),
  Appointment_type = AIAN %>% count(Appointment_Regrouped)
)

# visit day and dx day 
visit_day_dx_rural <- AIAN %>% 
  group_by(admit_dx, Rural) %>%
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_admit), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_admit), na.rm = TRUE),
    median_day = median(abs(days_dx_to_admit), na.rm = TRUE)
  ) # Check the time gap between visits day and dignosis day differentiated by Rural/Urban patients

visit_day_dx_appoint <- AIAN %>%
  group_by(admit_dx,Appointment_Regrouped) %>%
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_admit), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_admit), na.rm = TRUE),
    median_day = median(abs(days_dx_to_admit), na.rm = TRUE)
  ) # Check the time gap between visits day and dignosis day differentiated by appointment types 


## Inferential analysis 
AIAN <- AIAN %>%
  mutate(admit_dx = as.factor(admit_dx))

# rural test
data_tabler <- table(AIAN$admit_dx, AIAN$Rural)
chisq.test(data_tabler)
mosaicplot(data_tabler, shade = TRUE, main = "Rural Patient")

# appointment type
data_tableapp <- table(AIAN$admit_dx, AIAN$Appointment_Regrouped)
chisq.test(data_tableapp)
mosaicplot(data_tableapp, shade = TRUE, main = "Apponitnment type", legend = FALSE)

# insurance 
data_tableinsp <- table(AIAN$admit_dx, AIAN$Primary_Regrouped)
chisq.test(data_tableinsp)
mosaicplot(data_tableinsp, shade = TRUE, main = "Primary Insurance")

data_tableinss <- table(AIAN$admit_dx, AIAN$Secondary_Regrouped)
chisq.test(data_tableinss)
mosaicplot(data_tableinss, shade = TRUE, main = "Secondary Insurance")

# age
data_tableage <- table(AIAN$admit_dx, AIAN$Age_interval) # NOT sig
chisq.test(data_tableage)
mosaicplot(data_tableage, shade = TRUE, main = "Primary Insurance")

# gender
data_tablegen <- table(AIAN$admit_dx, AIAN$Sex) # sig but not intereted to the results 
chisq.test(data_tablegen)
mosaicplot(data_tablegen, shade = TRUE, main = "Gender")

##### Geographic plot of patient population to the cancer center 
## Load required libraries
library(usmap)      # state boundaries
library(zipcodeR)   # ZIP → lat/long
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)    # For better label placement

# 1. Prepare Patient Data
AIAN <- AIAN %>%
  mutate(
    zip = as.character(`Person Address- Zip Code`)
  )

# Join the latitude and longitude
zip_data <- zipcodeR::zip_code_db

AIAN_geo <- AIAN %>%
  left_join(
    zip_data %>% 
      select(zipcode, lat, lng, state),
    by = c("zip" = "zipcode")
  )

# MODIFIED: Filter for all three states
target_states <- c("MT", "WY", "ND")

selected_patients <- AIAN_geo %>%
  filter(state %in% target_states)

# MODIFIED: Group by both State and County to prevent mismatches
patient_counts <- selected_patients %>%
  group_by(state, County_Name) %>%
  summarise(
    n_patients = n_distinct(`MRN- Community`),
    .groups = "drop"
  )

# 2. Fetch Geographic Data for all three states
options(tigris_use_cache = TRUE)

# Fetch counties for MT, WY, and ND
tri_state_counties <- counties(
  state = target_states,
  year = 2022,
  cb = TRUE
)

# Fetch state outlines
tri_state_outlines <- states(cb = TRUE, year = 2022) %>%
  filter(STUSPS %in% target_states)

# 3. Process Map Data
# Note: Tigris uses 'STUSPS' for state abbreviations and 'NAME' for county names
tri_state_map_data <- tri_state_counties %>%
  mutate(
    County_Name = NAME,
    state = STUSPS
  ) %>%
  left_join(
    patient_counts,
    by = c("state", "County_Name") # Join on both to be precise
  ) %>%
  mutate(
    n_patients = if_else(is.na(n_patients), 0L, n_patients)
  )

# 4. Define Clinic Location (Billings, MT)
clinic_loc <- data.frame(
  name = "Cancer Center",
  lat = 45.79,
  lng = -108.51
) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(tri_state_outlines))

# 5. Prepare the State Outlines for a clean border
tri_state_outlines <- states(cb = TRUE, year = 2022) %>%
  filter(STUSPS %in% target_states)

# 6. Optimized Plotting
ggplot() +
  # LAYER 1: Background and County Fills
  # This sets a base color and fills in patient data
  geom_sf(
    data = tri_state_map_data,
    aes(fill = ifelse(n_patients > 0, n_patients, NA)),
    color = "gray50",  # Very light county lines
    linewidth = 0.1    # Thin lines so they don't clutter the map
  ) +
  
  # LAYER 2: Bold State Outlines
  # This "wraps" the states to make the map look organized
  geom_sf(
    data = tri_state_outlines,
    fill = "transparent",
    color = "black",   # Stronger state borders
    linewidth = 0.8    # Thicker line for visual structure
  ) +
  
  # LAYER 3: Clinic Location
  geom_sf(
    data = clinic_loc,
    shape = 18,
    size = 5,
    color = "red"
  ) +
  
  # LAYER 4: Clinic Label (using ggrepel logic)
  geom_sf_text(
    data = clinic_loc,
    aes(label = name),
    size = 5,
    fontface = "bold",
    nudge_y = 0.4
  ) +
  
  # Color scale
  scale_fill_gradient(
    name = "Patients Count",
    low = "#deebf7",
    high = "#08519c",
    na.value = "white" # Keeps counties with 0 patients clean/white
  ) +
  theme(
    legend.text = element_text(size = 10),   # Size of the numbers
    legend.title = element_text(size = 12)  # Size of "Number of Patients"
  )+
  
  # Clean up theme
  coord_sf(datum = NA) + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Regional Patient Distribution",
    subtitle = "Service Area",
    caption = "Data based on unique MRN per county"
  )


