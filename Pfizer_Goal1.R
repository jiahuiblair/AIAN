library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Import Excel files and add year column
mainreport <- read_excel("C:/Users/Jiahui/Documents/Loop/BillingsClinic/Pfizer/PFIZER report ONC patients Dec 1 2023 to Nov 30 2025.xlsx", sheet = "Main Report")
#Newreport <- read_excel("C:/Users/Jiahui/Documents/Loop/BillingsClinic/Pfizer/PFIZER report AI_AN ONC patients IP and OP visits.xlsx")

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

# Create the visit order by the admit time and patinent identity
mainreport <- mainreport %>%
  arrange(`MRN- Community`, admit_date) %>%   # or admit_dt if you kept POSIXct
  group_by(`MRN- Community`) %>%
  mutate(
    Visit_Order = row_number()
  ) %>%
  ungroup() %>%
  arrange(`MRN- Community`, admit_date)

# Create the first diagnosis time by the dx time and patinent identity
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

# Age Interval: 25-50; 51-65; 66-75; 76+
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

### ICD grouping 
### Group ICD 
icd_groups <- tibble(
  group = c(
    "C00-C14",
    "C15-C26",
    "C30-C39",
    "C40-C41",
    "C43-C44",
    "C45-C49",
    "C50-C50",
    "C51-C58",
    "C60-C63",
    "C64-C68",
    "C69-C72",
    "C73-C75",
    "C76-C80",
    "C7A-C7A",
    "C7B-C7B",
    "C81-C96",
    "D00-D09",
    "D10-D36",
    "D37-D48",
    "D3A-D3A",
    "D49-D49"
  ),
  start = c(
    "C00","C15","C30","C40","C43","C45","C50","C51","C60","C64",
    "C69","C73","C76","C7A","C7B","C81","D00","D10","D37","D3A","D49"
  ),
  end = c(
    "C14","C26","C39","C41","C44","C49","C50","C58","C63","C68",
    "C72","C75","C80","C7A","C7B","C96","D09","D36","D48","D3A","D49"
  )
)

## assign function 
assign_icd_group <- function(code) {
  if (is.na(code)) return(NA_character_)
  
  base <- str_extract(code, "^[A-Z][0-9A-Z]{2}")
  if (is.na(base)) return(NA_character_)
  
  match <- icd_groups %>%
    filter(start <= base & end >= base)
  
  if (nrow(match) == 0) {
    return("Other / Out of range")
  } else {
    return(match$group[1])
  }
}

# Assign group 
mainreport <- mainreport %>%
  mutate(Diagnosis_Group = sapply(`Diagnosis Code`, assign_icd_group))



# State and County 
mainreport <- mainreport %>%
  mutate(
    State = str_trim(str_extract(`Person Address- State & County`, "^[^-]+")),
    County_Name = str_trim(str_extract(`Person Address- State & County`, "(?<=-).*"))
  )


# Calculate the time difference between dignosis time and admitted time
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




# Subset the data for the American Indian 
AIAN_allDX <- mainreport %>%
  filter(Race == 'American Indian or Alaska Native')

# # Create the ICD group variable 
# valid_groups <- AIAN_allDX %>%
#   count(Diagnosis_Group) %>%
#   filter(n > 20) %>%
#   pull(Diagnosis_Group)
# 
# patient_dx <- AIAN_allDX %>%
#   filter(Diagnosis_Group %in% valid_groups) %>%
#   distinct(`MRN- Community`, Diagnosis_Group) %>%
#   mutate(value = 1) %>%
#   pivot_wider(
#     names_from  = Diagnosis_Group,
#     values_from = value,
#     values_fill = list(value = 0)
#   )
# 
# colnames(patient_dx) <- c(
#   "MRN- Community",
#   paste0(
#     "Dx_",
#     make.names(colnames(patient_dx)[-1])
#   )
# )
# 
# AIAN_allDX <- AIAN_allDX %>%
#   left_join(patient_dx, by = "MRN- Community") %>%
#   mutate(
#     across(
#       starts_with("Dx_") & where(is.numeric),
#       ~ replace_na(., 0)
#     )
#   )

AIAN <- AIAN_allDX %>%
  filter(Include == 'Yes')

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
                                       ),
    Ethnic_Regroup = fct_collapse(`Ethnic Group`,
                                  "Crow/Apsaalooke" = c("Crow/Apsaalooke"),
                                  "Northern Cheyenne/Tsistsistas" = c("Northern Cheyenne/Tsistsistas")
                                  )
  )

AIAN <- AIAN %>%
  mutate(
    Rural = ifelse(`RUCA Codes Primary` %in% c(10, 2), "Rural", "Urban")
  )

# Merge with the Newreport
# AIANmerge <- merge(AIAN, Newreport_clean[, c("MRN- Community", "Admit Date & Time", "Diagnosis Date & Time", "Appointment Type- Short", "Appointment Status- Short", "Medical Service", "Encounter Type Class", "Encounter Status")], 
#                   by = c("MRN- Community", "Admit Date & Time", "Diagnosis Date & Time"), all.x = TRUE)

# Remove duplicates based on your three key columns
# AIANmerge_clean <- AIANmerge %>%
#  distinct(`MRN- Community`, `Admit Date & Time`, .keep_all = TRUE)


# Descriptive 
# Patient level 
patient_demo <- AIAN %>%
  arrange(`MRN- Community`, Visit_Order) %>%
  group_by(`MRN- Community`) %>%
  summarise(
    Sex = unique(Sex),
    Race = unique(Race),
    Ethnic_Group = unique(`Ethnic Group`),
    RUCA = unique(`RUCA Codes Primary`),
    age_mean = mean(`Age- Years (Visit)`),
    age_sd = sd(`Age- Years (Visit)`),
    age_gap = max(`Age- Years (Visit)`)-min(`Age- Years (Visit)`),
    #primary_insurance = unique(`Primary Insurance Name from encntr_plan_reltn`),
    #secondary_insurance = unique(`Secondary Insurance Name from encntr_plan_reltn`),
    .groups = "drop"
  )

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
  )

visit_day_dx_appoint <- AIAN %>%
  group_by(admit_dx,Appointment_Regrouped) %>%
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_admit), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_admit), na.rm = TRUE),
    median_day = median(abs(days_dx_to_admit), na.rm = TRUE)
  )

visit_day_dx_rural_appoint <- AIAN %>%
  group_by(admit_dx, Rural, Appointment_Regrouped) %>%
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_admit), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_admit), na.rm = TRUE),
    median_day = median(abs(days_dx_to_admit), na.rm = TRUE)
  )

visit_day_dx_ICD <- AIAN %>%
  group_by(admit_dx, `Diagnosis Code`) %>%
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_admit), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_admit), na.rm = TRUE),
    median_day = median(abs(days_dx_to_admit), na.rm = TRUE)
  )

# days of death 
death_day <- AIAN %>%
  filter(days_dx_to_die>0) %>%
  group_by(`MRN- Community`) %>%  
  slice(1) %>%
  ungroup() %>% 
  summarise(
    count = n(),
    avrage_day = mean(abs(days_dx_to_die), na.rm = TRUE),
    sd_day = sd(abs(days_dx_to_die), na.rm = TRUE),
    median_day = median(abs(days_dx_to_die), na.rm = TRUE)
  )

## Calculate the patients count from northeast 
northeastPV <- AIAN %>%
  filter(`Person Address- State & County` %in% c("MT - Roosevelt", "MT - Blaine", "MT - Valley", "MT - Sheridan", "ND - Williams")) %>%
  group_by(`MRN- Community`) %>%
  slice(1) %>%
  ungroup() # average visits per patient = 134/20 = 6.7

## Inferential 
AIAN <- AIAN %>%
  mutate(admit_dx = as.factor(admit_dx))

dx_vars <- AIAN %>%
  select(starts_with("Dx_")) %>%
  names()

chisq_safe <- function(dx_var, data) {
  tbl <- table(data[[dx_var]], data$admit_dx)
  test <- suppressWarnings(chisq.test(tbl))
  tibble(
    dx_variable = dx_var,
    chi_sq = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value
  )
}
chi_results <- map_dfr(dx_vars, chisq_safe, data = AIAN)

# rural test
data_tabler <- table(AIAN$admit_dx, AIAN$Rural)
chisq.test(data_tabler)
mosaicplot(data_tabler, shade = TRUE, main = "Rural Patient")

mosaicplot(data_tabler, shade = TRUE, legend = FALSE)


# appointment type
data_tableapp <- table(AIAN$admit_dx, AIAN$Appointment_Regrouped)
chisq.test(data_tableapp)
mosaicplot(data_tableapp, shade = TRUE, main = "Apponitnment type", legend = FALSE)

data_tableappr <- table(AIAN$Rural, AIAN$Appointment_Regrouped) # No sig 
chisq.test(data_tableappr)

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

## Three states: MT, WY and ND
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
    subtitle = "MT, WY, and ND Service Area",
    caption = "Data based on unique MRN per county"
  )








# ## Plot the Montana patients
# library(usmap)      # state boundaries
# library(zipcodeR)   # ZIP → lat/long
# library(tigris)
# 
# AIAN <- AIAN %>%
#   mutate(
#     zip = as.character(`Person Address- Zip Code`)
#   )
# 
# # Join the latitude and longtitude
# zip_data <- zipcodeR::zip_code_db
# 
# AIAN_geo <- AIAN %>%
#   left_join(
#     zip_data %>% 
#       select(zipcode, lat, lng, state),
#     by = c("zip" = "zipcode")
#   )
# 
# MT_patients <- AIAN_geo %>%
#   filter(state == "MT")
# 
# mt_county_map_data <- MT_patients %>%
#   group_by(County_Name) %>%
#   summarise(
#     n_patients = n_distinct(`MRN- Community`),
#     .groups = "drop"
#   )
# 
# options(tigris_use_cache = TRUE)
# 
# mt_counties <- counties(
#   state = "MT",
#   year = 2022,
#   cb = TRUE
# )
# 
# mt_counties <- mt_counties %>%
#   mutate(
#     County_Name = gsub(" County", "", NAME)
#   )
# 
# mt_map_data <- mt_counties %>%
#   left_join(
#     mt_county_map_data,
#     by = "County_Name"
#   ) %>%
#   mutate(
#     n_patients = if_else(is.na(n_patients), 0L, n_patients)
#   )
# 
# mt_state <- states(cb = TRUE, year = 2022) %>%
#   dplyr::filter(STUSPS == "MT")
# 
# library(sf)
# library(ggrepel) # For better label placement
# 
# # 1. Create a data frame for the clinic location
# clinic_loc <- data.frame(
#   name = "Cancer Center",
#   lat = 45.79,
#   lng = -108.51
# ) %>%
#   st_as_sf(coords = c("lng", "lat"), crs = st_crs(mt_state))
# 
# # 2. Update the Plot
# ggplot() +
#   ## Montana outline
#   geom_sf(
#     data = mt_state,
#     fill = "gray95",
#     color = "black",
#     linewidth = 0.6
#   ) +
#   
#   ## Counties with patients
#   geom_sf(
#     data = mt_map_data,
#     aes(fill = ifelse(n_patients > 0, n_patients, NA)),
#     color = "white",
#     linewidth = 0.2
#   ) +
#   
#   ## Add the Clinic as a Star
#   geom_sf(
#     data = clinic_loc,
#     shape = 18,      # Diamond/Star shape (18 or 8 work well)
#     size = 4,
#     color = "red"    # Use a high-contrast color
#   ) +
#   
#   ## Add the Label (optional but recommended)
#   geom_sf_text(
#     data = clinic_loc,
#     aes(label = name),
#     size = 3,
#     fontface = "bold",
#     nudge_y = 0.3    # Moves text slightly above the star
#   ) +
#   
#   scale_fill_gradient(
#     name = "Patients Count",
#     low = "#deebf7",
#     high = "#08519c",
#     na.value = "transparent"
#   ) +
#   
#   theme(
#     legend.text = element_text(size = 4),   # Size of the numbers
#     legend.title = element_text(size = 6)  # Size of "Number of Patients"
#   ) +
#   
#   coord_sf(datum = NA) + 
#   
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()
#   ) +
#   labs(
#     title = "Geographic Distribution of Patients by County in Montana",
#     subtitle = "Red star indicates Billings Clinic Cancer Center",
#     caption = "Out-of-state participants (Wyoming, n=1; North Dakota, n=1) are not represented in the county-level distribution."
#   )