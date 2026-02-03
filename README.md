EHR Analysis: Oncology Care Disparities in American Indian (AIAN) Rural Populations

📋 Project Objective
This project utilizes Electronic Health Record (EHR) data to investigate clinical care disparities for American Indian and Alaska Native (AIAN) oncology patients within a rural healthcare catchment area spanning Montana, Wyoming, and the Dakotas. The study aims to:
Characterize Care Patterns: Analyze diagnostic prevalence, insurance utilization, and the timing of treatment initiation across 666 clinical encounters.
Identify Structural Barriers: Quantify the impact of rurality—with 75% of the cohort residing in rural areas—on healthcare access.
Support Equity: Inform culturally congruent strategies to improve clinical trial coordination and reduce geographic barriers to specialized care.

🛠️ Data Science Workflow & Key Skills
This project demonstrates a rigorous data science approach to sensitive healthcare research:
Advanced Data Engineering (R):
Temporal Feature Engineering: Calculated "days from diagnosis to admission" to identify patterns of diagnostic latency.
Data Normalization: Performed complex factor regrouping to categorize varied insurance types and 24 different regional partnerships into standardized analytical variables.
Statistical Analysis & Results:
Cohort Profiling: Managed a dataset of 108 unique patients with high tribal diversity, primarily representing the Crow/Apsaalooke (41.7%) and Northern Cheyenne (12.0%) nations.
Diagnostic Prevalence: Rather than using broad groupings, the analysis identified specific high-impact diagnoses: Colorectal Cancer (18.3%), Multiple Myeloma (10.7%), and Breast Cancer (10.4%).
Inferential Statistics: Conducted Chi-square analysis to demonstrate that rurality (p=0.04) and appointment type (p < 0.001) are significantly associated with the timing of cancer diagnosis.
Geospatial Insights:
Mapped patient distribution against cancer center locations, identifying that patients from northern and eastern counties (e.g., Blaine, Valley, Roosevelt) average 7 visits each, with one-way commutes of 170–350 miles.

📊 Key Findings & Impact

Diagnostic Latency: While most patients are diagnosed on the day of admission, those with staggered timelines face an average delay of 18.9 days when diagnosed after admission.
Access Disparity: Rural residency imposes a "logistical burden" of over five hours of travel time per visit for frontier communities.
Insurance Complexity: Over half of the encounters (54.8%) rely on the Indian Health Service as a secondary payer, highlighting the administrative complexity of coordinating rural oncology care.
Research Representation: Through the healthcare system–Tribal–academic partnership, AIAN participation in clinical trials reached 4.5%, significantly exceeding the national average of ~1%.

💻 Tech Stack
Language: R (Version 4.5.2) 
Libraries: tidyverse, ggplot2, lubridate, sf, readxl
Standards: ICD-10 Diagnosis Codes, RUCA Geographic Classifications

<img width="975" height="568" alt="image" src="https://github.com/user-attachments/assets/66444da5-6b21-4095-84a4-68798a01f998" />
