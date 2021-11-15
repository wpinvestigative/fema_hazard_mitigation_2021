# Data dictionary:

------

### county_summaries.csv

Summarized county data:

* GEOID - County FIPS code
* status - HMGP project status
* projects - Project count
* projectAmount - Total project sums
* federalShareObligated - Total federal share of project sums
* average_months_approval -Average number of months between disaster date and initial approval date
* average_months_closed - Average number of months between initial approval date and project closure
* average_months_disaster - Average number of months between disaster date and project closure
* average_disasters_per_project - Average number of disasters (specific to initial disaster)
* NAME - County and state names

### enhanced_fema_data.csv

A handful of FEMA [Hazard Mitigation Assistance Projects]((https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects-v2)) open data with enhanced data added from the [Disaster Declarations Summaries]((https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2)) to determine which designated disasters occurred between initial disaster date, project approval date, and project closing date.

* projectIdentifier - FEMA project ID
* projectType - Type of Hazard Mitigation project
* GEOID - County FIPS code
* projectAmount - Total cost of the project as submitted in the project application	
* federalShareObligated - Federal share obligated for eligible activities	
* dateInitiallyApproved - Date the project was initially approved by FEMA. For HMGP projects, this date is based on the Regional Director's signature date for the first amendment that was approved	
* dateApproved - Date the project was approved by FEMA. For HMGP projects, this date is based on the Regional Director's signature date for the latest amendment that was approved	
* dateClosed - Date the project was closed by FEMA	
* approval_date - date initially approved unless doesn't exist, then date approved
* dateClosed2 - Date project was closed, or if missing, set to October 1, 2021 to measure still-open interval
* incidentType - initial declared disaster site that made funding available
* declarationDate - declaration date of declared disaster
* disaster_approval - disaster ids between disaster and initial approval date
* disaster_approval_type - disaster types between disaster and initial approval date
* disaster_closed - disaster ids between project initial approval date and project close date
* disaster_closed_type - disaster types between project initial approval date and project close date
* disaster_wide - disaster ids between disaster declaration date and project close date
* disaster_wide_type - disaster types between disaster declaration date and project close date
* months_approval - number of months between disaster date and initial approval date
* months_closed - number of months between project initial approval date and months closed
* months_disaster - number of months between disaster date and months closed
* declaration_year - year of declared disaster
* incident_count - count of disasters between disaster declaration date and project close date only related to incidentType of originating disaster
