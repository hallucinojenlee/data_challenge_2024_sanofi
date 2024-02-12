
<img src="Output/banner.gif" width="800" height="500"/>

![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![GitHub Releases](https://img.shields.io/badge/available-syntax-blue)


# Sanofi Data Challenge

This is the LSHTM MSc Health Data Science 20224 - Sanofi for Data Challenge project

## Description
This project want to capture the pre- and post-COVID-19 sesonal Inflluenza and RSV pattern for Northern and Southern Hemisphere countries 

## Project items
### Dataset
The main data of this project consist of:
- `Consolidated_dataset_MASTER.xlsx` = dataset consolidation for Flu and RSV timeseries data 2017-2023 from France, Brazil, England, Australia, US, and Turkey

### Outputs
All of the output for the report and presentation including 
- Graphs in `graphs`
- Tables in `tables` 

### R Scripts
All of the R scripts used to produce the output in the report and presentation. First 5 characters of files name are codes for regions/countries 
- The global data =  `01`
- England = `02`
- France = `03`
- Turkey = `04`
- USA = `05`
- Australia = `06`
- Brazil = `07`

## Data and Scraping
### Data Dictionary
The main data

| Variable       | Description |
| :--------------| :---------- |
|`Country`       |Name of the country |
|`Disease`        |Influenza or RSV |
|`Year`           |Year |
|`Month`          |Month |
|`Week_num`       |Week Num  |
|`Week_type`      |Week type |
|`Week_date`      |Date of the week |
|`hospitalisation_num` |Number of hospitalisation admission |
|`hospitalisation_rate`|Hospitalisation rate per 100k population |
|`Flu_A`  |Flu A hospitalisation admission |
|`Flu_B`  | Flu B hospitalisation admission |
|`Population` |Number of population per year |

### Data Source

| Country | Source                                                                                                                    | Metric              | Data Extraction        | Timeframe |
| :------ | :------------------------------------------------------------------------------------------------------------------------ | :------------------ | :--------------------- | :-------- |
| GLobal  | [GISRS](https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/influenza-surveillance-outputs)  | Hospital admissions | Download .csv          | 2019-2023 |
| England | [National Flu and COVID-19 surveillance reports](https://www.gov.uk/government/collections/weekly-national-flu-reports)   | Rates per 100k      | Download .csv and .pdf | 2017-2023 | 
| France  | [Bulletin épidémiologique grippe and Bulletin IRA](https://www.santepubliquefrance.fr)                                    | ICU admissions      | Download .csv and .pdf | 2018-2023 | 
| Turkey  | [Haftalık İnfluenza Raporları ](https://grip.saglik.gov.tr/tr/haftalik-influenza-raporu)                                  | Hospital admissions | Download .csv and .pdf | 2018-2023 | 
| USA     | [CDC RESP-NET](https://www.cdc.gov/surveillance/resp-net/dashboard.html)                                                  | Rates per 100k      | Download .csv          | 2017-2023 | 
| Australia  | [Australian Influenza Surveillance Reports ](https://www.health.gov.au/resources/collections/australian-influenza-surveillance-reports-2023)    | Hospital admissions    | Download .csv   | 2017-2023 | 
| Brazil  | [OpenDataSUS](https://opendatasus.saude.gov.br)                                                                           | Hospital admissions | Download .csv          | 2019-2023 | 

### Scraping PDF
- [Digitizer](https://apps.automeris.io/wpd/)




