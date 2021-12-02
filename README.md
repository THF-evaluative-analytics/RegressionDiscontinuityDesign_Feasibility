# COVID Oximetry @home (CO<!-- -->@h):<br> Feasibility analysis for evaluation of CO<!-- -->@h using regression discontinuity


## Introduction

The code in this repository was used in an analysis aiming to identify whether an evaluation of COVID Oximetry @home (CO<!-- -->@h) was feasible using a regression discontinuity design (RDD).

The main objectives were:

* to determine the extent of any discontinuity in onboarding at age 65 — the standard operating procedures for CO<!-- -->@h provide recommended guidance that all patients aged 65 and over, or patients who are classed as clinically extremely vulnerable (CEV) should be onboarded (CEV patients were excluded for the purpose of this RDD study)
*	to determine whether there is sufficient statistical power for any evaluation to be able to detect the effect of CO<!-- -->@h on outcomes, given the discontinuity in onboarding.

Additionally, we investigated the possibility of:

*	a discontinuity at age 50. Other research streams in the evaluation programme had identified that some areas were using this as their cut-off for onboarding, instead of age 65
*	a discontinuity at later periods in the programme (from January 2021 onwards).


## Useful documents

1) Parry W & Lloyd T, COVID Oximetry @ home (CO<!-- -->@h): Feasibility analysis for evaluation of CO<!-- -->@h using regression discontinuity. The Health Foundation; 2021 [(https://health.org.uk/sites/default/files/2021-11/IAU_COVID_Oximetry@Home.pdf)](https://health.org.uk/sites/default/files/2021-11/Feasibility%20analysis%20for%20evaluation%20of%20Co%40h%20using%20regression%20discontinuity.pdf).
2) Lloyd T & Parry W, Statistical analysis protocol for an evaluation of COVID Oximetry @ home using a Regression Discontinuity Design. The Health Foundation; 2021 [(https://health.org.uk/sites/default/files/2021-03/SAP_Oximetry_Regression_Discontinuity_Design.pdf)](https://health.org.uk/sites/default/files/2021-03/SAP_Oximetry_Regression_Discontinuity_Design.pdf).
3) Cattaneo MD, Idrobo N & Titiunik R, *A Practical Introduction to Regression Discontinuity Designs: Volume II.* Draft. Cambridge University Press; 2018 [(https://cattaneo.princeton.edu/books/Cattaneo-Idrobo-Titiunik_2018_CUP-Vol2.pdf)](https://cattaneo.princeton.edu/books/Cattaneo-Idrobo-Titiunik_2018_CUP-Vol2.pdf).
4) NHS England & NHS Improvement, *Novel coronavirus (COVID-19) standard operating procedure - COVID Oximetry @home.* NHS England; 2021 [(https://www.england.nhs.uk/coronavirus/publication/novel-coronavirus-covid-19-standard-operating-procedure-covid-oximetry-home/)](https://www.england.nhs.uk/coronavirus/publication/novel-coronavirus-covid-19-standard-operating-procedure-covid-oximetry-home/).


## Data sources

The dataset described in the file [var_list.csv](Docs/var_list.csv) was compiled using data from the following sources:

* General Practice Extraction Service (GPES) Data for Pandemic Planning and Research (GDPPR)
* Second Generation Surveillance System (SGSS) COVID testing data
* COVID Oximetry @home (CO<!-- -->@h) programme patient recruitment/onboarding and discharge data
* National Health Application and Infrastructure Services (NHAIS) data on care home registrations


## Packages used in this project

* [here](https://cran.r-project.org/web/packages/here) - a package for working with relative file paths
* [tidyverse](https://cran.r-project.org/web/packages/tidyverse) - a collection of packages for manipulating, analysing and presenting data
* [data.table](https://cran.r-project.org/web/packages/data.table) - a package for fast processing and analysis of data
* [lubridate](https://cran.r-project.org/web/packages/lubridate) - a package for working with dates
* [pwr](https://cran.r-project.org/web/packages/pwr) - a package for conducting power analyses
* [scales](https://cran.r-project.org/web/packages/scales) - a package for formatting the scales of plots
* [DiagrammeR](https://cran.r-project.org/web/packages/DiagrammeR) - a package for drawing diagrams in R
* [DiagrammeRsvg](https://cran.r-project.org/web/packages/DiagrammeRsvg) - a package for exporting DiagrammeR graphs to SVG format
* [rsvg](https://cran.r-project.org/web/packages/rsvg) - a package for rendering SVG format images into other file formats
* [glue](https://cran.r-project.org/web/packages/glue) - a package for working with variables in strings


## How does it work

The folders contained in this repository store a reference document ([var_list.csv](Docs/var_list.csv) in [Docs/](Docs/)) which outlines the structure of the dataset used in the feasibility analysis, and two R scripts (in [R/](R/)) which were used to produce the outputs included in the associated report (the first document listed in 'Useful documents', above). The dataset itself cannot be shared as part of this repository due to data protection requirements.


### Ordered R scripts

1) [RDD_eligibility_flow_chart.R](R/RDD_eligibility_flow_chart.R) - this script produces counts of patients meeting various eligibility requirements for the feasibility analyses included in the report. The counts are used to populate the eligibility flow chart (Figure 1) included in the feasibility report. The flow chart is then drawn using the {DiagrammeR} package.

2) [RDD_feasibility.R](R/RDD_feasibility.R) - this script contains code to produce all of the plots contained in the feasibility report, along with the power analysis (presented in Table 1 of the report). There are three main types of plot produced by the script:

   1. bar charts of patient counts - for showing the numbers of patients testing positive to COVID and being onboarded onto CO<!-- -->@h over calendar time
   2. discontinuity plots - for showing how the proportion of patients onboarded onto CO<!-- -->@h (relative to those testing positive for COVID) varies by how old the patient was at the time they tested positive
   3. CCG-level plots - for showing the proportions of patients onboarded onto CO<!-- -->@h either side of the RDD cut-off by Clinical Commissioning Group (CCG).

The power analysis was conducted using the {pwr} package. A custom function (rddpwr) is used in the [RDD_feasibliity.R](R/RDD_feasibility.R) script to create estimates of the power attainable under a variety of scenarios where compliance with the standard operating procedure for  CO<!-- -->@h (the fourth document listed in 'Useful documents', above) is poor and rates of onboarding are low.


## License

This project is licensed under the [MIT License](LICENSE).


## Authors - please feel free to get in touch

* Will Parry, PhD - [website](http://willparry.net) / [on twitter](https://twitter.com/DrWillParry) / [on Linkedin](https://www.linked.com/in/DrWillParry) / [on github](https://github.com/DrWillParry)
* Therese Lloyd - [on twitter](https://twitter.com/ThereseTHF) / [on github](https://github.com/ThereseLloydTHF)

<br>
<br>
