dataQCtools: automate data cropping, QC, and more
====

Both the code and documentation in this repository are being actively developed for use by Tribes throughout the Tribal Exchange Network. For guides on two ways you can try this code out for your own data workflow, see the "How to use this code" section below.

## Overview

This code was developed by [Jess Kunke](https://github.com/jpierkunke), Kim Bray, and [Angie Reed](https://github.com/angiereed04468) to automate several steps of the Hoh Tribe's high-frequency water and air temperature data processing and quality control (QC). Specifically, this code currently does the following tasks:

- Reads in many different csv files from HOBO sensors at different sites and combines the data together into a common dataset.
- Crops the data to remove any measurements recorded before sensor deployment (between the time the sensor is turned on and the time it is fully installed in the field) or after sensor retrieval (between the time the sensor is removed from the field and brought back to the lab).
- Plots the cropped and raw data together to visualize how much data was cropped for each site.
- Generates QC plots for identifying suspicious values to be manually corrected. This step could be fully automated, but the current script allows the user to manually explore the results and make decisions.
- Reads in quality-controlled data (after the user has manually removed or corrected any spurious values), computes the seven-day average daily maximum temperature (7DADM), compares these values against water quality standards to determine which sites were impaired on which dates, and outputs a table to be included in the Tribal assessment report (TAR).


## Setting up

You will need to install several following R packages once before using this code for the first time on a given computer. To do this, you can run this line of code in RStudio:

```r
install.packages(c("zoo", "grid", "plotly", "readxl", "scales", "runner", 
"ggthemes", "tidyverse", "lubridate", "gridExtra", "flextable", "RColorBrewer", "magrittr"))
```

You can use the provided example data to practice using the code, then substitute your own data. Please see the articles and documentation for code examples and guidance.

There are two basic approaches you can take:

1. Copy and paste the scripts provided in the articles (see the top menu of [the dataQCtools page](https://tribalxg.github.io/dataQCtools/index.html)) into blank R scripts in RStudio on your computer. 
  - This approach does NOT require doing anything with git or GitHub or the `dataQCtools` repository. You can just visit the website and copy code to paste into an R script on your computer.
  - These script versions of the code do not use the `dataQCtools` package functions but rather combine the code in those functions into task-specific scripts. These can be modified for your particular equipment, data collection, analysis workflow, and other considerations.
  - Please feel free to reach out to [Jess](https://jpierkunke.github.io/) if you have questions or feedback about how to adapt the code for your equipment, workflow, etc.

2. Install the `dataQCtools` R package.
  - Ideally over time we will continue to incorporate a variety of workflows, equipment types, and more into the code to make it flexible across Tribes, and then you can simply install the R package and use the functions and documentation.

