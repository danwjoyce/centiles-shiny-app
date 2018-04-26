
# Centiles for Blood Pressure in Children

Last Updated : Dan W Joyce - 26th April 2018

# Introduction
This is an app built in R and Shiny which helps visualise and generate blood pressure centiles / normal ranges in children.  It was built to fill a niche: in the clinical service I worked in (England, UK), we used multiple-page, tabulated data that where time-consuming to read/calculate and prone to error.  Please read the following carefully.

The app is 'live' at http://danjoyce.shinyapps.io/ADHD_Project, but please note, we don't have funds to pay for more than the basic (free) CPU usage so it may be down for a proportion of the month.

As an alternative, you can follow the instructions below and run the app in your own R / RStudio / Shiny installation.

## Disclaimer
This is a tool, devised to help me quickly calculate BP centiles from age and height data on my phone, tablet and PC via the `shinyapps.io` website.  I shared it with colleagues.  There are two very important caveats:

  1. Blood pressure ranges are a function of population-normed stature-for-age, and it uses the US CDC data (because it's freely available, and I couldn't get the UK data licenced when I was building it in 2015) for LMS model parameters and similarly, the calculation of BP data uses the US formulae (see Acknowledgements / References below).  I cannot speak to the generalisability of the app's output for non-US populations.

  2. It has *not* been validated against any clinical gold standard and is *not* a tested medical device (see https://www.gov.uk/government/publications/medical-devices-software-applications-apps)

I do not have the resources to pursue any kind of classification / certification, and as such, this code / app is provided for people wanting to implement similar projects and who understand it is provided without warranty / liability (see licence.txt).

# Install R and RStudio
You'll need to download and install:

  * R - https://www.r-project.org/
  * RStudio - https://www.rstudio.com/

You'll need to install some packages (copy and paste into your RStudio console):

```
# graphics and data manipulation tools
  install.packages(reshape)  
  install.packages(ggplot2)
  install.packages(grid)

# if running Linux, this was needed explicitly prior to installing shiny
  install.packages("httpuv")
# shiny library itself ...
  install.packages("shiny")
```

For more information on the Shiny package, see https://www.r-project.org/nosvn/pandoc/shiny.html

# Installing The App
To try out the app, open up RStudio and find your working directory
```
getwd()
```

Now, copy the `deploy` directory (and it's contents) into your working directory, so that you have the following directory structure:

```
Your Working Directory
  |- deploy
      |
      |- ADHD_Project.Rproj
      |- about_txt.html
      |- bloodPressure.R
      |- centileFunctions.R
      |- ui.R
      |- server.R
      |- data
          |
          |- BP_params_US_2004.csv
          |- LMS_stature_CDC.csv
      |- www
          |
          |- app_logo_final.png
```

# Running
In the RStudio console, simply copy and paste this instruction:
```
library(shiny)
runApp("deploy")
```

And the app should come to life.  Note, although there is an option to switch between UK and CDC/US parameters, this option is *not* live because I do not have a licence to use the UK data (see Disclaimer above).

# Acknowledgements / References

  * Blood pressure model : see *The Fourth Report on the Diagnosis, Evaluation, and Treatment of High Blood Pressure in Children and Adolescents*, **Pediatrics**, Aug 2004, 114 (Supplement 2) 555-576. https://www.ncbi.nlm.nih.gov/pubmed/15286277

  * CDC Growth Chart Data : see https://www.cdc.gov/growthcharts/percentile_data_files.htm
