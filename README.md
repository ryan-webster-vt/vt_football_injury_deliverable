# VT Football Non-Contact Injury Probability Script
## Summary
Using the Catapult and Valid Performance datasets, two models were created using the Cox Proportional Hazards model, one for each dataset, to determine what variables contribute and prevent non-contact injuries and calculate probability of non-contact injury within a given number of days. This repository contains a script written in R that will ask the user whether to use the Catapult or Valid Performance. Then, the user can either manually input data for one athlete or load in data using a .csv file for multiple athletes. With this, the application will ask the user how many days forward would they like to calculate probability of injury. Now, the script will output these probabilities for each athlete. 
## Instructions
* To use the script, it is recommended that you install R and RStudio on your machine. R is the language used for this project and needed to run the code while RStudio is the IDE that allows you to execute R code. You can download both R and RStudio here: https://posit.co/download/rstudio-desktop/
* Download the repository onto your machine. We reccomend that you clone the repository using Git.
```bash
git clone 
