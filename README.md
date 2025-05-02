# VT Football Non-Contact Injury Probability Script
## Summary
Using the Catapult and Valid Performance datasets, two models were created using the Cox Proportional Hazards model, one for each dataset, to determine what variables contribute and prevent non-contact injuries and calculate probability of non-contact injury within a given number of days. This repository contains a script written in R that will ask the user whether to use the Catapult or Valid Performance. Then, the user can either manually input data for one athlete or load in data using a .csv file for multiple athletes. With this, the application will ask the user how many days forward would they like to calculate probability of injury. Now, the script will output these probabilities for each athlete. 
## Instructions
* To use the script, it is recommended that you install R and RStudio on your machine. R is the language used for this project and needed to run the code while RStudio is the IDE that allows you to execute R code. You can download both R and RStudio here: https://posit.co/download/rstudio-desktop/
* Download the repository onto your machine. We recomend that you clone the repository using Git. You can also manually download each file, just insure that all the files are contained in the same directory when being used.
```bash
git clone https://github.com/ryan-webster-vt/VT-Injury-Deliverable.git
```
* There will be five files: user_file.R, catapult_backend.R, vald_backend.R, catapult_final_model.Rds, vald_final_model.Rds. Open user_file.R (the other two R files may remain closed). To run the script, press Ctrl+A+Enter, which will execute the entire code in that file. Insure that your console is open. You will be prompted whether to use the Catapult or Valid Performance model, choose by either typing c (for Catapult) or v (for Valid Performace).
* Next, you'll be prompted to either manually insert data (type 'manual') for an individual player or load data for multiple athletes (type 'load'). If loading data, ensure that the data is in the same directory as the code and that the data is saved as a .csv file (not .xlsx or .xls). If the user has an Excel sheet not saved as a .csv, one can save the file as a .csv. Also, the model expects the variable headings to be equivalent to the ones used in the Catapult or Valid Performance datasets.

