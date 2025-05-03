# VT Football Non-Contact Injury Probability Script 🏈

## Summary

Using the Catapult and Valid Performance datasets, two Cox Proportional Hazards models were created—one for each dataset—to determine which variables contribute to or help prevent non-contact injuries, and to calculate the probability of a non-contact injury occurring within a given number of days.

This repository contains an R script that allows the user to choose either the Catapult or Valid Performance model. The user can then manually input data for a single athlete or load data from a `.csv` file for multiple athletes. After entering the number of days forward for the prediction, the script will output injury probabilities for each athlete.

## Instructions

* To use the script, it is recommended that you install R and RStudio on your machine. R is the language used for this project, and RStudio is the IDE that allows you to execute R code. You can download both here: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

* Download the repository onto your machine. We recommend cloning it using Git, but you may also manually download the files. Just ensure all files are located in the same directory when running the script.

```bash
git clone https://github.com/ryan-webster-vt/VT-Injury-Deliverable.git
```

* There are five files: `user_file.R`, `catapult_backend.R`, `vald_backend.R`, `catapult_final_model.Rds`, and `vald_final_model.Rds`. Open `user_file.R` (you do not need to open the other two `.R` files). To run the script, press `Ctrl + A` followed by `Enter` in RStudio. Make sure your console is open. You will be prompted to choose either the Catapult or Valid Performance model by typing `c` (for Catapult) or `v` (for Valid Performance). The required libraries—`survival`, `tidyverse`, and `rstudioapi`—will be installed automatically if not already present.

* Next, you will be asked whether you want to manually input data (type `'manual'`) for a single athlete or load data from a `.csv` file (type `'load'`). If loading data, ensure that:

  * The file is in the same directory as the script
  * The file format is `.csv` (not `.xlsx` or `.xls`)
  * The column headers match the expected format for either the Catapult or Valid Performance dataset

## Example Input Formats

**Catapult Model**

| About          | total\_player\_load | ima\_decel\_high | ima\_cod\_left\_high | ima\_cod\_right\_low | max\_deceleration |
| -------------- | ------------------- | ---------------- | -------------------- | -------------------- | ----------------- |
| Michael Vick   | 250                 | 2                | 3                    | 6                    | -0.5              |
| Kam Chancellor | 300                 | 4                | 5                    | 4                    | -0.3              |

**Valid Performance Model**

| About        | Average.Force | Nordic.Left.Avg |
| ------------ | ------------- | --------------- |
| Cam Phillips | 500           | 500             |
| Isaiah Ford  | 500           | 600             |

* Once the data is loaded, the script will prompt you to enter the number of days forward for the injury probability calculation. Enter a number (e.g., `30`), and the script will return a list (or single entry if manual) of athletes and their respective probabilities in descending order.

* To re-run the script, simply press `Ctrl + A` and `Enter` again in `user_file.R`.
