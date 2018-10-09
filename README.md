# Information on deaf and hearing STEM students and professionals

- stemDay.xlsx contains estimates of median incomes for deaf and hearing full-time workers, ages 25-64, in stem, stem-related, or non-stem-related occupations, with a stem, stem-related, or non-stem-related bachelors degree or no bachelors degree
- proportionStem.xlsx contains estimated numbers and proportions of deaf or hearing full-time workers, ages 25-64, with any of those job or education categories

The estimates are based on the 2016 American Community Survey 5-year sample

To replicate, run the file `stemDay.r` in `R`

The file quant2.r estimates medians from replicate-weighted surveys, and is modified from the `svyquantile.svyrep.design` function in the `R` Survey package [which can be found here](https://github.com/cran/survey/blob/4bc6f900be6c085805e3912c07521c67129487cd/R/surveyrep.R)

# Raw Data
This repository does not contain the raw data, which can be downloaded from <https://www.census.gov/programs-surveys/acs/data/pums.html>. 
