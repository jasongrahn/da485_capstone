build interactions
================

Our task today is to build interactions between each of the numeric variables.

CHECK - `renaming` - Marlie already renamed the variables, so we don't need to do that step.

CHECK - `filtering` - CHECK - We also learned that we only want the institutions that are similar to BC, so those need filtering too.

CHECK - `selecting` - There are a bunch of variables that we've kept for the sake of analysis (items like latitude and longitude). Those are not required for the sake of modeling. Before we build the interactions, we need to select only those variables we'll use.

interaction - After we do that, THEN we can build the interactions.

splitting - Then we should build training and test datasets. Hmmm, one more thing; since **Bellevue College** is our customer, we should make sure this isn't part of the training data somehow.

Let's start by importing.

``` r
library(readr)
institution_data <- read_csv(here::here("data/institution_data_explore_mc - institution_data.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   institution_name = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   level = col_character(),
    ##   funding = col_character(),
    ##   basic = col_character(),
    ##   flagship = col_character(),
    ##   med_sat_value = col_character(),
    ##   endow_value = col_character(),
    ##   grad_on_time_pct = col_character()
    ## )

    ## See spec(...) for full column specifications.

Then filtering for just bc-like colleges

``` r
nrow(institution_data)
```

    ## [1] 3798

``` r
institution_data <- institution_data %>% 
  filter(level == "4-year",
         funding == "Public",
         flagship_binary == 0)

nrow(institution_data)
```

    ## [1] 582

we've dropped the number of rows from 3,798 to 582; which is what we wanted.

Now we'll write some code to retain *only* the modeling variables.

``` r
ncol(institution_data)
```

    ## [1] 23

``` r
bc_institution_data <- institution_data %>% 
  select(institution_name,
         grad_rate,
         basic,
         student_count,
         spending_per_award,
         full_time_pct,
         full_time_count,
         med_sat_value,
         aid_value,
         endow_value,
         grad_on_time_pct, #ahh, i need to convert this to decimal as well.
         pell_value,
         fresh_retain_value,
         full_time_fac_pct,
         flagship_binary)

ncol(bc_institution_data)
```

    ## [1] 15

so we've dropped the number of columns from 23 to 15; which is what we expect as well. What we DONT need are interactions with names or graduation rates. Names are just indentifiers and grad rates are what we're trying to predict.
