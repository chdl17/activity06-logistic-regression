Activity 6 - Logistic Regression
================

# Loading the Library

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.0.0 ──
    ## ✔ broom        1.0.0     ✔ rsample      1.1.0
    ## ✔ dials        1.0.0     ✔ tune         1.0.0
    ## ✔ infer        1.0.3     ✔ workflows    1.0.0
    ## ✔ modeldata    1.0.0     ✔ workflowsets 1.0.0
    ## ✔ parsnip      1.0.1     ✔ yardstick    1.0.0
    ## ✔ recipes      1.0.1     
    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## • Dig deeper into tidy modeling with R at https://www.tmwr.org

``` r
library(dplyr)
```

# Reading the dataset

``` r
resume <- read.csv("https://www.openintro.org/data/csv/resume.csv")

head(resume)
```

    ##   job_ad_id job_city               job_industry   job_type job_fed_contractor
    ## 1       384  Chicago              manufacturing supervisor                 NA
    ## 2       384  Chicago              manufacturing supervisor                 NA
    ## 3       384  Chicago              manufacturing supervisor                 NA
    ## 4       384  Chicago              manufacturing supervisor                 NA
    ## 5       385  Chicago              other_service  secretary                  0
    ## 6       386  Chicago wholesale_and_retail_trade  sales_rep                  0
    ##   job_equal_opp_employer job_ownership job_req_any job_req_communication
    ## 1                      1       unknown           1                     0
    ## 2                      1       unknown           1                     0
    ## 3                      1       unknown           1                     0
    ## 4                      1       unknown           1                     0
    ## 5                      1     nonprofit           1                     0
    ## 6                      1       private           0                     0
    ##   job_req_education job_req_min_experience job_req_computer
    ## 1                 0                      5                1
    ## 2                 0                      5                1
    ## 3                 0                      5                1
    ## 4                 0                      5                1
    ## 5                 0                   some                1
    ## 6                 0                                       0
    ##   job_req_organization job_req_school received_callback firstname  race gender
    ## 1                    0    none_listed                 0   Allison white      f
    ## 2                    0    none_listed                 0   Kristen white      f
    ## 3                    0    none_listed                 0   Lakisha black      f
    ## 4                    0    none_listed                 0   Latonya black      f
    ## 5                    1    none_listed                 0    Carrie white      f
    ## 6                    0    none_listed                 0       Jay white      m
    ##   years_college college_degree honors worked_during_school years_experience
    ## 1             4              1      0                    0                6
    ## 2             3              0      0                    1                6
    ## 3             4              1      0                    1                6
    ## 4             3              0      0                    0                6
    ## 5             3              0      0                    1               22
    ## 6             4              1      1                    0                6
    ##   computer_skills special_skills volunteer military employment_holes
    ## 1               1              0         0        0                1
    ## 2               1              0         1        1                0
    ## 3               1              0         0        0                0
    ## 4               1              1         1        0                1
    ## 5               1              0         0        0                0
    ## 6               0              1         0        0                0
    ##   has_email_address resume_quality
    ## 1                 0            low
    ## 2                 1           high
    ## 3                 0            low
    ## 4                 1           high
    ## 5                 1           high
    ## 6                 0            low

``` r
summary(resume)
```

    ##    job_ad_id        job_city         job_industry         job_type        
    ##  Min.   :   1.0   Length:4870        Length:4870        Length:4870       
    ##  1st Qu.: 306.2   Class :character   Class :character   Class :character  
    ##  Median : 647.0   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   : 651.8                                                           
    ##  3rd Qu.: 979.8                                                           
    ##  Max.   :1344.0                                                           
    ##                                                                           
    ##  job_fed_contractor job_equal_opp_employer job_ownership       job_req_any    
    ##  Min.   :0.0000     Min.   :0.0000         Length:4870        Min.   :0.0000  
    ##  1st Qu.:0.0000     1st Qu.:0.0000         Class :character   1st Qu.:1.0000  
    ##  Median :0.0000     Median :0.0000         Mode  :character   Median :1.0000  
    ##  Mean   :0.1148     Mean   :0.2912                            Mean   :0.7873  
    ##  3rd Qu.:0.0000     3rd Qu.:1.0000                            3rd Qu.:1.0000  
    ##  Max.   :1.0000     Max.   :1.0000                            Max.   :1.0000  
    ##  NA's   :1768                                                                 
    ##  job_req_communication job_req_education job_req_min_experience
    ##  Min.   :0.0000        Min.   :0.0000    Length:4870           
    ##  1st Qu.:0.0000        1st Qu.:0.0000    Class :character      
    ##  Median :0.0000        Median :0.0000    Mode  :character      
    ##  Mean   :0.1248        Mean   :0.1068                          
    ##  3rd Qu.:0.0000        3rd Qu.:0.0000                          
    ##  Max.   :1.0000        Max.   :1.0000                          
    ##                                                                
    ##  job_req_computer job_req_organization job_req_school     received_callback
    ##  Min.   :0.0000   Min.   :0.00000      Length:4870        Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000      Class :character   1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.00000      Mode  :character   Median :0.00000  
    ##  Mean   :0.4372   Mean   :0.07269                         Mean   :0.08049  
    ##  3rd Qu.:1.0000   3rd Qu.:0.00000                         3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.00000                         Max.   :1.00000  
    ##                                                                            
    ##   firstname             race              gender          years_college  
    ##  Length:4870        Length:4870        Length:4870        Min.   :0.000  
    ##  Class :character   Class :character   Class :character   1st Qu.:3.000  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :4.000  
    ##                                                           Mean   :3.618  
    ##                                                           3rd Qu.:4.000  
    ##                                                           Max.   :4.000  
    ##                                                                          
    ##  college_degree       honors        worked_during_school years_experience
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.0000       Min.   : 1.000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000       1st Qu.: 5.000  
    ##  Median :1.0000   Median :0.00000   Median :1.0000       Median : 6.000  
    ##  Mean   :0.7195   Mean   :0.05277   Mean   :0.5595       Mean   : 7.843  
    ##  3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000       3rd Qu.: 9.000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.0000       Max.   :44.000  
    ##                                                                          
    ##  computer_skills  special_skills     volunteer         military      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Median :1.0000   Median :0.0000   Median :0.0000   Median :0.00000  
    ##  Mean   :0.8205   Mean   :0.3287   Mean   :0.4115   Mean   :0.09713  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
    ##                                                                      
    ##  employment_holes has_email_address resume_quality    
    ##  Min.   :0.000    Min.   :0.0000    Length:4870       
    ##  1st Qu.:0.000    1st Qu.:0.0000    Class :character  
    ##  Median :0.000    Median :0.0000    Mode  :character  
    ##  Mean   :0.448    Mean   :0.4793                      
    ##  3rd Qu.:1.000    3rd Qu.:1.0000                      
    ##  Max.   :1.000    Max.   :1.0000                      
    ## 

``` r
# The {tidymodels} method for logistic regression requires that the response be a factor variable
resume <- resume %>% 
  mutate(received_callback = as.factor(received_callback))

resume_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(received_callback ~ race, data = resume, family = "binomial")

tidy(resume_mod) %>% 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |   -2.675 |     0.083 |   -32.417 |       0 |
| racewhite   |    0.438 |     0.107 |     4.083 |       0 |

## Day 02

``` r
resume_select <- resume %>% 
  rename(sex = gender) %>% 
  filter(job_city == "Chicago") %>% 
  mutate(race = case_when(
         race == "white" ~ "White",
         TRUE ~ "Black"
       ),
       sex = case_when(
         sex == "f" ~ "female",
         TRUE ~ "male"
       )) %>% 
  select(received_callback, years_experience, race, sex)
```
