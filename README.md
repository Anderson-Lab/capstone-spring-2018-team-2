# Understanding the Impact of Medical Plan Design on Healthcare Utilization
### Partnership with College of Charleston and Benefitfocus

## Project Background
Navigating modern health care policy has become increasingly daunting as employers are strained by rising healthcare costs while employees are further expected to take greater responsibility choosing, managing, and paying for their coverage. Benefitfocus seeks to mediate these problems for both employer and employee and has partnered with College of Charleston to help predict one of the most costly healthcare expenditures, inpatient hospitalizations.

This project focuses on providing insights to employers and benefits administrators on how plan design can impact healthcare utilization, specifically hospitalizations. Various models were built using benefit plan variables (deductible, coinsurance, copay, etc.), to compute this impact. Using these models we've built an interactive R-shiny web app to enable the user to visualize this relationship between plan design and healthcare utilization.

## Data Preparation
Much of the data prep takes place in ```Join_MEPS.r```
* ```Join_MEPS(year)``` -> Join together a PRP and FYC file for a given year </br>
* ```Private_Filter(df, yr_ending)``` -> Filter a MEPS dataframe to only include private plans</br>
* ```Age.to.Cat(df, Age.col)``` -> Turn a MEPS age column to categories</br>
* ```Combine_MEPS_Years(df15,df14,df13,join_vars)``` -> Combine three years of MEPS data into one dataframe. (Example of this function being called in Build_Ranger.R)</br>

## Data Models
The primary scripts for generating models are
* ```Build_Ranger.R``` -> Builds a model for predicting hospitilization 
* ```Predict_Behaviors.R``` -> Builds a ranger model for each behavior
* ```Buckets_follow_notfollow.R``` -> Groups MEPS respondents into groups based on frequency of behaviors
* ```Build_all_models_buckets.R``` -> (In a broken state)</br>
      * Uses buckets / health plan design variables to predict hospitilization</br>
      * Uses health plan design variables to predict behavior buckets 

## R-Shiny Web App
(Add link to methods/code/demostration)

### https-github.com-Anderson-Lab-capstone-spring-2018-team-2
