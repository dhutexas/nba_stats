# Modeling NBA Data

This repository contains different models of NBA data. Data for each model are stored in the `Data` folder and the code required to run each model can be found in the current folder. Individual modeling projects, and their details, are described below.

## What’s Your Basketball Personality?
#### Using Machine Learning to Redefine NBA Positions

`NBA_LPA_model.R` Data and model files from Gaussian Mixture Model analysis of NBA player statistics. Related article available at https://link.medium.com/Tmqi4A16D9

## Is There An Optimal Lineup?
#### Predicting the Best Combination of Positions to Maximize Net Rating with Multilevel Modeling

`NBA_lineup_model.R` Model which calculates estimates for each position (fixed effect) while allowing for variation in each team for each season (teams nested in seasons). In other words, obtains effect of each position when placed on an average team in an average year (controls for team makeup).

