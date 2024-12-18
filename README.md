
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Mapping Orca Encounters in the Pacific Northwest

<!-- badges: start -->
<!-- badges: end -->

This repository contains R code and analysis for visualizing and
exploring orca encounter data in the Pacific Northwest waters. The
project focuses on creating interactive and static maps using various R
mapping packages, specifically analyzing the spatial patterns of orca
sightings over time.

## Overview

This analysis uses data from the \#TidyTuesday project, focusing on orca
encounters in the Pacific Northwest. The main objectives are:  
- Create spatial visualizations using open-source R tools  
- Transform point data into lines and centroids  
- Analyze spatial patterns of orca encounters across different years  
- Produce both interactive and static maps

## Data Source

The data comes from the [\#TidyTuesday
project](https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-10-15/readme.md)
and includes information about:  
- Encounter dates and locations  
- Beginning and ending coordinates  
- Pod information  
- Year of sighting

## Analysis Methods

The analysis workflow includes: 1. Data cleaning and processing  
2. Converting coordinate pairs to spatial objects using `sf`  
3. Creating line geometries from start/end points  
4. Calculating centroids for encounter paths  
5. Visualizing data using both `leaflet` and `ggplot2`

## Required R Packages

``` r
library(tidyverse)
library(sf)
library(leaflet)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(viridis)
library(rnaturalearth)
library(rmapshaper)
```

## Visualizations

The repository includes code for creating:  
- Interactive maps using `leaflet`  
- Static maps using `ggplot2`  
- Temporal analysis of encounter locations  
- Mean center analysis by year

## File Structure

- `orcas-analysis-quarto.qmd`: Main Quarto document containing
  analysis  
- `data/`: Directory containing the orca encounters dataset  
- `README.md`: Project documentation

## Running the Analysis

1.  Clone this repository  

2.  Ensure all required R packages are installed  

3.  Run the Quarto document using RStudio or command line:

    ``` bash
    quarto render orcas-analysis-quarto.qmd  
    ```
