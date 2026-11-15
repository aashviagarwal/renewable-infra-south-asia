# renewable-infra-south-asia
# Evaluating South Asia's Renewable Energy Transition

## Overview

This project examines renewable energy transition preparedness across five South Asian countries : India, Pakistan, Bangladesh, Nepal, and Sri Lanka, between 2010 and 2023.

The analysis explores how energy policy frameworks, implementation indicators, solar energy consumption, and fuel import dependence evolve over time. It aims to understand whether improvements in sustainable energy governance are accompanied by changes in energy consumption patterns, and how these relationships differ across countries.

A central component of the project is the **Energy Transition Preparedness Index (ETPI)**, a composite indicator developed to compare energy transition preparedness across countries and years.

## Research Questions

- How have sustainable energy policy and governance indicators evolved across South Asia between 2010 and 2023?
- How do renewable energy consumption and fuel import dependence vary alongside changes in policy frameworks?
- Do countries demonstrate similar patterns of energy transition preparedness, or are their trajectories distinct?
- How can policy indicators and energy consumption data be combined into a comparative framework for evaluating transition preparedness?

## Countries and Period

| Dimension | Coverage |
|---|---|
| Countries | India, Pakistan, Bangladesh, Nepal, Sri Lanka |
| Period | 2010–2023 |
| Focus | Sustainable energy governance, solar consumption, fuel imports, transition preparedness |

## Data Sources

The project combines three datasets:

- **World Bank – Regulatory Indicators for Sustainable Energy (RISE):** Policy and regulatory indicators related to sustainable energy, including energy efficiency, renewables, electrification, and clean cooking.
- **World Bank – Fuel Imports:** Fuel imports as a percentage of total merchandise imports, used as an indicator of external fuel dependence.
- **Our World in Data – Solar Energy Consumption:** Country-level solar energy consumption data used to examine changes in renewable energy utilization.

Additional total energy consumption figures and regional solar potential estimates are incorporated into selected analyses.

## Methodology

### 1. Data Cleaning and Integration

- Filtered datasets to the five countries and the 2010–2023 period.
- Restructured annual observations into long-format data for comparative analysis.
- Joined policy indicators, fuel import measures, and solar consumption data by country and year.
- Calculated solar energy consumption as a proportion of total energy consumption.

### 2. Policy Indicator Categorization

RISE indicators were grouped into six policy categories:

- Clean Cooking
- Grid Framework
- Renewables Policy
- Economic Incentives
- Household & Efficiency
- Transport

Average indicator scores were calculated within categories to facilitate comparisons across countries and years.

### 3. Country-Level Analysis

Each country was examined individually to explore changes in policy scores alongside fuel imports and solar energy consumption.

The analysis uses time-series visualizations and comparative plots to identify patterns, divergences, and potential relationships between governance and energy outcomes.

### 4. Energy Transition Preparedness Index (ETPI)

The project develops a composite index using four components:

| Pillar | Description |
|---|---|
| Top-down Governance | Economic incentives and renewable energy policy |
| Bottom-up Governance | Clean cooking and grid framework indicators |
| Implementation Depth | Household and energy efficiency indicators |
| Fuel Import Score | 100 minus fuel imports as a percentage of total imports |

The ETPI is calculated as the arithmetic mean of the four pillar scores for each country-year.

This provides a comparative framework for examining energy transition preparedness across countries over time.

### 5. Regional Solar Potential

The project also includes a geographic visualization of solar potential across Indian states using Global Horizontal Irradiance (GHI) estimates and approximate state coordinates.

## Tools and Technologies

- **R**
- tidyverse
- dplyr
- tidyr
- ggplot2
- plotly
- readr
- readxl
- maps
- mapdata

## Visualizations

The project includes:

- Country-level policy score trends
- Comparative solar consumption and fuel import plots
- Policy category heatmaps
- Country-specific governance and energy outcome analyses
- Interactive ETPI time-series visualization
- Indian state-level solar potential bubble map

## Key Findings

The analysis highlights differences in energy transition trajectories across the five countries.

- India demonstrates comparatively strong policy scores and higher solar energy consumption, while continuing to rely on fuel imports.
- Pakistan exhibits changes in policy indicators alongside increasing solar consumption.
- Bangladesh demonstrates variation across policy categories, including differences between grid frameworks and clean cooking indicators.
- Nepal shows improvements in selected governance indicators, particularly clean cooking and grid frameworks, alongside relatively low solar consumption.
- Sri Lanka illustrates how economic instability and changing energy conditions complicate the interpretation of policy and consumption trends.

These observations are descriptive and should not be interpreted as evidence of causal relationships.

## Limitations

- Fuel imports as a percentage of total merchandise imports do not directly measure domestic fossil fuel consumption or overall energy dependence.
- Solar consumption alone does not capture the complete renewable energy mix, including hydropower.
- Policy scores measure regulatory and governance dimensions rather than directly measuring implementation outcomes.
- The ETPI is an exploratory composite indicator constructed for comparative analysis; it is not an official or validated measure of national energy transition preparedness.
- Cross-country comparisons may be affected by differences in economic conditions, energy infrastructure, data availability, and national circumstances.
- Correlations and trends identified in the analysis do not establish causal relationships.

## Project Files

| File | Description |
|---|---|
| `project_2_cleaned.R` | R script containing data cleaning, analysis, index construction, and visualizations |
| `project2-presentation.Rmd` | R Markdown presentation documenting the project and its analysis |

## Reproducibility

The analysis was conducted in R.

To reproduce the project:

1. Install R and the required packages.
2. Obtain the original datasets from their respective sources.
3. Place the datasets in the working directory or update the file paths in the script.
4. Run `project_2_cleaned.R`.

The original datasets are not included in this repository unless otherwise specified.

## References

- World Bank. Regulatory Indicators for Sustainable Energy (RISE). https://data360.worldbank.org/en/dataset/WB_RISE
- World Bank. Fuel Imports. https://data.worldbank.org/indicator/TM.VAL.FUEL.ZS.UN
- Our World in Data. Solar Energy Consumption. https://ourworldindata.org/grapher/solar-energy-consumption
- Pareek et al. (2024). *Solar Energy Advances*, 4, 100052.


