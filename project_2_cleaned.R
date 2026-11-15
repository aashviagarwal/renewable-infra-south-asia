# STEP 1: Introducing the DATASETS and the purpose of this project. 

#installing dataset that displays fuel imports around the world.

# https://data.worldbank.org/indicator/TM.VAL.FUEL.ZS.UN,
library(readr)
fuel_imports <- read_csv("fuel_imports_wbod.csv")
# View(fuel_imports)

# https://data360.worldbank.org/en/dataset/WB_RISE
#installing dataset that tracks how well countries' policies and regulations support sustainable energy
library(readxl)
WB_RISE_WIDEF <- read_csv("WB_RISE_WIDEF.csv")
# View(WB_RISE_WIDEF)

#https://ourworldindata.org/grapher/solar-energy-consumption?tab=table&country=~NPL&mapSelect=~NPL
#installing dataset that tracks the solar power consumption of countries from the year 1965 to 2023.
solar_energy_worldwide <- read_csv("solar-energy-consumption.csv")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)

#DATASET 1- RISE
#Clean repetitive columns
rise_clean <- WB_RISE_WIDEF |>
  select(!c(FREQ, FREQ_LABEL, UNIT_MEASURE, UNIT_MEASURE_LABEL, DATABASE_ID,
            DATABASE_ID_LABEL, UNIT_MULT, UNIT_MULT_LABEL, OBS_STATUS,
            OBS_STATUS_LABEL, OBS_CONF, OBS_CONF_LABEL))

#Filter, pivot to long format, fix year type
sust_indicator <- rise_clean |>
  filter(INDICATOR_LABEL == "Sustainable Energy - Energy Efficiency Score") |>
  filter(REF_AREA_LABEL %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal")) |>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "score"
  ) |>
  mutate(year = as.integer(year))

#Rename column
rise_visual <- sust_indicator |>
  rename(country = REF_AREA_LABEL)

rise_southasia <- rise_clean |> filter(REF_AREA_LABEL %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal")) |> pivot_longer(
  cols = starts_with("20"),
  names_to = "year",
  values_to = "score"
)

#Plot
ggplot(rise_visual, aes(x = year, y = score, color = country)) +
  geom_line() +
  labs(
    title = "Energy Efficiency Governance Scores Over Time",
    subtitle = "South Asian oil-importing countries, 2010–2023",
    x = "Year",
    y = "RISE Score",
    color = "Country",
  ) 

#DATASET 2- FUEL IMPORTS

#cleaning data and filtering the data we need for visualization
fuel_clean <- fuel_imports |>
  select(!c(`Indicator Name`, `Indicator Code`, `Country Code`)) |>
  filter(`Country Name` %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal")) |>
  pivot_longer(
    cols = `2010`:`2023`,
    names_to = "year",
    values_to = "fuel_imports_pct"
  ) |>
  mutate(year = as.integer(year),
         fuel_imports_pct = as.numeric(fuel_imports_pct))

#Plot
ggplot(
  fuel_clean, 
  aes(
    x = year, 
    y= fuel_imports_pct, 
    color = `Country Name`
  )
) +
  geom_point()

#DATASET 3 - Solar Power Consumption

solar_country <- solar_energy_worldwide |>
  filter( !(Entity %in% c("ASEAN (Ember)" , "Asia" , "Asia (Ember)" , "Asia Pacific (EI)" , "Africa" , "Africa (EI)" , "Africa (Ember)" , "Eastern Africa (EI)" , "Middle Africa (EI)" , "Western Africa (EI)" , "Europe" , "Europe (EI)" , "Europe (Ember)" , "European Union (27)" , "Central America (EI)" , "Latin America and Caribbean (Ember)" , "North America" , "North America (EI)" , "North America (Ember)" , "South America" , "South and Central America (EI)"))) |>
  filter(Year %in% c(2010:2023)) 


solar_southasia <- solar_country |>
  filter(Entity %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal"))|>
  rename(solar_energy_consumption_terawatt = Solar , country = Entity , year = Year)

#Plot 3
ggplot(
  solar_southasia,
  aes( x = year, y = solar_energy_consumption_terawatt)
)+
  geom_line(aes(color = country))

solar_by_country <- solar_southasia |>
  group_by(country) |>
  summarize(
    count = n()
  )

#Cleaning the fuel imports dataset for convenience
fuel_clean <- fuel_clean |>
  select(`Country Name`, year, fuel_imports_pct) |>
  rename(country = `Country Name`)

fuel_country <- fuel_clean |>
  group_by(country) |>
  summarize(
    average_fuel_imports_by_country = mean(fuel_imports_pct , na.rm = TRUE),
    count = n()
  )
# according to this link, https://data.worldbank.org/indicator/TM.VAL.FUEL.ZS.UN, "fuels" mean commodities like coal, petroleum and related products, natural and manufactured gas and electric current. We are using this dataset to evaluate how much a country depends 
# on non renewable sources of energy, by the percentage of fuel they are importing. Since the Indian subcontinent is not necessarily rich in oil and coal reserves, all the countries depend on importing their fuels. Therefore, the fuel imports method is useful at comparing and evaluating.

#let's compare each country's fuel imports and solar energy consumption to get a stronger understanding of their dependence on fossil fuels as compared to solar power.
# Is there a correlation between fuel imports and solar energy consumption? 

compare_solar_fuel <- solar_southasia |>
  left_join(fuel_clean , by = c("country" , "year"))

ggplot(compare_solar_fuel , aes(x = year))+
  geom_smooth(aes(y = solar_energy_consumption_terawatt , color = country) , linewidth = 0.75)+
  geom_point(aes(y = fuel_imports_pct , shape = country , color = country))+
  labs(
    y = "Solar Consumption in TWh (smooth lines) and Percentage of imports that are fuel"
  )

# clearly, even though India relies the most on imports, India is adapting very well. What is happening in the other countries though? Lets zoom in.

compare_4 <- compare_solar_fuel |>
  filter(!(country == "India"))

ggplot(compare_4 , aes(x = year))+
  geom_smooth(aes(y = solar_energy_consumption_terawatt , color = country) , linewidth = 0.75)+
  geom_point(aes(y = fuel_imports_pct , color = country , size = country))+
  labs(
    y = "Solar Consumption in TWh (smooth lines) and Percentage of imports that"
  )

# Pakistan is next in importing fuel in proportion to its total imports. Their import trends are not the most consistent, however there is a considerable rise in relying on solar power for consumption.
# Sri Lanka seems to consistently increase their imports since 2016, even though their solar consumption stays low. 
# Nepal is inconsistent with their proportion of imports on fuel, while their solar energy consumption stays extremely low.
ggplot(compare_4 |> filter(country == "Nepal"), aes(x = year))+
  geom_smooth(aes(y = solar_energy_consumption_terawatt) , linewidth = 0.75)

# Bangladesh relies as little on fuel imports as necessary, as seen. At the same time, it is barely consuming any solar power.

ggplot(compare_4 |> filter(country == "Bangladesh"), aes(x = year))+
  geom_smooth(aes(y = solar_energy_consumption_terawatt) , linewidth = 0.75)+
  geom_point(aes(y = fuel_imports_pct))+
  labs(
    y = "Solar Consumption in TWh (smooth lines) and Percentage of imports that"
  )

compare_solar_fuel|>
  group_by(country)|>
  summarize(
    average_consumption = mean(solar_energy_consumption_terawatt , na.rm = TRUE) ,
    average_imports = mean(fuel_imports_pct , na.rm = TRUE)
  )


# Let's build on this, using the same source for solar energy consumption, I am finding the proportion of energy consumption that was solar for these countries.

# https://ourworldindata.org/energy

total_energy <- c(251.07442, 271.6291, 295.02753, 306.57666, 318.55838, 385.10175, 386.70114, 402.96527, 430.288, 482.562, 457.44717, 492.4704, 512.17395, 506.65912, 6247.89, 6587.0103, 6937.768, 7163.0615, 7652.732, 7892.4897, 8262.856, 8571.671, 9063.36, 9281.595, 8792.22, 9544.969, 10059.478, 10827.859, 17.683733, 18.739851, 20.153275, 21.680906, 23.470415, 21.088612, 32.492287, 37.348503, 41.98377, 36.38938, 45.60831, 48.148914, 55.67673, 57.468346, 749.46985, 753.3283, 701.7945, 775.3793, 786.30774, 829.31226, 900.4373, 951.9927, 967.48016, 973.6355, 959.6698, 1066.0114, 979.10126, 904.24133, 69.6159, 75.57897, 77.95245, 79.214134, 85.89107, 91.3002, 100.87175, 106.731476, 112.36917, 113.035385, 104.98076, 108.8493, 97.40702, 100.07797)
totalenergy_dataset <- data.frame(
  country = c(rep("Bangladesh" , 14) , rep("India" , 14) , rep("Nepal" , 14) , rep("Pakistan" , 14) , rep("Sri Lanka" , 14)) ,
  year = c(rep(c(2010:2023), 5)) ,
  energy_consumed_TWh = total_energy
)

solar_southasia_proportion <- solar_southasia |> left_join(totalenergy_dataset , by = c("country" ,"year"))

solar_southasia_proportion <- solar_southasia_proportion |> mutate(proportion_solar = (solar_energy_consumption_terawatt * 100)/energy_consumed_TWh)

# Let's do a final comparison of two proportions

ggplot(
  solar_southasia_proportion |> 
    left_join(fuel_clean, by = c("country" , "year")) |>
    pivot_longer(
      cols = c(proportion_solar, fuel_imports_pct),
      names_to = "metric",
      values_to = "value"
    ),
  aes(x = year , y = value, color = country)
)+
  geom_point()+
  geom_smooth(aes(linetype = country) , se = FALSE)+
  facet_wrap(~ metric , scales = "free_y") +
  labs(
    y = "Solar Consumption in TWh (smooth lines) and Percentage of imports that are fuel"
  )

# From 2010 to 2016, proportion of fuel imports as well as the increase of solar power consumption in all 5 countries fall, or are slow. From 2016 onwards, however, you can see an increase in both metrics. This helps us establish a correlation between both metrics. 
# This exploration of data helps us understand: 
# How India and Pakistan are actively adopting policies to increase sustainable energy choices, while also increasing their dependence on fossil fuels.
# How Nepal, Bangladesh and Sri Lanka are following a somewhat similar trend, however it is very gradual and change is inconsistent. 

# What happened in 2016?
# According to the World Economic Forum, oil prices fell from $115 a barrel in 2013 to $30 in 2016, so while demand hadn't changed, the prices had made oil cheaper over the world.
# The Paris Agreement in 2015 had accelerated the use of sustainable energy alternatives, especially in India, which is visible in the graph with a steeper rise in proportion of solar energy consumption from 2016 onwards.

#So the Paris Agreement was a landmark in motivating energy-related policies to be more renewable energy oriented. As we saw in the previous graph, all South Asian countries started amending their policies. 
# What was actually happening on the ground while all these changes are taking place at the top? 

# Let us compare the RISE dataset with the fuel imports dataset to see what was going on.

#STEP 1: Dissecting the RISE dataset

rise_categories <- rise_southasia |>
  group_by(INDICATOR_LABEL) |>
  summarize(
    count = n()
  )

# There are 27 sustainability indicators discussed in the RISE dataset, each telling us about how a country is performing in that sector. For sake of better understanding and ease of comparison, I have grouped these indicators into subcategories, such that each country is comparable. 
# Subcategories

subcategories<- data.frame(
  clean_cooking = c("Clean Cooking National Planning" , "Implementation Scope of Clean Cooking Planning" , "Incentives and Attributes for Clean Cooking" , NA , NA , NA , NA) ,
  grid_network = c("Grid Electrification Framework" , "Electrification Governance and Planning" , "Minigrids Framework" , "Off-Grid Systems Framework" , NA , NA , NA) ,
  renewables_policy = c("Electricity Sector Renewables" , "Renewable Energy - Governance" , "Heating and Cooling Renewables" , "Leveling the Playing Field for Renewables" , NA , NA, NA ) ,
  econ_incentives = c("Financing Mechanisms for Energy Efficiency" , "Incentives and Mandates: Energy Utility Programs" , "Consumer Affordability of Electricity" , NA, NA, NA, NA) ,
  household_efficiency = c("Building Energy Codes" , "Energy Labeling Systems" , "Minimum Energy Efficiency Performance Standards" , "Standards and Labeling" , "Energy Efficiency Governance" , "Energy Efficiency in Public Sector" , "Industrial and Commercial End Users") ,
  transport = c("Transport Sector" , "Transport Sector Renewables", NA, NA, NA, NA, NA)
)

# Demonstrating use of separate_wider_delim for indicator label structure
rise_demo_split <- rise_southasia |>
  separate_longer_delim(
    INDICATOR_LABEL,
    delim = " "
  )

# Let us go ahead and make a new dataframe using the rise_southasia dataset.

cooking_indicators <- c(
  "Clean Cooking National Planning",
  "Implementation Scope of Clean Cooking Planning",
  "Incentives and Attributes for Clean Cooking"
)

grid_indicators <- c(
  "Grid Electrification Framework",
  "Electrification Governance and Planning",
  "Minigrids Framework",
  "Off-Grid Systems Framework"
)

renewables_indicators <- c(
  "Electricity Sector Renewables",
  "Renewable Energy - Governance",
  "Heating and Cooling Renewables",
  "Leveling the Playing Field for Renewables"
)

economic_indicators <- c(
  "Financing Mechanisms for Energy Efficiency",
  "Incentives & Mandates: Energy Utility Programs",
  "Consumer Affordability of Electricity"
)

household_indicators <- c(
  "Building Energy Codes",
  "Energy Labeling Systems",
  "Minimum Energy Efficiency Performance Standards",
  "Standards and Labeling",
  "Energy Efficiency Governance",
  "Energy Efficiency in Public Sector",
  "Industrial and Commercial End Users"
)

transport_indicators <- c(
  "Transport Sector",
  "Transport Sector Renewables"
)

rise_subcat_southasia <- rise_southasia |>
  mutate(sub_categories =case_when(
    INDICATOR_LABEL %in% cooking_indicators ~ "Clean Cooking" ,
    INDICATOR_LABEL %in% grid_indicators      ~ "Grid Framework",
    INDICATOR_LABEL %in% renewables_indicators ~ "Renewables Policy",
    INDICATOR_LABEL %in% economic_indicators  ~ "Economic Incentives",
    INDICATOR_LABEL %in% household_indicators ~ "Household & Efficiency",
    INDICATOR_LABEL %in% transport_indicators ~ "Transport",
    TRUE ~ "Other"
  )) |>
  filter(!(sub_categories == "Other")) |>
  group_by(REF_AREA_LABEL , year, sub_categories) |>
  summarize(
    indicator_score = mean(score , na.rm = TRUE) , .groups = "drop"
  ) |>
  select(REF_AREA_LABEL , year , sub_categories, indicator_score)

# Let's compare the countries by each subcategory now!

# BEGIN AI ASSISTED CODE ****************************************************************** 

# **************************************************************************************************** 

rise_subcat_southasia |>
  ggplot(aes(x = year, y = REF_AREA_LABEL, fill = indicator_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(indicator_score, 1)), size = 2.5) +
  scale_fill_gradient(low = "red", high = "green") +
  facet_wrap(~ sub_categories) +
  labs(
    title = "RISE Policy Scores by Category and Year",
    x = "Year", y = NULL, fill = "Avg Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# END AI ASSISTED CODE ********************************************************************* 

# **************************************************************************************************** 

# PART 2: Looking into each country
# All countries score okay in economic incentives, while showing considerable improvement in grid framework. India seems to show a upward movement in all scores, while Sri Lanka, Nepal and Bangladesh show a mixed bag of poor outcomes in some indicators, and doing well in some. Pakistan seems to be trying to improve, but has suffered some fallbacks in indicators like clean cooking, policy of renewables and transport. 
# Let us look into how these scores reflect when compared with their solar energy consumption and/or fuel imports.

# 1. India

india_policy_outcome <- rise_subcat_southasia |>
  filter(REF_AREA_LABEL == "India")|>
  mutate(year = as.integer(year))

ggplot(india_policy_outcome , aes(x = as.integer(year) , y = indicator_score , color = sub_categories))+
  geom_smooth(se = F)+
  coord_cartesian(ylim = c(0,100))+
  labs(
    title = "RISE scores of India, over time, categorised" ,
    x = "Year (2010 to 2023)" ,
    y = "Score (out of 100)" ,
    color = "Subcategories"
  )

# Fuel imports, as discussed before, include coal, petroleum products and natural gas. These are used for cooking, transport and electricity access at a household level. 
# Let's plot a graph that compares fuel imports against three subcategories of indicators: cooking, transport and household energy efficiency. 
# Most households in India, and the rest of South Asia, rely on LPG and CNG for cooking, Petroleum, Diesel and CNG for Transport and coal for electricity. Keeping these in mind, lets see.

india_plot_bottomup <- india_policy_outcome |>
  rename(country = REF_AREA_LABEL) |>
  left_join(fuel_clean , by = c("country" , "year")) |>
  filter(sub_categories %in% c("Clean Cooking" , "Transport" , "Household & Efficiency")) 


ggplot(india_plot_bottomup , aes(x = year))+
  geom_smooth(aes(y = fuel_imports_pct) , color = "black" , linewidth = 1.5 , se = F)+
  geom_smooth(aes(y = indicator_score , color = sub_categories) , linewidth = 1 , se = F)

# On looking at the graph and taking priors into consideration, this looks pretty consistent. The ground reality here does reflect the numbers we've been seeing until now. 
# What about solar consumption? If India is consuming so much solar energy, is it actually paying off?
# Things to take into consideration: EVs are not very prevalent in India. Most transportation rely on fossil fuels. Same as cooking. Lets look at economic incentives, renewables policy, and grid network. 

india_plot_solar_payoff <- india_policy_outcome |>
  rename(country = REF_AREA_LABEL) |>
  left_join(solar_southasia_proportion , by = c("country" , "year"))|>
  select(country , year , sub_categories , indicator_score , proportion_solar)|>
  filter(sub_categories %in% c("Economic Incentives" , "Grid Framework" , "Renewables Policy"))

# since the proportion_solar curve is not helping us evaluate the relationship, i am using AI to help.

# BEGIN AI ASSISTED CODE ****************************************************************** 

# **************************************************************************************************** 

ggplot(india_plot_solar_payoff, aes(x = year)) +
  geom_smooth(aes(y = proportion_solar * 70), color = "black", linewidth = 0.5, se = FALSE) +
  geom_smooth(aes(y = indicator_score, color = sub_categories, linetype = sub_categories), linewidth = 1, se = FALSE) +
  scale_y_continuous(
    name = "RISE Indicator Score (0-100)",
    sec.axis = sec_axis(~ . / 70, name = "Solar Proportion (%)")
  ) +
  labs(
    title = "India — Top-Down Policy Scores vs Solar Adoption",
    x = "Year", color = "Subcategory", linetype = "Subcategory"
  ) 

# END AI ASSISTED CODE ********************************************************************* 

# **************************************************************************************************** 

# 2. Pakistan

pak_policy_outcomes <- rise_subcat_southasia |>
  filter(REF_AREA_LABEL == "Pakistan") |>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL)

ggplot(pak_policy_outcomes , aes(x = year , y = indicator_score , color = sub_categories))+
  geom_smooth(se = F)+
  labs(
    title = "RISE scores of Pakistan, over time, categorised" ,
    x = "Year (2010 to 2023)" ,
    y = "Score (out of 100)" ,
    color = "Subcategories"
  )

# It is interesting to see how Pakistan scores well on economic incentives, while they stay consistently low on transportation. Along with that, until 2019, they were growing consistently in clean cooking, but dropped later to scores between 10 and 20. 
# What could be the cause?
# Are economic incentives consistent with the solar energy consumption in the country? 
# Did fuel imports increase, that made cooking with petroleum and natural gases much more favourable? 
# Are there any outlying reasons we haven't taken into consideration? COVID? Recession? Civil unrest? 

pakistan_plot_solar <- pak_policy_outcomes |>
  left_join(solar_southasia_proportion, by = c("year", "country")) |>
  select(country, year, sub_categories, indicator_score, proportion_solar) |>
  filter(sub_categories %in% c("Economic Incentives", "Grid Framework", "Renewables Policy"))

ggplot(pakistan_plot_solar, aes(x = year)) +
  geom_smooth(aes(y = proportion_solar * 70), color = "black", linewidth = 1.5, se = FALSE) +
  geom_smooth(
    data = filter(pakistan_plot_solar, sub_categories == "Economic Incentives"),
    aes(y = indicator_score),
    color = "red", linewidth = 1.3, se = FALSE
  ) +
  geom_smooth(
    data = filter(pakistan_plot_solar, sub_categories != "Economic Incentives"),
    aes(y = indicator_score, color = sub_categories),
    linewidth = 1.0, se = FALSE
  ) +
  # BEGIN AI ASSISTED CODE ******************************************************************
  scale_y_continuous(
    name = "RISE Indicator Score (0-100)",
    sec.axis = sec_axis(~ . / 70, name = "Solar Proportion (%)")
  ) +
  # END AI ASSISTED CODE *********************************************************************
  labs(
    title = "Pakistan — Top-Down Policy Scores vs Solar Adoption",
    x = "Year", color = "Subcategory"
  ) 

# The answer to the first question is yes, atleast if we restrict our understanding to the data in front of us, it looks like it is consistent. 
# Let's address the elephant in this room, the drop in clean cooking.

pak_cooking_fuel <- pak_policy_outcomes |>
  left_join(fuel_clean , by = c("country" , "year"))

ggplot(pak_cooking_fuel , aes(x = year))+
  geom_smooth(
    aes(y = fuel_imports_pct) ,
    color = "black" ,
    linewidth = 1.5
  )+
  geom_smooth(
    data = filter(pak_cooking_fuel, sub_categories == "Clean Cooking"),
    aes(y = indicator_score),
    color = "blue", linewidth = 1.3, se = FALSE
  ) +
  geom_smooth(
    data = filter(pak_cooking_fuel, sub_categories %in% c("Transport" , "Household & Efficiency")),
    aes(y = indicator_score, color = sub_categories),
    linewidth = 1.0, se = FALSE
  )+
  labs(
    title = "Fuel Imports vs Bottom Up Policy Adoption in Pakistan" ,
    x = "Year" ,
    color = "Subcategory"
  ) 

# No information we can *properly* extract from this graph. The rise in fuel imports by percentage of total imports is gradual and normal, whereas the drop in clean cooking scores is abnormal.
# What else is at play here?

cleancooking_pak <- rise_southasia |>
  filter(REF_AREA_LABEL == "Pakistan" , INDICATOR_LABEL %in% cooking_indicators ) |> mutate(year = as.integer(year)) |>
  select(year , REF_AREA_LABEL , INDICATOR_LABEL , score)

ggplot(
  cleancooking_pak ,
  aes(x = year , y = score , color = INDICATOR_LABEL)
) +
  geom_smooth(se = F)

# Clean Cooking dropped sharply with all the RISE indicators. 
# There is an external reason explaining this: Pakistan suffered from devastating floods in 2022, leading to 1700+ deaths and $30 billion dollars of damages. The government had to pivot their attention from sustainability indicators to getting through a widespread natural calamity.
# There was also major political unrest, with the ousting and imprisonment of Prime Minister Imran Khan, which can also explain a change in policies and pivoting.

# This brings an important matter to light: Countries can only achieve sustainability when the political climate is stable, and the country has enough safety nets to get through natural calamities. 
# Wait, then how come the economic incentives stay high?

econincentives_pak <- rise_southasia |>
  filter(REF_AREA_LABEL == "Pakistan" , INDICATOR_LABEL %in% economic_indicators ) |> mutate(year = as.integer(year)) |>
  select(year , REF_AREA_LABEL , INDICATOR_LABEL , score)

ggplot(
  econincentives_pak ,
  aes(x = year , y = score , color = INDICATOR_LABEL)
) +
  geom_smooth(se = F)

# Financing Mechanisms for Energy Efficiency being 0 means there is no private investment into Energy Efficiency, meaning consumer affordability and energy utility programs are all funded directly by government subsidies, which makes this expenditure fiscally unsustainable for the country (risky because it can lead to more civil unrest in the future) and crowding out is causing private disinvestment.

# 3. Bangladesh

bgd_policy_outcomes <- rise_subcat_southasia |>
  filter(REF_AREA_LABEL == "Bangladesh") |>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL)

ggplot(bgd_policy_outcomes , aes(x = year , y = indicator_score , color = sub_categories))+
  geom_smooth(se = F)+
  labs(
    title = "RISE scores of Bangladesh, over time, categorised" ,
    x = "Year (2010 to 2023)" ,
    y = "Score (out of 100)" ,
    color = "Subcategories"
  )
# There is an overall rise in all indicators for Bangladesh, but it is interesting to notice the plateau in Clean Cooking with Bangladesh in 2015, that lasted till 2018.
# Lets zoom in there.

cleancooking_bgd <- rise_southasia |>
  filter(REF_AREA_LABEL == "Bangladesh" , INDICATOR_LABEL %in% cooking_indicators , year %in% c("2014" , "2015" , "2016" , "2017" , "2018", "2019") ) |> mutate(year = as.integer(year)) |>
  select(year , REF_AREA_LABEL , INDICATOR_LABEL , score)

ggplot(
  cleancooking_bgd ,
  aes(x = year , y = score , color = INDICATOR_LABEL)
) +
  geom_smooth(se = F)

# The values don't move at all for those 5 years, in fact, within the 14 years, the change only happens thrice, rather than every year. 
# Why are the numbers being repeated here? --> Bangladeshi policies only changed thrice in those 14 years for clean cooking. According to official sources, laws were passed and policies were created to invest more into clean cooking initiatives, but this stagnant and slow growth says otherwise.
# Did fuel imports have something to do here?

bgd_policy_fuel <- bgd_policy_outcomes |>
  left_join(fuel_clean , by = c("country" , "year"))

ggplot(bgd_policy_fuel , aes(x = year))+
  geom_smooth(aes(y = fuel_imports_pct) , color = "black" , linewidth = 1.3 , se = F)+
  geom_smooth(
    data = filter(bgd_policy_fuel , sub_categories == "Clean Cooking") ,
    aes(y = indicator_score) ,
    color = "navy",
    linewidth = 1.0 , se = F
  )+
  geom_smooth(
    data = filter(bgd_policy_fuel , sub_categories != "Clean Cooking") ,
    aes(y = indicator_score , color = sub_categories) ,
    linewidth = 0.8 , se = F
  )

# Incomplete information for fuel imports. However, we have enough to see what happened till 2017. There wasn't a considerable increase in fuel imports that made cooking with natural gas more convenient. 
# External reason: Bangladesh relies on domestic reserves of natural gas for its needs, which could also explain the lax in policy implementation.

# How come the grid framework of Bangladesh is their strongest feature? Let us investigate by dissecting the indicator and comparing with solar energy consumption

grid_bgd <- rise_southasia |>
  filter(REF_AREA_LABEL == "Bangladesh" , INDICATOR_LABEL %in% grid_indicators ) |> mutate(year = as.integer(year)) |>
  select(year , REF_AREA_LABEL , INDICATOR_LABEL , score)
ggplot(grid_bgd , aes(x=year, y= score , color= INDICATOR_LABEL))+
  geom_smooth(se = F)

# From 2010 to 2023, Bangladesh has built an off-grid systems framework that spans across almost the entire country, which is quite commendable for a densely populated developing country.

bgd_grid_solar <- grid_bgd |>
  rename(country = REF_AREA_LABEL) |>
  left_join(solar_southasia_proportion , by = c("country" , "year")) |>
  select(country , year, INDICATOR_LABEL , score, proportion_solar)

ggplot(bgd_grid_solar, aes(x = year)) +
  geom_smooth(aes(y = proportion_solar * 500), color = "black", linewidth = 1.5, se = FALSE) +
  geom_smooth(
    data = filter(bgd_grid_solar, INDICATOR_LABEL == "Off-Grid Systems Framework"),
    aes(y = score),
    color = "orange", linewidth = 1.3, se = FALSE
  ) +
  geom_smooth(
    data = filter(bgd_grid_solar, INDICATOR_LABEL != "Off-Grid Systems Framework"),
    aes(y = score, color = INDICATOR_LABEL),
    linewidth = 1.0, se = FALSE
  ) +
  # BEGIN AI ASSISTED CODE ******************************************************************
  scale_y_continuous(
    name = "RISE Indicator Score (0-100)",
    sec.axis = sec_axis(~ . / 500, name = "Solar Proportion (%)")
  ) +
  # END AI ASSISTED CODE *********************************************************************
  labs(
    title = "Bangladesh — Electrification Grid Adoption vs Solar Adoption",
    x = "Year", color = "Subcategory"
  ) 

# When scaled, one can say that the increase in solar energy consumption is positively correlated with this indicator. Is there any other external factor here too? Yes, biomass.

# 4. Nepal

nep_policy_outcomes <- rise_subcat_southasia |>
  filter(REF_AREA_LABEL == "Nepal") |>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL)

ggplot(nep_policy_outcomes , aes(x = year , y = indicator_score , color = sub_categories))+
  geom_smooth(se = F)+
  labs(
    title = "Indicator Scores of Nepal from 2010 to 2023, over 5 categories"
  )+
  coord_cartesian(ylim = c(0 , 100))

# Nepal scores the best on clean cooking, peaking at around 74 in 2023, while they also have shown considerable growth in constructing a grid network aroung the country. 
# does this rise in clean cooking initiatives coincide with a fall in fuel imports?

nepal_cooking_fuel <- rise_southasia|>
  filter(REF_AREA_LABEL == "Nepal") |>
  filter(INDICATOR_LABEL %in% cooking_indicators)|>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL) |>
  left_join(fuel_clean , by = c("country" , "year")) |>
  select(country , year , INDICATOR_LABEL , score , fuel_imports_pct)

ggplot(nepal_cooking_fuel , aes(x = year))+
  geom_smooth(aes(y = fuel_imports_pct) , color = "black" , linewidth = 1.3 , se = F)+
  geom_smooth(aes(y = score , color = INDICATOR_LABEL) , se = F)

# So, fuel imports have barely risen in proportion to total imports since 2016. At the same time, the score on clean cooking national planning rose by 93 points since 2010, showing great investment and dedication by the lawmakers. However, the score on incentives and implementation stay slow or stagnant, meaning that there is a gap between policymaking and policy implementation in Nepal. Given that they recently experienced civil unrest, and the rise of a new government, maybe we will see the closing of this gap. 
# also, does the rise in scores on grid networks coincide with an increase in solar energy consumption?

nepal_grid_solar <- rise_southasia |>
  filter(REF_AREA_LABEL == "Nepal") |>
  filter(INDICATOR_LABEL %in% grid_indicators)|>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL) |>
  left_join(solar_southasia_proportion , by = c("country" , "year")) |>
  select(country , year , INDICATOR_LABEL , score , proportion_solar)

ggplot(nepal_grid_solar, aes(x = year)) +
  geom_smooth(aes(y = proportion_solar * 10), color = "black", linewidth = 1.5, se = FALSE) +
  geom_smooth(
    aes(y = score , color = INDICATOR_LABEL)
    , linewidth = 1.3, se = FALSE
  ) +
  # BEGIN AI ASSISTED CODE ******************************************************************
  scale_y_continuous(
    name = "RISE Indicator Score (0-100)",
    sec.axis = sec_axis(~ . / 10, name = "Solar Proportion (%)")
  ) +
  # END AI ASSISTED CODE *********************************************************************
  labs(
    title = "NEPAL — Electrification Grid Adoption vs Solar Adoption",
    x = "Year", color = "Subcategory"
  ) 

# The rise in solar energy consumption isn't high, but the country has been successful in creating an off-grid systems framework.
# So, if Nepal isn't importing much fuel, or consuming much solar power, how is it scoring so high on clean cooking and grid framework?
# Our data analysis points to an outlying reason: Nepal relies heavily on hydropower as a source of electricity, with a potential of generating 83000 MW of energy.


# 5. Sri Lanka

sl_policy_outcomes <- rise_subcat_southasia |>
  filter(REF_AREA_LABEL == "Sri Lanka") |>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL)

ggplot(sl_policy_outcomes , aes(x = year , y = indicator_score , color = sub_categories))+
  geom_smooth(se = F)+
  coord_cartesian(ylim = c(0,100))

# Sri Lanka is showing really strong growth in grid framework and household & efficiency, considerable but slow growth in economic incentives and policy on renewables, and extremely slow growth on clean cooking and transport.
# What led to this shoot up in grid framework and slight tapering and fall off in 2022? 

srilanka_grid <- rise_southasia |>
  filter(REF_AREA_LABEL == "Sri Lanka") |>
  filter(INDICATOR_LABEL %in% grid_indicators)|>
  mutate(year = as.integer(year)) |>
  rename(country = REF_AREA_LABEL)

ggplot(srilanka_grid , aes(x = year , y = score , color = INDICATOR_LABEL))+
  geom_smooth(se = F)

# It is interesting to see the score go beyond 100 for electrification governance and planning, and grid electrification framework, and especially see the rise of grid electrification network from less than 10 to 100.
# Does this have anything to do with fuel imports and solar energy consumption?

srilanka_grid_fuel_solar <- srilanka_grid |>
  left_join(fuel_clean , by = c("country" , "year")) |>
  left_join(solar_southasia_proportion , by = c("country" , "year")) |>
  select(country , year , INDICATOR_LABEL , score , fuel_imports_pct , proportion_solar)

ggplot(srilanka_grid_fuel_solar , aes(x = year))+
  geom_smooth(aes(y = fuel_imports_pct) , color = "blue" , linewidth = 1.3 , se = F)+
  geom_smooth(aes(y = score , color = INDICATOR_LABEL) , linewidth = 1.0 , se = F)+
  geom_smooth(aes(y = proportion_solar * 100), color = "brown", linewidth = 1.5, se = FALSE)+
  # BEGIN AI ASSISTED CODE ******************************************************************
  scale_y_continuous(
    name = "RISE Indicator Score (0-100)",
    sec.axis = sec_axis(~ . / 100, name = "Solar Proportion (%)")
  ) +
  # END AI ASSISTED CODE *********************************************************************
  labs(
    title = "Sri Lanka — Electrification Grid Adoption , Solar Consumption and Fuel Imports",
    subtitle = "brown is solar consumption, blue is fuel imports.",
    x = "Year", color = "Subcategory"
  ) 

# Fuel imports as a proportion of total imports did increase steadily over time, while solar energy consumption rose greatly in a short span. However, these do not explain the fall in grid network scores.
# What explains it then? Sri Lanka had suffered from a complete crash of the economy, and severe recession, along with no money left in their foreign reserves. While this explains the fall in scores, it doesn't explain how fuel imports stayed consistent and solar energy consumption grew.
# This helps us identify a limitation in our analysis; the variable used for fuel imports is restricted into being a proportion, meaning that even if Sri Lankan imports had fallen to $1, they still would have used 20 cents to purchase fuel. Our methods do not help us demonstrate times of crisis through our variables all the time; it is thus always important to take outlying stories into consideration before building a credible causal story.
# It is interesting to note that with a fall in imports, the country resorted to solar power as a primary source since it was the most stable and reliable one, reinforcing the positives of transitioning to renewable sources of energy.

# Can we identify what other indicators were hit the hardest by the crash?
# besides grid framework, household efficiency seems to have plateaued, while economic incentives had slowed down. Let's look at both the indicators.


srilanka_recession_indicators_household <- rise_southasia |>
  filter(REF_AREA_LABEL == "Sri Lanka") |>
  filter(INDICATOR_LABEL %in% household_indicators)|>
  mutate(year = as.integer(year))

ggplot(srilanka_recession_indicators_household , aes(x = year , y = score , color = INDICATOR_LABEL))+
  geom_smooth(se = F)

# Most indicators here indicate the recession well, especially energy efficiency in the public sector and energy efficiency governance. 

srilanka_recession_indicators_incentives <- rise_southasia |>
  filter(REF_AREA_LABEL == "Sri Lanka") |>
  filter(INDICATOR_LABEL %in% economic_indicators)|>
  mutate(year = as.integer(year))

ggplot(srilanka_recession_indicators_incentives , aes(x = year , y = score , color = INDICATOR_LABEL))+
  geom_smooth(se = F)

# While consumer affordability of electricity and financing mechanism for energy efficiency fell a little bit, the incentives for energy utility programs stayed rising. This means that the government kept subsidising electricity to the people, or the country had found a much cheaper source of electricity. 
# Is there any other source of electricity that Sri Lanka utilises heavily? Yes, they rely on hydropower the same way Nepal does. However, Sri Lanka suffers from the risk of droughts, which is why their sources of energy are still unstable and need restructuring. 

#Takeaways from this data exploration:
# Solar Power has not been uniformly adopted, or utilised by all South Asian countries, and justifiably so. 
# Most countries here are extremely sensitive to instability inside their countries, from natural calamities to civil insurgencies. Any such event affects their economies and long-term goals strongly.
# There is a noticeable gap between making laws and implementing them.Therefore, the best way to understand how the country is transitioning or adapting to newer sources to energy is by looking at indicators that reflect a bottom-up approach, like clean cooking.


# I am making a metric that shows us the preparedness of a country to switch from fossil fuels to renewable sources.
# This metric will be scaled between 0 and 100, 100 indicating that the country has all the necessary infrastructure and incentives to switch to a purely non-fossil fuel energy framework. 
# We use 4 pillars to achieve this. One indicator that shows us a top-down approach, one that shows us the bottom-up approach, one that addresses the gap between the first two pillars, basically one that shows the implementation depth, and the last shows us the proportion of imports that are still fuel.

# top_down_pillar <- renewables_indicators + economic_indicators
# bottom_up_pillar <- cooking_indicators + grid_indicators
# implementation_pillar <- household_indicators
# import_dependency_pillar <- fuel_clean

# Pillar 1: Top-Down — Economic Incentives + Renewables Policy
p1 <- rise_subcat_southasia |>
  filter(sub_categories %in% c("Economic Incentives", "Renewables Policy")) |>
  group_by(REF_AREA_LABEL, year) |>
  summarize(topdown_score = mean(indicator_score, na.rm = TRUE)) |>
  rename(country = REF_AREA_LABEL) |>
  select(country , year, topdown_score) |>
  mutate(year = as.integer(year))

# Pillar 2: Bottom-Up — Clean Cooking + Grid Framework
p2 <- rise_subcat_southasia |>
  filter(sub_categories %in% c("Clean Cooking", "Grid Framework")) |>
  group_by(REF_AREA_LABEL, year) |>
  summarize(bottomup_score = mean(indicator_score, na.rm = TRUE)) |>
  rename(country = REF_AREA_LABEL) |>
  select(country , year, bottomup_score)|>
  mutate(year = as.integer(year))

# Pillar 3: Implementation Depth — Household & Efficiency
p3 <- rise_subcat_southasia |>
  filter(sub_categories == "Household & Efficiency") |>
  rename(country = REF_AREA_LABEL) |>
  select(country , year, efficiency_score = indicator_score)|>
  mutate(year = as.integer(year))

# Pillar 4: Fuel Import Reduction — year by year change from 2010 baseline
p4 <- fuel_clean |>
  group_by(country) |>
  mutate(fuel_score = 100 - fuel_imports_pct) |>
  select(country, year, fuel_score)|>
  mutate(year = as.integer(year))

# Assembling Energy Transition Preparedness Index (ETPI) per year
etpi_yearly <- p1 |>
  left_join(p2, by = c("country", "year")) |>
  left_join(p3, by = c("country", "year")) |>
  left_join(p4, by = c("country", "year")) |>
  group_by(year) |>
  mutate(
    ETPI = (topdown_score + bottomup_score + efficiency_score + fuel_score) / 4
  ) |>
  arrange(country, year)


# Plot
etpi_plot <- ggplot(etpi_yearly, aes(x = year, y = ETPI, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Energy Transition Preparedness Index (ETPI) Over Time",
    subtitle = "South Asian countries, 2010–2023",
    x = "Year", y = "ETPI Score (0-100)", color = "Country"
  ) +
  theme_minimal()

etpi_plot

# Interactive -- plotly is above and beyond
ggplotly(etpi_plot)

# India scores the highest here, even though they are the highest in fuel imports, they still have enough laws and implementation in place to be prepared to transition. It should be noted that the country hasn't experienced civil unrest or natural calamities to the same extent as the other countries mentioned here, and the larger size and stronger economy of the country equips it with more safety nets than others.
# Pakistan is next here, showing a slow but dedicated approach towards becoming more prepared.
# Bangladesh does not have much data here, but they show fragmented and inconsistent growth. As discussed before, the country relies on domestic reserves for their needs.
# Nepal shows consistent growth, as proved before, and has the ability to become even more prepared with the new, more progressive government and existing hydropower infrastructure.
# Sri Lanka shows growth as well, but seems to have slowed down due to the recession. However, over time and recovery of the economy, they also have the ability to increase their preparedness.


# Saving dataframes

write.csv(rise_visual , "rise_visual.csv" , row.names = F)
write.csv(compare_solar_fuel , "compare_solar_fuel.csv" , row.names = F)
write.csv(india_policy_outcome , "india_policy_outcome.csv" , row.names = F)
write.csv(pak_policy_outcomes , "pak_policy_outcomes.csv" , row.names = F)
write.csv(grid_bgd , "grid_bgd.csv" , row.names = F)
write.csv(nep_policy_outcomes , "nep_policy_outcomes.csv" , row.names = F)
write.csv(sl_policy_outcomes , "sl_policy_outcomes.csv" , row.names = F)
write.csv(etpi_yearly , "etpi_yearly.csv" , row.names = F)

install.packages("mapdata")
install.packages("maps")

# BEGIN AI ASSISTED CODE ******************************************************************
# India Solar Potential — Bubble Map
# Source: Pareek et al. (2024), Solar Energy Advances 4, 100052

library(maps)
library(mapdata)

# beyond the ETPI metric, we can also take a step further and evaluate the potential each region has in these countries to supplement the transition. Here, I have used India and Nepal, because they have the largest infrastructure in solar and hydropower across South Asia, respectively

# State centroids (approximate) + GHI from Table 1, Pareek et al. (2024)
india_solar <- data.frame(
  state = c("Andhra Pradesh", "Assam", "Bihar", "Chhattisgarh", "Delhi",
            "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand",
            "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur",
            "Meghalaya", "Odisha", "Punjab", "Rajasthan", "Tamil Nadu",
            "Telangana", "Uttar Pradesh", "Uttarakhand", "West Bengal"),
  long = c(79.7, 92.9, 85.3, 81.9, 77.1, 74.1, 71.5, 76.1, 77.2, 85.3,
           75.7, 76.2, 78.6, 75.7, 93.9, 91.4, 85.1, 75.3, 74.2, 78.7,
           79.0, 80.9, 79.3, 87.9),
  lat  = c(15.9, 26.2, 25.1, 21.3, 28.6, 15.3, 22.3, 29.1, 31.1, 23.6,
           15.3, 10.8, 22.9, 19.7, 24.7, 25.5, 20.9, 31.1, 27.0, 11.1,
           17.4, 26.8, 30.1, 22.9),
  GHI = c(1796, 1454, 1600, 1745, 1741, 1573, 1797, 1663, 1520, 1752,
          1809, 1866, 1798, 1736, 1580, 1371, 1654, 1683, 1854, 1725,
          1814, 1662, 1537, 1491)
)

india_outline <- map_data("worldHires", region = "India")

ggplot() +
  geom_polygon(data = india_outline,
               aes(x = long, y = lat, group = group),
               fill = "grey90", color = "white", linewidth = 0.3) +
  geom_point(data = india_solar,
             aes(x = long, y = lat, size = GHI, color = GHI),
             alpha = 0.8) +
  scale_color_gradient(low = "#fff7bc", high = "#d95f0e",
                       name = "GHI\n(kWh/m²/year)") +
  scale_size_continuous(range = c(3, 10), name = "GHI\n(kWh/m²/year)") +
  guides(size = "none") +
  coord_fixed(1.3) +
  labs(
    title = "Solar Energy Potential Across Indian States",
    subtitle = "Bubble size and color = Total Annual GHI — higher = more solar potential",
    caption = "Source: Pareek et al. (2024), Solar Energy Advances 4, 100052"
  ) +
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

# END AI ASSISTED CODE *********************************************************************

