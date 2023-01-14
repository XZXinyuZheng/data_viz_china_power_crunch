# setup -------------------------------------------------------------------

library(stringr)
library(ggh4x)
library(ggstream)
library(lubridate)
library(treemapify)
library(tidyverse)
Sys.setlocale("LC_TIME", "English")


# data viz 01 -------------------------------------------------------------

# Load in the coal price data

price <- 
  readxl::read_excel('data/coal_price.xlsx') %>% 
  transmute(
    date = as_date(date),
    electricity_price,
    electricity_cost = coal_price / (1000000 / 300 )) 
# %>% pivot_longer(
#     cols = electricity_price:electricity_cost,
#     names_to = 'price_or_cost',
#     values_to = 'value')

# Plot the coal price data

price %>% 
  ggplot(
    aes(x = date)) +
  stat_difference(
    aes(
      ymin = electricity_price, 
      ymax = electricity_cost), 
    alpha = 0.3) +
  geom_line(
    aes(
      y = electricity_price),
    color = '#92CFFA',
    size = 1) +
  geom_line(
    aes(
      y = electricity_cost),
    color = '#FA6B66',
    size = 1) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("#FA6B66"), 
      colorspace::lighten("#92CFFA"), 
      "grey60")) +
  scale_x_date(
    breaks = 
      seq(
        as_date('2020-01-01'),
        as_date("2022-01-01"),
        by = '3 month'),
    date_labels = '%m') +
  scale_y_continuous(
    limits = c(0.15, 0.260),
    expand = c(0, 0)) +
  
  # Set label
  
  labs(
    y = 'The price or cost of per kWh of electricity (RMB)') +
  
  # Set theme
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    # axis.line = element_line(colour = '#dcdcdc'),
    axis.ticks = element_line(colour = '#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))

# Load in Guangdong power consumption data

guangdong_cons <- 
  readxl::read_excel('data/guangdong_power_consumption.xlsx') %>% 
  mutate(
    date = as_date(date))

# Load in Guangdong power generation data

guangdong_prod <- 
  readxl::read_excel('data/guangdong_power_generation.xlsx') %>%   
  mutate(
    date = as_date(date),
    source = fct_relevel(
      source,
      c('solar',
        'hydro',
        'wind',
        'nuclear',
        'thermal')))

# Plot the coal production data

guangdong_prod %>% 
  ggplot() +
  geom_area(
    aes(
      x = date,
      y = generation_100_million_kwh,
      fill = source,
      color = source),
    alpha = 0.8) +
  guangdong_cons %>% 
  geom_line(
    mapping = 
      aes(
        x = date,
        y = consumption_100_million_kwh),
    color = '#A6C2F5',
    size = 1,
    linetype = 'dashed') +
  
  # Set Scale
  
  scale_fill_manual(
    values = c('#DBC5FA', 
               '#C4E5FA',
               '#B7DFAE',
               '#F5CFCE',
               '#FADE9D')) +
  scale_color_manual(
    values = c('#B19DFA',
               '#91BFFA',
               '#ADDE98',
               '#F5BABA',
               '#FACB71')) +
  scale_x_date(
    breaks = 
      seq(
        as_date('2020-01-01'),
        as_date("2022-01-01"),
        by = '3 month'),
    date_labels = '%m') +
  scale_y_continuous(
    limits = c(0, 850),
    expand = c(0, 0)) +
  
  # Set label
  
  labs(
    y = 'Power generation (100 million kwh)') +
  
  # Set theme
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    # axis.line = element_line(colour = '#dcdcdc'),
    axis.ticks = element_line(colour = '#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))


# data viz 02 -------------------------------------------------------------

# Load in temperature data

tem <- 
  readxl::read_excel('data/chengdu_temperature.xlsx') %>%
  select(!avg) %>% 
  mutate(
    date = as_date(date)) %>% 
  pivot_longer(
    cols = max:min,
    names_to = 'temperature',
    values_to = 'value')

# Plot temperature data

tem %>% 
  ggplot(
    aes(
      x = date,
      y = value,
      color = temperature)) +
  geom_line(
    size =1) +
  
  # Set Scale
  
  scale_color_manual(
    values = c('#FA6B66', '#92CFFA')) +
  scale_x_date(
    breaks = 
      seq(
        as_date('2021-01-01'),
        as_date("2022-10-31"),
        by = '3 month'),
    date_labels = '%m') +
  scale_y_continuous(
    limits = c(20, 110),
    expand = c(0, 0)) +
  
  # Set label
  
  labs(
    y = 'Fahrenheit Degree (°F)') +
  
  # Set theme
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    # axis.line = element_line(colour = '#dcdcdc'),
    axis.ticks = element_line(colour = '#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))

# Load in the Sichuan energy consumption data

sichuan_cons <- 
  readxl::read_excel('data/sichuan_energy_consumption.xlsx') %>%
  mutate(
    date = as_date(date)) 

sichuan_cons_main <- 
  sichuan_cons %>% 
  filter(
    date %in% as_date("2022-07-01"):as_date("2022-10-01"))

# Plot the Sichuan energy consumption data

sichuan_cons %>% 
  ggplot(
    aes(x = date,
        y = energy_consumption_10000_tons_of_standard_coal)) +
  geom_line(
    color = '#F5BC2C',
    size =1,
    alpha = 0.7) +
  geom_point(
    shape = 16,
    size = 4,
    color = '#F5BC2C',
    alpha = 0.7) +
  geom_point(
    shape = 16,
    size = 2,
    color = 'white') +
  
  # Layer the part I want to emphasis
  
  geom_line(
    sichuan_cons_main,
    mapping = aes(x = date,
                  y = energy_consumption_10000_tons_of_standard_coal),
    color = '#F5832C',
    size =1) +
  geom_point(
    sichuan_cons_main,
    mapping = aes(x = date,
                  y = energy_consumption_10000_tons_of_standard_coal),
    shape = 16,
    size = 4,
    color = '#F5832C') +
  geom_point(
    sichuan_cons_main,
    mapping = aes(x = date,
                  y = energy_consumption_10000_tons_of_standard_coal),
    shape = 16,
    size = 2,
    color = 'white') +
  
  # Set scale
  
  scale_y_continuous(
    limits = c(650, 950),
    expand = c(0, 0)) +
  scale_x_date(
    breaks = 
      seq(
        as_date('2021-01-01'),
        as_date("2022-11-01"),
        by = '3 month'),
    date_labels = '%m') +
  # coord_cartesian(
  #   ylim = c(600, 1000)) +
  
  # Set label
  
  labs(
    y = 'Energy consumption (10000 tons of standard coal)') +
  
  # Set theme
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    # axis.line = element_line(colour = '#dcdcdc'),
    axis.ticks = element_line(colour = '#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))


# data viz 03 -------------------------------------------------------------

# Load in Sichuan power generation data

sichuan_gen <- 
  readxl::read_excel('data/sichuan_power_generation.xlsx') %>%   
  filter(
    source != 'nuclear') %>% 
  mutate(
    date = as_date(date),
    source = fct_relevel(
      source,
      c('solar',
        'wind',
        'thermal',
        'hydro'))) 

# Plot Sichuan power generation data

sichuan_gen %>% 
  ggplot(
    aes(
      x = date,
      y = generation_100_million_kwh,
      fill = source,
      color = source)) +
  geom_area(
    alpha = 0.8) +
  
  # Set Scale
  
  scale_fill_manual(
    values = c('#DBC5FA', 
               '#B7DFAE',
               '#FADE9D',
               '#C4E5FA')) +
  scale_color_manual(
    values = c('#B19DFA', 
               '#ADDE98',
               '#FACB71',
               '#91BFFA')) +
  scale_x_date(
    breaks = 
      seq(
        as_date('2021-01-01'),
        as_date("2022-10-31"),
        by = '3 month'),
    date_labels = '%m') +
  scale_y_continuous(
    limits = c(0, 600),
    expand = c(0, 0)) +
  
  # Set label
  
  labs(
    y = 'Power generation (100 million kwh)') +
  
  # Set theme
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    # axis.line = element_line(colour = '#dcdcdc'),
    axis.ticks = element_line(colour = '#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))

# data viz 04 -------------------------------------------------------------

# Load in coal production, imports and consumption data

coal_petro <- 
  readxl::read_excel('data/coal_petro.xls') %>% 
  mutate(
    across(
      where(is.character),
      as.numeric)) %>% 
  mutate(
    `Net Production of Petroleum` = 
      `Production of Petroleum(10000 tons)` - 
      `Exports of Petroleum (-)(10000 tons)`,
    `Net Production of Coal` = 
      `Production of Coal(10000 tons)` - 
      `Exports of Coal (-)(10000 tons)`,
    .keep = 'unused') %>% 
  pivot_longer(
    cols = `Imports of Petroleum(10000 tons)`:
      `Net Production of Coal`,
    names_to = 'indicator',
    values_to = 'value') %>% 
  mutate(
    value = as.numeric(value),
    type = if_else(
      indicator %>% 
        str_detect('Coal'),
      'coal',
      'petroleum'),
    indicator = indicator %>% 
      str_remove('[:blank:]\\(-\\)') %>% 
      str_remove('\\(10000 tons\\)') %>% 
      str_remove('[:blank:]of[:blank:]Coal') %>% 
      str_remove('[:blank:]Coal') %>% 
      str_remove('[:blank:]of[:blank:]Petroleum') %>% 
      str_remove('[:blank:]Petroleum')) %>% 
  set_names(
    names(.) %>% 
      tolower())

# Plot the coal production, imports and consumption data

barwidth <-  0.4

coal_petro %>%
  filter(
    type == 'coal',
    indicator != 'Total Consumption') %>% 
  ggplot() +
  
  # Rectangular for decreasing area in the background
  
  geom_rect(
    aes(
      xmin = 2013 - (1/2) * barwidth,
      xmax = 2016 + (3/2) * barwidth + 0.1,
      ymin = -Inf,
      ymax = Inf),
    alpha = 0.2,
    fill = '#edf8fb') +
  
  # Rectangular for increasing area in the background
  
  geom_rect(
    aes(
      xmin = 2017 - (1/2) * barwidth - 0.1,
      xmax = 2020 + (3/2) * barwidth + 0.01,
      ymin = -Inf,
      ymax = Inf),
    alpha = 0.2,
    fill = '#ffffe5') +
  
  # Coal production and Imports
  
  geom_bar(
    aes(
      x = as.numeric(year),
      y = value / 10000,
      fill = indicator),
    position = 'stack',
    stat = 'identity',
    width = barwidth) +
  
  # Coal Consumption
  
  geom_bar(
    data = coal_petro %>% 
      filter(
        type == 'coal',
        indicator == 'Total Consumption'),
    aes(
      x = as.numeric(year) + barwidth + 0.01,
      y = value / 10000,
      fill = indicator),
    stat = 'identity',
    width = barwidth)  +
  
  # Adjust scales
  
  scale_x_continuous(
    n.breaks = 20,
    limits = c(2001.5, 2021)) +
  scale_y_continuous(
    limits = c(0, 50),
    expand = c(0, 0)) +
  scale_fill_manual(
    values = c('#8c96c6', '#8856a7', '#fed976')) +
  
  # Set labels
  
  labs(
    title = 
      'Coal Production, Imports, and Consumption in China from 2002 to 2020',
    subtitle = 
      paste(
        'Experiencing a rapid expansion in coal production, China',
        'intentionally reduced its coal production in 2013, committing to its',
        '\ndecarbonization promise by widely closing under-standardized coal',
        'miners. However, under the pressure of high coal \nconsumption, which',
        'even cannot be alleviated by coal imports, China started to rebound',
        'its coal production in 2017.'),
    x = 'Year',
    y = 'Production / Import / Consumption (10 Million Tons)',
    caption = 'Source: National Bureau of Statistic China',
    fill = '') +
  
  # Set theme
  
  theme(
    plot.title = element_text(
      face = 'bold', 
      size = 14, 
      hjust = 0),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0),
    plot.caption = element_text(size = 11),
    axis.title.x = element_text(
      face = 'bold.italic',
      size = 12),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = '#dcdcdc'),
    axis.ticks = element_line('#dcdcdc'),
    legend.position = 'bottom',
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = 'white'),
    panel.background = element_rect('white'),
    panel.grid.major.y = 
      element_line(
        color = '#f0f0f0',
        linetype = 'solid'))


# data viz 05 -------------------------------------------------------------

# Load in data of coal consumption by sectors

coal <- 
  readxl::read_excel('data/coal_consumption_by_sector.xls')

# Plot data of coal consumption by sectors

coal %>% 
  ggplot(
    aes(
      area = value,
      subgroup = sector,
      subgroup2 = subsector,
      fill = subsector,
      label = indicator)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_subgroup2_border(
    color = 'white',
    size  = 3,
    show.legend = FALSE) +
  geom_treemap_subgroup_border(
    color = 'white',
    size = 5,
    show.legend = FALSE) +
  geom_treemap_text(
    fontface = "italic",
    color = "white",
    place = "center",
    grow = FALSE,
    reflow = TRUE) +
  geom_treemap_subgroup2_text(
    color = "white",
    place = "topleft",
    grow = FALSE,
    reflow = TRUE,
    alpha  = 0.7) +
  
  # Set scale
  
  scale_fill_brewer(
    type = 'qual',
    palette = 7) +
  
  # Set labels
  
  labs(
    title = 
      paste(
        "China's Coal Consumption in 2020 Concentrated on the Supply of,'
        'Electricity and Heat, and Fuel Production \nand Processing"),
    subtitle = 
      paste(
        'The majority of coal consumption in China is for the production and',
        'supply of electric and heat power, as well as mining， manufacturing,',
        '\nand processing of coal, petroleum, and other fuels, which suggests two',
        'main approaches to reducing coal consumption - improving the \nefficiency',
        'in fuel production and processing, and leveraging the renewables in',
        'electricity and heat supply'),
    caption = 'Source: National Bureau of Statistics of China') +
  
  # Set theme
  
  theme(
    plot.title = element_text(
      face = 'bold', 
      size = 15, 
      hjust = 0),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0),
    plot.caption = element_text(size = 11))


# data viz 06 -------------------------------------------------------------

# Load in data of global energy consumption

coal_world <- 
  read_csv('data/global_energy_consumption_by_source_and_region.csv') %>% 
  filter(
    !entity %in% 
      c('Africa',
        'Africa (BP)',
        'Asia',
        'Asia Pacific (BP)',
        'CIS (BP)',
        'Central America (BP)',
        'Eastern Africa (BP)',
        'Europe',
        'Europe (BP)',
        'European Union (27)',
        'High-income countries',
        'Low-income countries',
        'Lower-middle-income countries',
        'Upper-middle-income countries',
        'Middle Africa (BP)',
        'Middle East (BP)',
        'Non-OECD (BP)',
        'North America',
        'North America (BP)',
        'OECD (BP)',
        'Other Africa (BP)',
        'Other Asia Pacific (BP)',
        'Other CIS (BP)',
        'Other Caribbean (BP)',
        'Other Europe (BP)',
        'Other Middle East (BP)',
        'Other North America (BP)',
        'Other Northern Africa (BP)',
        'Other South America (BP)',
        'Other South and Central America (BP)',
        'Other Southern Africa (BP)',
        'South America',
        'South and Central America (BP)',
        'Western Africa (BP)',
        'USSR',
        'World'),
    year %in% 2000:2018)

# Load in GDP data by countries

gdp <- 
  read_csv('assignment/assignment5/BP_gdp.csv') %>% 
  filter(
    !entity %in% 
      c('East Asia',
        'Eastern Europe',
        'Latin America',
        'Middle East',
        'South and South-East Asia',
        'Sub-Sahara Africa',
        'Western Europe',
        'Western Offshoots',
        'USSR',
        'World'),
    year %in% 2000:2018)

# Join tables can calculate coal use intensity 

coal_intensity <- 
  coal_world %>% 
  left_join(
    gdp,
    by = c('entity', 'year')) %>% 
  filter(
    !is.na(gdp),
    coal_consumption <= 1500) %>% 
  mutate(
    energy_intensity =
      cut(
        coal_consumption / (gdp / 1000000000),
        breaks = seq(0, 2, 0.1),
        right = TRUE,
        include.lowest = TRUE)) %>% 
  filter(!is.na(energy_intensity))

# Plot coal use intensity

coal_intensity %>% 
  ggplot(
    aes(
      x = coal_consumption,
      y = energy_intensity,
      fill = energy_intensity,
      color = energy_intensity)) +
  ggridges::geom_density_ridges(
    alpha = 0.7) +
  
  # Set scale 
  
  scale_y_discrete(
    expand = c(0, 0)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(
    title = 
      'Decrease in the Coal Intensity is the Major Driver of the Reduction in Coal Consumption',
    subtitle = 
      paste(
        'Coal intensity, calculated by coal consumption per GDP, measures the efficiency of coal usage, with high coal intensity \norresponding to low efficiency of coal usage. The coal consumption for countries with low coal intensity (high coal use \nefficiency) distributes at lower places than for countries with high coal intensity (low coal use efficiency).'),
    x = 'Coal consumption in TWh',
    y = 'Coal consumption in TWh per billion international dollars (2011) of GDP',
    caption = 'Source: Maddison Project Database 2020 (Bolt and van Zanden, 2020) and BP Statistical Review of World Energy (2022)') +
  
  # Set theme
  
  theme(
    plot.title = element_text(
      face = 'bold', 
      size = 15, 
      hjust = 0),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0),
    plot.caption = element_text(size = 11),
    axis.title.x = element_text(
      face = 'bold.italic',
      size = 12),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major = element_line(colour = '#f0f0f0'))

# data viz 07 -------------------------------------------------------------

# Load in data of primary energy, renewable energy, and GDP

bp <- 
  list.files(
    path = 'data',
    pattern = 'BP',
    full.names = TRUE) %>% 
  map(
    ~ .x %>% 
      read_csv() %>% 
      set_names(
        names(.) %>% 
          str_replace_all(' ', '_') %>% 
          tolower()) %>% 
      select(!code)) %>% 
  set_names(
    c('gdp',
      'consumption',
      'renewables'))

# Calculate the proportion of primary energy from renewable sources

prop_renew <- 
  left_join(
    bp$gdp %>% 
      filter(
        !entity %in% 
          c('East Asia',
            'Eastern Europe',
            'Latin America',
            'Middle East',
            'South and South-East Asia',
            'Sub-Sahara Africa',
            'Western Europe',
            'Western Offshoots',
            'USSR',
            'World')),
    bp$consumption,
    by = c('entity', 'year')) %>% 
  left_join(
    bp$renewables,
    by = c('entity', 'year')) %>% 
  filter(
    !is.na(primary_energy_consumption) &
      !is.na(renewables),
    renewables <= 17) %>% 
  transmute(
    entity = entity,
    consumption_per_gdp = primary_energy_consumption / (gdp / 10^9),
    renewable =
      cut(
        renewables,
        breaks = 0:17,
        right = TRUE,
        include.lowest = TRUE))

# Plot data of the proportion of energy from renewable sources

prop_renew %>% 
  ggplot(
    aes(
      x = renewable,
      y = consumption_per_gdp,
      fill = renewable,
      color = renewable,
      fill = renewable)) +
  geom_boxplot(
    alpha = 0.7) +
  
  # Set Scale
  
  scale_y_continuous(
    limits = c(0, 10),
    expand = c(0, 0)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  # 
  # Set labels
  
  labs(
    title = 
      'Energy Consumption per GDP by Proportion of the Primary Energy from Renewable Sources \n(Across 71 Counties from 1980 to 2018)',
    subtitle = 
      paste(
        'The average energy consumption decreased with the increase in the',
        'proportion of primary energy from renewable sources, \ncontrolling',
        'for GDP growth over time and across countries. Further, energy',
        'consumption is more dispersed at the lower range \nof the proportion',
        'of primary energy from renewable sources.'),
    x = 'The proportion of primary energy from renewable sources (%)',
    y = 'Total energy consumption in TWh per billion international dollars (2011) of GDP',
    caption = 'Source: Maddison Project Database 2020 (Bolt and van Zanden, 2020) and BP Statistical Review of World Energy (2022)') +
  
  # Set theme
  
  theme(
    plot.title = element_text(
      face = 'bold', 
      size = 15, 
      hjust = 0),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0),
    plot.caption = element_text(size = 11),
    axis.title.x = element_text(
      face = 'bold.italic',
      size = 12),
    axis.title.y = element_text(
      face = 'bold.italic',
      size = 12),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = '#dcdcdc'),
    axis.ticks = element_line('#dcdcdc'),
    legend.position = "none",
    panel.background = element_rect('white'),
    panel.grid.major.y = element_line(colour = '#f0f0f0'))


# data viz 08 -------------------------------------------------------------

# Create the list of 20 countries with highest GDP

gdp_20 <- 
  filter(
    bp$gdp, 
    !entity %in% 
      c('East Asia',
        'Eastern Europe',
        'Latin America',
        'Middle East',
        'South and South-East Asia',
        'Sub-Sahara Africa',
        'Western Europe',
        'Western Offshoots',
        'USSR',
        'World'),
    year == 2018) %>% 
  arrange(desc(gdp)) %>% 
  slice(1:20) %>% 
  pull(entity)

# Filter to 20 countries with highest GDP 

prop_renew_20 <- 
  left_join(
    bp$gdp %>% 
      filter(
        entity %in% gdp_20,
        year %in% 2000:2018),
    bp$renewables,
    by = c('entity', 'year')) %>% 
  mutate(
    entity = factor(
      entity,
      levels = gdp_20),
    renewables_cat = 
      cut(
        renewables,
        breaks = c(0, 3, 6, 9, 12, 15, 50),
        right = FALSE,
        include_lowest = TRUE))

# Plot the renewable energy development of the 20 countries with highest GDP

prop_renew_20 %>% 
  ggplot(
    aes(
      x = year,
      y = entity)) +
  geom_tile(
    aes(fill = renewables_cat)) +
  geom_vline(
    xintercept = 2006,
    linetype = 'dashed',
    color = 'grey50') +
  
  # Set scale
  
  scale_x_continuous(
    breaks = seq(2000, 2021),
    expand = c(0,0)) +
  scale_y_discrete(limits = rev) + 
  scale_fill_manual(
    values = 
      c(
        # '#deebf7',
        '#c6dbef',
        '#9ecae1',
        '#6baed6',
        '#4292c6',
        '#2171b5',
        '#084594')) +
  
  # Set labels
  
  labs(
    title = 
      paste(
        'The Change in the Proportion of Primary Energy from Renewable Resources',
        '\n(In 20 Countries with the Highest GDP in 2018)'),
    subtitle = 
      paste(
        "The proportion of primary energy from renewable resources in China has",
        "increased rapidly since 2006, thanks to \nthe Renewable Energy Law and",
        "the dramatic rise in the intensity of Feed-in Tariffs (TIF) under",
        "China's 11th Five-year \nPlan (FYP). But still, there is a significant",
        "gap in renewable development between China's and Europe economies."),
    x = '',
    y = '',
    caption = 'Source: Maddison Project Database 2020 (Bolt and van Zanden, 2020) and BP Statistical Review of World Energy (2022)',
    fill = 'The proportion of primary energy from renewable sources (%)') +
  
  # Set theme
  
  theme(
    plot.title = element_text(
      face = 'bold', 
      size = 15, 
      hjust = 0),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0),
    plot.caption = element_text(size = 11),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = '#dcdcdc'),
    axis.ticks = element_line('#dcdcdc'),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.background = element_rect('white'))
