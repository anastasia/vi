library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(eulerr)

data <- read.csv("../NAIOP.csv") |>
  tidyr::separate(employee_count, into = c("employee_min", "employee_max"), sep = "-", fill = "right") |>
  dplyr::mutate(
    employee_min = as.integer(employee_min),
    employee_max = as.integer(employee_max),
    employee_count = NA
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    employee_count = ifelse(!is.na(employee_max), mean(c(employee_min, employee_max)), employee_min)
  ) |> 
  dplyr::mutate(employee_count = as.integer(employee_count)) |>
  dplyr::select(-c(employee_min, employee_max, historic)) |>
  dplyr::ungroup() 
# View(data)
# subset only those rows that 
# data <- data |> slice(1:226)
# data <- data |> group_by(Name) |> 
only_odd <- seq_len(nrow(data)) %% 2      
data_odd <- data[only_odd == 1, ]             # Subset odd rows
data_even <- data[only_odd == 0, ] 

sector_cols <- c("created_year", "international", "landlording", "REIT", 
             "investment_other",  "development_commercial", "development_residential", "development_industrial", "development_city", 
             "property_management", "asset_management", "brokerage", "historic_rehabilitation", "redevelopment_renovation", 
             "architecture","engineering","construction", "operator", "consulting", "master_plan", "planning", "leasing", 
             "insurance", "mortgages_loans", "homesales_realty",  "landscaping",  
             "analytics_logistics")
# only one development instead of many; only one investment
sector_unique_cols <- c("landlording", "investment", "development", 
                 "property_management", "asset_management", "brokerage", "architecture","engineering",
                 "construction", "operator", "consulting", "planning", "leasing", 
                 "insurance", "mortgages_loans", "homesales_realty",  "landscaping",  
                 "analytics_logistics")

print("created year, size of firm per firm")
ggplot(data_even |> dplyr::filter(nchar(created_year) > 0), aes(created_year, Name, size = employee_count)) + 
  geom_point() + 
  scale_size_continuous(range = c(1, 10)) 

long_data <- data_even |>
  dplyr::mutate(RowID = row_number(), created_year = as.character(created_year)) |>
  tidyr::pivot_longer(cols = (sector_cols), names_to = "sector", values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  dplyr::filter(nchar(year) > 1) |>
  dplyr::select(RowID, year, sector, Name)
  
  # filter(!is.na(created_year)) 
# |> dplyr::select(created_year, sector, RowID)
ggplot(long_data |> 
         dplyr::filter(sector == "REIT" | sector == "investment_other" | sector == "created_year"),  aes(x = year, y = Name, group = Name, color = as.factor(sector))) +
  #geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_discrete(name = "sector") +
  labs(
    title = "___---__",
    x = "sector",
    y = "Name"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# long_data <- data |>
#   dplyr::select(-c(historic, type_of_company, URL)) |>
#   dplyr::mutate(RowID = row_number()) |>
#   tidyr::pivot_longer(cols = dplyr::select(-c(Name, URL)), names_to = "YearType", values_to = "Year") |>
#   dplyr::filter(!is.na(Year)) |>
#   dplyr::rename(company = Name)
  

#### Ratio of firms active in finance ###
ratio_of_financial_firms <- 
  nrow(long_data |> 
         dplyr::group_by(Name) |> 
         tidyr::pivot_wider(names_from=sector, values_from = c(year)) |>
         dplyr::select(c(REIT, investment_other)) |>
         dplyr::filter(!is.na(REIT) | !is.na(investment_other))
  ) / nrow(long_data |> dplyr::group_by(Name) |> dplyr::filter(sector == "created_year") |> dplyr::summarize()) 


investment_df <- long_data |> 
  dplyr::group_by(Name) |> 
  dplyr::rename(company = Name) |>
  # dplyr::filter(any(c("property_management", "development_commercial", "development_residential", "development_industrial", "development_city") %in% sector) | any(c("REIT", "investment_other") %in% sector)) |>
  dplyr::filter(any(c("REIT", "investment_other", "landlording") %in% sector)) |>
  tidyr::pivot_wider(names_from=sector, values_from = c(year)) |>
  # dplyr::select(c(created_year, REIT, investment_other, landlording, property_management, development_commercial, development_residential, development_industrial, development_city)) |>
  dplyr::select(c(created_year, REIT, investment_other, landlording)) |>
  # dplyr::filter(!is.na(REIT) | !is.na(investment_other)) |>
  #tidyr::pivot_longer(cols = c(created_year, REIT, investment_other, landlording, property_management, development_commercial, development_residential, development_industrial, development_city), values_to = "year") |>
  tidyr::pivot_longer(cols = c(created_year, REIT, investment_other, landlording), values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  tidyr::separate(year, into = c("year_start", "year_end"), sep = "-", fill = "right") |>
  dplyr::mutate(year_start = as.numeric(year_start), year_end = as.numeric(year_end)) |>
  dplyr::mutate(year_end = ifelse(is.na(year_end) & name != "created_year", 2024, year_end))

investment_and_development_and_pm_df <- long_data |> 
  dplyr::group_by(Name) |> 
  dplyr::rename(company = Name) |>
  dplyr::filter(any(c("property_management", "development_commercial", "development_residential", "development_industrial", "development_city") %in% sector) | any(c("REIT", "investment_other") %in% sector)) |>
  tidyr::pivot_wider(names_from=sector, values_from = c(year)) |>
  dplyr::select(c(created_year, REIT, investment_other, landlording, property_management, development_commercial, development_residential, development_industrial, development_city)) |>
  # dplyr::filter(!is.na(REIT) | !is.na(investment_other)) |>
  tidyr::pivot_longer(cols = c(created_year, REIT, investment_other, landlording, property_management, development_commercial, development_residential, development_industrial, development_city), values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  tidyr::separate(year, into = c("year_start", "year_end"), sep = "-", fill = "right") |>
  dplyr::mutate(year_start = as.numeric(year_start), year_end = as.numeric(year_end)) |>
  dplyr::mutate(year_end = ifelse(is.na(year_end) & name != "created_year", 2024, year_end))


plot_with_segment <- function(df, title="investment graph", xlab="name", ylab="company") {
  ggplot(df) +
    # geom_line(size = .2, color = "black") +
    geom_point(aes(x = year_start, y = company, group = company, color = as.factor(name), shape=name), position = position_jitter(width = 0.3, height = 0.2), size = 2) +
    scale_color_discrete(name = xlab) +
    scale_x_continuous(breaks = round(seq(plyr::round_any(min(df$year_start), 10), plyr::round_any(max(df$year_end, na.rm=TRUE) + 10, 10), by = 10),10)) +
    geom_segment(
      data = df |> 
        dplyr::filter(!is.na(year_end)),
      aes(x = year_start, xend = year_end, y = company, yend = company, color=name),
      size = 0.2,
      lineend = "butt"
    ) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    # scale_shape_manual(values=c(8, 16, 17, 5, 4, 3, 10, 12, 15))+
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_with_segment(investment_df, title="investment graph", xlab="name", ylab="company")
plot_with_segment(investment_and_development_and_pm_df, title="investment, development, property management graph", xlab="name", ylab="company")
# ggplot(df) +
#   geom_point(aes(x = start_year, y = type, color = type), size = 3) + # Plot start years as points
#   geom_segment(
#     data = df %>% filter(!is.na(end_year)), # Filter rows where end_year is not NA
#     aes(x = start_year, xend = end_year, y = type, yend = type, color = type),
#     size = 1
#   ) +
#   labs(
#     title = "Visualization of Types with Start and End Years",
#     x = "Year",
#     y = "Type"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

print("<<<<<<<<<<<<<<")
print("how many developers have investment and vice versa out of all developers OR investors")
nrow(investment_and_development_and_pm_df |> 
  dplyr::group_by(company) |> 
  dplyr::filter(any(c("development_commercial", "development_residential", "development_industrial", "development_city") %in% name) & any(c("REIT", "investment_other") %in% name))) /
nrow(investment_df_for_plotting |> 
  dplyr::group_by(company))
print(">>>>>>>>>>>>>>")  
  

print("<<<<<<<<<<<<<<")
print("how many property_managers have investment out of all property_managers OR investors")
nrow(investment_and_development_and_pm_df |> 
       dplyr::group_by(company) |> 
       dplyr::filter(any(c("property_management") %in% name) & any(c("REIT", "investment_other") %in% name))) /
  nrow(investment_df_for_plotting |> 
         dplyr::group_by(company))
print(">>>>>>>>>>>>>>")  



print("<<<<<<<<<<<<<<")
print("what happened to brokers?")
brokerage_df <- long_data |> 
  dplyr::group_by(Name) |> 
  dplyr::rename(company = Name) |>
  dplyr::filter(all(c("brokerage", "created_year") %in% sector)) |>
  tidyr::pivot_wider(names_from=sector, values_from = c(year)) |>
  dplyr::select(c(created_year, brokerage)) |>
  # dplyr::filter(!is.na(REIT) | !is.na(investment_other)) |>
  tidyr::pivot_longer(cols = c(created_year, brokerage), values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  tidyr::separate(year, into = c("year_start", "year_end"), sep = "-", fill = "right") |>
  dplyr::mutate(year_start = as.numeric(year_start)) |>
  dplyr::mutate(year_start= ifelse(!is.na(year_start) & name != "created_year", as.numeric(year_start), as.numeric(year_start[name == "created_year" & company == cur_group_id()])), year_end = as.numeric(year_end)) |>
  dplyr::mutate(year_end = ifelse(is.na(year_end) & name != "created_year", 2024, year_end)) 

plot_with_segment(brokerage_df, title="Brokerage graph", xlab="name", ylab="company")

print(">>>>>>>>>>>>>>")  
print("can we see a latent focus on residential? commercial?")
focus_df <- data_even |>
  dplyr::mutate(RowID = row_number(), created_year = as.character(created_year)) |>
  dplyr::rename(company = Name) |>
  # tidyr::pivot_longer(cols = ( c("created_year", "focused_on_residential", "focused_on_commercial")), names_to = "name", values_to = "year") |>
  tidyr::pivot_longer(cols = ( c("created_year", "focused_on_residential")), names_to = "name", values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  dplyr::filter(nchar(year) > 1) |>
  dplyr::select(RowID, year, name, company) |>
  dplyr::group_by(company) |> 
  tidyr::pivot_wider(names_from=name, values_from = c(year)) |>
  dplyr::select(-c(RowID)) |>
  # dplyr::filter(!is.na(REIT) | !is.na(investment_other)) |>
  #tidyr::pivot_longer(cols = c(created_year, focused_on_residential, focused_on_commercial), values_to = "year") |>
  tidyr::pivot_longer(cols = c(created_year, focused_on_residential), values_to = "year") |>
  dplyr::filter(!is.na(year)) |>
  dplyr::mutate(year = as.numeric(year)) |>
  group_by(company) 

ggplot(focus_df) +
    # geom_line(size = .2, color = "black") +
    geom_point(aes(x = year, y = company, group = company, color = as.factor(name), shape=name), position = position_jitter(width = 0.3, height = 0.2), size = 2) +
    scale_color_discrete(name = "name") +
    scale_x_continuous(breaks = round(seq(plyr::round_any(min(focus_df$year), 10), 2024, by = 10),10)) +
    labs(
      title = "Focus on residential?",
      x = "name",
      y = "company"
    ) +
    # scale_shape_manual(values=c(8, 16, 17, 5, 4, 3, 10, 12, 15))+
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
print("<<<<<<<<<<<<<<")

### count sectors
print(">>>>>>>>>>>>>>")  
print("COUNT SECTORS")
sector_count_df <- data_odd |> 
  dplyr::select(c("Name","landlording", "REIT", 
              "investment_other",  "development_commercial", "development_residential", "development_industrial", "development_city", 
              "property_management", "asset_management", "brokerage", "historic_rehabilitation", "redevelopment_renovation", 
              "architecture","engineering","construction", "operator", "consulting", "master_plan", "planning", "leasing", 
              "insurance", "mortgages_loans", "homesales_realty",  "landscaping",  
            "analytics_logistics")) |>
  dplyr::mutate(developer = NA) |>
  dplyr::rowwise() |>
  dplyr::mutate(developer = ifelse(development_commercial == "X" | development_residential == "X" | development_industrial == "X" | development_city == "X", "X", NA)) |>
  dplyr::mutate(investor = ifelse(REIT == "X" | investment_other == "X", "X", NA)) |>
  dplyr::select(-c(development_commercial, development_residential, development_industrial, development_city, REIT, investment_other)) |>
  dplyr::mutate(count = sum(c_across(!where(is.integer)) == "X", na.rm = TRUE)) %>%
  dplyr::ungroup() |>
  dplyr::left_join(data_even |> dplyr::select(c(Name, created_year, employee_count)), by="Name") |>
  dplyr::filter(!is.na(created_year)) |>
  dplyr::select(c(count, created_year, employee_count)) |>
  dplyr::filter(count > 0)

ggplot(sector_count_df) +
  geom_point(aes(x = created_year, y = count, group = Name), size = 2) 

ggplot(sector_count_df |>
         mutate(decade = floor(created_year / 10) * 10) |>
         dplyr::group_by(decade) |>
         dplyr::summarize(mean_count = mean(count, na.rm = TRUE), total_firms = n())) +
  geom_point(aes(x = decade, y = mean_count, group=decade), size = 2) 
ggplot(sector_count_df) + 
  geom_point(aes(x=created_year, y=count, fill = employee_count)) + 
  scale_fill_gradient(low = "#c9cbfc", high = "blue")  # Set gradient colors
  
print("<<<<<<<<<<<<<< COUNT SECTORS")


print(">>>>>>>>>>>>>>")  
print("delta between founded and second sector")

sector_delta_df <- data_even |>
  dplyr::mutate(across(all_of(sector_cols), ~ as.integer(.))) |>
  dplyr::mutate(created_year = as.integer(created_year)) |>
  dplyr::mutate(development = NA) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    landlording = ifelse(!is.na(landlording), landlording - created_year, NA),
    REIT = ifelse(!is.na(REIT), REIT - created_year, NA),
    investment_other = ifelse(!is.na(investment_other), investment_other - created_year, NA),
    development_commercial = ifelse(!is.na(development_commercial), development_commercial - created_year, NA),
    development_residential = ifelse(!is.na(development_residential), development_residential - created_year, NA),
    development_industrial = ifelse(!is.na(development_industrial), development_industrial - created_year, NA),
    development_city = ifelse(!is.na(development_city), development_city - created_year, NA),
    property_management = ifelse(!is.na(property_management), property_management - created_year, NA),
    asset_management = ifelse(!is.na(asset_management), asset_management - created_year, NA),
    brokerage = ifelse(!is.na(brokerage), brokerage - created_year, NA),
    historic_rehabilitation = ifelse(!is.na(historic_rehabilitation), historic_rehabilitation - created_year, NA),
    redevelopment_renovation = ifelse(!is.na(redevelopment_renovation), redevelopment_renovation - created_year, NA),
    architecture = ifelse(!is.na(architecture), architecture - created_year, NA),
    engineering = ifelse(!is.na(engineering), engineering - created_year, NA),
    construction = ifelse(!is.na(construction), construction - created_year, NA),
    operator = ifelse(!is.na(operator), operator - created_year, NA),
    consulting = ifelse(!is.na(consulting), consulting - created_year, NA),
    master_plan = ifelse(!is.na(master_plan), master_plan - created_year, NA),
    planning = ifelse(!is.na(planning), planning - created_year, NA),
    leasing = ifelse(!is.na(leasing), leasing - created_year, NA),
    insurance = ifelse(!is.na(insurance), insurance - created_year, NA),
    mortgages_loans = ifelse(!is.na(mortgages_loans), mortgages_loans - created_year, NA),
    homesales_realty = ifelse(!is.na(homesales_realty), homesales_realty - created_year, NA),
    landscaping = ifelse(!is.na(landscaping), landscaping - created_year, NA),
    analytics_logistics = ifelse(!is.na(analytics_logistics), analytics_logistics - created_year, NA)
  ) |>
  dplyr::mutate(
    development = min(c(development_city, development_commercial, development_residential, development_industrial, historic_rehabilitation, redevelopment_renovation), na.rm=TRUE),
    investment = min(c(REIT, investment_other), na.rm=TRUE),
    planning = min(c(planning, master_plan), na.rm=TRUE)
  ) |>
  dplyr::mutate(
    development = ifelse(development == Inf, NA, development),
    investment = ifelse(investment == Inf, NA, investment),
    planning = ifelse(planning == Inf, NA, planning),
  ) |>
  dplyr::select(all_of(c("Name", "created_year", "employee_count", sector_unique_cols))) |>
  tidyr::pivot_longer(cols = (sector_unique_cols), names_to = "sector", values_to="delta") |>
  dplyr::group_by(Name, delta) |>
  dplyr::summarize(count = n(), .groups = "drop") |>
  dplyr::filter(!is.na(delta)) 

ggplot(sector_delta_df |> dplyr::filter(delta > -1), aes(x = Name, y = factor(delta), fill = count)) +
  geom_tile(color = "#c9cbfc") +  # Add grid lines
  scale_fill_gradient(low = "#c9cbfc", high = "blue") +  # Set gradient colors
  labs(
    x = "delta", 
    y = "name", 
    fill = "Count",
    title = "Heatmap of Delta Counts by ID"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print("<<<<<<<<<<<<<<<<<< DELTA")  


print(">>>>>>>>>>>>>>>>>>")
print("how many firms are in the sectors we care about")
main_sector_cols = c("sector_development", "sector_investment", "sector_pm", "sector_architecture", "sector_construction")
sectors_df <- data_odd |>
  dplyr::select(c("Name", "type_of_company", "landlording", "REIT", 
                  "investment_other",  "development_commercial", "development_residential", "development_industrial", "development_city", 
                  "property_management", "asset_management", "architecture", "construction")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    sector_development = ifelse((grepl("Development", type_of_company) | development_commercial == "X" | development_residential == "X" | development_industrial == "X" | development_city == "X"), TRUE, FALSE),
    sector_investment = ifelse((grepl("Investment", type_of_company) | grepl("Landlording", type_of_company) | grepl("Asset", type_of_company) | investment_other == "X" | landlording == "X" | REIT == "X" | asset_management == "X"), TRUE, FALSE),
    sector_pm = ifelse((grepl("Property", type_of_company) | property_management == "X"), TRUE, FALSE),
    sector_architecture = ifelse((grepl("Architecture", type_of_company) | architecture == "X"), TRUE, FALSE),
    sector_construction = ifelse((grepl("Construction", type_of_company) | construction == "X"), TRUE, FALSE)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(main_sector_cols)

main_sector_summary <- sectors_df |>
  dplyr::rowwise() |>
  dplyr::mutate(
    development = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0), 
    development_investment = ifelse((sector_development == TRUE & sector_investment == TRUE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0), ,
    development_investment_pm = ifelse((sector_development == TRUE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0),
    development_investment_pm_architecture = ifelse((sector_development == TRUE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0),
    development_investment_pm_architecture_construction = ifelse((sector_development == TRUE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0),
    development_pm = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0),
    development_pm_architecture = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0),
    development_pm_architecture_construction = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0), 
    development_architecture = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0), 
    development_architecture_construction = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0), 
    development_construction = ifelse((sector_development == TRUE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == TRUE), 1, 0), 
    investment = ifelse((sector_development == FALSE & sector_investment == TRUE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0),
    investment_pm = ifelse((sector_development == FALSE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0), 
    investment_pm_architecture = ifelse((sector_development == FALSE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0), 
    investment_pm_architecture_construction = ifelse((sector_development == FALSE & sector_investment == TRUE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0),
    investment_architecture = ifelse((sector_development == FALSE & sector_investment == TRUE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0), 
    investment_architecture_construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0), 
    investment_construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == TRUE), 1, 0), 
    pm = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == FALSE & sector_construction == FALSE), 1, 0), 
    pm_architecture = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0), 
    pm_architecture_construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0), 
    pm_construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == TRUE & sector_architecture == FALSE & sector_construction == TRUE), 1, 0), 
    architecture = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == FALSE), 1, 0), 
    architecture_construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == TRUE & sector_construction == TRUE), 1, 0), 
    construction = ifelse((sector_development == FALSE & sector_investment == FALSE & sector_pm == FALSE & sector_architecture == FALSE & sector_construction == TRUE), 1, 0)
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(count = n(),
                   development = sum(development),
                   development_investment = sum(development_investment),
                   development_investment_pm = sum(development_investment_pm),
                   development_investment_pm_architecture = sum(development_investment_pm_architecture),
                   development_investment_pm_architecture_construction = sum(development_investment_pm_architecture_construction), 
                   development_pm = sum(development_pm), 
                   development_pm_architecture = sum(development_pm_architecture), 
                   development_pm_architecture_construction = sum(development_pm_architecture_construction),
                   development_architecture = sum(development_architecture),
                   development_architecture_construction = sum(development_architecture_construction),
                   development_construction = sum(development_construction),
                   investment = sum(investment),
                   investment_pm = sum(investment_pm),
                   investment_pm_architecture = sum(investment_pm_architecture),
                   investment_pm_architecture_construction = sum(investment_pm_architecture_construction), 
                   investment_architecture = sum(investment_architecture),
                   investment_architecture_construction = sum(investment_architecture_construction),
                   investment_construction = sum(investment_construction),
                   pm = sum(pm),
                   pm_architecture = sum(pm_architecture),
                   pm_architecture_construction = sum(pm_architecture_construction),
                   pm_construction = sum(pm_construction),
                   architecture = sum(architecture),
                   architecture_construction = sum(architecture_construction),
                   construction = sum(construction)
  )

main_sector_summary <- main_sector_summary |> 
  pivot_longer(cols = colnames(main_sector_summary)) |>
  dplyr::filter(value > 0) |> 
  dplyr::arrange(desc(value))

values <- as.numeric((main_sector_summary |> dplyr::filter(name != "count"))$value) 
names(values) <- (main_sector_summary |> dplyr::filter(name != "count"))$name
pie(values, main = "Pie Chart of Categories")
sum(values)
print("<<<<<<<<<<<<<<<<< sectors we care about")
