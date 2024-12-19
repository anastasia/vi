print(">>>>>>>>>>>>>>>>>>")
print("how many firms are in the sectors we care about")
main_sector_cols = c("sector_development", "sector_investment", "sector_pm", "sector_architecture", "sector_construction", "sector_other")
sectors_df <- data_odd |>
  dplyr::filter(type_of_company != "") |>
  # dplyr::select(c("Name", "type_of_company", "landlording", "REIT", 
  #                 "investment_other",  "development_commercial", "development_residential", "development_industrial", "development_city", 
  #                 "property_management", "asset_management", "architecture", "construction")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    sector_development = case_when((grepl("Development", type_of_company) | development_commercial == "X" | development_residential == "X" | development_industrial == "X" | development_city == "X") ~ TRUE),
    sector_investment = case_when((grepl("Investment", type_of_company) | grepl("Landlording", type_of_company) | grepl("Asset", type_of_company) | investment_other == "X" | landlording == "X" | REIT == "X" | asset_management == "X") ~ TRUE),
    sector_pm = case_when((grepl("Property", type_of_company) | property_management == "X") ~ TRUE),
    sector_architecture = case_when((grepl("Architecture", type_of_company) | architecture == "X") ~ TRUE),
    sector_construction = case_when((grepl("Construction", type_of_company) | construction == "X") ~ TRUE),
    sector_other = case_when((!(grepl("Development", type_of_company)) &
                                !(grepl("Investment", type_of_company)) & 
                                !(grepl("Property", type_of_company)) & 
                                !(grepl("Architecture", type_of_company)) & 
                                !(grepl("Construction", type_of_company)) ) |
                               (development_commercial != "X" &
                                  development_residential != "X" &
                                  development_industrial == "X" &
                                  development_city == "X" &
                                  construction != "X" &
                                  investment_other != "X" & 
                                  landlording != "X" &
                                  REIT != "X" &
                                  asset_management != "X" &
                                  property_management != "X" &
                                  architecture != "X" &
                                  construction != "X")
                             ~ TRUE),
  ) |>
  dplyr::ungroup() |>
  dplyr::select(append(
    c("focused_on_commercial", "focused_on_residential", "focused_on_industrial", "focused_on_public_or_pp"),
    main_sector_cols))

### LIST GENERATED PROGRAMMATICALLY IN PYTHON USING ITERTOOLS
### for i in range(len(sectors) + 1):
###    res = list(itertools.combinations(sectors, i))
###    for j in range(len(res)):
###         print("_".join(res[j]))

main_sector_summary <- sectors_df |>
  dplyr::rowwise() |>
  dplyr::mutate(
    focused_on_commercial = focused_on_commercial,
    focused_on_residential = focused_on_residential,
    focused_on_industrial = focused_on_industrial,
    focused_on_public_or_pp = focused_on_public_or_pp,
    # list generated programmatically, see above
    development = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    investment = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    pm = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0),  
    architecture = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    construction = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    other = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    development_investment = ifelse(sector_development & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_pm = ifelse(sector_development & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_architecture = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0),
    development_construction = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0),  
    development_other = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0),  
    investment_pm = ifelse(is.na(sector_development) & sector_investment & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    investment_architecture = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    investment_construction = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    investment_other = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    pm_architecture = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    pm_construction = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    pm_other = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    architecture_construction = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    architecture_other = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    construction_other = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    development_investment_pm = ifelse(sector_development & sector_investment & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_investment_architecture = ifelse(sector_development & sector_investment & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_investment_construction = ifelse(sector_development & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    development_investment_other = ifelse(sector_development & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    development_pm_architecture = ifelse(sector_development  & is.na(sector_investment) & sector_pm & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_pm_construction = ifelse(sector_development  & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    development_pm_other = ifelse(sector_development & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    development_architecture_construction = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    development_architecture_other = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    development_construction_other = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    investment_pm_architecture = ifelse(is.na(sector_development) & sector_investment & sector_pm & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    investment_pm_construction = ifelse(is.na(sector_development) & sector_investment & sector_pm & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    investment_pm_other = ifelse(is.na(sector_development) & sector_investment & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    investment_architecture_construction = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    investment_architecture_other = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    investment_construction_other = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    pm_architecture_construction = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    pm_architecture_other = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    pm_construction_other = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    architecture_construction_other = ifelse(is.na(sector_development) & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & sector_construction & sector_other, 1, 0), 
    development_investment_pm_architecture = ifelse(sector_development & sector_investment & sector_pm & sector_architecture & is.na(sector_construction) & is.na(sector_other), 1, 0), 
    development_investment_pm_construction = ifelse(sector_development & sector_investment & sector_pm & is.na(sector_architecture) & sector_construction & is.na(sector_other), 1, 0), 
    development_investment_pm_other = ifelse(sector_development & sector_investment & sector_pm & is.na(sector_architecture) & is.na(sector_construction) & sector_other, 1, 0), 
    development_investment_architecture_construction = ifelse(sector_development & sector_investment & is.na(sector_pm) & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    development_investment_architecture_other = ifelse(sector_development & sector_investment & is.na(sector_pm) & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    development_investment_construction_other = ifelse(sector_development & sector_investment & is.na(sector_pm) & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    development_pm_architecture_construction = ifelse(sector_development  & is.na(sector_investment) & sector_pm & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    development_pm_architecture_other = ifelse(sector_development  & is.na(sector_investment) & sector_pm & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    development_pm_construction_other = ifelse(sector_development  & is.na(sector_investment) & sector_pm & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    development_architecture_construction_other = ifelse(sector_development & is.na(sector_investment) & is.na(sector_pm) & sector_architecture & sector_construction & sector_other, 1, 0), 
    investment_pm_architecture_construction = ifelse(is.na(sector_development) & sector_investment & sector_pm & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    investment_pm_architecture_other = ifelse(is.na(sector_development) & sector_investment & sector_pm & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    investment_pm_construction_other = ifelse(is.na(sector_development) & sector_investment & sector_pm & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    investment_architecture_construction_other = ifelse(is.na(sector_development) & sector_investment & is.na(sector_pm) & sector_architecture & sector_construction & sector_other, 1, 0), 
    pm_architecture_construction_other = ifelse(is.na(sector_development) & is.na(sector_investment) & sector_pm & sector_architecture & sector_construction & sector_other, 1, 0), 
    development_investment_pm_architecture_construction = ifelse(sector_development & sector_investment & sector_pm & sector_architecture & sector_construction & is.na(sector_other), 1, 0), 
    development_investment_pm_architecture_other = ifelse(sector_development & sector_investment & sector_pm & sector_architecture & is.na(sector_construction) & sector_other, 1, 0), 
    development_investment_pm_construction_other = ifelse(sector_development & sector_investment & sector_pm & is.na(sector_architecture) & sector_construction & sector_other, 1, 0), 
    development_investment_architecture_construction_other =  ifelse(sector_development & sector_investment & is.na(sector_pm) & sector_architecture & sector_construction & sector_other, 1, 0), 
    development_pm_architecture_construction_other = ifelse(sector_development  & is.na(sector_investment) & sector_pm & sector_architecture & sector_construction & sector_other, 1, 0), 
    investment_pm_architecture_construction_other = ifelse(is.na(sector_development) & sector_investment & sector_pm & sector_architecture & sector_construction & sector_other, 1, 0), 
    development_investment_pm_architecture_construction_other = ifelse(sector_development & sector_investment & sector_pm & sector_architecture & sector_construction & sector_other, 1, 0)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    focused_on_commercial = ifelse(focused_on_commercial == "X", TRUE, FALSE),
    focused_on_residential = ifelse(focused_on_residential == "X", TRUE, FALSE),
    focused_on_industrial = ifelse(focused_on_industrial == "X", TRUE, FALSE),
    focused_on_public_or_pp =ifelse(focused_on_public_or_pp == "X", TRUE, FALSE) 
  ) |>
  dplyr::group_by(focused_on_commercial,
                  focused_on_residential,
                  focused_on_industrial,
                  focused_on_public_or_pp) |>
  dplyr::summarise(development = sum(development, na.rm=TRUE),
                   investment = sum(investment, na.rm=TRUE),
                   pm = sum(pm, na.rm=TRUE),
                   architecture = sum(architecture, na.rm=TRUE),
                   construction = sum(construction, na.rm=TRUE),
                   other = sum(other, na.rm=TRUE),
                   development_investment = sum(development_investment, na.rm=TRUE),
                   development_pm = sum(development_pm, na.rm=TRUE),
                   development_architecture = sum(development_architecture, na.rm=TRUE),
                   development_construction = sum(development_construction, na.rm=TRUE),
                   development_other = sum(development_other, na.rm=TRUE),
                   investment_pm = sum(investment_pm, na.rm=TRUE),
                   investment_architecture = sum(investment_architecture, na.rm=TRUE),
                   investment_construction = sum(investment_construction, na.rm=TRUE),
                   investment_other = sum(investment_other, na.rm=TRUE),
                   pm_architecture = sum(pm_architecture, na.rm=TRUE),
                   pm_construction = sum(pm_construction, na.rm=TRUE),
                   pm_other = sum(pm_other, na.rm=TRUE),
                   architecture_construction = sum(architecture_construction, na.rm=TRUE),
                   architecture_other = sum(architecture_other, na.rm=TRUE),
                   construction_other = sum(construction_other, na.rm=TRUE),
                   development_investment_pm = sum(development_investment_pm, na.rm=TRUE),
                   development_investment_architecture = sum(development_investment_architecture, na.rm=TRUE),
                   development_investment_construction = sum(development_investment_construction, na.rm=TRUE),
                   development_investment_other = sum(development_investment_other, na.rm=TRUE),
                   development_pm_architecture = sum(development_pm_architecture, na.rm=TRUE),
                   development_pm_construction = sum(development_pm_construction, na.rm=TRUE),
                   development_pm_other = sum(development_pm_other, na.rm=TRUE),
                   development_architecture_construction = sum(development_architecture_construction, na.rm=TRUE),
                   development_architecture_other = sum(development_architecture_other, na.rm=TRUE),
                   development_construction_other = sum(development_construction_other, na.rm=TRUE),
                   investment_pm_architecture = sum(investment_pm_architecture, na.rm=TRUE),
                   investment_pm_construction = sum(investment_pm_construction, na.rm=TRUE),
                   investment_pm_other = sum(investment_pm_other, na.rm=TRUE),
                   investment_architecture_construction = sum(investment_architecture_construction, na.rm=TRUE),
                   investment_architecture_other = sum(investment_architecture_other, na.rm=TRUE),
                   investment_construction_other = sum(investment_construction_other, na.rm=TRUE),
                   pm_architecture_construction = sum(pm_architecture_construction, na.rm=TRUE),
                   pm_architecture_other = sum(pm_architecture_other, na.rm=TRUE),
                   pm_construction_other = sum(pm_construction_other, na.rm=TRUE),
                   architecture_construction_other = sum(architecture_construction_other, na.rm=TRUE),
                   development_investment_pm_architecture = sum(development_investment_pm_architecture, na.rm=TRUE),
                   development_investment_pm_construction = sum(development_investment_pm_construction, na.rm=TRUE),
                   development_investment_pm_other = sum(development_investment_pm_other, na.rm=TRUE),
                   development_investment_architecture_construction = sum(development_investment_architecture_construction, na.rm=TRUE),
                   development_investment_architecture_other = sum(development_investment_architecture_other, na.rm=TRUE),
                   development_investment_construction_other = sum(development_investment_construction_other, na.rm=TRUE),
                   development_pm_architecture_construction = sum(development_pm_architecture_construction, na.rm=TRUE),
                   development_pm_architecture_other = sum(development_pm_architecture_other, na.rm=TRUE),
                   development_pm_construction_other = sum(development_pm_construction_other, na.rm=TRUE),
                   development_architecture_construction_other = sum(development_architecture_construction_other, na.rm=TRUE),
                   investment_pm_architecture_construction = sum(investment_pm_architecture_construction, na.rm=TRUE),
                   investment_pm_architecture_other = sum(investment_pm_architecture_other, na.rm=TRUE),
                   investment_pm_construction_other = sum(investment_pm_construction_other, na.rm=TRUE),
                   investment_architecture_construction_other = sum(investment_architecture_construction_other, na.rm=TRUE),
                   pm_architecture_construction_other = sum(pm_architecture_construction_other, na.rm=TRUE),
                   development_investment_pm_architecture_construction = sum(development_investment_pm_architecture_construction, na.rm=TRUE),
                   development_investment_pm_architecture_other = sum(development_investment_pm_architecture_other, na.rm=TRUE),
                   development_investment_pm_construction_other = sum(development_investment_pm_construction_other, na.rm=TRUE),
                   development_investment_architecture_construction_other =  sum(development_investment_architecture_construction_other, na.rm=TRUE),
                   development_pm_architecture_construction_other = sum(development_pm_architecture_construction_other, na.rm=TRUE),
                   investment_pm_architecture_construction_other = sum(investment_pm_architecture_construction_other, na.rm=TRUE),
                   development_investment_pm_architecture_construction_other = sum(development_investment_pm_architecture_construction_other, na.rm=TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(count = 0) |>
  dplyr::rowwise() |>
  dplyr::mutate(count = sum(as.numeric(unlist(pick(where(is.numeric)))), na.rm = TRUE))


commercial_focus_df <- main_sector_summary |> 
  dplyr::filter(focused_on_commercial == TRUE) |>
  dplyr::select(-c(
    "focused_on_residential",
    "focused_on_industrial",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_commercial) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_commercial")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_commerical = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_commerical > 0)


residential_focus_df <- main_sector_summary |> 
  dplyr::filter(focused_on_residential == TRUE) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_industrial",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_residential) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_residential")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_residential = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_residential > 0) |>
  dplyr::select(-c(count))


industrial_focus_df <- main_sector_summary |> 
  dplyr::filter(focused_on_industrial == TRUE) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_residential",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_industrial) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_industrial")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_industrial = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_industrial > 0) 

pp_focus_df <- main_sector_summary |> 
  dplyr::filter(focused_on_public_or_pp == TRUE) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_residential",
    "focused_on_industrial")) |>
  group_by(focused_on_public_or_pp) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_public_or_pp")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_public_or_pp = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(value > 0) 

# sector_summary_updated <- main_sector_summary |> 
#   pivot_longer(cols = colnames(main_sector_summary)) |>
#   dplyr::filter(value > 0) |> 
#   dplyr::arrange(desc(value)) |>
#   dplyr::slice(-1) |>
#   dplyr::mutate(sum_of_known = sum(value)) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     ratio = (as.numeric(value) / sum_of_known) * 100
#   )
###### ---------------------------
# firms in single focuses, that is when sectors don't intersect -- ONLY commercial, ONLY residential (not both)

commercial_focus_nonintersect_df <- main_sector_summary |> 
  dplyr::filter(focused_on_commercial & !focused_on_residential & !focused_on_industrial & !focused_on_public_or_pp) |>
  dplyr::select(-c(
    "focused_on_residential",
    "focused_on_industrial",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_commercial) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_commercial")) |>
  tidyr::pivot_longer(cols=-c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_commerical = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_commerical > 0) 


residential_focus_nonintersect_df <- main_sector_summary |> 
  dplyr::filter(!focused_on_commercial & focused_on_residential & !focused_on_industrial & !focused_on_public_or_pp) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_industrial",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_residential) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_residential")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_residential = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_residential > 0) |>
  dplyr::filter(name != "count")


industrial_focus_nonintersect_df <- main_sector_summary |> 
  dplyr::filter(!focused_on_commercial & !focused_on_residential & focused_on_industrial & !focused_on_public_or_pp) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_residential",
    "focused_on_public_or_pp")) |>
  group_by(focused_on_industrial) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_industrial")) |>
  tidyr::pivot_longer(cols = -c(count)) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_industrial = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_industrial > 0) |>
  dplyr::filter(name != "count")

pp_focus_nonintersect_df <- main_sector_summary |> 
  dplyr::filter(!focused_on_commercial & !focused_on_residential & !focused_on_industrial & focused_on_public_or_pp) |>
  dplyr::select(-c(
    "focused_on_commercial",
    "focused_on_residential",
    "focused_on_industrial")) |>
  group_by(focused_on_public_or_pp) |>
  summarise_if(is.numeric, sum, na.rm = TRUE) |>
  dplyr::select(-c("focused_on_public_or_pp")) |>
  tidyr::pivot_longer(cols = -c("count")) |>
  dplyr::rowwise() |>
  dplyr::mutate(proportion_of_public_or_pp = value / count,
                proportion_of_all = value / sum(main_sector_summary$count)) |>
  dplyr::filter(proportion_of_public_or_pp > 0) |>
  dplyr::filter(name != "count")

###### ---------------------------



### ALL vertically integrated sectors
vi_sectors <- main_sector_summary |>
  dplyr::select(-c("focused_on_commercial", "focused_on_residential", "focused_on_industrial", "focused_on_public_or_pp", "count")) |>
  tidyr::pivot_longer(everything()) |>
  dplyr::filter(grepl("_", name)) 
print("Proportion of vertically integrated sectors out of everything") # 67.5%
sum(vi_sectors$value) / sum(main_sector_summary$count)

sector_piechart <- function(df, title) {
  df_ <- df |>
    dplyr::arrange(-value)
  values <- as.numeric(df_$value) 
  names(values) <- df_$name
  return(pie(values, main = title))
}

sector_piechart(commercial_focus_df, "Focused on commercial") 
sector_piechart(residential_focus_df, "Focused on residential")
sector_piechart(industrial_focus_df, "Focused on industrial")
sector_piechart(pp_focus_df, "Focused on public or public-private")


### ------- nonintersecting sectors 
sector_piechart(commercial_focus_nonintersect_df, "Firms only focused on commercial real estate") 
sector_piechart(residential_focus_nonintersect_df, "Firms only focused on residential real estate")
### ------- nonintersect


vertical_integration_proportion <- function(df) {
  df_vi <- df |> 
    dplyr::filter(grepl("_", name)) |>
    summarize(
      vi_value = sum(value, na.rm = TRUE)
    ) 
  df_not_vi <- df |>
    dplyr::filter(!(grepl("_", name))) |>
    summarize(
      not_vi_value = sum(value)
    ) 
  df_ <- merge(df_vi, df_not_vi) |>
    pivot_longer(everything())
  df_  <- df_ |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ratio = value / sum(df$value)
    )
  return(df_)
}
vertical_integration_proportion(commercial_focus_df)
vertical_integration_proportion(residential_focus_df)
vertical_integration_proportion(industrial_focus_df)
vertical_integration_proportion(pp_focus_df)

sector_piechart(vertical_integration_proportion(commercial_focus_df), "Commercial focus: vertically integrated or not?")
sector_piechart(vertical_integration_proportion(residential_focus_df), "Residential focus: vertically integrated or not?")
sector_piechart(vertical_integration_proportion(industrial_focus_df), "Industrial focus: vertically integrated or not?")
sector_piechart(vertical_integration_proportion(pp_focus_df), "Public-private focus: vertically integrated or not?")

### ------- single focuses (ONLY commercial, ONLY residential)
vertical_integration_proportion(commercial_focus_nonintersect_df)
vertical_integration_proportion(residential_focus_nonintersect_df)
### ------- nonintersect

print("<<<<<<<<<<<<<<<<< sectors we care about")

print("singular focus")
singular_focus_df <- data.frame(
  sector = c(
    "development", "investment", "pm", "architecture", "construction", "other"
  ),
  ratio = c(
    sum(main_sector_summary$development) / sum(main_sector_summary$count),
    sum(main_sector_summary$investment) / sum(main_sector_summary$count),
    sum(main_sector_summary$pm) / sum(main_sector_summary$count),
    sum(main_sector_summary$architecture) / sum(main_sector_summary$count),
    sum(main_sector_summary$construction) / sum(main_sector_summary$count),
    sum(main_sector_summary$other) / sum(main_sector_summary$count)
  )
)

print("sectors development will integrate with")
development_sector_integration_df <- 
  data.frame(
    sector = c(
      "development", "investment", "pm", "architecture", "construction", "other"
    ),
    ratio = c(
    sum(main_sector_summary$development) / sum(main_sector_summary$count),
    sum(main_sector_summary$development_investment) / sum(main_sector_summary$count),
    sum(main_sector_summary$development_pm) / sum(main_sector_summary$count),
    sum(main_sector_summary$development_architecture) / sum(main_sector_summary$count),
    sum(main_sector_summary$development_construction) / sum(main_sector_summary$count),
    sum(main_sector_summary$development_other) / sum(main_sector_summary$count)
    )
  )

print("sectors investment will integrate with")
investment_sector_integration_df <- 
  data.frame(
    sector = c(
      "development", "investment", "pm", "architecture", "construction", "other"
    ),
    ratio = c(
      sum(main_sector_summary$development_investment) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_pm) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_other) / sum(main_sector_summary$count)
    )
  )

print("sectors property management will integrate with")
pm_sector_integration_df <- 
  data.frame(
    sector = c(
      "development", "investment", "pm", "architecture", "construction", "other"
    ),
    ratio = c(
      sum(main_sector_summary$development_pm) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_pm) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm_architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm_other) / sum(main_sector_summary$count)
    )
  )

print("sectors architecture will integrate with")
architecture_sector_integration_df <- 
  data.frame(
    sector = c(
      "development", "investment", "pm", "architecture", "construction", "other"
    ),
    ratio = c(
      sum(main_sector_summary$development_architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm_architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$architecture) / sum(main_sector_summary$count),
      sum(main_sector_summary$architecture_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$architecture_other) / sum(main_sector_summary$count)
    )
  )

print("sectors construction will integrate with")
construction_sector_integration_df <- 
  data.frame(
    sector = c(
      "development", "investment", "pm", "architecture", "construction", "other"
    ),
    ratio = c(
      sum(main_sector_summary$development_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$investment_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$pm_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$architecture_construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$construction) / sum(main_sector_summary$count),
      sum(main_sector_summary$construction_other) / sum(main_sector_summary$count)
    )
  )

