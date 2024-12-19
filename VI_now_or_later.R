VI_now_or_later_df <- 
  flowchart_df |> 
    dplyr::rowwise() |>
    dplyr::mutate(
      development = min(development_city, development_commercial, development_residential, development_industrial, na.rm=TRUE),
      development = ifelse(development == Inf, NA, development),
      investment = min(investment_other, REIT, asset_management, na.rm=TRUE),
      investment = ifelse(investment == Inf, NA, investment),
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(development_commercial, development_residential, development_industrial, development_city, investment_other, REIT, asset_management)) |>
    pivot_longer(-c(Name, created_year)) |>
    dplyr::filter(!(is.na(value))) |>
    dplyr::mutate(
      delta = value - created_year
    ) |>
    dplyr::rename(sector = name) |>
      dplyr::mutate(
      sector_first = delta == 0,
      VI_later = delta > 0
    ) |>
    dplyr::group_by(Name) |>
    dplyr::filter(any(sector_first == 0)) |>
    dplyr::mutate(VI_first = ifelse(sum(sector_first) > 1, TRUE, FALSE)) |>
    dplyr::ungroup() |> 
    dplyr::select(Name, VI_first, VI_later) |>
    dplyr::group_by(Name) |>
    dplyr::summarise(
      VI_first = any(VI_first),                    
      VI_later = any(VI_later)
    )

  nrow(VI_now_or_later_df |> dplyr::group_by(Name))
    
     
