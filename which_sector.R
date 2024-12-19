print("Which sector more likely by area focus?")
df_commercial_focus <- 
  main_sector_summary |> 
  dplyr::filter(focused_on_commercial) |>
  dplyr::select(-c(starts_with("focused_on"))) |>
  tidyr::pivot_longer(everything()) 

df_residential_focus <- 
  main_sector_summary |> 
  dplyr::filter(focused_on_residential) |>
  dplyr::select(-c(starts_with("focused_on"))) |>
  tidyr::pivot_longer(everything()) 

df_industrial_focus <- 
  main_sector_summary |> 
  dplyr::filter(focused_on_industrial) |>
  dplyr::select(-c(starts_with("focused_on"))) |>
  tidyr::pivot_longer(everything()) 

df_pp_focus <- 
  main_sector_summary |> 
  dplyr::filter(focused_on_public_or_pp) |>
  dplyr::select(-c(starts_with("focused_on"))) |>
  tidyr::pivot_longer(everything()) 

which_sector <- function(df) {
  data.frame(
    focus = c("development", "investment", "pm", "architecture", "construction", "other"),
    count = c(
      sum((df |> dplyr::filter(grepl("development", name)))$value),
      sum((df |> dplyr::filter(grepl("investment", name)))$value),
      sum((df |> dplyr::filter(grepl("pm", name)))$value),
      sum((df |> dplyr::filter(grepl("architecture", name)))$value),
      sum((df |> dplyr::filter(grepl("construction", name)))$value),
      sum((df |> dplyr::filter(grepl("other", name)))$value)
    )) |>
    dplyr::mutate(total = sum(df$value)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ratio = count / total
    ) |>
    dplyr::arrange(ratio)
}

which_sector_commercial_focus_df <- which_sector(df_commercial_focus)
which_sector_residential_focus_df <- which_sector(df_residential_focus)
which_sector_industrial_focus_df <- which_sector(df_industrial_focus)
which_sector_pp_focus_df <- which_sector(df_pp_focus)

