library(tidyverse)
library(gt)
library(gtExtras)
library(readxl)

Spotify_Top_10_GER_2025_01_15 <- read_excel("Spotify_Top_10_GER_2025-01-15.xlsx", 
                                            col_types = c("numeric",
                                                          "text", 
                                                          "text", 
                                                          "numeric", 
                                                          "date"))

Spotify <- Spotify_Top_10_GER_2025_01_15 |>
  rename(Dauer_alt = Dauer) |> 
  mutate(minutes = minute(Dauer_alt),
         seconds = second(Dauer_alt),
         Dauer = minutes*60 + seconds) |> 
  select(-c(Dauer_alt))


Spotify |> 
  select(-c("minutes", "seconds")) |> 
    gt() |> 
  cols_label(Kuenstler_erster="Erste(r) Interpret(in)",
             Wiedergaben = "Anzahl Wiedergaben") |> 
  tab_style(
    style = "font-weight: bold",
    locations = cells_column_labels()
  )|> 
  cols_align(align = "left",
             columns = c("Rang")) |> 
  gt_plt_bar(column = Wiedergaben, color="#F29E4C", scale_type = "number", width=50) |> 
  fmt_duration(
    columns = c("Dauer"),
    input_units = "seconds",
    output_units = c("minutes", "seconds"),
    duration_style = c("colon-sep"),
    use_seps = TRUE,
    sep_mark = ","
  ) |>
  tab_header(
    title = md("**Top 10 Songs auf Spotify in Deutschland**"),
    subtitle = "14. Januar 2024"
  ) |> 
  tab_source_note(source_note = md(
    "Note: Die Tabelle zeigt die 10 meistgespielten Songs auf Spotify am 14. Januar 2025. Datenabruf am 15. Januar 2025. <br> Quelle: Spotify.")) |> 
  opt_stylize(style=3)|> 
  gtsave_extra("Spotify_Table.png", vwidth=850, vheight=570, zoom=4)