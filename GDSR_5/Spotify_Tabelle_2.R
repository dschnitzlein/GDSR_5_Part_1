library(tidyverse)
library(gt)
library(gtExtras)
library(readxl)

Spotify_Top_10 <- read_excel("Spotify_Top_10_GER_multiple_Days.xlsx", 
                                            col_types = c("numeric",
                                                          "text", 
                                                          "text", 
                                                          "numeric", 
                                                          "date",
                                                          "date"))

Spotify <- Spotify_Top_10 |>
  rename(Dauer_alt = Dauer) |> 
  mutate(minutes = minute(Dauer_alt),
         seconds = second(Dauer_alt),
         Dauer = minutes*60 + seconds) |> 
  select(-c(Dauer_alt))


# Alte Tabelle ohne Filter

Spotify |> 
    select(-c("minutes", "seconds")) |> 
  gt() |> 
  cols_label(Kuenstler_erster="Erste(r) Interpret(in)",
             Wiedergaben = "Anzahl Wiedergaben") |> 
  tab_style(
    style = "font-weight: bold",
    locations = cells_column_labels()
  ) |> 
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
  opt_stylize(style=3) 

## Mit Filter

Spotify |> 
  filter(Datum == as_date("2025-01-16")) |> 
  select(-c("minutes", "seconds", "Datum")) |> 
  gt() |> 
  cols_label(Kuenstler_erster="Erste(r) Interpret(in)",
             Wiedergaben = "Anzahl Wiedergaben") |> 
  tab_style(
    style = "font-weight: bold",
    locations = cells_column_labels()
  ) |> 
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
  opt_stylize(style=3) 


## Mit Loop

Tage <- c(as_date("2025-01-15"), as_date("2025-01-16"))
Sys.setlocale("LC_TIME", "de_DE") # Damit später im Subtitle Januar und nicht January steht

for (i in Tage) {
  
tabnam <- paste0("Spotify_Table_", as_date(i), ".png")
  
  Spotify |> 
    filter(Datum == as_date(i)) |> 
    select(-c("minutes", "seconds", "Datum")) |> 
    gt() |> 
    cols_label(Kuenstler_erster="Erste(r) Interpret(in)",
               Wiedergaben = "Anzahl Wiedergaben") |> 
    tab_style(
      style = "font-weight: bold",
      locations = cells_column_labels()
    ) |> 
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
      subtitle = format(as_date(i), format = "%d. %B %Y")
    ) |> 
    tab_source_note(source_note = paste0("Note: Die Tabelle zeigt die 10 meistgespielten Songs auf Spotify am ", format(as_date(i-1), format = "%d. %B %Y"), ". Datenabruf am ", format(as_date(i), format = "%d. %B %Y"), ". \n Quelle: Spotify.")) |> 
    opt_stylize(style=3) |> 
    gtsave_extra(tabnam, vwidth=850, vheight=570, zoom=4)
}