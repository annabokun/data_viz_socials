  # ------------------------------------------------------------
  # Anna Bokun
  # The Generational Care Crunch (Census Projections)
  # 80+ vs. 20–64 growth, 2030 → 2060 (Indexed to 2030)
  # Source: U.S. Census Bureau, NP2023, Main series 
  # Table 3. Projected Population by Five-Year Age Group and Sex
  # https://www2.census.gov/programs-surveys/popproj/tables/2023/2023-summary-tables/np2023-t3.xlsx
  # ------------------------------------------------------------
  
  
  # Load required packages ---------------------------  
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(ggplot2)
    library(scales)
    library(purrr)
  
  
  
  # ---- Settings ----
    setwd("~/Library/CloudStorage/GoogleDrive-bokun.anna@gmail.com/My Drive/Socials Data Viz /Generational Care Crunch")
    path <- "np2023-t3.xlsx"      
    start_year <- 2030
    end_year   <- 2060            
  
    
    
  # ---- Load & prep header row ----
   raw <- read_excel(path, sheet = "Main series", col_names = FALSE)
  
    
  # Row 5 contains the year headers (e.g., 2022, 2025, 2030, ...). Make them names:
    hdr <- as.character(unlist(raw[5, ]))
    names(raw) <- hdr
    names(raw)[1] <- "label"
  # Clean "2030.00" -> "2030"
    names(raw) <- str_replace(names(raw), "\\.0+$", "")
  
    
    
  # Identify the main age section (both sexes combined)
    start_idx <- which(str_detect(raw$label, "Under 5 years"))[1]
    end_idx   <- which(str_detect(raw$label, "100 years and over"))[1]
    ages      <- raw[start_idx:end_idx, ]
  
    
    
  # Extract all year columns as numeric vector 
    available_years <- names(ages) %>%
      keep(~ str_detect(.x, "^[12][0-9]{3}$")) %>%  # only 4-digit year-like headers
      as.integer() %>%
      sort()
  
    
  # Keep only years from start_year through end_year that exist
    years_to_use <- available_years[available_years >= start_year & available_years <= end_year]
    stopifnot(length(years_to_use) > 0)
    
    
    
  # Helper to fetch numeric column for a given year
    get_year <- function(df, yr) {
      col <- as.character(yr)
      if (!col %in% names(df)) stop(paste("Year", yr, "not in columns."))
      suppressWarnings(as.numeric(df[[col]]))
    }
  
    
  # Rows for groups
    rows_80plus <- ages %>%
      filter(str_detect(label, "80 to 84|85 to 89|90 to 94|95 to 99|100 years"))
    
    rows_20_64 <- ages %>%
      filter(str_detect(label, "20 to 19|20 to 24|25 to 29|30 to 34|35 to 39|40 to 44|45 to 49|50 to 54|55 to 59|60 to 64"))
    
    
    
  # Build a tidy table of totals (thousands) by group x year
    totals <- tibble(
      year  = years_to_use,
      `Age 80+`   = map_dbl(years_to_use, ~ sum(get_year(rows_80plus, .x), na.rm = TRUE)),
      `Age 20–64` = map_dbl(years_to_use, ~ sum(get_year(rows_20_64, .x),  na.rm = TRUE))
    ) |>
      pivot_longer(cols = c(`Age 80+`, `Age 20–64`), names_to = "group", values_to = "thousands")
    
    
    
  # Baseline values at 2030 for % change
    baseline <- totals |>
      filter(year == start_year) |>
      select(group, base_thousands = thousands)
    
    plot_df <- totals |>
      left_join(baseline, by = "group") |>
      mutate(pct_change = 100 * (thousands / base_thousands - 1))
    
    
  # Quick check of 2030 and 2040 values 
    vals_2030_2040 <- plot_df |>
      filter(year %in% c(2030, 2040)) |>
      select(group, year, thousands) |>
      mutate(millions = thousands / 1000)
    print(vals_2030_2040)
  
    
  # For shaded gap (wide by year)
    wide_pct <- plot_df |>
      select(group, year, pct_change) |>
      pivot_wider(names_from = group, values_from = pct_change)
    
    
    
  # ---- Plot ----
    caption_text <- "Source: U.S. Census Bureau, Projections for the United States: 2022–2100 (NP2023), Main series, Table 3. Resident population (July 1)."

  # White background 
    p <- ggplot(plot_df, aes(x = year, y = pct_change, color = group)) +
      # ribbon needs x + ymin/ymax
      geom_ribbon(
        data = wide_pct,
        aes(x = year, ymin = `Age 20–64`, ymax = `Age 80+`),
        inherit.aes = FALSE,
        alpha = 0.15
      ) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = years_to_use) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(
        title = "The Generational Care Crunch (Census Projections)",
        subtitle = paste0("% change from ", start_year),
        y = "% change from 2030",
        color = NULL,
        caption = caption_text
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 13),
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
    
    print(p)
    
    
    
  # Black background 
    p <- ggplot(plot_df, aes(x = year, y = pct_change, color = group)) +
      geom_ribbon(
        data = wide_pct,
        aes(x = year, ymin = `Age 20–64`, ymax = `Age 80+`),
        inherit.aes = FALSE,
        alpha = 0.25,            # slightly stronger shading for dark bg
        fill = "gray70"
      ) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = years_to_use) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(
        title = "The Generational Care Crunch (Census Projections)",
        subtitle = paste0("% change from ", start_year, " (", start_year, " = 0%)"),
        x = "Year",
        y = "% change from 2030",
        color = NULL,
        caption = caption_text
      ) +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_line(color = "gray30"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(face = "bold", size = 20, color = "white"),
        plot.subtitle = element_text(size = 13, color = "white"),
        legend.position = "top",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(fill = "black", color = NA),
        legend.text = element_text(color = "white"),
        plot.caption = element_text(color = "white", size = 9, hjust = 0)
      )
    
    print(p)
  
    
    
  # Annotations at the last year used
    end_vals <- plot_df |>
      filter(year == max(year)) |>
      select(group, pct_change) |>
      deframe()
  
    p_annot <- p +
      annotate("text", x = max(years_to_use), y = end_vals[["Age 80+"]]+3,
               label = paste0(sprintf("%+.1f", end_vals[["Age 80+"]]), "% (80+)"),
               hjust = 1) +
      annotate("text", x = max(years_to_use)-1, y = end_vals[["Age 20–64"]]+3,
               label = paste0(sprintf("%+.1f", end_vals[["Age 20–64"]]), "% (20–64)"),
               hjust = 1)
  
    
    
  # ---- Save graphics ----
    ggsave("care_crunch_pctchange_2030_to_end_square.png", p_annot,
           width = 1080/96, height = 1080/96, dpi = 96)  # 1080x1080 px
  
    ggsave("care_crunch_pctchange_2030_to_end_16x9.png", p_annot,
           width = 1600/96, height = 900/96, dpi = 96)   # ~1600x900 px
    
    
    
    
    
# -------------------- End script ---------------------------------------------
  