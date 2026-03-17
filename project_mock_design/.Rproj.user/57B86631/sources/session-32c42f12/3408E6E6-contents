
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tidyr)
library(ggplot2)
library(htmltools)
library(DT)

# ── 1. Load Data ─────────────────────────────────────────────
# Paths — adjust if running from a different working directory
master_raw  <- read.csv("clean_data/master_cd.csv",              stringsAsFactors = FALSE)
demog_raw   <- read.csv("clean_data/demographics_cd_clean.csv",  stringsAsFactors = FALSE)
hvi_raw     <- read.csv("clean_data/hvi_cd_clean.csv",            stringsAsFactors = FALSE)
pov_raw     <- read.csv("clean_data/poverty_cleaned.csv",         stringsAsFactors = FALSE)
parks_raw   <- read.csv("clean_data/park_with_cd.csv",            stringsAsFactors = FALSE)

# ── 2. Prepare Master SF ─────────────────────────────────────
master_sf <- st_as_sf(master_raw, wkt = "geometry_wkt", crs = 4326)

master_sf <- master_sf %>%
  mutate(
    boro_cd   = as.integer(boro_cd),
    borough   = case_when(
      floor(boro_cd / 100) == 1 ~ "Manhattan",
      floor(boro_cd / 100) == 2 ~ "Bronx",
      floor(boro_cd / 100) == 3 ~ "Brooklyn",
      floor(boro_cd / 100) == 4 ~ "Queens",
      floor(boro_cd / 100) == 5 ~ "Staten Island",
      TRUE ~ "Unknown"
    ),
    short_name = paste0(borough, " ", boro_cd),
    park_acres_per1k = park_acres_per_capita * 1000,
    poverty_pct      = poverty_rate,
    priority_tier    = factor(
      trimws(priority_tier),
      levels = c("Very high priority", "High priority",
                 "Moderate priority",  "Lower priority"),
      ordered = TRUE
    )
  )

master_df <- st_drop_geometry(master_sf)

# ── 3. Prepare Demographics ──────────────────────────────────
demog_df <- demog_raw %>%
  mutate(boro_cd = as.integer(boro_cd)) %>%
  select(boro_cd, population_2010,
         white_nonhisp_2010, black_nonhisp_2010,
         asian_pi_nonhisp_2010, hispanic_2010,
         pct_white_nonhisp_2010, pct_black_nonhisp_2010,
         pct_asian_pi_nonhisp_2010, pct_hispanic_2010,
         pct_nonwhite_2010)

# ── 4. Poverty Time Series ───────────────────────────────────
pov_ts <- pov_raw %>%
  mutate(boro_cd = as.integer(boro_cd)) %>%
  select(boro_cd, boro_name, time_period, poverty_count, poverty_rate) %>%
  filter(!is.na(poverty_rate))

# Pivot to wide for change calculation
pov_wide <- pov_ts %>%
  pivot_wider(id_cols   = c(boro_cd, boro_name),
              names_from  = time_period,
              values_from = c(poverty_rate, poverty_count))

pov_wide <- pov_wide %>%
  mutate(poverty_change = `poverty_rate_2017-21` - `poverty_rate_2016-20`)

# ── 5. Park-level Data ───────────────────────────────────────
parks_clean <- parks_raw %>%
  filter(nchar(boro_cd) == 3, grepl("^[0-9]+$", boro_cd)) %>%
  mutate(
    boro_cd  = as.integer(boro_cd),
    acres    = suppressWarnings(as.numeric(acres)),
    waterfront = (waterfront == "TRUE"),
    typecategory = ifelse(typecategory %in% c("FALSE", ""), NA, typecategory),
    subcategory  = ifelse(subcategory  %in% c("FALSE", ""), NA, subcategory)
  ) %>%
  filter(!is.na(boro_cd))

# Park counts + composition per CD
park_cd_summary <- parks_clean %>%
  group_by(boro_cd) %>%
  summarise(
    park_count       = n(),
    waterfront_parks = sum(waterfront, na.rm = TRUE),
    large_parks      = sum(typecategory == "Large Park",       na.rm = TRUE),
    playgrounds      = sum(typecategory %in% c("Playground","Jointly Operated Playground","Neighborhood Plgd","JOP"), na.rm = TRUE),
    gardens          = sum(typecategory == "Garden",           na.rm = TRUE),
    nature_areas     = sum(typecategory == "Nature Area",      na.rm = TRUE),
    triangles        = sum(typecategory == "Triangle/Plaza",   na.rm = TRUE),
    .groups = "drop"
  )

# ── 6. Join Enriched Master ──────────────────────────────────
enriched_df <- master_df %>%
  left_join(demog_df,       by = "boro_cd") %>%
  left_join(pov_wide,       by = "boro_cd") %>%
  left_join(park_cd_summary, by = "boro_cd")

enriched_sf <- master_sf %>%
  left_join(demog_df,       by = "boro_cd") %>%
  left_join(pov_wide,       by = "boro_cd") %>%
  left_join(park_cd_summary, by = "boro_cd")

# ── 7. City-wide Summaries ───────────────────────────────────
city_avg_park   <- round(mean(enriched_df$park_acres_per1k, na.rm = TRUE), 2)
city_avg_pov    <- round(mean(enriched_df$poverty_pct,       na.rm = TRUE), 1)
city_avg_hvi    <- round(mean(enriched_df$hvi,               na.rm = TRUE), 2)
city_avg_score  <- round(mean(enriched_df$overall_score,     na.rm = TRUE), 2)
nyc_median_park <- median(enriched_df$park_acres_per1k, na.rm = TRUE)
total_parks     <- nrow(parks_clean)
total_wf_parks  <- sum(parks_clean$waterfront, na.rm = TRUE)

# ── 8. Colour Palettes ───────────────────────────────────────
PRIORITY_COLS <- c(
  "Very high priority" = "#c0392b",
  "High priority"      = "#e67e22",
  "Moderate priority"  = "#f1c40f",
  "Lower priority"     = "#2e7d32"
)

priority_pal <- colorFactor(
  palette  = unname(PRIORITY_COLS),
  levels   = names(PRIORITY_COLS),
  na.color = "#aaaaaa"
)

# Light white/green ggplot2 theme
theme_equity <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major = element_line(color = "#e8f5e9"),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#1a2e1a"),
      axis.text        = element_text(color = "#5a7a5a", size = 9),
      axis.title       = element_text(color = "#2e7d32", size = 10),
      plot.title       = element_text(color = "#1a2e1a", size = 12, face = "bold"),
      plot.subtitle    = element_text(color = "#5a7a5a", size = 9),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.text      = element_text(color = "#1a2e1a"),
      legend.title     = element_text(color = "#2e7d32"),
      strip.text       = element_text(color = "#2e7d32")
    )
}

# ── 9. Helper: Popup ─────────────────────────────────────────
make_popup <- function(df) {
  paste0(
    "<div style='font-family:sans-serif;font-size:13px;color:#1a2e1a;background:#ffffff;padding:10px 12px;border-radius:8px;border:1px solid #c8e0c8;'>",
    "<strong style='font-size:14px;color:#2e7d32'>", df$district_name, "</strong><br/>",
    "<hr style='border-color:#e8f5e9;margin:5px 0'>",
    "<b>Park Access:</b> ", round(df$park_acres_per1k, 2), " ac / 1k res<br/>",
    "<b>Park Count:</b> ", df$park_count, " parks<br/>",
    "<b>Poverty Rate:</b> ", round(df$poverty_pct, 1), "%<br/>",
    "<b>Heat Vulnerability:</b> ", round(df$hvi, 2), "<br/>",
    "<b>Priority:</b> <span style='color:",
    PRIORITY_COLS[as.character(df$priority_tier)], ";font-weight:600'>",
    df$priority_tier, "</span></div>"
  )
}

# ── 10. District Choices ─────────────────────────────────────
district_choices <- setNames(enriched_df$boro_cd, enriched_df$short_name)

# ── 11. CSS ──────────────────────────────────────────────────
APP_CSS <- "
  @import url('https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=DM+Sans:wght@400;500;600&display=swap');

  body {
    font-family: 'DM Sans', sans-serif;
    background: #f7faf7;
    color: #1a2e1a;
    margin: 0; padding: 0;
  }
  h1,h2,h3,h4 { font-family: 'DM Serif Display', serif; color: #1a2e1a; }

  /* Navbar */
  .top-bar {
    background: #ffffff;
    border-bottom: 1.5px solid #c8e0c8;
    padding: 12px 28px;
    display: flex; align-items: center; flex-wrap: wrap; gap: 8px;
    position: sticky; top: 0; z-index: 1000;
    box-shadow: 0 1px 8px rgba(46,125,50,0.07);
  }
  .logo { font-family:'DM Serif Display',serif; font-size:1.2rem; color:#2e7d32; margin-right:auto; }
  .nav-pill {
    font-size:0.78rem; color:#5a7a5a; cursor:pointer;
    padding:5px 13px; border-radius:20px; text-decoration:none;
    border: 1px solid transparent;
    transition: background .2s, color .2s;
  }
  .nav-pill:hover { background:#e8f5e9; color:#2e7d32; border-color:#c8e0c8; }

  /* Sections */
  .section-wrap { padding:36px 28px; border-bottom:1px solid #deeede; background:#f7faf7; }
  .section-wrap:nth-child(even) { background:#ffffff; }
  .section-title { font-size:1.5rem; color:#1a2e1a; margin:0 0 4px 0; }
  .section-sub   { font-size:0.83rem; color:#5a7a5a; margin:0 0 22px 0; }

  /* KPI Cards */
  .kpi-row { display:flex; gap:14px; flex-wrap:wrap; margin-bottom:26px; }
  .kpi-card {
    flex:1; min-width:150px;
    background:#ffffff; border:1.5px solid #c8e0c8;
    border-radius:10px; padding:16px 20px;
    box-shadow: 0 2px 6px rgba(46,125,50,0.06);
  }
  .kpi-label { font-size:0.68rem; color:#5a7a5a; text-transform:uppercase; letter-spacing:.08em; }
  .kpi-value { font-size:1.9rem; font-family:'DM Serif Display',serif; color:#2e7d32; line-height:1.1; }
  .kpi-unit  { font-size:0.72rem; color:#8faa8f; margin-top:2px; }

  /* Chart cards */
  .chart-card {
    background:#ffffff; border:1.5px solid #c8e0c8;
    border-radius:10px; padding:18px 16px; margin-bottom:18px; height:100%;
    box-shadow: 0 2px 6px rgba(46,125,50,0.05);
  }
  .chart-title { font-size:0.85rem; font-weight:600; color:#2e7d32; margin-bottom:10px; }

  /* Leaflet maps */
  .leaflet-container { background:#eef4ee !important; border-radius:8px; }

  /* DT table */
  .dataTables_wrapper { color:#1a2e1a !important; }
  .dataTables_wrapper input, .dataTables_wrapper select { background:#ffffff !important; color:#1a2e1a !important; border:1.5px solid #c8e0c8 !important; border-radius:5px !important; }
  table.dataTable thead th { background:#e8f5e9 !important; color:#2e7d32 !important; border-bottom:1.5px solid #c8e0c8 !important; font-weight:600 !important; }
  table.dataTable tbody tr { background:#ffffff !important; color:#1a2e1a !important; }
  table.dataTable tbody tr.odd td { background:#f9fdf9 !important; }
  table.dataTable tbody tr:hover td { background:#f0faf0 !important; }
  .dataTables_info, .dataTables_paginate { color:#5a7a5a !important; }
  .paginate_button { color:#5a7a5a !important; }
  .paginate_button.current { background:#e8f5e9 !important; color:#2e7d32 !important; border:1px solid #4caf50 !important; border-radius:5px !important; }

  /* Profile panel */
  .profile-card {
    background:#ffffff; border:1.5px solid #c8e0c8;
    border-radius:10px; padding:20px; height:100%;
    box-shadow: 0 2px 6px rgba(46,125,50,0.05);
  }
  .profile-name  { font-size:1.2rem; color:#1a2e1a; font-family:'DM Serif Display',serif; margin-bottom:14px; }
  .profile-row   { display:flex; gap:14px; flex-wrap:wrap; margin-bottom:12px; }
  .profile-stat  { flex:1; min-width:120px; }
  .p-label  { font-size:0.66rem; color:#5a7a5a; text-transform:uppercase; letter-spacing:.07em; }
  .p-value  { font-size:1.4rem; font-family:'DM Serif Display',serif; color:#1a2e1a; }
  .priority-badge {
    display:inline-block; padding:4px 14px; border-radius:20px;
    font-size:0.78rem; font-weight:600; color:#fff;
  }
  .badge-vhigh  { background:#c0392b; }
  .badge-high   { background:#e67e22; }
  .badge-mod    { background:#c7a800; }
  .badge-low    { background:#2e7d32; }

  /* Type breakdown bars */
  .type-bar-wrap { margin:6px 0; }
  .type-bar-label { font-size:0.75rem; color:#5a7a5a; display:flex; justify-content:space-between; }
  .type-bar-bg    { background:#e8f5e9; border-radius:4px; height:7px; margin-top:2px; }
  .type-bar-fill  { background:#4caf50; border-radius:4px; height:7px; transition:width .4s; }

  /* Comparison */
  .compare-header { font-size:0.9rem; font-weight:600; color:#2e7d32; margin-bottom:8px; }
  .compare-vs { font-size:1.1rem; color:#8faa8f; text-align:center; padding-top:24px; }

  /* Insight box */
  .insight-box {
    background:#e8f5e9; border-left:3px solid #4caf50;
    border-radius:0 8px 8px 0; padding:12px 16px; margin-top:10px;
    font-size:0.83rem; color:#2e5c2e; line-height:1.6;
  }

  /* Tabs */
  .nav-tabs { border-bottom:1.5px solid #c8e0c8 !important; }
  .nav-tabs .nav-link { color:#5a7a5a !important; background:transparent !important; border:none !important; }
  .nav-tabs .nav-link.active { color:#2e7d32 !important; border-bottom:2px solid #2e7d32 !important; }

  /* Shiny inputs */
  .selectize-input { background:#ffffff !important; color:#1a2e1a !important; border:1.5px solid #c8e0c8 !important; border-radius:7px !important; }
  .selectize-dropdown { background:#ffffff !important; color:#1a2e1a !important; border:1px solid #c8e0c8 !important; }
  .selectize-dropdown-content .option:hover { background:#e8f5e9 !important; color:#2e7d32 !important; }
  label { color:#5a7a5a !important; font-size:0.82rem !important; }

  ::-webkit-scrollbar { width:6px; height:6px; }
  ::-webkit-scrollbar-track { background:#f0f7f0; }
  ::-webkit-scrollbar-thumb { background:#b0d0b0; border-radius:3px; }
  ::-webkit-scrollbar-thumb:hover { background:#4caf50; }

  @media(max-width:768px){
    .section-wrap { padding:20px 14px; }
    .top-bar { padding:10px 14px; }
  }
"

# ══════════════════════════════════════════════════════════════
# 12. UI
# ══════════════════════════════════════════════════════════════
ui <- fluidPage(
  title = "NYC Park Equity Explorer",
  tags$head(tags$style(HTML(APP_CSS))),
  
  # ── Navbar ──────────────────────────────────────────────────
  div(class = "top-bar",
      span(class = "logo", "🌳 NYC Park Equity Explorer"),
      tags$a(class = "nav-pill", href = "#sec-overview",   "Overview"),
      tags$a(class = "nav-pill", href = "#sec-greenspace", "Green Space"),
      tags$a(class = "nav-pill", href = "#sec-inequality", "Inequality"),
      tags$a(class = "nav-pill", href = "#sec-heat",       "Heat"),
      tags$a(class = "nav-pill", href = "#sec-priority",   "Priority"),
      tags$a(class = "nav-pill", href = "#sec-explorer",   "Explorer"),
      tags$a(class = "nav-pill", href = "#sec-compare",    "Compare")
  ),
  
  # ══ SECTION 1 — OVERVIEW ════════════════════════════════════
  div(id = "sec-overview", class = "section-wrap",
      h2(class = "section-title", "Overview"),
      p(class = "section-sub",
        "City-wide snapshot of park access, poverty, and heat vulnerability across NYC's 59 community districts."),
      
      div(class = "kpi-row",
          div(class = "kpi-card",
              div(class = "kpi-label", "Avg Park Access"),
              div(class = "kpi-value", city_avg_park),
              div(class = "kpi-unit",  "acres per 1 000 residents")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Avg Poverty Rate"),
              div(class = "kpi-value", paste0(city_avg_pov, "%")),
              div(class = "kpi-unit",  "share below poverty line")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Avg Heat Vulnerability"),
              div(class = "kpi-value", city_avg_hvi),
              div(class = "kpi-unit",  "index (1–5 scale)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Total Park Properties"),
              div(class = "kpi-value", format(total_parks, big.mark = ",")),
              div(class = "kpi-unit",  paste0(total_wf_parks, " waterfront sites"))
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Avg Priority Score"),
              div(class = "kpi-value", city_avg_score),
              div(class = "kpi-unit",  "composite equity index")
          )
      ),
      
      fluidRow(
        column(8,
               div(class = "chart-card",
                   div(class = "chart-title", "Priority Map — Overall Equity Score by District"),
                   leafletOutput("overview_map", height = "420px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Districts by Priority Tier"),
                   plotlyOutput("overview_tier_bar", height = "180px")
               ),
               div(class = "chart-card",
                   div(class = "chart-title", "Score Components — City Average"),
                   plotlyOutput("overview_radar", height = "200px")
               )
        )
      )
  ),
  
  # ══ SECTION 2 — GREEN SPACE ACCESS ═════════════════════════
  div(id = "sec-greenspace", class = "section-wrap",
      h2(class = "section-title", "Green Space Access"),
      p(class = "section-sub",
        "How does access to public green space vary across community districts? Includes park counts, types, and acreage."),
      
      fluidRow(
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Park Access Map (Acres per 1 000 Residents)"),
                   leafletOutput("park_map", height = "380px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Top 15 Districts with Lowest Park Access"),
                   plotlyOutput("park_bottom_bar", height = "380px")
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Distribution of Park Access Across Districts"),
                   plotlyOutput("park_hist", height = "240px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Park Count vs Acreage by Borough"),
                   plotlyOutput("park_count_acres", height = "240px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Park Type Composition — NYC-wide"),
                   plotlyOutput("park_type_pie", height = "240px")
               )
        )
      ),
      div(class = "insight-box",
          "📍 Districts in the Bronx and northern Brooklyn consistently rank lowest for park acreage per resident.
       Park count alone is misleading — many low-access districts have many small triangles and gardens
       rather than large recreational spaces. Waterfront parks are concentrated in Manhattan and Staten Island."
      )
  ),
  
  # ══ SECTION 3 — SOCIAL INEQUALITY ══════════════════════════
  div(id = "sec-inequality", class = "section-wrap",
      h2(class = "section-title", "Social & Economic Inequality"),
      p(class = "section-sub",
        "Racial and economic vulnerability patterns, poverty trends, and their relationship with park access."),
      
      fluidRow(
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Park Access vs Poverty Rate (bubble = % nonwhite)"),
                   plotlyOutput("scatter_park_pov", height = "340px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Poverty Map (2017–21 Estimate)"),
                   leafletOutput("pov_map", height = "340px")
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Racial Composition by Borough"),
                   plotlyOutput("race_stacked", height = "280px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Poverty Change: 2016-20 → 2017-21"),
                   plotlyOutput("poverty_change", height = "280px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "% Nonwhite vs Park Access"),
                   plotlyOutput("nonwhite_park", height = "280px")
               )
        )
      ),
      div(class = "insight-box",
          "📍 Districts with the highest share of nonwhite residents cluster in the bottom-left quadrant —
       low park access AND high poverty. Poverty rates stayed relatively stable between the two survey
       periods, but the Bronx and parts of Brooklyn show persistent high poverty alongside the lowest
       park access in the city."
      )
  ),
  
  # ══ SECTION 4 — HEAT VULNERABILITY ═════════════════════════
  div(id = "sec-heat", class = "section-wrap",
      h2(class = "section-title", "Heat Vulnerability"),
      p(class = "section-sub",
        "Which districts face the highest heat risk, and how does green space relate to heat exposure?"),
      
      fluidRow(
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Heat Vulnerability Index Map"),
                   leafletOutput("heat_map", height = "360px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Park Access vs Heat Vulnerability (Risk Quadrants)"),
                   plotlyOutput("heat_scatter", height = "360px")
               )
        )
      ),
      fluidRow(
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Heat Vulnerability Distribution by Borough"),
                   plotlyOutput("heat_box", height = "240px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   div(class = "chart-title", "Top 10 Highest Heat Vulnerability Districts"),
                   plotlyOutput("heat_top10", height = "240px")
               )
        )
      ),
      div(class = "insight-box",
          "📍 The upper-left quadrant of the scatter plot (low park / high heat) is dominated by Bronx and
       Brooklyn districts. Heat vulnerability inversely tracks park coverage — more green space correlates
       with lower urban heat. Waterfront parks may provide additional cooling effects in coastal districts."
      )
  ),
  
  # ══ SECTION 5 — PLANNING PRIORITY ══════════════════════════
  div(id = "sec-priority", class = "section-wrap",
      h2(class = "section-title", "Planning Priority"),
      p(class = "section-sub",
        "Which districts should be prioritised for green infrastructure investment, and why?"),
      
      fluidRow(
        column(5,
               div(class = "chart-card",
                   div(class = "chart-title", "Composite Priority Score — All Districts"),
                   plotlyOutput("priority_ranking", height = "480px")
               )
        ),
        column(7,
               div(class = "chart-card",
                   div(class = "chart-title", "Score Breakdown: Park Access · Poverty · Heat (ranked by overall score)"),
                   plotlyOutput("score_breakdown", height = "480px")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "chart-card",
                   div(class = "chart-title", "Full District Priority Table"),
                   DTOutput("priority_table")
               )
        )
      )
  ),
  
  # ══ SECTION 6 — DISTRICT EXPLORER ══════════════════════════
  div(id = "sec-explorer", class = "section-wrap",
      h2(class = "section-title", "District Explorer"),
      p(class = "section-sub",
        "Select a community district to see its full profile — demographics, parks, poverty trend, and ranking."),
      
      fluidRow(
        column(3,
               selectInput("explorer_cd", "Select District",
                           choices = district_choices, selectize = TRUE),
               div(class = "profile-card", uiOutput("district_profile"))
        ),
        column(5,
               div(class = "chart-card",
                   div(class = "chart-title", "District Profile — Radar Chart"),
                   plotlyOutput("radar_chart", height = "300px")
               ),
               div(class = "chart-card",
                   div(class = "chart-title", "Park Type Breakdown in This District"),
                   plotlyOutput("park_type_dist", height = "200px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "District Ranking (lower = more access)"),
                   uiOutput("ranking_badges")
               ),
               div(class = "chart-card",
                   div(class = "chart-title", "Poverty Trend: 2016-20 vs 2017-21"),
                   plotlyOutput("poverty_trend_dist", height = "180px")
               )
        )
      )
  ),
  
  # ══ SECTION 7 — DISTRICT COMPARISON ════════════════════════
  div(id = "sec-compare", class = "section-wrap",
      h2(class = "section-title", "District Comparison"),
      p(class = "section-sub",
        "Compare two community districts side by side across all equity dimensions."),
      
      fluidRow(
        column(3, selectInput("compare_cd1", "District A", choices = district_choices, selected = district_choices[1])),
        column(1, div(class = "compare-vs", "vs")),
        column(3, selectInput("compare_cd2", "District B", choices = district_choices, selected = district_choices[10]))
      ),
      fluidRow(
        column(5,
               div(class = "chart-card",
                   div(class = "chart-title", "Side-by-Side Metric Comparison"),
                   plotlyOutput("compare_bars", height = "360px")
               )
        ),
        column(4,
               div(class = "chart-card",
                   div(class = "chart-title", "Radar Overlay"),
                   plotlyOutput("compare_radar", height = "360px")
               )
        ),
        column(3,
               div(class = "chart-card",
                   uiOutput("compare_cards")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "chart-card",
                   div(class = "chart-title", "Racial Composition Comparison"),
                   plotlyOutput("compare_race", height = "220px")
               )
        )
      )
  )
)

# ══════════════════════════════════════════════════════════════
# 13. SERVER
# ══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # Leaflet tile options (dark)
  TILES <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
  ATTR  <- "&copy; <a href='https://www.openstreetmap.org/copyright'>OSM</a> &copy; <a href='https://carto.com/'>CARTO</a>"
  
  # ── OVERVIEW ───────────────────────────────────────────────
  output$overview_map <- renderLeaflet({
    pal <- colorFactor(unname(PRIORITY_COLS), levels = names(PRIORITY_COLS), na.color = "#5a7a5a")
    leaflet(enriched_sf) %>%
      addTiles(urlTemplate = TILES, attribution = ATTR) %>%
      addPolygons(
        fillColor   = ~pal(priority_tier),
        fillOpacity = 0.75,
        color       = "#ffffff", weight = 0.5, opacity = 0.3,
        popup       = ~make_popup(enriched_sf),
        label       = ~paste0(short_name, ": ", priority_tier),
        highlightOptions = highlightOptions(
          color = "#fff", weight = 2, bringToFront = TRUE, fillOpacity = 0.9)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~priority_tier,
                title = "Priority Tier", opacity = 0.9) %>%
      setView(lng = -73.95, lat = 40.72, zoom = 10)
  })
  
  output$overview_tier_bar <- renderPlotly({
    tier_counts <- enriched_df %>%
      count(priority_tier) %>%
      arrange(priority_tier)
    plot_ly(tier_counts,
            x = ~n, y = ~priority_tier, type = "bar", orientation = "h",
            marker = list(color = PRIORITY_COLS[as.character(tier_counts$priority_tier)]),
            hovertemplate = "%{y}: %{x} districts<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "", gridcolor = "#e8f5e9"),
        yaxis = list(title = "", autorange = "reversed"),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        showlegend = FALSE
      )
  })
  
  output$overview_radar <- renderPlotly({
    avg_vals <- c(
      round(mean(enriched_df$park_norm,    na.rm = TRUE), 2),
      round(mean(enriched_df$poverty_norm, na.rm = TRUE), 2),
      round(mean(enriched_df$hvi_norm,     na.rm = TRUE), 2)
    )
    plot_ly(type = "scatterpolar", fill = "toself",
            r = c(avg_vals, avg_vals[1]),
            theta = c("Park (normalised)", "Poverty (normalised)", "HVI (normalised)", "Park (normalised)"),
            line = list(color = "#7fc47f"),
            fillcolor = "rgba(127,196,127,0.25)",
            hovertemplate = "%{theta}: %{r}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        polar = list(
          bgcolor = "#ffffff",
          radialaxis = list(visible = TRUE, range = c(0, 1), gridcolor = "#e8f5e9", color = "#5a7a5a"),
          angularaxis = list(color = "#5a7a5a")
        ),
        margin = list(l = 20, r = 20, t = 20, b = 20),
        showlegend = FALSE,
        font = list(color = "#1a2e1a", size = 10)
      )
  })
  
  # ── GREEN SPACE ────────────────────────────────────────────
  output$park_map <- renderLeaflet({
    pal <- colorNumeric("YlGn", domain = enriched_sf$park_acres_per1k, na.color = "#555")
    leaflet(enriched_sf) %>%
      addTiles(urlTemplate = TILES, attribution = ATTR) %>%
      addPolygons(
        fillColor   = ~pal(park_acres_per1k),
        fillOpacity = 0.8,
        color = "#ffffff", weight = 0.4, opacity = 0.3,
        popup = ~make_popup(enriched_sf),
        label = ~paste0(short_name, ": ", round(park_acres_per1k, 2), " ac/1k"),
        highlightOptions = highlightOptions(color = "#fff", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~park_acres_per1k,
                title = "Acres / 1k res", opacity = 0.9) %>%
      setView(-73.95, 40.72, 10)
  })
  
  output$park_bottom_bar <- renderPlotly({
    bottom15 <- enriched_df %>%
      arrange(park_acres_per1k) %>%
      head(15) %>%
      mutate(short_name = factor(short_name, levels = rev(short_name)))
    rng      <- range(bottom15$park_acres_per1k, na.rm = TRUE)
    norm_val <- (bottom15$park_acres_per1k - rng[1]) / max(rng[2] - rng[1], 0.001)
    bar_cols  <- colorRampPalette(c("#c0392b", "#e67e22", "#f1c40f"))(100)[
      pmax(1, pmin(100, round(norm_val * 99) + 1))
    ]
    plot_ly(bottom15,
            x = ~park_acres_per1k, y = ~short_name, type = "bar", orientation = "h",
            marker = list(color = bar_cols),
            hovertemplate = "<b>%{y}</b><br>%{x:.2f} acres/1k<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Acres per 1 000 residents", gridcolor = "#e8f5e9"),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  output$park_hist <- renderPlotly({
    plot_ly(enriched_df, x = ~park_acres_per1k, type = "histogram",
            nbinsx = 20,
            marker = list(color = "#7fc47f", line = list(color = "#ffffff", width = 1)),
            hovertemplate = "%{x:.1f} ac/1k: %{y} districts<extra></extra>") %>%
      add_segments(x = nyc_median_park, xend = nyc_median_park,
                   y = 0, yend = 10, line = list(color = "#e67e22", dash = "dash", width = 1.5),
                   name = "Median") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Acres per 1k residents", gridcolor = "#e8f5e9"),
        yaxis = list(title = "# Districts",            gridcolor = "#e8f5e9"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  output$park_count_acres <- renderPlotly({
    borough_summary <- enriched_df %>%
      group_by(borough) %>%
      summarise(
        total_parks = sum(park_count, na.rm = TRUE),
        total_acres = sum(park_acres,  na.rm = TRUE),
        .groups = "drop"
      )
    pal5 <- c("#7fc47f","#e67e22","#3498db","#9b59b6","#e74c3c")
    plot_ly(borough_summary,
            x = ~total_parks, y = ~total_acres, type = "scatter", mode = "markers+text",
            text = ~borough, textposition = "top center",
            marker = list(size = 18, color = pal5, opacity = 0.85),
            hovertemplate = "<b>%{text}</b><br>Parks: %{x}<br>Acres: %{y:.0f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Number of Park Properties", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Total Park Acres",          gridcolor = "#e8f5e9"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  output$park_type_pie <- renderPlotly({
    type_totals <- parks_clean %>%
      filter(!is.na(typecategory)) %>%
      count(typecategory, sort = TRUE) %>%
      mutate(typecategory = ifelse(row_number() > 8, "Other", typecategory)) %>%
      group_by(typecategory) %>%
      summarise(n = sum(n), .groups = "drop")
    plot_ly(type_totals, labels = ~typecategory, values = ~n,
            type = "pie", hole = 0.45,
            marker = list(colors = colorRampPalette(c("#7fc47f","#3498db","#e67e22","#9b59b6","#e74c3c","#1abc9c","#f39c12","#2980b9"))(nrow(type_totals)),
                          line = list(color = "#ffffff", width = 1)),
            hovertemplate = "%{label}: %{value} (%{percent})<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff",
        font = list(color = "#1a2e1a", size = 10),
        margin = list(l = 0, r = 0, t = 10, b = 0),
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5, font = list(color = "#1a2e1a", size = 9))
      )
  })
  
  # ── INEQUALITY ─────────────────────────────────────────────
  output$scatter_park_pov <- renderPlotly({
    plot_ly(enriched_df,
            x = ~park_acres_per1k, y = ~poverty_pct,
            size = ~pct_nonwhite * 100,
            color = ~borough,
            type = "scatter", mode = "markers",
            marker = list(opacity = 0.75, sizemode = "diameter", sizeref = 0.15),
            text = ~paste0(short_name, "<br>Park: ", round(park_acres_per1k,2),
                           " ac/1k<br>Poverty: ", round(poverty_pct,1),
                           "%<br>% Nonwhite: ", round(pct_nonwhite*100,0), "%"),
            hovertemplate = "%{text}<extra></extra>") %>%
      add_segments(x = nyc_median_park, xend = nyc_median_park,
                   y = 0, yend = max(enriched_df$poverty_pct, na.rm=TRUE),
                   line = list(color="#4caf50", dash="dot", width=1), showlegend=FALSE) %>%
      add_segments(x = 0, xend = max(enriched_df$park_acres_per1k, na.rm=TRUE),
                   y = city_avg_pov, yend = city_avg_pov,
                   line = list(color="#4caf50", dash="dot", width=1), showlegend=FALSE) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Park Acres per 1 000 Residents", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Poverty Rate (%)",               gridcolor = "#e8f5e9"),
        legend = list(x = 0.75, y = 1, font = list(color="#1a2e1a")),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  output$pov_map <- renderLeaflet({
    pal <- colorNumeric("PuRd", domain = enriched_sf$poverty_pct, na.color = "#555")
    leaflet(enriched_sf) %>%
      addTiles(urlTemplate = TILES, attribution = ATTR) %>%
      addPolygons(
        fillColor   = ~pal(poverty_pct),
        fillOpacity = 0.8,
        color = "#ffffff", weight = 0.4, opacity = 0.3,
        popup = ~make_popup(enriched_sf),
        label = ~paste0(short_name, ": ", round(poverty_pct, 1), "%"),
        highlightOptions = highlightOptions(color = "#fff", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~poverty_pct,
                title = "Poverty Rate %", opacity = 0.9) %>%
      setView(-73.95, 40.72, 10)
  })
  
  output$race_stacked <- renderPlotly({
    race_boro <- demog_df %>%
      left_join(select(master_df, boro_cd, borough), by = "boro_cd") %>%
      group_by(borough) %>%
      summarise(
        White    = mean(pct_white_nonhisp_2010,   na.rm=TRUE)*100,
        Black    = mean(pct_black_nonhisp_2010,   na.rm=TRUE)*100,
        Asian    = mean(pct_asian_pi_nonhisp_2010, na.rm=TRUE)*100,
        Hispanic = mean(pct_hispanic_2010,         na.rm=TRUE)*100,
        .groups  = "drop"
      ) %>%
      pivot_longer(-borough, names_to="Race", values_to="Pct")
    
    race_cols <- c(White="#5dade2", Black="#a569bd", Asian="#58d68d", Hispanic="#f0b27a")
    plot_ly(race_boro, x = ~borough, y = ~Pct, color = ~Race,
            type = "bar", colors = race_cols,
            hovertemplate = "%{x} — %{fullData.name}: %{y:.1f}%<extra></extra>") %>%
      layout(
        barmode = "stack",
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = ""),
        yaxis = list(title = "% of Population", gridcolor = "#e8f5e9"),
        legend = list(x = 1,    y = 1, font = list(color="#1a2e1a")),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  output$poverty_change <- renderPlotly({
    pv_chg <- pov_wide %>%
      left_join(select(master_df, boro_cd, short_name, borough), by = "boro_cd") %>%
      filter(!is.na(poverty_change)) %>%
      arrange(desc(abs(poverty_change))) %>%
      head(20)
    
    plot_ly(pv_chg,
            x = ~poverty_change,
            y = ~reorder(short_name, poverty_change),
            type = "bar", orientation = "h",
            marker = list(color = ~ifelse(poverty_change > 0, "#c0392b", "#2e7d32")),
            hovertemplate = "<b>%{y}</b><br>Δ poverty: %{x:.2f}pp<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Percentage Point Change", gridcolor = "#e8f5e9",
                     zeroline = TRUE, zerolinecolor = "#555"),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  output$nonwhite_park <- renderPlotly({
    plot_ly(enriched_df,
            x = ~pct_nonwhite*100, y = ~park_acres_per1k,
            color = ~borough, type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.75),
            text = ~short_name,
            hovertemplate = "<b>%{text}</b><br>% Nonwhite: %{x:.0f}%<br>Park: %{y:.2f} ac/1k<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "% Nonwhite Population", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Park Acres / 1k Residents", gridcolor = "#e8f5e9"),
        legend = list(x = 0.6,  y = 1, font = list(color="#1a2e1a")),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  # ── HEAT ───────────────────────────────────────────────────
  output$heat_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = enriched_sf$hvi, na.color = "#555")
    leaflet(enriched_sf) %>%
      addTiles(urlTemplate = TILES, attribution = ATTR) %>%
      addPolygons(
        fillColor   = ~pal(hvi),
        fillOpacity = 0.8,
        color = "#ffffff", weight = 0.4, opacity = 0.3,
        popup = ~make_popup(enriched_sf),
        label = ~paste0(short_name, ": HVI ", round(hvi, 2)),
        highlightOptions = highlightOptions(color = "#fff", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~hvi,
                title = "Heat Vulnerability Index", opacity = 0.9) %>%
      setView(-73.95, 40.72, 10)
  })
  
  output$heat_scatter <- renderPlotly({
    med_park <- nyc_median_park
    med_hvi  <- median(enriched_df$hvi, na.rm = TRUE)
    d <- enriched_df %>%
      mutate(quadrant = case_when(
        park_acres_per1k >= med_park & hvi <= med_hvi ~ "High Park / Low Heat",
        park_acres_per1k <  med_park & hvi <= med_hvi ~ "Low Park / Low Heat",
        park_acres_per1k >= med_park & hvi >  med_hvi ~ "High Park / High Heat",
        TRUE                                           ~ "Low Park / High Heat ⚠️"
      ))
    q_cols <- c("High Park / Low Heat"="#27ae60",
                "Low Park / Low Heat" ="#f1c40f",
                "High Park / High Heat"="#e67e22",
                "Low Park / High Heat ⚠️"="#c0392b")
    plot_ly(d, x = ~park_acres_per1k, y = ~hvi,
            color = ~quadrant, colors = q_cols,
            type = "scatter", mode = "markers",
            marker = list(size = 9, opacity = 0.8),
            text = ~short_name,
            hovertemplate = "<b>%{text}</b><br>Park: %{x:.2f} ac/1k<br>HVI: %{y:.2f}<extra></extra>") %>%
      add_segments(x = med_park, xend = med_park,
                   y = min(d$hvi,na.rm=T), yend = max(d$hvi,na.rm=T),
                   line = list(color="#4caf50", dash="dot", width=1), showlegend=FALSE) %>%
      add_segments(x = 0, xend = max(d$park_acres_per1k,na.rm=T),
                   y = med_hvi, yend = med_hvi,
                   line = list(color="#4caf50", dash="dot", width=1), showlegend=FALSE) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Park Acres / 1k Residents", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Heat Vulnerability Index",  gridcolor = "#e8f5e9"),
        legend = list(x = 0.5, y = 1, font = list(color = "#1a2e1a", size = 9)),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  output$heat_box <- renderPlotly({
    plot_ly(enriched_df, x = ~borough, y = ~hvi,
            type = "box", color = ~borough,
            colors = c("#7fc47f","#e67e22","#3498db","#9b59b6","#e74c3c"),
            line = list(width = 1.5),
            hovertemplate = "%{y:.2f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = ""),
        yaxis = list(title = "HVI", gridcolor = "#e8f5e9"),
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  output$heat_top10 <- renderPlotly({
    top10 <- enriched_df %>%
      arrange(desc(hvi)) %>%
      head(10) %>%
      mutate(short_name = factor(short_name, levels = rev(short_name)))
    # Map HVI values to a colour gradient manually
    rng      <- range(top10$hvi, na.rm = TRUE)
    norm_val <- (top10$hvi - rng[1]) / max(rng[2] - rng[1], 0.001)
    bar_cols  <- colorRampPalette(c("#f39c12", "#c0392b"))(100)[
      pmax(1, pmin(100, round(norm_val * 99) + 1))
    ]
    plot_ly(top10, x = ~hvi, y = ~short_name, type = "bar", orientation = "h",
            marker = list(color = bar_cols),
            hovertemplate = "<b>%{y}</b><br>HVI: %{x:.2f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "Heat Vulnerability Index", gridcolor = "#e8f5e9"),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  # ── PRIORITY ───────────────────────────────────────────────
  output$priority_ranking <- renderPlotly({
    ranked <- enriched_df %>%
      arrange(desc(overall_score)) %>%
      mutate(short_name = factor(short_name, levels = rev(short_name)))
    tier_hex <- dplyr::recode(as.character(ranked$priority_tier),
                              "Very high priority" = "#c0392b", "High priority" = "#e67e22",
                              "Moderate priority"  = "#f1c40f", "Lower priority" = "#2e7d32",
                              .default = "#aaaaaa")
    plot_ly(ranked, x = ~overall_score, y = ~short_name,
            type = "bar", orientation = "h",
            marker = list(color = tier_hex),
            hovertemplate = "<b>%{y}</b><br>Score: %{x:.3f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a", size = 8),
        xaxis = list(title = "Overall Priority Score", gridcolor = "#e8f5e9"),
        yaxis = list(title = "", tickfont = list(size = 8)),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE,
        height = 480
      )
  })
  
  output$score_breakdown <- renderPlotly({
    bd <- enriched_df %>%
      arrange(desc(overall_score)) %>%
      mutate(short_name = factor(short_name, levels = rev(short_name)))
    plot_ly(bd) %>%
      add_bars(x = ~park_norm,    y = ~short_name, name = "Park Access",
               marker = list(color = "#27ae60"), orientation = "h",
               hovertemplate = "Park: %{x:.2f}<extra></extra>") %>%
      add_bars(x = ~poverty_norm, y = ~short_name, name = "Poverty",
               marker = list(color = "#9b59b6"), orientation = "h",
               hovertemplate = "Poverty: %{x:.2f}<extra></extra>") %>%
      add_bars(x = ~hvi_norm,     y = ~short_name, name = "Heat Vulnerability",
               marker = list(color = "#e74c3c"), orientation = "h",
               hovertemplate = "Heat: %{x:.2f}<extra></extra>") %>%
      layout(
        barmode = "stack",
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a", size = 8),
        xaxis = list(title = "Normalised Score (stacked)", gridcolor = "#e8f5e9"),
        yaxis = list(title = "", tickfont = list(size = 8)),
        legend = list(x = 0.6, y = 0.05, font = list(color = "#1a2e1a", size = 9)),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        height = 480
      )
  })
  
  output$priority_table <- renderDT({
    enriched_df %>%
      arrange(desc(overall_score)) %>%
      select(
        District    = short_name,
        Borough     = borough,
        `Priority`  = priority_tier,
        `Score`     = overall_score,
        `Park ac/1k`= park_acres_per1k,
        `# Parks`   = park_count,
        `Poverty %` = poverty_pct,
        `HVI`       = hvi,
        `% Nonwhite`= pct_nonwhite
      ) %>%
      mutate(
        Score        = round(Score, 3),
        `Park ac/1k` = round(`Park ac/1k`, 2),
        `Poverty %`  = round(`Poverty %`, 1),
        HVI          = round(HVI, 2),
        `% Nonwhite` = round(`% Nonwhite` * 100, 0)
      ) %>%
      datatable(
        options = list(
          pageLength = 15, scrollX = TRUE,
          dom = "ftip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE,
        class = "cell-border compact"
      ) %>%
      formatStyle("Priority",
                  backgroundColor = styleEqual(
                    c("Very high priority","High priority","Moderate priority","Lower priority"),
                    c("#c0392b","#e67e22","#f1c40f","#2e7d32")),
                  color = styleEqual(
                    c("Very high priority","High priority","Moderate priority","Lower priority"),
                    c("white","white","white","white")),
                  fontWeight = "bold"
      ) %>%
      formatStyle("Score",
                  background = styleColorBar(range(enriched_df$overall_score), "#c8e0c8"),
                  backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      )
  }, server = FALSE)
  
  # ── EXPLORER ───────────────────────────────────────────────
  selected_cd <- reactive({ as.integer(input$explorer_cd) })
  
  sel_row <- reactive({
    enriched_df %>% filter(boro_cd == selected_cd())
  })
  
  output$district_profile <- renderUI({
    r <- sel_row()
    if (nrow(r) == 0) return(NULL)
    
    tier  <- as.character(r$priority_tier)
    badge_class <- switch(tier,
                          "Very high priority" = "badge-vhigh",
                          "High priority"      = "badge-high",
                          "Moderate priority"  = "badge-mod",
                          "Lower priority"     = "badge-low",
                          "badge-low"
    )
    
    # Poverty trend
    pov_row <- pov_ts %>% filter(boro_cd == r$boro_cd)
    pov_str <- if (nrow(pov_row) == 2) {
      rates <- pov_row$poverty_rate
      chg   <- round(diff(rates), 2)
      arrow <- if (chg > 0) "↑" else "↓"
      paste0(round(rates[2], 1), "% (", arrow, abs(chg), "pp)")
    } else {
      paste0(round(r$poverty_pct, 1), "%")
    }
    
    tagList(
      div(class = "profile-name", r$district_name),
      div(class = "profile-row",
          div(class = "profile-stat",
              div(class = "p-label", "Park Access"),
              div(class = "p-value", paste0(round(r$park_acres_per1k, 2)))
          ),
          div(class = "profile-stat",
              div(class = "p-label", "# Park Properties"),
              div(class = "p-value", r$park_count %||% "—")
          )
      ),
      div(class = "profile-row",
          div(class = "profile-stat",
              div(class = "p-label", "Poverty Rate"),
              div(class = "p-value", pov_str)
          ),
          div(class = "profile-stat",
              div(class = "p-label", "Heat Vulnerability"),
              div(class = "p-value", round(r$hvi, 2))
          )
      ),
      div(class = "profile-row",
          div(class = "profile-stat",
              div(class = "p-label", "% Nonwhite"),
              div(class = "p-value", paste0(round(r$pct_nonwhite * 100, 0), "%"))
          ),
          div(class = "profile-stat",
              div(class = "p-label", "Population (2010)"),
              div(class = "p-value", format(r$population, big.mark = ","))
          )
      ),
      div(class = "profile-stat",
          div(class = "p-label", "Priority Tier"),
          span(class = paste("priority-badge", badge_class), tier)
      ),
      br(),
      # Park type mini-bars
      {
        type_vals <- c(
          Playgrounds   = r$playgrounds   %||% 0,
          Gardens       = r$gardens       %||% 0,
          Triangles     = r$triangles     %||% 0,
          `Large Parks` = r$large_parks   %||% 0,
          `Nature Areas`= r$nature_areas  %||% 0
        )
        mx <- max(type_vals, 1)
        tagList(
          tags$small(style="color:#777;font-size:0.7rem;text-transform:uppercase;letter-spacing:.06em;",
                     "Park Types"),
          lapply(names(type_vals), function(nm) {
            div(class = "type-bar-wrap",
                div(class = "type-bar-label",
                    span(nm), span(type_vals[[nm]])
                ),
                div(class = "type-bar-bg",
                    div(class = "type-bar-fill",
                        style = paste0("width:", round(type_vals[[nm]] / mx * 100), "%"))
                )
            )
          })
        )
      }
    )
  })
  
  output$radar_chart <- renderPlotly({
    r <- sel_row()
    if (nrow(r) == 0) return(NULL)
    # Normalise park_acres_per1k 0-1 for display
    park_n <- (r$park_acres_per1k - min(enriched_df$park_acres_per1k, na.rm=TRUE)) /
      (max(enriched_df$park_acres_per1k, na.rm=TRUE) - min(enriched_df$park_acres_per1k, na.rm=TRUE))
    pov_n  <- r$poverty_norm
    hvi_n  <- r$hvi_norm
    score_n <- r$overall_score
    
    vals   <- c(park_n, pov_n, hvi_n, score_n, park_n)
    labels <- c("Park Access", "Poverty", "Heat Vuln.", "Overall Score", "Park Access")
    
    plot_ly(type = "scatterpolar", fill = "toself",
            r = vals, theta = labels,
            line = list(color = "#7fc47f"),
            fillcolor = "rgba(127,196,127,0.2)",
            hovertemplate = "%{theta}: %{r:.2f}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff",
        polar = list(
          bgcolor = "#ffffff",
          radialaxis = list(visible = TRUE, range = c(0,1), gridcolor = "#e8f5e9", color = "#5a7a5a"),
          angularaxis = list(color = "#5a7a5a")
        ),
        margin = list(l = 30, r = 30, t = 20, b = 20),
        showlegend = FALSE,
        font = list(color = "#1a2e1a", size = 10)
      )
  })
  
  output$park_type_dist <- renderPlotly({
    r <- sel_row()
    if (nrow(r) == 0) return(NULL)
    parks_cd <- parks_clean %>%
      filter(boro_cd == r$boro_cd, !is.na(typecategory)) %>%
      count(typecategory, sort = TRUE) %>%
      head(8)
    if (nrow(parks_cd) == 0) return(plotly_empty())
    plot_ly(parks_cd, x = ~n, y = ~reorder(typecategory, n),
            type = "bar", orientation = "h",
            marker = list(color = "#3498db"),
            hovertemplate = "%{y}: %{x}<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a", size = 9),
        xaxis = list(title = "Count", gridcolor = "#e8f5e9"),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 20),
        showlegend = FALSE
      )
  })
  
  output$ranking_badges <- renderUI({
    r <- sel_row()
    if (nrow(r) == 0) return(NULL)
    park_rank  <- rank(enriched_df$park_acres_per1k)[enriched_df$boro_cd == r$boro_cd]
    pov_rank   <- rank(-enriched_df$poverty_pct)[enriched_df$boro_cd == r$boro_cd]
    hvi_rank   <- rank(-enriched_df$hvi)[enriched_df$boro_cd == r$boro_cd]
    score_rank <- rank(-enriched_df$overall_score)[enriched_df$boro_cd == r$boro_cd]
    n_total    <- nrow(enriched_df)
    
    rank_colour <- function(rk, n, higher_bad = TRUE) {
      pct <- if (higher_bad) rk / n else (n - rk + 1) / n
      if (pct <= 0.25) "#c0392b" else if (pct <= 0.5) "#e67e22" else "#27ae60"
    }
    
    make_rank <- function(label, rk, n, higher_bad = TRUE) {
      col <- rank_colour(rk, n, higher_bad)
      div(style = "margin-bottom:10px;",
          div(style = "font-size:0.7rem;color:#777;text-transform:uppercase;", label),
          div(style = paste0("font-size:1.3rem;font-family:'DM Serif Display',serif;color:", col, ";"),
              paste0("#", round(rk), " / ", n)),
          div(style = paste0("font-size:0.72rem;color:", col, ";"),
              if (higher_bad && rk / n <= 0.25) "⚠️ Bottom quartile"
              else if (!higher_bad && rk / n >= 0.75) "⚠️ Bottom quartile"
              else "")
      )
    }
    
    tagList(
      div(class = "profile-name", "District Rankings"),
      make_rank("Park Access (higher = better)",   park_rank,  nrow(enriched_df), higher_bad = FALSE),
      make_rank("Poverty Rate (lower = better)",   pov_rank,   nrow(enriched_df), higher_bad = FALSE),
      make_rank("Heat Vulnerability (lower = better)", hvi_rank, nrow(enriched_df), higher_bad = FALSE),
      make_rank("Priority Score (higher = more urgent)", score_rank, nrow(enriched_df), higher_bad = TRUE)
    )
  })
  
  output$poverty_trend_dist <- renderPlotly({
    r   <- sel_row()
    pov_d <- pov_ts %>% filter(boro_cd == r$boro_cd)
    if (nrow(pov_d) == 0) return(plotly_empty())
    plot_ly(pov_d, x = ~time_period, y = ~poverty_rate,
            type = "scatter", mode = "lines+markers",
            line  = list(color = "#9b59b6", width = 2),
            marker = list(size = 8, color = "#9b59b6"),
            hovertemplate = "%{x}: %{y:.1f}%<extra></extra>") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Poverty %", gridcolor = "#e8f5e9"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE
      )
  })
  
  # ── COMPARISON ─────────────────────────────────────────────
  cd1_row <- reactive({ enriched_df %>% filter(boro_cd == as.integer(input$compare_cd1)) })
  cd2_row <- reactive({ enriched_df %>% filter(boro_cd == as.integer(input$compare_cd2)) })
  
  output$compare_bars <- renderPlotly({
    r1 <- cd1_row(); r2 <- cd2_row()
    if (nrow(r1) == 0 || nrow(r2) == 0) return(NULL)
    
    metrics <- c("Park ac/1k", "Poverty %", "HVI", "% Nonwhite", "# Parks")
    v1 <- c(r1$park_acres_per1k, r1$poverty_pct, r1$hvi,
            r1$pct_nonwhite * 100, r1$park_count %||% 0)
    v2 <- c(r2$park_acres_per1k, r2$poverty_pct, r2$hvi,
            r2$pct_nonwhite * 100, r2$park_count %||% 0)
    
    plot_ly() %>%
      add_bars(x = metrics, y = v1, name = r1$short_name,
               marker = list(color = "#7fc47f"),
               hovertemplate = paste0(r1$short_name, " — %{x}: %{y:.2f}<extra></extra>")) %>%
      add_bars(x = metrics, y = v2, name = r2$short_name,
               marker = list(color = "#3498db"),
               hovertemplate = paste0(r2$short_name, " — %{x}: %{y:.2f}<extra></extra>")) %>%
      layout(
        barmode = "group",
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = "", gridcolor = "#e8f5e9"),
        yaxis = list(title = "Value", gridcolor = "#e8f5e9"),
        legend = list(x = 0,    y = 1, font = list(color="#1a2e1a")),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
  
  output$compare_radar <- renderPlotly({
    r1 <- cd1_row(); r2 <- cd2_row()
    if (nrow(r1) == 0 || nrow(r2) == 0) return(NULL)
    
    norm <- function(x, col) {
      rng <- range(enriched_df[[col]], na.rm = TRUE)
      (x - rng[1]) / (rng[2] - rng[1])
    }
    
    v1 <- c(norm(r1$park_acres_per1k,"park_acres_per1k"),
            norm(r1$poverty_pct,"poverty_pct"),
            norm(r1$hvi,"hvi"),
            norm(r1$overall_score,"overall_score"))
    v2 <- c(norm(r2$park_acres_per1k,"park_acres_per1k"),
            norm(r2$poverty_pct,"poverty_pct"),
            norm(r2$hvi,"hvi"),
            norm(r2$overall_score,"overall_score"))
    
    labels <- c("Park Access","Poverty","Heat Vuln.","Priority Score","Park Access")
    
    plot_ly(type = "scatterpolar") %>%
      add_trace(r = c(v1, v1[1]), theta = labels, name = r1$short_name,
                fill = "toself", fillcolor = "rgba(127,196,127,0.2)",
                line = list(color = "#7fc47f")) %>%
      add_trace(r = c(v2, v2[1]), theta = labels, name = r2$short_name,
                fill = "toself", fillcolor = "rgba(52,152,219,0.2)",
                line = list(color = "#3498db")) %>%
      layout(
        paper_bgcolor = "#ffffff",
        polar = list(
          bgcolor = "#ffffff",
          radialaxis = list(visible=TRUE, range=c(0,1), gridcolor="#333", color="#888"),
          angularaxis = list(color="#aaa")
        ),
        legend = list(x=0, y=1, font=list(color="#ccc",size=9)),
        margin = list(l=30, r=30, t=20, b=20),
        font = list(color="#ccc", size=10)
      )
  })
  
  output$compare_cards <- renderUI({
    r1 <- cd1_row(); r2 <- cd2_row()
    if (nrow(r1) == 0 || nrow(r2) == 0) return(NULL)
    
    tier_badge <- function(tier) {
      cls <- switch(as.character(tier),
                    "Very high priority" = "badge-vhigh",
                    "High priority"      = "badge-high",
                    "Moderate priority"  = "badge-mod",
                    "Lower priority"     = "badge-low", "badge-low")
      span(class = paste("priority-badge", cls), as.character(tier))
    }
    
    make_col <- function(r, col) {
      div(style = "margin-bottom:16px;",
          div(class = "compare-header", r$short_name),
          div(style="font-size:0.72rem;color:#777;text-transform:uppercase;margin-bottom:2px;","Park Access"),
          div(style="font-size:1.1rem;color:#e0e0e0;", paste0(round(r$park_acres_per1k,2), " ac/1k")),
          div(style="font-size:0.72rem;color:#777;text-transform:uppercase;margin-bottom:2px;margin-top:8px;","Poverty"),
          div(style="font-size:1.1rem;color:#e0e0e0;", paste0(round(r$poverty_pct,1), "%")),
          div(style="font-size:0.72rem;color:#777;text-transform:uppercase;margin-bottom:2px;margin-top:8px;","Heat Vuln."),
          div(style="font-size:1.1rem;color:#e0e0e0;", round(r$hvi,2)),
          div(style="margin-top:10px;", tier_badge(r$priority_tier))
      )
    }
    
    tagList(
      div(class = "chart-title", "Quick Summary"),
      make_col(r1, 1),
      tags$hr(style = "border-color:#2a2a3a;"),
      make_col(r2, 2)
    )
  })
  
  output$compare_race <- renderPlotly({
    r1 <- cd1_row(); r2 <- cd2_row()
    if (nrow(r1) == 0 || nrow(r2) == 0) return(NULL)
    
    races <- c("White","Black","Asian","Hispanic")
    v1 <- c(r1$pct_white_nonhisp*100, r1$pct_black_nonhisp*100,
            r1$pct_asian_pi_nonhisp*100, r1$pct_hispanic*100) %>%
      round(1)
    v2 <- c(r2$pct_white_nonhisp*100, r2$pct_black_nonhisp*100,
            r2$pct_asian_pi_nonhisp*100, r2$pct_hispanic*100) %>%
      round(1)
    
    race_cols <- c("#5dade2","#a569bd","#58d68d","#f0b27a")
    plot_ly() %>%
      { p <- .
      for (i in seq_along(races)) {
        p <- add_bars(p, x = c(r1$short_name, r2$short_name),
                      y = c(v1[i], v2[i]), name = races[i],
                      marker = list(color = race_cols[i]),
                      hovertemplate = paste0(races[i],": %{y:.1f}%<extra></extra>"))
      }
      p
      } %>%
      layout(
        barmode = "group",
        paper_bgcolor = "#ffffff", plot_bgcolor  = "#ffffff",
        font = list(color = "#1a2e1a"),
        xaxis = list(title = ""),
        yaxis = list(title = "% of Population", gridcolor = "#e8f5e9"),
        legend = list(x = 1,    y = 1, font = list(color="#1a2e1a")),
        margin = list(l = 10, r = 10, t = 10, b = 30)
      )
  })
}

# ── 14. Run ──────────────────────────────────────────────────
shinyApp(ui = ui, server = server)