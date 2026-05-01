library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(viridisLite)

# ---- Data load & preprocessing -------------------------------------------
csv_path     <- file.path(".", "data", "WCPFC_L_PUBLIC_BY_YY_FLAG.csv")
country_path <- file.path(".", "data", "country_codes.csv")

dt       <- fread(csv_path)
ccodes   <- fread(country_path)

# Convert LAT5/LON5 to numeric centroids on a 0-360 longitude convention
# Strip direction suffix first so fifelse evaluates clean numerics in both branches
dt[, lon_num := as.numeric(sub("[EW]$", "", LON5))]
dt[, `:=`(
  lat = (as.numeric(sub("[NS]$", "", LAT5)) * fifelse(grepl("S$", LAT5), -1, 1)) + 2.5,
  lon = fifelse(grepl("E$", LON5), lon_num, 360 - lon_num) + 2.5
)]
dt[, lon_num := NULL]

# Filter out records with empty flag_code
dt <- dt[flag_code != ""]

# Reassign US flag_code south of the equator to AS (American Samoa)
dt[flag_code == "US" & lat < 0, flag_code := "AS"]

# Active flags: only those with at least one non-zero effort or catch record
active_flags <- sort(unique(dt[HHOOKS > 0 | BET_C > 0 | BET_N > 0, flag_code]))

# ---- Flag metadata: full names, custom group, ordering, palette -----------
# Treat US as DWFN and AS as Territory for plotting purposes,
# overriding the group1 column from the CSV.
flag_meta <- ccodes[country_code %in% active_flags,
                    .(flag_code = country_code, country, group1)]

flag_meta[, plot_group := fcase(
  flag_code == "US",                              "DWFN",
  flag_code == "AS",                              "Territory",
  group1 == "FFA PNA",                            "FFA PNA",
  group1 == "FFA SPG",                            "FFA SPG",
  group1 == "FFA Other",                          "FFA Other",
  flag_code %in% c("JP", "KR", "TW", "CN"),       "DWFN",
  flag_code %in% c("NC", "PF"),                   "Territory",
  default = "Other"
)]

# Group ordering controls stack order in the plot (bottom -> top)
group_order <- c("FFA PNA", "FFA SPG", "FFA Other", "DWFN", "Territory", "Other")
flag_meta[, plot_group := factor(plot_group, levels = group_order)]
setorder(flag_meta, plot_group, country)

# ---- Flag metadata: full names, alphabetical order, rainbow palette -------
flag_meta <- ccodes[country_code %in% active_flags,
                    .(flag_code = country_code, country)]
setorder(flag_meta, flag_code)  # alphabetical by code

flag_levels <- flag_meta$flag_code
flag_labels <- setNames(sprintf("%s (%s)", flag_meta$country, flag_meta$flag_code),
                        flag_meta$flag_code)
flag_choices <- setNames(flag_meta$flag_code, flag_labels)

# Rainbow palette across the alphabetically ordered flags
mix_with_white <- function(cols, amount = 0.25) {
  rgb_mat <- col2rgb(cols) / 255
  white   <- matrix(1, nrow = 3, ncol = ncol(rgb_mat))
  blended <- rgb_mat * (1 - amount) + white * amount
  rgb(blended[1, ], blended[2, ], blended[3, ])
}

flag_pal <- setNames(
  mix_with_white(viridisLite::turbo(length(flag_levels)), amount = 0.25),
  flag_levels
)

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("WCPFC Longline: Effort & Bigeye Catch by Management Zone"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range",
                  "Year range:",
                  min = min(dt$YY), max = max(dt$YY),
                  value = c(2000, max(dt$YY)),
                  step = 1, sep = ""),
      sliderInput("lat_band",
                  "Tropical LL bounds (lower, upper):",
                  min = -40, max = 40,
                  value = c(-10, 20),
                  step = 5),
      helpText("Defines the latitude bounds of the Tropical LL band. ",
              "Southern LL is south of the lower bound; Northern LL is north of the upper bound."),
      conditionalPanel(
        condition = "input.main_tabs !== 'CPUE plot' && input.main_tabs !== 'CPUE data'",
        radioButtons("display_mode", "Display values as:",
                     choices = c("Proportion (%)" = "proportion", "Nominal" = "nominal"),
                     selected = "proportion", inline = TRUE),
        pickerInput(
          inputId  = "sel_metrics",
          label    = "Data to show:",
          choices  = c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"),
          selected = c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"),
          multiple = TRUE,
          options  = list(`actions-box` = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs === 'CPUE plot' || input.main_tabs === 'CPUE data'",
        radioButtons("cpue_numerator", "CPUE numerator:",
                     choices = c("Weight (kg / 100 hooks)"    = "weight",
                                 "Numbers (fish / 100 hooks)" = "numbers"),
                     selected = "weight", inline = FALSE)
      ),
      pickerInput(
        inputId  = "sel_flags",
        label    = "Flags:",
        choices  = flag_choices,
        selected = flag_levels,
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      conditionalPanel(
        condition = "input.display_mode === 'proportion' && input.main_tabs !== 'CPUE plot' && input.main_tabs !== 'CPUE data'",
        radioButtons("prop_basis", "Proportion relative to:",
                     choices = c("Selected flags only" = "selected",
                                 "All flags (grand total)" = "grand"),
                     selected = "selected")
      ),
      tags$hr(),
      tags$div(
        tags$p(
          tags$strong("Source"),
          style = "margin-bottom: 4px; font-size: 12px;"
        ),
        tags$p(
          "WCPFC public-domain longline aggregated catch and effort data ",
          "(5\u00B0 \u00D7 5\u00B0 grid, by flag and year, longline, 1950\u20132024, ",
          "WCPFC Convention Area). ",
          tags$a(
            href   = "https://www.wcpfc.int/sustainability/scientific-data/wcpfc-public-domain-aggregated-catcheffort-data-download-page",
            target = "_blank",
            "Data download page"
          ), ".",
          style = "color: #666; font-size: 11px; line-height: 1.4;"
        ),
        style = "margin-top: 12px;"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Summary plot",
          tags$br(),
          uiOutput("plot_caption"),
          DT::DTOutput("band_summary"),
          tags$br(),
          plotlyOutput("prop_plot", height = "750px")
        ),
        tabPanel("Summary data",
          tags$br(),
          DT::DTOutput("flag_data_table")
        ),
        tabPanel("Time-series plot",
          tags$br(),
          plotlyOutput("ts_plot", height = "900px")
        ),
        tabPanel("Time-series data",
          tags$br(),
          DT::DTOutput("ts_data_table")
        ),
        tabPanel("CPUE plot",
          tags$br(),
          plotlyOutput("cpue_plot", height = "500px")
        ),
        tabPanel("CPUE data",
          tags$br(),
          DT::DTOutput("cpue_data_table")
        )
      )
    )
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    lo <- input$lat_band[1]
    hi <- input$lat_band[2]

    d <- dt[YY >= input$year_range[1] & YY <= input$year_range[2]]

    d[, lat_band := fcase(
      lat <  lo,             "Southern LL",
      lat >= lo & lat <= hi, "Tropical LL",
      lat >  hi,             "Northern LL"
    )]

    d[, lat_band := factor(lat_band,
                           levels = c("Southern LL", "Tropical LL", "Northern LL"))]
    d <- d[flag_code %in% input$sel_flags]
    d
  })

  # Aggregation over ALL active flags (grand-total denominator)
  all_flags_agg <- reactive({
    lo <- input$lat_band[1]
    hi <- input$lat_band[2]
    d <- dt[YY >= input$year_range[1] & YY <= input$year_range[2] &
              flag_code %in% active_flags]
    d[, lat_band := fcase(
      lat <  lo,             "Southern LL",
      lat >= lo & lat <= hi, "Tropical LL",
      lat >  hi,             "Northern LL"
    )]
    d[, .(HHOOKS = sum(HHOOKS, na.rm = TRUE),
          BET_C  = sum(BET_C,  na.rm = TRUE),
          BET_N  = sum(BET_N,  na.rm = TRUE)),
      by = .(lat_band, flag_code)]
  })

  long_data <- reactive({
    validate(need(length(input$sel_flags) > 0,
                  "No flags selected \u2014 use the Flags selector to include at least one flag."))
    validate(need(length(input$sel_metrics) > 0,
                  "No data selected \u2014 use the Data to show selector to include at least one metric."))
    d <- filtered()
    validate(need(nrow(d) > 0, "No data in selected year range."))

    agg <- d[, .(
      HHOOKS = sum(HHOOKS, na.rm = TRUE),
      BET_C  = sum(BET_C,  na.rm = TRUE),
      BET_N  = sum(BET_N,  na.rm = TRUE)
    ), by = .(lat_band, flag_code)]

    full_grid <- CJ(
      lat_band  = factor(c("Southern LL", "Tropical LL", "Northern LL"),
                         levels = c("Southern LL", "Tropical LL", "Northern LL")),
      flag_code = input$sel_flags
    )
    agg <- agg[full_grid, on = .(lat_band, flag_code)]
    agg[is.na(HHOOKS), `:=`(HHOOKS = 0L, BET_C = 0, BET_N = 0)]

    tot_base <- if (isTRUE(input$prop_basis == "grand")) all_flags_agg() else agg
    tot_h <- sum(tot_base$HHOOKS); tot_c <- sum(tot_base$BET_C); tot_n <- sum(tot_base$BET_N)
    agg[, `:=`(
      p_HHOOKS = if (tot_h > 0) HHOOKS / tot_h else 0,
      p_BET_C  = if (tot_c > 0) BET_C  / tot_c else 0,
      p_BET_N  = if (tot_n > 0) BET_N  / tot_n else 0,
      HHOOKS   = as.double(HHOOKS),
      BET_C    = as.double(BET_C),
      BET_N    = as.double(BET_N)
    )]

    long <- melt(agg,
                 id.vars = c("lat_band", "flag_code"),
                 measure.vars = list(
                   proportion = c("p_HHOOKS", "p_BET_C", "p_BET_N"),
                   raw_value  = c("HHOOKS",   "BET_C",   "BET_N")
                 ))

    long[, variable := factor(variable, levels = 1:3,
                              labels = c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"))]
    setnames(long, "variable", "metric")

    # Filter to only selected metrics
    long <- long[as.character(metric) %in% input$sel_metrics]

    # Apply group-based ordering of flags (controls stacking + legend order)
    long[, flag_code := factor(flag_code, levels = flag_levels)]
    # Add display label for tooltips / legend
    long[, flag_label := flag_labels[as.character(flag_code)]]
    long[, flag_label := factor(flag_label,
                                levels = flag_labels[flag_levels])]
    long
  })

  # ---- Time-series reactive: year x zone x flag, per-year denominators ----
  long_ts_data <- reactive({
    validate(need(length(input$sel_flags) > 0,
                  "No flags selected \u2014 use the Flags selector to include at least one flag."))
    validate(need(length(input$sel_metrics) > 0,
                  "No data selected \u2014 use the Data to show selector to include at least one metric."))
    d <- filtered()
    validate(need(nrow(d) > 0, "No data in selected year range."))

    # Aggregate selected flags by year x zone x flag
    agg <- d[, .(
      HHOOKS = sum(HHOOKS, na.rm = TRUE),
      BET_C  = sum(BET_C,  na.rm = TRUE),
      BET_N  = sum(BET_N,  na.rm = TRUE)
    ), by = .(YY, lat_band, flag_code)]

    # Full grid: every year x zone x selected flag, so geom_path / geom_area
    # see continuous (zero-filled) series rather than gaps.
    yrs <- seq.int(input$year_range[1], input$year_range[2])
    full_grid <- CJ(
      YY        = yrs,
      lat_band  = factor(c("Southern LL", "Tropical LL", "Northern LL"),
                         levels = c("Southern LL", "Tropical LL", "Northern LL")),
      flag_code = input$sel_flags
    )
    agg <- agg[full_grid, on = .(YY, lat_band, flag_code)]
    agg[is.na(HHOOKS), `:=`(HHOOKS = 0L, BET_C = 0, BET_N = 0)]

    # Per-year-per-zone denominator. "selected" sums over selected flags;
    # "grand" sums over all active flags (so selected stacks may sum to <100%).
    if (isTRUE(input$prop_basis == "grand")) {
      lo <- input$lat_band[1]; hi <- input$lat_band[2]
      grand <- dt[YY >= input$year_range[1] & YY <= input$year_range[2] &
                    flag_code %in% active_flags]
      grand[, lat_band := fcase(
        lat <  lo,             "Southern LL",
        lat >= lo & lat <= hi, "Tropical LL",
        lat >  hi,             "Northern LL"
      )]
      grand[, lat_band := factor(lat_band,
                                 levels = c("Southern LL", "Tropical LL", "Northern LL"))]
      denom <- grand[, .(
        tot_HHOOKS = sum(HHOOKS, na.rm = TRUE),
        tot_BET_C  = sum(BET_C,  na.rm = TRUE),
        tot_BET_N  = sum(BET_N,  na.rm = TRUE)
      ), by = .(YY, lat_band)]
    } else {
      denom <- agg[, .(
        tot_HHOOKS = sum(HHOOKS, na.rm = TRUE),
        tot_BET_C  = sum(BET_C,  na.rm = TRUE),
        tot_BET_N  = sum(BET_N,  na.rm = TRUE)
      ), by = .(YY, lat_band)]
    }

    agg <- denom[agg, on = .(YY, lat_band)]
    agg[, `:=`(
      p_HHOOKS = fifelse(tot_HHOOKS > 0, HHOOKS / tot_HHOOKS, 0),
      p_BET_C  = fifelse(tot_BET_C  > 0, BET_C  / tot_BET_C,  0),
      p_BET_N  = fifelse(tot_BET_N  > 0, BET_N  / tot_BET_N,  0),
      HHOOKS   = as.double(HHOOKS),
      BET_C    = as.double(BET_C),
      BET_N    = as.double(BET_N)
    )]
    agg[, c("tot_HHOOKS", "tot_BET_C", "tot_BET_N") := NULL]

    long <- melt(agg,
                 id.vars      = c("YY", "lat_band", "flag_code"),
                 measure.vars = list(
                   proportion = c("p_HHOOKS", "p_BET_C", "p_BET_N"),
                   raw_value  = c("HHOOKS",   "BET_C",   "BET_N")
                 ))

    long[, variable := factor(variable, levels = 1:3,
                              labels = c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"))]
    setnames(long, "variable", "metric")

    long <- long[as.character(metric) %in% input$sel_metrics]

    long[, flag_code  := factor(flag_code, levels = flag_levels)]
    long[, flag_label := flag_labels[as.character(flag_code)]]
    long[, flag_label := factor(flag_label, levels = flag_labels[flag_levels])]
    long
  })

  # ---- CPUE: cell-level CPUE, then unweighted mean across cells -----------
  cpue_ts_data <- reactive({
    validate(need(length(input$sel_flags) > 0,
                  "No flags selected \u2014 use the Flags selector to include at least one flag."))
    d <- filtered()
    validate(need(nrow(d) > 0, "No data in selected year range."))

    # Numerator: BET_C (mt) -> kg, or BET_N (numbers)
    num_col   <- if (isTRUE(input$cpue_numerator == "numbers")) "BET_N" else "BET_C"
    num_scale <- if (isTRUE(input$cpue_numerator == "numbers")) 1 else 1000  # mt -> kg

    # Cell-level: one CPUE per (year, zone, flag, lat-cell, lon-cell).
    # HHOOKS is hundred-hooks, so cell_cpue is already "per 100 hooks".
    # Multiply weight numerator by 1000 to give kg / 100 hooks.
    cells <- d[HHOOKS > 0,
               .(cell_cpue = (sum(get(num_col), na.rm = TRUE) * num_scale) /
                              sum(HHOOKS, na.rm = TRUE)),
               by = .(YY, lat_band, flag_code, lat, lon)]

    # Unweighted mean across cells, by (year, zone, flag), and count of cells.
    # No zero-fill: missing combos stay missing -> geom_path shows gaps.
    agg <- cells[, .(cpue    = mean(cell_cpue, na.rm = TRUE),
                     n_cells = .N),
                 by = .(YY, lat_band, flag_code)]

    agg[, flag_code  := factor(flag_code, levels = flag_levels)]
    agg[, flag_label := flag_labels[as.character(flag_code)]]
    agg[, flag_label := factor(flag_label, levels = flag_labels[flag_levels])]
    agg
  })

  # Reactive table data: band totals, wide format
  band_table_dat <- reactive({
    long    <- long_data()
    mode    <- input$display_mode
    val_col <- if (mode == "proportion") "proportion" else "raw_value"
    band_totals <- long[, .(total = sum(get(val_col))), by = .(metric, lat_band)]
    wide <- dcast(band_totals, metric ~ lat_band, value.var = "total", fill = 0)
    setnames(wide, "metric", "Metric")
    val_cols <- intersect(c("Southern LL", "Tropical LL", "Northern LL"), names(wide))
    if (mode == "proportion") {
      wide[, (val_cols) := lapply(.SD, function(x) x * 100), .SDcols = val_cols]
    }
    as.data.frame(wide)
  })

  output$plot_caption <- renderUI({
    lo <- input$lat_band[1]; hi <- input$lat_band[2]
    lo_lab <- if (lo < 0) sprintf("%d\u00B0S", abs(lo)) else sprintf("%d\u00B0N", lo)
    hi_lab <- if (hi < 0) sprintf("%d\u00B0S", abs(hi)) else sprintf("%d\u00B0N", hi)
    tags$p(
      tags$strong(sprintf("Years %d\u2013%d", input$year_range[1], input$year_range[2])),
      tags$span(sprintf(" | Tropical LL: %s to %s", lo_lab, hi_lab)),
      style = "margin-bottom: 6px; font-size: 14px;"
    )
  })

  output$band_summary <- DT::renderDT({
    dat <- band_table_dat()

    lo <- input$lat_band[1]; hi <- input$lat_band[2]
    lo_lab <- if (lo < 0) sprintf("%d\u00B0S", abs(lo)) else sprintf("%d\u00B0N", lo)
    hi_lab <- if (hi < 0) sprintf("%d\u00B0S", abs(hi)) else sprintf("%d\u00B0N", hi)

    flag_note <- if (setequal(input$sel_flags, flag_levels)) {
      "All flags"
    } else {
      paste0("flags: ", paste(sort(input$sel_flags), collapse = ", "))
    }

    cap <- if (input$display_mode == "proportion") {
      paste0("Proportion of total by management band (%) \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    } else {
      paste0("Nominal values by management band \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    }

    val_cols <- intersect(c("Southern LL", "Tropical LL", "Northern LL"), names(dat))

    DT::datatable(
      dat,
      extensions = "Buttons",
      caption    = cap,
      rownames   = FALSE,
      options    = list(
        dom        = "Bfrtip",
        buttons    = list(
          list(extend = "copy",  text = "Copy",  title = "WCPFC LL band summary"),
          list(extend = "csv",   text = "CSV",   filename = "wcpfc_ll_band_summary"),
          list(extend = "excel", text = "Excel", filename = "wcpfc_ll_band_summary",
               title = "WCPFC LL band summary"),
          list(extend = "pdf",   text = "PDF",   filename = "wcpfc_ll_band_summary",
               title = "WCPFC LL band summary",
               orientation = "landscape")
        ),
        paging     = FALSE,
        searching  = FALSE,
        info       = FALSE,
        ordering   = FALSE,
        scrollX    = TRUE
      ),
      class = "stripe hover compact"
    ) |>
      DT::formatRound(columns = val_cols,
                      digits  = if (input$display_mode == "proportion") 2 else 0,
                      mark    = if (input$display_mode == "nominal") "," else "") |>
      DT::formatStyle(val_cols, `text-align` = "right")
  }, server = FALSE)

  output$prop_plot <- renderPlotly({
    long <- long_data()
    mode <- input$display_mode

    # Named palette using flag labels (so legend shows full names, colors stay correct)
    pal_named <- setNames(flag_pal[flag_levels],
                          flag_labels[flag_levels])

    if (mode == "proportion") {
      p <- suppressWarnings(
        ggplot(long, aes(x = lat_band, y = proportion, fill = flag_label)) +
          geom_col(aes(text = paste0(
            "Flag: ", flag_label,
            "<br>Percent of total: ", sprintf("%.2f%%", proportion * 100)
          ))) +
          facet_wrap(~ metric, ncol = 1) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             limits = c(0, 1), expand = c(0, 0)) +
          scale_fill_manual(values = pal_named, drop = FALSE) +
          labs(x = NULL, y = "Proportion of total", fill = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text = element_text(face = "bold"))
      )
    } else {
      p <- suppressWarnings(
        ggplot(long, aes(x = lat_band, y = raw_value, fill = flag_label)) +
          geom_col(aes(text = paste0(
            "Flag: ", flag_label,
            "<br>Value: ", format(round(raw_value), big.mark = ",", scientific = FALSE)
          ))) +
          facet_wrap(~ metric, ncol = 1, scales = "free_y") +
          scale_y_continuous(labels = scales::comma_format(), expand = c(0, 0)) +
          scale_fill_manual(values = pal_named, drop = FALSE) +
          labs(x = NULL, y = "Value", fill = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text = element_text(face = "bold"))
      )
    }

    ggplotly(p, tooltip = "text")
  })

  # ---- Time-series plot ---------------------------------------------------
  output$ts_plot <- renderPlotly({
    long <- long_ts_data()
    mode <- input$display_mode

    pal_named <- setNames(flag_pal[flag_levels], flag_labels[flag_levels])

    if (mode == "proportion") {
      p <- suppressWarnings(
        ggplot(long, aes(x = YY, y = proportion,
                         fill = flag_label, group = flag_label)) +
          geom_col(aes(text = paste0(
            "Year: ", YY,
            "<br>Flag: ", flag_label,
            "<br>Percent of year: ", sprintf("%.2f%%", proportion * 100)
          )), position = "stack", width = 1) +
          facet_grid(metric ~ lat_band, scales = "fixed") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             expand = c(0, 0)) +
          scale_fill_manual(values = pal_named, drop = FALSE) +
          labs(x = NULL, y = "Proportion of year total", fill = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text      = element_text(face = "bold"),
                legend.position = "bottom")
      )
    } else {
      p <- suppressWarnings(
        ggplot(long, aes(x = YY, y = raw_value,
                         colour = flag_label, group = flag_label)) +
          geom_path(aes(text = paste0(
            "Year: ", YY,
            "<br>Flag: ", flag_label,
            "<br>Value: ", format(round(raw_value), big.mark = ",", scientific = FALSE)
          ))) +
          facet_grid(metric ~ lat_band, scales = "free_y") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(labels = scales::comma_format()) +
          scale_colour_manual(values = pal_named, drop = FALSE) +
          labs(x = NULL, y = "Value", colour = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text      = element_text(face = "bold"),
                legend.position = "bottom")
      )
    }

    ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "h", y = -0.15))
  })

  # ---- CPUE plot ----------------------------------------------------------
  output$cpue_plot <- renderPlotly({
    agg <- cpue_ts_data()
    validate(need(nrow(agg) > 0, "No CPUE data for the current selection."))

    pal_named <- setNames(flag_pal[flag_levels], flag_labels[flag_levels])

    y_lab <- if (isTRUE(input$cpue_numerator == "numbers")) {
      "CPUE (fish / 100 hooks)"
    } else {
      "CPUE (kg / 100 hooks)"
    }

    p <- suppressWarnings(
      ggplot(agg, aes(x = YY, y = cpue,
                      colour = flag_label, group = flag_label)) +
        geom_path(aes(text = paste0(
          "Year: ", YY,
          "<br>Flag: ", flag_label,
          "<br>CPUE: ", format(round(cpue, 2), big.mark = ",", scientific = FALSE)
        ))) +
        facet_grid(. ~ lat_band, scales = "fixed") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_colour_manual(values = pal_named, drop = FALSE) +
        labs(x = NULL, y = y_lab, colour = "Flag") +
        theme_bw(base_size = 13) +
        theme(strip.text      = element_text(face = "bold"),
              legend.position = "bottom")
    )

    ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "h", y = -0.25))
  })

  # ---- Data tab: flag-level wide table ------------------------------------
  flag_table_dat <- reactive({
    long    <- long_data()
    mode    <- input$display_mode
    val_col <- if (mode == "proportion") "proportion" else "raw_value"

    # Keep only the columns we need and rename value col generically
    d <- long[, .(flag_code, lat_band, metric, value = get(val_col))]

    # Build combined column name: "metric | zone"
    d[, col_name := paste0(as.character(metric), " | ", as.character(lat_band))]

    # Dcast: one row per flag, columns = metric x zone combos
    col_order <- as.vector(outer(
      intersect(c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"), input$sel_metrics),
      c("Southern LL", "Tropical LL", "Northern LL"),
      FUN = function(m, z) paste0(m, " | ", z)
    ))

    wide <- dcast(d, flag_code ~ col_name, value.var = "value", fill = 0)

    # Add missing columns (in case some combos don't exist)
    for (cn in col_order) {
      if (!cn %in% names(wide)) wide[[cn]] <- 0
    }
    setcolorder(wide, c("flag_code", intersect(col_order, names(wide))))

    # Replace flag_code with full labels
    wide[, Flag := flag_labels[as.character(flag_code)]]
    wide[, flag_code := NULL]
    setcolorder(wide, c("Flag", intersect(col_order, names(wide))))

    # Scale proportions to percent
    val_cols <- intersect(col_order, names(wide))
    if (mode == "proportion") {
      wide[, (val_cols) := lapply(.SD, function(x) x * 100), .SDcols = val_cols]
    }

    as.data.frame(wide)
  })

  # ---- Time-series tab: flag x year wide table ----------------------------
  ts_table_dat <- reactive({
    long    <- long_ts_data()
    mode    <- input$display_mode
    val_col <- if (mode == "proportion") "proportion" else "raw_value"

    d <- long[, .(flag_code, YY, lat_band, metric, value = get(val_col))]

    d[, col_name := paste0(as.character(metric), " | ", as.character(lat_band))]

    col_order <- as.vector(outer(
      intersect(c("Effort (hooks)", "Catch (mt)", "Catch (numbers)"), input$sel_metrics),
      c("Southern LL", "Tropical LL", "Northern LL"),
      FUN = function(m, z) paste0(m, " | ", z)
    ))

    wide <- dcast(d, flag_code + YY ~ col_name, value.var = "value", fill = 0)

    for (cn in col_order) {
      if (!cn %in% names(wide)) wide[[cn]] <- 0
    }

    # Replace flag_code with full label, rename year column
    wide[, Flag := flag_labels[as.character(flag_code)]]
    wide[, flag_code := NULL]
    setnames(wide, "YY", "Year")
    setcolorder(wide, c("Flag", "Year", intersect(col_order, names(wide))))
    setorder(wide, Flag, Year)

    val_cols <- intersect(col_order, names(wide))
    if (mode == "proportion") {
      wide[, (val_cols) := lapply(.SD, function(x) x * 100), .SDcols = val_cols]
    }

    as.data.frame(wide)
  })

  # ---- CPUE tab: flag x year wide table -----------------------------------
  cpue_table_dat <- reactive({
    agg <- cpue_ts_data()

    # Long -> wide for both cpue and n_cells, then interleave columns.
    wide_cpue <- dcast(agg, flag_code + YY ~ lat_band, value.var = "cpue")
    wide_n    <- dcast(agg, flag_code + YY ~ lat_band, value.var = "n_cells")

    zones <- c("Southern LL", "Tropical LL", "Northern LL")
    for (cn in zones) {
      if (!cn %in% names(wide_cpue)) wide_cpue[[cn]] <- NA_real_
      if (!cn %in% names(wide_n))    wide_n[[cn]]    <- NA_integer_
    }

    # Rename the n_cells columns and merge
    setnames(wide_n, zones, paste0(zones, " (n)"))
    wide <- wide_cpue[wide_n, on = .(flag_code, YY)]

    wide[, Flag := flag_labels[as.character(flag_code)]]
    wide[, flag_code := NULL]
    setnames(wide, "YY", "Year")

    # Interleave: SLL, SLL (n), TLL, TLL (n), NLL, NLL (n)
    interleaved <- as.vector(rbind(zones, paste0(zones, " (n)")))
    setcolorder(wide, c("Flag", "Year", interleaved))
    setorder(wide, Flag, Year)

    as.data.frame(wide)
  })

  output$flag_data_table <- DT::renderDT({
    dat <- flag_table_dat()

    lo <- input$lat_band[1]; hi <- input$lat_band[2]
    lo_lab <- if (lo < 0) sprintf("%d\u00B0S", abs(lo)) else sprintf("%d\u00B0N", lo)
    hi_lab <- if (hi < 0) sprintf("%d\u00B0S", abs(hi)) else sprintf("%d\u00B0N", hi)

    flag_note <- if (setequal(input$sel_flags, flag_levels)) {
      "All flags"
    } else {
      paste0("flags: ", paste(sort(input$sel_flags), collapse = ", "))
    }

    cap <- if (input$display_mode == "proportion") {
      paste0("Proportion of total by flag and management band (%) \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    } else {
      paste0("Nominal values by flag and management band \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    }

    val_cols <- setdiff(names(dat), "Flag")

    DT::datatable(
      dat,
      extensions = "Buttons",
      caption    = cap,
      rownames   = FALSE,
      options    = list(
        dom        = "Bfrtip",
        buttons    = list(
          list(extend = "copy",  text = "Copy",  title = "WCPFC LL flag data"),
          list(extend = "csv",   text = "CSV",   filename = "wcpfc_ll_flag_data"),
          list(extend = "excel", text = "Excel", filename = "wcpfc_ll_flag_data",
               title = "WCPFC LL flag data"),
          list(extend = "pdf",   text = "PDF",   filename = "wcpfc_ll_flag_data",
               title = "WCPFC LL flag data",
               orientation = "landscape")
        ),
        pageLength = 20,
        ordering   = TRUE,
        searching  = TRUE,
        info       = TRUE,
        scrollX    = TRUE
      ),
      class = "stripe hover compact"
    ) |>
      (\(tbl) if (input$display_mode == "nominal")
        DT::formatCurrency(tbl, columns = val_cols, currency = "", digits = 0)
      else
        DT::formatRound(tbl, columns = val_cols, digits = 2)
      )() |>
      DT::formatStyle(val_cols, `text-align` = "right")
  }, server = FALSE)

  output$ts_data_table <- DT::renderDT({
    dat <- ts_table_dat()

    lo <- input$lat_band[1]; hi <- input$lat_band[2]
    lo_lab <- if (lo < 0) sprintf("%d\u00B0S", abs(lo)) else sprintf("%d\u00B0N", lo)
    hi_lab <- if (hi < 0) sprintf("%d\u00B0S", abs(hi)) else sprintf("%d\u00B0N", hi)

    flag_note <- if (setequal(input$sel_flags, flag_levels)) {
      "All flags"
    } else {
      paste0("flags: ", paste(sort(input$sel_flags), collapse = ", "))
    }

    cap <- if (input$display_mode == "proportion") {
      paste0("Annual proportion of year total by flag and management band (%) \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    } else {
      paste0("Annual nominal values by flag and management band \u2014 Years: ",
             input$year_range[1], "\u2013", input$year_range[2],
             " | Tropical LL: ", lo_lab, " to ", hi_lab,
             " | ", flag_note)
    }

    val_cols <- setdiff(names(dat), c("Flag", "Year"))

    DT::datatable(
      dat,
      extensions = "Buttons",
      caption    = cap,
      rownames   = FALSE,
      options    = list(
        dom        = "Bfrtip",
        buttons    = list(
          list(extend = "copy",  text = "Copy",  title = "WCPFC LL flag annual data"),
          list(extend = "csv",   text = "CSV",   filename = "wcpfc_ll_flag_annual_data"),
          list(extend = "excel", text = "Excel", filename = "wcpfc_ll_flag_annual_data",
               title = "WCPFC LL flag annual data"),
          list(extend = "pdf",   text = "PDF",   filename = "wcpfc_ll_flag_annual_data",
               title = "WCPFC LL flag annual data",
               orientation = "landscape")
        ),
        pageLength = 25,
        ordering   = TRUE,
        searching  = TRUE,
        info       = TRUE,
        scrollX    = TRUE
      ),
      class = "stripe hover compact"
    ) |>
      (\(tbl) if (input$display_mode == "nominal")
        DT::formatCurrency(tbl, columns = val_cols, currency = "", digits = 0)
      else
        DT::formatRound(tbl, columns = val_cols, digits = 2)
      )() |>
      DT::formatStyle(val_cols, `text-align` = "right")
  }, server = FALSE)

  output$cpue_data_table <- DT::renderDT({
    dat <- cpue_table_dat()
    validate(need(nrow(dat) > 0, "No CPUE data for the current selection."))

    lo <- input$lat_band[1]; hi <- input$lat_band[2]
    lo_lab <- if (lo < 0) sprintf("%d\u00B0S", abs(lo)) else sprintf("%d\u00B0N", lo)
    hi_lab <- if (hi < 0) sprintf("%d\u00B0S", abs(hi)) else sprintf("%d\u00B0N", hi)

    flag_note <- if (setequal(input$sel_flags, flag_levels)) {
      "All flags"
    } else {
      paste0("flags: ", paste(sort(input$sel_flags), collapse = ", "))
    }

    units_lab <- if (isTRUE(input$cpue_numerator == "numbers")) {
      "fish / 100 hooks"
    } else {
      "kg / 100 hooks"
    }

    cap <- paste0("Annual CPUE (", units_lab,
                  ") by flag and management band \u2014 mean of cell-level CPUEs \u2014 Years: ",
                  input$year_range[1], "\u2013", input$year_range[2],
                  " | Tropical LL: ", lo_lab, " to ", hi_lab,
                  " | ", flag_note)

    cpue_cols <- c("Southern LL", "Tropical LL", "Northern LL")
    n_cols    <- paste0(cpue_cols, " (n)")
    val_cols  <- c(cpue_cols, n_cols)

    DT::datatable(
      dat,
      extensions = "Buttons",
      caption    = cap,
      rownames   = FALSE,
      options    = list(
        dom        = "Bfrtip",
        buttons    = list(
          list(extend = "copy",  text = "Copy",  title = "WCPFC LL CPUE"),
          list(extend = "csv",   text = "CSV",   filename = "wcpfc_ll_cpue"),
          list(extend = "excel", text = "Excel", filename = "wcpfc_ll_cpue",
               title = "WCPFC LL CPUE"),
          list(extend = "pdf",   text = "PDF",   filename = "wcpfc_ll_cpue",
               title = "WCPFC LL CPUE",
               orientation = "landscape")
        ),
        pageLength = 25,
        ordering   = TRUE,
        searching  = TRUE,
        info       = TRUE,
        scrollX    = TRUE
      ),
      class = "stripe hover compact"
    ) |>
      DT::formatRound(columns = cpue_cols, digits = 2) |>
      DT::formatRound(columns = n_cols,    digits = 0) |>
      DT::formatStyle(val_cols, `text-align` = "right")
  }, server = FALSE)

}

# ---- Run -----------------------------------------------------------------
shinyApp(ui, server)