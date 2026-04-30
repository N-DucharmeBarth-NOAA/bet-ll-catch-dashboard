library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)

# ---- Data load & preprocessing -------------------------------------------
csv_path <- file.path(".","data","WCPFC_L_PUBLIC_BY_YY_FLAG.csv")

dt <- fread(csv_path)

# Convert LAT5/LON5 to numeric centroids on a 0-360 longitude convention
dt[, `:=`(
  lat = (as.numeric(sub("[NS]$", "", LAT5)) * fifelse(grepl("S$", LAT5), -1, 1)) + 2.5,
  lon = fifelse(grepl("E$", LON5),
                as.numeric(sub("E$", "", LON5)),
                360 - as.numeric(sub("W$", "", LON5))) + 2.5
)]

# Filter out records with empty flag_code
dt <- dt[flag_code != ""]

# Reassign US flag_code south of the equator to AS (American Samoa)
dt[flag_code == "US" & lat < 0, flag_code := "AS"]

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("WCPFC Longline: Effort & Bigeye Catch by Management Band"),
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
              "Southern LL is south of the lower bound; Northern LL is north of the upper bound.")
    ),
    mainPanel(
      uiOutput("plot_caption"),
      DT::DTOutput("band_summary"),
      tags$br(),
      plotlyOutput("prop_plot", height = "750px")
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
    d
  })

  long_data <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data in selected year range."))

    agg <- d[, .(
      HHOOKS = sum(HHOOKS, na.rm = TRUE),
      BET_C  = sum(BET_C,  na.rm = TRUE),
      BET_N  = sum(BET_N,  na.rm = TRUE)
    ), by = .(lat_band, flag_code)]

    tot_h <- sum(agg$HHOOKS); tot_c <- sum(agg$BET_C); tot_n <- sum(agg$BET_N)
    agg[, `:=`(
      p_HHOOKS = if (tot_h > 0) HHOOKS / tot_h else 0,
      p_BET_C  = if (tot_c > 0) BET_C  / tot_c else 0,
      p_BET_N  = if (tot_n > 0) BET_N  / tot_n else 0
    )]

    long <- melt(agg,
                 id.vars = c("lat_band", "flag_code"),
                 measure.vars = c("p_HHOOKS", "p_BET_C", "p_BET_N"),
                 variable.name = "metric", value.name = "proportion")

    long[, metric := factor(metric,
                            levels = c("p_HHOOKS", "p_BET_C", "p_BET_N"),
                            labels = c("Effort (hooks)",
                                       "Catch (mt)",
                                       "Catch (numbers)"))]
    long
  })

  # Reactive table data: band totals as percentages, wide format
  band_table_dat <- reactive({
    long <- long_data()
    band_totals <- long[, .(band_total = sum(proportion)), by = .(metric, lat_band)]
    wide <- dcast(band_totals, metric ~ lat_band, value.var = "band_total", fill = 0)
    setnames(wide, "metric", "Metric")
    # Convert proportions to percentages (numeric, formatted by DT)
    pct_cols <- intersect(c("Southern LL", "Tropical LL", "Northern LL"), names(wide))
    wide[, (pct_cols) := lapply(.SD, function(x) x * 100), .SDcols = pct_cols]
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

    cap <- paste0(
      "Proportion of total by management band (%) \u2014 Years: ",
      input$year_range[1], "\u2013", input$year_range[2],
      " | Tropical LL: ", lo_lab, " to ", hi_lab
    )

    pct_cols <- intersect(c("Southern LL", "Tropical LL", "Northern LL"), names(dat))

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
      DT::formatRound(columns = pct_cols, digits = 1) |>
      DT::formatStyle(pct_cols, `text-align` = "right")
  }, server = FALSE)

  output$prop_plot <- renderPlotly({
    long <- long_data()

    p <- ggplot(long, aes(x = lat_band, y = proportion, fill = flag_code)) +
      geom_col(aes(text = paste0(
        "Flag: ", flag_code,
        "<br>Percent of total: ", sprintf("%.2f%%", proportion * 100)
      ))) +
      facet_wrap(~ metric, ncol = 1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), expand = c(0, 0)) +
      labs(x = NULL, y = "Proportion of total", fill = "Flag") +
      theme_bw(base_size = 13) +
      theme(strip.text = element_text(face = "bold"))

    ggplotly(p, tooltip = "text")
  })
}

# ---- Run -----------------------------------------------------------------
shinyApp(ui, server)