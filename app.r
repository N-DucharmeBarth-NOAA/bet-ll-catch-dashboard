library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)

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

# Active flags: only those with at least one non-zero effort or catch record
active_flags <- sort(unique(dt[HHOOKS > 0 | BET_C > 0 | BET_N > 0, flag_code]))

# Fixed color palette mapped to flag codes (stable across selections)
flag_pal <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(active_flags))
names(flag_pal) <- active_flags

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
      radioButtons("display_mode", "Display values as:",
                   choices = c("Proportion (%)" = "proportion", "Nominal" = "nominal"),
                   selected = "proportion", inline = TRUE),
      selectInput("sel_flags", "Flags:",
                  choices  = active_flags,
                  selected = active_flags,
                  multiple = TRUE,
                  selectize = TRUE),
      div(
        style = "margin-bottom: 10px;",
        actionButton("btn_all",  "Select all",   class = "btn btn-xs btn-default"),
        actionButton("btn_none", "Deselect all", class = "btn btn-xs btn-default")
      ),
      conditionalPanel(
        condition = "input.display_mode === 'proportion'",
        radioButtons("prop_basis", "Proportion relative to:",
                     choices = c("Selected flags only" = "selected",
                                 "All flags (grand total)" = "grand"),
                     selected = "selected")
      )
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
    d <- d[flag_code %in% input$sel_flags]
    d
  })

  # Aggregation over ALL active flags for the current year/lat slice (grand-total denominator)
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
    d <- filtered()
    validate(need(nrow(d) > 0, "No data in selected year range."))

    agg <- d[, .(
      HHOOKS = sum(HHOOKS, na.rm = TRUE),
      BET_C  = sum(BET_C,  na.rm = TRUE),
      BET_N  = sum(BET_N,  na.rm = TRUE)
    ), by = .(lat_band, flag_code)]

    # Ensure every flag × band combination exists (fill absent cells with 0)
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
    long
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

  observeEvent(input$btn_all, {
    updateSelectInput(session, "sel_flags", selected = active_flags)
  })

  observeEvent(input$btn_none, {
    updateSelectInput(session, "sel_flags", selected = character(0))
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

    flag_note <- if (setequal(input$sel_flags, active_flags)) {
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

    if (mode == "proportion") {
      p <- suppressWarnings(
        ggplot(long, aes(x = lat_band, y = proportion, fill = flag_code)) +
          geom_col(aes(text = paste0(
            "Flag: ", flag_code,
            "<br>Percent of total: ", sprintf("%.2f%%", proportion * 100)
          ))) +
          facet_wrap(~ metric, ncol = 1) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             limits = c(0, 1), expand = c(0, 0)) +
          scale_fill_manual(values = flag_pal) +
          labs(x = NULL, y = "Proportion of total", fill = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text = element_text(face = "bold"))
      )
    } else {
      p <- suppressWarnings(
        ggplot(long, aes(x = lat_band, y = raw_value, fill = flag_code)) +
          geom_col(aes(text = paste0(
            "Flag: ", flag_code,
            "<br>Value: ", format(round(raw_value), big.mark = ",", scientific = FALSE)
          ))) +
          facet_wrap(~ metric, ncol = 1, scales = "free_y") +
          scale_y_continuous(labels = scales::comma_format(), expand = c(0, 0)) +
          scale_fill_manual(values = flag_pal) +
          labs(x = NULL, y = "Value", fill = "Flag") +
          theme_bw(base_size = 13) +
          theme(strip.text = element_text(face = "bold"))
      )
    }

    ggplotly(p, tooltip = "text")
  })
}

# ---- Run -----------------------------------------------------------------
shinyApp(ui, server)