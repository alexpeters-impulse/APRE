library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ── APRE 6RM Adjustment Logic ──────────────────────────────────────────────────

# Returns weight adjustment (in lbs) based on reps performed
apre_adjustment <- function(reps) {
  if      (reps <= 2)  -7.5
  else if (reps <= 4)  -2.5
  else if (reps <= 7)   0
  else if (reps <= 12)  7.5
  else                  12.5
}

# Round to nearest plate increment (2.5 lbs or 1.25 kg)
round_to_increment <- function(weight, unit) {
  inc <- if (unit == "lbs") 2.5 else 1.25
  round(weight / inc) * inc
}

# Calculate all APRE 6RM set weights and recommendations
apre_calc <- function(working_weight, set3_reps, set4_reps, unit) {
  w <- working_weight

  # Warm-up sets
  set1_weight <- round_to_increment(w * 0.50, unit)
  set2_weight <- round_to_increment(w * 0.75, unit)

  # Set 4 adjustment (based on Set 3 performance)
  set4_weight <- round_to_increment(w + apre_adjustment(set3_reps), unit)

  # Next session adjustment (based on Set 4 performance)
  next_weight <- round_to_increment(set4_weight + apre_adjustment(set4_reps), unit)

  list(
    set1 = set1_weight,
    set2 = set2_weight,
    set3 = w,
    set4 = set4_weight,
    next_weight = next_weight,
    set3_adj = apre_adjustment(set3_reps),
    set4_adj = apre_adjustment(set4_reps)
  )
}

# ── History helpers ────────────────────────────────────────────────────────────

history_file <- file.path("session_history.csv")

load_history <- function() {
  if (file.exists(history_file)) {
    df <- read.csv(history_file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df
  } else {
    data.frame(
      date          = as.Date(character()),
      exercise      = character(),
      unit          = character(),
      working_weight = numeric(),
      set3_reps     = integer(),
      set4_reps     = integer(),
      set4_weight   = numeric(),
      next_weight   = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

save_history <- function(df) {
  write.csv(df, history_file, row.names = FALSE)
}

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "barbell.svg", height = "28px", style = "margin-right:8px; vertical-align:middle;"),
    "APRE 6RM Calculator"
  ),
  theme = bs_theme(
    version = 5,
    primary = "#2563EB",
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  header = tags$head(
    tags$style(HTML("
      .result-card { border-left: 4px solid #2563EB; }
      .next-session-card { border-left: 4px solid #16a34a; background: #f0fdf4; }
      .set-row { display: flex; justify-content: space-between; align-items: center;
                 padding: 10px 14px; border-radius: 8px; margin-bottom: 6px; }
      .set-warmup  { background: #f1f5f9; }
      .set-working { background: #eff6ff; border: 1px solid #bfdbfe; }
      .set-four    { background: #fef9c3; border: 1px solid #fde047; }
      .set-label   { font-weight: 600; color: #374151; }
      .set-weight  { font-size: 1.15rem; font-weight: 700; color: #1d4ed8; }
      .set-note    { font-size: 0.8rem; color: #6b7280; }
      .next-weight { font-size: 2rem; font-weight: 800; color: #15803d; }
      .hero-label  { font-size: 0.85rem; text-transform: uppercase;
                     letter-spacing: .05em; color: #6b7280; }
      .history-empty { text-align:center; color:#9ca3af; padding:40px 0; }
      .adj-badge-pos { color:#15803d; font-weight:700; }
      .adj-badge-neg { color:#dc2626; font-weight:700; }
      .adj-badge-neu { color:#6b7280; font-weight:700; }
    "))
  ),

  # ── Calculator Tab ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Calculator",
    icon  = icon("calculator"),

    layout_columns(
      col_widths = c(4, 8),

      # — Input card —
      card(
        card_header("Session Input"),
        card_body(
          selectInput("unit", "Weight Unit",
                      choices = c("Pounds (lbs)" = "lbs", "Kilograms (kg)" = "kg"),
                      selected = "lbs"),
          textInput("exercise", "Exercise Name", placeholder = "e.g. Back Squat"),
          numericInput("working_weight", "Current 6RM Working Weight",
                       value = 100, min = 1, step = 2.5),
          hr(),
          h6("After completing Sets 3 & 4, enter reps performed:"),
          numericInput("set3_reps", "Set 3 — Reps performed (AMRAP @ working weight)",
                       value = NULL, min = 0, max = 30, step = 1),
          numericInput("set4_reps", "Set 4 — Reps performed (AMRAP @ adjusted weight)",
                       value = NULL, min = 0, max = 30, step = 1),
          hr(),
          actionButton("calculate", "Calculate", class = "btn-primary w-100",
                       icon = icon("play")),
          br(), br(),
          actionButton("save_session", "Save to History", class = "btn-success w-100",
                       icon = icon("floppy-disk")),
          uiOutput("save_status")
        )
      ),

      # — Results card —
      card(
        card_header("Session Results"),
        card_body(
          uiOutput("results_ui")
        )
      )
    )
  ),

  # ── History Tab ────────────────────────────────────────────────────────────
  nav_panel(
    title = "Progress History",
    icon  = icon("chart-line"),

    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          layout_columns(
            col_widths = c(6, 6),
            "Session History",
            div(style = "text-align:right;",
                actionButton("clear_history", "Clear All", class = "btn-outline-danger btn-sm",
                             icon = icon("trash")))
          )
        ),
        card_body(
          uiOutput("history_filter_ui"),
          br(),
          uiOutput("history_plot_ui"),
          hr(),
          tableOutput("history_table")
        )
      )
    )
  ),

  # ── About Tab ──────────────────────────────────────────────────────────────
  nav_panel(
    title = "About APRE",
    icon  = icon("circle-info"),
    card(
      card_body(
        h4("What is APRE?"),
        p("Autoregulatory Progressive Resistance Exercise (APRE) is an evidence-based
          training system developed by Dr. Thomas DeLorme and later refined by Knight (1979).
          Unlike fixed-percentage programs, APRE adjusts training loads based on your
          actual performance that day — accounting for fatigue, sleep, stress, and readiness."),
        hr(),
        h4("APRE 6RM — Session Structure"),
        tags$table(class = "table table-bordered table-sm",
          tags$thead(tags$tr(
            tags$th("Set"), tags$th("Load"), tags$th("Reps"), tags$th("Purpose")
          )),
          tags$tbody(
            tags$tr(tags$td("1"), tags$td("50% of 6RM"), tags$td("10"), tags$td("Warm-up")),
            tags$tr(tags$td("2"), tags$td("75% of 6RM"), tags$td("6"),  tags$td("Warm-up")),
            tags$tr(tags$td("3"), tags$td("100% of 6RM"), tags$td("AMRAP"), tags$td("Working set — determines Set 4 weight")),
            tags$tr(tags$td("4"), tags$td("Adjusted weight"), tags$td("AMRAP"), tags$td("Working set — determines next session weight"))
          )
        ),
        hr(),
        h4("Weight Adjustment Table"),
        tags$table(class = "table table-bordered table-sm",
          tags$thead(tags$tr(
            tags$th("Reps Performed"), tags$th("Adjustment (lbs)"), tags$th("Adjustment (kg)")
          )),
          tags$tbody(
            tags$tr(tags$td("0–2"),  tags$td("−5 to −10 (−7.5)"), tags$td("−2.5 to −5 (−3.5)")),
            tags$tr(tags$td("3–4"),  tags$td("0 to −5 (−2.5)"),   tags$td("0 to −2.5 (−1.25)")),
            tags$tr(tags$td("5–7"),  tags$td("No change"),          tags$td("No change")),
            tags$tr(tags$td("8–12"), tags$td("+5 to +10 (+7.5)"),  tags$td("+2.5 to +5 (+3.5)")),
            tags$tr(tags$td("13+"),  tags$td("+10 to +15 (+12.5)"),tags$td("+5 to +7.5 (+6.25)"))
          )
        ),
        tags$small(class = "text-muted",
          "Adjustment values used in this calculator are the midpoints of each range,
           rounded to the nearest 2.5 lb / 1.25 kg plate increment."),
        hr(),
        h5("References"),
        tags$ul(
          tags$li("Knight, K.L. (1979). Quadriceps strengthening with the DAPRE technique.
                   Medicine & Science in Sports, 11(4), 336–340."),
          tags$li("Mann, J.B. et al. (2010). The effect of autoregulatory progressive
                   resistance exercise vs. linear periodization on strength improvement
                   in college athletes. Journal of Strength and Conditioning Research.")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive history store
  history <- reactiveVal(load_history())

  # Update weight label based on unit
  observe({
    unit_label <- if (input$unit == "lbs") "Pounds (lbs)" else "Kilograms (kg)"
    step_val   <- if (input$unit == "lbs") 2.5 else 1.25
    updateNumericInput(session, "working_weight",
                       label = paste("Current 6RM Working Weight (", input$unit, ")"),
                       step  = step_val)
  })

  # Computed result (reactive to Calculate button)
  result <- eventReactive(input$calculate, {
    req(input$working_weight, input$set3_reps, input$set4_reps)
    validate(
      need(input$working_weight > 0, "Please enter a positive working weight."),
      need(input$set3_reps >= 0,     "Set 3 reps must be 0 or more."),
      need(input$set4_reps >= 0,     "Set 4 reps must be 0 or more.")
    )
    apre_calc(input$working_weight, input$set3_reps, input$set4_reps, input$unit)
  })

  # Helper to format weight with unit
  fmt <- reactive({
    u <- input$unit
    function(w) paste0(w, " ", u)
  })

  # Adjustment badge HTML
  adj_badge <- function(adj, unit) {
    label <- if (adj > 0) paste0("+", adj) else as.character(adj)
    label <- paste0(label, " ", unit)
    cls   <- if (adj > 0) "adj-badge-pos" else if (adj < 0) "adj-badge-neg" else "adj-badge-neu"
    tags$span(class = cls, paste0("(", label, ")"))
  }

  # ── Results UI ──
  output$results_ui <- renderUI({
    r <- tryCatch(result(), error = function(e) NULL)
    if (is.null(r)) {
      return(div(class = "history-empty",
        icon("dumbbell", style = "font-size:3rem; color:#d1d5db;"),
        br(), br(),
        p("Enter your working weight and reps, then click Calculate.")))
    }

    u   <- input$unit
    f   <- fmt()
    ex  <- if (nchar(trimws(input$exercise)) > 0) input$exercise else "Exercise"

    tagList(
      h5(ex, "— APRE 6RM Session Plan", class = "mb-3"),

      # Warm-up sets
      div(class = "set-row set-warmup",
        div(span(class = "set-label", "Set 1"), br(), span(class = "set-note", "Warm-up • 10 reps")),
        span(class = "set-weight", f(r$set1))
      ),
      div(class = "set-row set-warmup",
        div(span(class = "set-label", "Set 2"), br(), span(class = "set-note", "Warm-up • 6 reps")),
        span(class = "set-weight", f(r$set2))
      ),

      # Working sets
      div(class = "set-row set-working",
        div(span(class = "set-label", "Set 3"), br(), span(class = "set-note", "AMRAP @ working weight")),
        div(style = "text-align:right;",
          span(class = "set-weight", f(r$set3)), br(),
          if (!is.null(input$set3_reps)) span(class = "set-note", input$set3_reps, "reps performed ", adj_badge(r$set3_adj, u))
        )
      ),
      div(class = "set-row set-four",
        div(span(class = "set-label", "Set 4"), br(), span(class = "set-note", "AMRAP @ adjusted weight")),
        div(style = "text-align:right;",
          span(class = "set-weight", style = "color:#b45309;", f(r$set4)), br(),
          if (!is.null(input$set4_reps)) span(class = "set-note", input$set4_reps, "reps performed ", adj_badge(r$set4_adj, u))
        )
      ),

      # Next session recommendation
      br(),
      card(
        class = "next-session-card",
        card_body(
          div(class = "hero-label", "Next Session Working Weight"),
          div(class = "next-weight", f(r$next_weight)),
          tags$small(class = "text-muted",
            "Use this as your new 6RM working weight next session.")
        )
      )
    )
  })

  # ── Save session ──
  output$save_status <- renderUI(NULL)

  observeEvent(input$save_session, {
    r <- tryCatch(result(), error = function(e) NULL)
    if (is.null(r)) {
      output$save_status <- renderUI(
        div(class = "alert alert-warning mt-2 py-2",
            "Please calculate a session first before saving."))
      return()
    }

    new_row <- data.frame(
      date           = Sys.Date(),
      exercise       = if (nchar(trimws(input$exercise)) > 0) trimws(input$exercise) else "Unknown",
      unit           = input$unit,
      working_weight = input$working_weight,
      set3_reps      = input$set3_reps,
      set4_reps      = input$set4_reps,
      set4_weight    = r$set4,
      next_weight    = r$next_weight,
      stringsAsFactors = FALSE
    )

    updated <- rbind(history(), new_row)
    history(updated)
    save_history(updated)

    output$save_status <- renderUI(
      div(class = "alert alert-success mt-2 py-2",
          icon("check"), " Session saved!"))
  })

  # ── History filter ──
  output$history_filter_ui <- renderUI({
    df <- history()
    if (nrow(df) == 0) return(NULL)
    exercises <- sort(unique(df$exercise))
    selectInput("hist_exercise", "Filter by exercise:",
                choices = c("All" = "all", exercises),
                selected = "all")
  })

  filtered_history <- reactive({
    df <- history()
    if (is.null(input$hist_exercise) || input$hist_exercise == "all") return(df)
    df[df$exercise == input$hist_exercise, ]
  })

  # ── History plot ──
  output$history_plot_ui <- renderUI({
    df <- filtered_history()
    if (nrow(df) == 0) return(NULL)
    plotOutput("history_plot", height = "300px")
  })

  output$history_plot <- renderPlot({
    df <- filtered_history()
    req(nrow(df) > 0)

    df <- df %>%
      mutate(session = seq_len(n()),
             label   = paste0(exercise, " (", unit, ")"))

    ggplot(df, aes(x = session, y = next_weight, color = exercise, group = exercise)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(
        title  = "Working Weight Progression",
        x      = "Session #",
        y      = "Next Session Weight",
        color  = "Exercise"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold"),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  # ── History table ──
  output$history_table <- renderTable({
    df <- filtered_history()
    if (nrow(df) == 0) return(NULL)

    df %>%
      arrange(desc(date), desc(row_number())) %>%
      transmute(
        Date           = format(date, "%Y-%m-%d"),
        Exercise       = exercise,
        Unit           = unit,
        `Working Wt`   = working_weight,
        `Set 3 Reps`   = set3_reps,
        `Set 4 Wt`     = set4_weight,
        `Set 4 Reps`   = set4_reps,
        `Next Session` = next_weight
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Clear history ──
  observeEvent(input$clear_history, {
    showModal(modalDialog(
      title  = "Clear All History",
      "Are you sure you want to delete all saved sessions? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Yes, Delete All", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_clear, {
    empty <- load_history()[FALSE, ]
    history(empty)
    if (file.exists(history_file)) file.remove(history_file)
    removeModal()
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
