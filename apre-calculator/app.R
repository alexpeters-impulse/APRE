library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ── APRE 6RM Adjustment Logic ──────────────────────────────────────────────────

apre_adjustment <- function(reps) {
  if      (reps <= 2)  -7.5
  else if (reps <= 4)  -2.5
  else if (reps <= 7)   0
  else if (reps <= 12)  7.5
  else                  12.5
}

round_to_increment <- function(weight, unit) {
  inc <- if (unit == "lbs") 2.5 else 1.25
  round(weight / inc) * inc
}

apre_calc <- function(working_weight, set3_reps, set4_reps, unit) {
  w <- working_weight
  set1_weight <- round_to_increment(w * 0.50, unit)
  set2_weight <- round_to_increment(w * 0.75, unit)
  set4_weight <- round_to_increment(w + apre_adjustment(set3_reps), unit)
  next_weight <- round_to_increment(set4_weight + apre_adjustment(set4_reps), unit)
  list(
    set1        = set1_weight,
    set2        = set2_weight,
    set3        = w,
    set4        = set4_weight,
    next_weight = next_weight,
    set3_adj    = apre_adjustment(set3_reps),
    set4_adj    = apre_adjustment(set4_reps)
  )
}

# ── History helpers ────────────────────────────────────────────────────────────

history_file <- "session_history.csv"

load_history <- function() {
  if (file.exists(history_file)) {
    df <- read.csv(history_file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df
  } else {
    data.frame(
      date           = as.Date(character()),
      exercise       = character(),
      unit           = character(),
      working_weight = numeric(),
      set1_reps      = integer(),
      set2_reps      = integer(),
      set3_reps      = integer(),
      set4_reps      = integer(),
      set4_weight    = numeric(),
      next_weight    = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

save_history <- function(df) write.csv(df, history_file, row.names = FALSE)

# ── UI ─────────────────────────────────────────────────────────────────────────

STEPS <- c("Setup", "Set 1", "Set 2", "Set 3", "Set 4", "Results")

ui <- page_navbar(
  title = "APRE 6RM Calculator",
  theme = bs_theme(
    version   = 5,
    primary   = "#2563EB",
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  header = tags$head(tags$style(HTML("

    /* ── Stepper ── */
    .stepper {
      display: flex; align-items: center; justify-content: center;
      gap: 0; margin: 0 0 28px 0; flex-wrap: nowrap;
    }
    .step-item { display: flex; flex-direction: column; align-items: center;
                 flex: 1; position: relative; }
    .step-circle {
      width: 36px; height: 36px; border-radius: 50%; display: flex;
      align-items: center; justify-content: center; font-weight: 700;
      font-size: .9rem; z-index: 1; transition: all .2s;
    }
    .step-circle.done    { background:#2563EB; color:#fff; }
    .step-circle.active  { background:#2563EB; color:#fff;
                           box-shadow: 0 0 0 4px rgba(37,99,235,.25); }
    .step-circle.pending { background:#e5e7eb; color:#9ca3af; }
    .step-label { font-size:.72rem; margin-top:4px; color:#6b7280; white-space:nowrap; }
    .step-label.active  { color:#2563EB; font-weight:700; }
    .step-connector {
      flex: 1; height: 3px; background: #e5e7eb;
      margin-bottom: 20px; max-width: 60px;
    }
    .step-connector.done { background: #2563EB; }

    /* ── Instruction cards ── */
    .instr-card {
      border-radius: 12px; padding: 24px 28px; margin-bottom: 20px;
      border-left: 6px solid;
    }
    .instr-warmup  { background:#f8fafc; border-color:#94a3b8; }
    .instr-working { background:#eff6ff; border-color:#2563EB; }
    .instr-set4    { background:#fffbeb; border-color:#d97706; }
    .instr-result  { background:#f0fdf4; border-color:#16a34a; }

    .instr-tag {
      font-size:.7rem; font-weight:700; letter-spacing:.08em;
      text-transform:uppercase; margin-bottom:6px;
    }
    .instr-tag-warmup  { color:#64748b; }
    .instr-tag-working { color:#1d4ed8; }
    .instr-tag-set4    { color:#b45309; }
    .instr-tag-result  { color:#15803d; }

    .instr-headline { font-size:1.5rem; font-weight:800; margin-bottom:10px; color:#111827; }
    .instr-weight   { font-size:2.4rem; font-weight:900; color:#2563EB; line-height:1; }
    .instr-weight-set4 { color:#d97706; }
    .instr-detail   { color:#4b5563; margin-top:10px; line-height:1.6; }
    .instr-tip {
      margin-top:14px; padding:10px 14px; background:rgba(0,0,0,.04);
      border-radius:8px; font-size:.85rem; color:#6b7280;
    }

    .next-weight-big { font-size:3rem; font-weight:900; color:#15803d; line-height:1; }
    .unit-label { font-size:1.1rem; color:#6b7280; margin-left:4px; }

    .summary-row {
      display:flex; justify-content:space-between; align-items:center;
      padding:10px 0; border-bottom:1px solid #f3f4f6;
    }
    .summary-label { color:#6b7280; font-size:.9rem; }
    .summary-value { font-weight:700; color:#111827; }

    .history-empty { text-align:center; color:#9ca3af; padding:40px 0; }
    .adj-pos { color:#15803d; font-weight:700; }
    .adj-neg { color:#dc2626; font-weight:700; }
    .adj-neu { color:#6b7280; font-weight:700; }

    /* nav buttons */
    .nav-btns { display:flex; gap:10px; margin-top:20px; }
    .nav-btns .btn { flex:1; }
  "))),

  # ── Calculator Tab ───────────────────────────────────────────────────────────
  nav_panel(
    title = "Calculator",
    icon  = icon("calculator"),
    div(
      style = "max-width:620px; margin:0 auto; padding:20px 0;",

      # Progress stepper
      uiOutput("stepper_ui"),

      # Step content
      uiOutput("step_ui")
    )
  ),

  # ── History Tab ──────────────────────────────────────────────────────────────
  nav_panel(
    title = "Progress History",
    icon  = icon("chart-line"),
    card(
      card_header(
        layout_columns(
          col_widths = c(6, 6),
          "Session History",
          div(style = "text-align:right;",
            actionButton("clear_history", "Clear All",
                         class = "btn-outline-danger btn-sm", icon = icon("trash")))
        )
      ),
      card_body(
        uiOutput("history_filter_ui"), br(),
        uiOutput("history_plot_ui"),
        hr(),
        tableOutput("history_table")
      )
    )
  ),

  # ── About Tab ────────────────────────────────────────────────────────────────
  nav_panel(
    title = "About APRE",
    icon  = icon("circle-info"),
    card(card_body(
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
          tags$tr(tags$td("1"), tags$td("50% of 6RM"), tags$td("10"),    tags$td("Warm-up")),
          tags$tr(tags$td("2"), tags$td("75% of 6RM"), tags$td("6"),     tags$td("Warm-up")),
          tags$tr(tags$td("3"), tags$td("100% of 6RM"), tags$td("AMRAP"), tags$td("Working — sets Set 4 weight")),
          tags$tr(tags$td("4"), tags$td("Adjusted weight"), tags$td("AMRAP"), tags$td("Working — sets next session weight"))
        )
      ),
      hr(),
      h4("Weight Adjustment Table"),
      tags$table(class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("Reps Performed"), tags$th("Adjustment (lbs)"), tags$th("Adjustment (kg)")
        )),
        tags$tbody(
          tags$tr(tags$td("0–2"),  tags$td("−5 to −10 (−7.5)"),   tags$td("−2.5 to −5 (−3.5)")),
          tags$tr(tags$td("3–4"),  tags$td("0 to −5 (−2.5)"),     tags$td("0 to −2.5 (−1.25)")),
          tags$tr(tags$td("5–7"),  tags$td("No change"),            tags$td("No change")),
          tags$tr(tags$td("8–12"), tags$td("+5 to +10 (+7.5)"),   tags$td("+2.5 to +5 (+3.5)")),
          tags$tr(tags$td("13+"),  tags$td("+10 to +15 (+12.5)"), tags$td("+5 to +7.5 (+6.25)"))
        )
      ),
      tags$small(class = "text-muted",
        "Values used here are midpoints, rounded to the nearest plate increment."),
      hr(),
      h5("References"),
      tags$ul(
        tags$li("Knight, K.L. (1979). Quadriceps strengthening with the DAPRE technique.
                 Medicine & Science in Sports, 11(4), 336–340."),
        tags$li("Mann, J.B. et al. (2010). The effect of autoregulatory progressive
                 resistance exercise vs. linear periodization on strength improvement
                 in college athletes. Journal of Strength and Conditioning Research.")
      )
    ))
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Wizard state ──
  step     <- reactiveVal(0)   # 0=Setup 1=Set1 2=Set2 3=Set3 4=Set4 5=Results
  calc     <- reactiveVal(NULL) # apre_calc() result, populated after setup
  s1_reps  <- reactiveVal(NA)
  s2_reps  <- reactiveVal(NA)
  s3_reps  <- reactiveVal(NA)
  s4_reps  <- reactiveVal(NA)
  history  <- reactiveVal(load_history())

  # ── Stepper UI ──
  output$stepper_ui <- renderUI({
    cur <- step()
    items <- list()
    for (i in seq_along(STEPS)) {
      idx  <- i - 1  # 0-based
      circ_cls <- if (idx < cur) "done" else if (idx == cur) "active" else "pending"
      lbl_cls  <- if (idx == cur) "active" else ""
      icon_content <- if (idx < cur) HTML("&#10003;") else as.character(i)
      items[[length(items) + 1]] <- div(class = "step-item",
        div(class = paste("step-circle", circ_cls), icon_content),
        div(class = paste("step-label", lbl_cls), STEPS[i])
      )
      if (i < length(STEPS)) {
        conn_cls <- if (idx < cur) "done" else ""
        items[[length(items) + 1]] <- div(class = paste("step-connector", conn_cls))
      }
    }
    div(class = "stepper", tagList(items))
  })

  # ── Helper: weight formatter ──
  fw <- function(w, u) tags$strong(paste0(w, " ", u))

  # ── Step UI dispatcher ──
  output$step_ui <- renderUI({
    s  <- step()
    r  <- calc()
    u  <- if (!is.null(r)) isolate(input$setup_unit) else "lbs"

    if (s == 0) step_setup()
    else if (s == 1) step_set1(r, u)
    else if (s == 2) step_set2(r, u)
    else if (s == 3) step_set3(r, u)
    else if (s == 4) step_set4(r, u)
    else             step_results(r, u)
  })

  # ── Step 0: Setup ──
  step_setup <- function() {
    tagList(
      div(class = "instr-card instr-warmup",
        div(class = "instr-tag instr-tag-warmup", "Session Setup"),
        div(class = "instr-headline", "Let's set up your session"),
        div(class = "instr-detail",
          "Enter your exercise and your current 6-rep max working weight.
           The app will guide you through each set."
        )
      ),
      selectInput("setup_unit", "Weight unit",
                  choices  = c("Pounds (lbs)" = "lbs", "Kilograms (kg)" = "kg"),
                  selected = "lbs"),
      textInput("setup_exercise", "Exercise name", placeholder = "e.g. Back Squat"),
      numericInput("setup_weight", "Current 6RM working weight",
                   value = 100, min = 1, step = 2.5),
      uiOutput("setup_preview"),
      div(class = "nav-btns",
        actionButton("btn_start", "Start Session →", class = "btn-primary btn-lg")
      )
    )
  }

  output$setup_preview <- renderUI({
    req(input$setup_weight, input$setup_unit)
    w  <- input$setup_weight
    u  <- input$setup_unit
    s1 <- round_to_increment(w * 0.50, u)
    s2 <- round_to_increment(w * 0.75, u)
    div(class = "instr-tip",
      icon("circle-info"), " ",
      tags$strong("Warm-up weights: "),
      "Set 1 → ", tags$strong(paste0(s1, " ", u)), " for 10 reps    |    ",
      "Set 2 → ", tags$strong(paste0(s2, " ", u)), " for 6 reps"
    )
  })

  observeEvent(input$btn_start, {
    req(input$setup_weight)
    validate(need(input$setup_weight > 0, ""))
    # Store a partial calc now (set3/4 reps unknown yet; fill with placeholders)
    w <- input$setup_weight
    u <- input$setup_unit
    calc(list(
      set1 = round_to_increment(w * 0.50, u),
      set2 = round_to_increment(w * 0.75, u),
      set3 = w,
      set4 = NA, next_weight = NA,
      set3_adj = NA, set4_adj = NA
    ))
    step(1)
  })

  # ── Step 1: Set 1 ──
  step_set1 <- function(r, u) {
    tagList(
      div(class = "instr-card instr-warmup",
        div(class = "instr-tag instr-tag-warmup", "Set 1 of 4 — Warm-Up"),
        div(class = "instr-headline", "Load the bar to"),
        div(class = "instr-weight", r$set1, tags$span(class = "unit-label", u)),
        div(class = "instr-detail",
          tags$strong("Perform 10 repetitions"), " at a comfortable, controlled pace.",
          br(), "This set prepares your muscles and joints for heavier work. Do not go to failure."
        ),
        div(class = "instr-tip",
          icon("clock"), " Rest ", tags$strong("2–3 minutes"), " after this set before moving to Set 2."
        )
      ),
      numericInput("s1_reps_input", "Reps completed:", value = 10, min = 0, max = 30, step = 1),
      div(class = "nav-btns",
        actionButton("btn_s1_back", "← Back", class = "btn-outline-secondary"),
        actionButton("btn_s1_next", "Done — Next: Set 2 →", class = "btn-primary")
      )
    )
  }

  observeEvent(input$btn_s1_back, step(0))
  observeEvent(input$btn_s1_next, {
    req(input$s1_reps_input)
    s1_reps(input$s1_reps_input)
    step(2)
  })

  # ── Step 2: Set 2 ──
  step_set2 <- function(r, u) {
    tagList(
      div(class = "instr-card instr-warmup",
        div(class = "instr-tag instr-tag-warmup", "Set 2 of 4 — Warm-Up"),
        div(class = "instr-headline", "Load the bar to"),
        div(class = "instr-weight", r$set2, tags$span(class = "unit-label", u)),
        div(class = "instr-detail",
          tags$strong("Perform 6 repetitions"), " with good form.",
          br(), "This set bridges the warm-up and your working weight. Controlled tempo throughout."
        ),
        div(class = "instr-tip",
          icon("clock"), " Rest ", tags$strong("2–3 minutes"), " after this set before Set 3."
        )
      ),
      numericInput("s2_reps_input", "Reps completed:", value = 6, min = 0, max = 30, step = 1),
      div(class = "nav-btns",
        actionButton("btn_s2_back", "← Back", class = "btn-outline-secondary"),
        actionButton("btn_s2_next", "Done — Next: Set 3 →", class = "btn-primary")
      )
    )
  }

  observeEvent(input$btn_s2_back, step(1))
  observeEvent(input$btn_s2_next, {
    req(input$s2_reps_input)
    s2_reps(input$s2_reps_input)
    step(3)
  })

  # ── Step 3: Set 3 ──
  step_set3 <- function(r, u) {
    tagList(
      div(class = "instr-card instr-working",
        div(class = "instr-tag instr-tag-working", "Set 3 of 4 — Working Set"),
        div(class = "instr-headline", "Load the bar to"),
        div(class = "instr-weight", r$set3, tags$span(class = "unit-label", u)),
        div(class = "instr-detail",
          tags$strong("Perform as many reps as possible (AMRAP)"), " with safe, controlled form.",
          br(), "Give maximum effort — the number of reps you complete here determines your Set 4 weight."
        ),
        div(class = "instr-tip",
          icon("triangle-exclamation"), " Stop before form breaks down. Safety first.",
          br(),
          icon("clock"), " Rest ", tags$strong("3–5 minutes"), " before Set 4."
        )
      ),
      numericInput("s3_reps_input", "Reps completed:", value = NULL, min = 0, max = 50, step = 1),
      uiOutput("s3_preview"),
      div(class = "nav-btns",
        actionButton("btn_s3_back", "← Back", class = "btn-outline-secondary"),
        actionButton("btn_s3_next", "Done — See Set 4 Weight →", class = "btn-primary")
      )
    )
  }

  output$s3_preview <- renderUI({
    req(input$s3_reps_input, calc())
    reps <- input$s3_reps_input
    r    <- calc()
    u    <- input$setup_unit
    adj  <- apre_adjustment(reps)
    s4w  <- round_to_increment(r$set3 + adj, u)
    adj_lbl <- if (adj > 0) paste0("+", adj) else as.character(adj)
    adj_cls <- if (adj > 0) "adj-pos" else if (adj < 0) "adj-neg" else "adj-neu"
    div(class = "instr-tip",
      icon("arrow-right"), " Based on ", tags$strong(reps), " reps, your Set 4 weight will be ",
      tags$strong(paste0(s4w, " ", u)),
      " ", tags$span(class = adj_cls, paste0("(", adj_lbl, " ", u, ")"))
    )
  })

  observeEvent(input$btn_s3_back, step(2))
  observeEvent(input$btn_s3_next, {
    req(input$s3_reps_input)
    reps <- input$s3_reps_input
    s3_reps(reps)
    # Update calc with set4 weight now that we know set3 reps
    r  <- calc()
    u  <- input$setup_unit
    s4 <- round_to_increment(r$set3 + apre_adjustment(reps), u)
    calc(modifyList(r, list(set4 = s4, set3_adj = apre_adjustment(reps))))
    step(4)
  })

  # ── Step 4: Set 4 ──
  step_set4 <- function(r, u) {
    tagList(
      div(class = "instr-card instr-set4",
        div(class = "instr-tag instr-tag-set4", "Set 4 of 4 — Working Set"),
        div(class = "instr-headline", "Adjust bar to"),
        div(class = "instr-weight instr-weight-set4", r$set4, tags$span(class = "unit-label", u)),
        div(class = "instr-detail",
          tags$strong("Perform as many reps as possible (AMRAP)"), " with safe, controlled form.",
          br(), "The reps you hit here will determine the weight you use at your next session."
        ),
        div(class = "instr-tip",
          icon("triangle-exclamation"), " Stop before form breaks down.",
          br(),
          icon("lightbulb"), " This set autoregulates your training — do your best!"
        )
      ),
      numericInput("s4_reps_input", "Reps completed:", value = NULL, min = 0, max = 50, step = 1),
      uiOutput("s4_preview"),
      div(class = "nav-btns",
        actionButton("btn_s4_back", "← Back", class = "btn-outline-secondary"),
        actionButton("btn_s4_next", "Done — See Results →", class = "btn-primary")
      )
    )
  }

  output$s4_preview <- renderUI({
    req(input$s4_reps_input, calc())
    reps <- input$s4_reps_input
    r    <- calc()
    u    <- input$setup_unit
    adj  <- apre_adjustment(reps)
    nxt  <- round_to_increment(r$set4 + adj, u)
    adj_lbl <- if (adj > 0) paste0("+", adj) else as.character(adj)
    adj_cls <- if (adj > 0) "adj-pos" else if (adj < 0) "adj-neg" else "adj-neu"
    div(class = "instr-tip",
      icon("arrow-right"), " Based on ", tags$strong(reps), " reps, your next session weight will be ",
      tags$strong(paste0(nxt, " ", u)),
      " ", tags$span(class = adj_cls, paste0("(", adj_lbl, " ", u, ")"))
    )
  })

  observeEvent(input$btn_s4_back, step(3))
  observeEvent(input$btn_s4_next, {
    req(input$s4_reps_input)
    reps <- input$s4_reps_input
    s4_reps(reps)
    r   <- calc()
    u   <- input$setup_unit
    adj <- apre_adjustment(reps)
    nxt <- round_to_increment(r$set4 + adj, u)
    calc(modifyList(r, list(next_weight = nxt, set4_adj = adj)))
    step(5)
  })

  # ── Step 5: Results ──
  step_results <- function(r, u) {
    ex <- if (!is.null(input$setup_exercise) && nchar(trimws(input$setup_exercise)) > 0)
            trimws(input$setup_exercise) else "Exercise"

    adj4_lbl <- if (!is.na(r$set4_adj)) {
      if (r$set4_adj > 0) paste0("+", r$set4_adj) else as.character(r$set4_adj)
    } else "—"
    adj4_cls <- if (!is.na(r$set4_adj)) {
      if (r$set4_adj > 0) "adj-pos" else if (r$set4_adj < 0) "adj-neg" else "adj-neu"
    } else "adj-neu"

    tagList(
      div(class = "instr-card instr-result",
        div(class = "instr-tag instr-tag-result", "Session Complete"),
        div(class = "instr-headline", paste0("Great work on ", ex, "!")),
        div(class = "instr-detail", "Here is your next session working weight:"),
        div(class = "next-weight-big", r$next_weight, tags$span(class = "unit-label", u))
      ),

      # Session summary
      h6("Session Summary", class = "mt-3 mb-2 text-muted"),
      div(
        div(class = "summary-row",
          span(class = "summary-label", "Exercise"),
          span(class = "summary-value", ex)
        ),
        div(class = "summary-row",
          span(class = "summary-label", "Set 1 — Warm-up"),
          span(class = "summary-value", paste0(r$set1, " ", u, "  ×  ", s1_reps(), " reps"))
        ),
        div(class = "summary-row",
          span(class = "summary-label", "Set 2 — Warm-up"),
          span(class = "summary-value", paste0(r$set2, " ", u, "  ×  ", s2_reps(), " reps"))
        ),
        div(class = "summary-row",
          span(class = "summary-label", "Set 3 — AMRAP"),
          span(class = "summary-value", paste0(r$set3, " ", u, "  ×  ", s3_reps(), " reps"))
        ),
        div(class = "summary-row",
          span(class = "summary-label", "Set 4 — AMRAP"),
          span(class = "summary-value", paste0(r$set4, " ", u, "  ×  ", s4_reps(), " reps",
            "  ", tags$span(class = adj4_cls, paste0("(", adj4_lbl, " ", u, ")"))))
        ),
        div(class = "summary-row",
          span(class = "summary-label", tags$strong("Next session weight")),
          span(class = "summary-value", style = "color:#15803d; font-size:1.1rem;",
            paste0(r$next_weight, " ", u))
        )
      ),

      br(),
      uiOutput("results_save_status"),
      div(class = "nav-btns",
        actionButton("btn_save", "Save to History", class = "btn-success",
                     icon = icon("floppy-disk")),
        actionButton("btn_new_session", "Start New Session", class = "btn-outline-primary",
                     icon = icon("rotate-right"))
      )
    )
  }

  output$results_save_status <- renderUI(NULL)

  observeEvent(input$btn_save, {
    r  <- calc()
    u  <- input$setup_unit
    ex <- if (nchar(trimws(input$setup_exercise)) > 0) trimws(input$setup_exercise) else "Unknown"

    new_row <- data.frame(
      date           = Sys.Date(),
      exercise       = ex,
      unit           = u,
      working_weight = input$setup_weight,
      set1_reps      = s1_reps(),
      set2_reps      = s2_reps(),
      set3_reps      = s3_reps(),
      set4_reps      = s4_reps(),
      set4_weight    = r$set4,
      next_weight    = r$next_weight,
      stringsAsFactors = FALSE
    )
    updated <- rbind(history(), new_row)
    history(updated)
    save_history(updated)
    output$results_save_status <- renderUI(
      div(class = "alert alert-success py-2", icon("check"), " Session saved to history!"))
  })

  observeEvent(input$btn_new_session, {
    step(0); calc(NULL)
    s1_reps(NA); s2_reps(NA); s3_reps(NA); s4_reps(NA)
    output$results_save_status <- renderUI(NULL)
  })

  # ── History Tab ──────────────────────────────────────────────────────────────

  output$history_filter_ui <- renderUI({
    df <- history()
    if (nrow(df) == 0) return(NULL)
    exercises <- sort(unique(df$exercise))
    selectInput("hist_exercise", "Filter by exercise:",
                choices = c("All" = "all", exercises), selected = "all")
  })

  filtered_history <- reactive({
    df <- history()
    if (is.null(input$hist_exercise) || input$hist_exercise == "all") return(df)
    df[df$exercise == input$hist_exercise, ]
  })

  output$history_plot_ui <- renderUI({
    df <- filtered_history()
    if (nrow(df) == 0) return(div(class = "history-empty",
      icon("dumbbell", style = "font-size:3rem;"), br(), br(),
      p("No sessions saved yet. Complete a workout and save it to see your progress.")))
    plotOutput("history_plot", height = "300px")
  })

  output$history_plot <- renderPlot({
    df <- filtered_history()
    req(nrow(df) > 0)
    df <- df %>% mutate(session = seq_len(n()))
    ggplot(df, aes(x = session, y = next_weight, color = exercise, group = exercise)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(title = "Working Weight Progression", x = "Session #",
           y = "Next Session Weight", color = "Exercise") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom", panel.grid.minor = element_blank())
  })

  output$history_table <- renderTable({
    df <- filtered_history()
    if (nrow(df) == 0) return(NULL)
    df %>%
      arrange(desc(date)) %>%
      transmute(
        Date           = format(date, "%Y-%m-%d"),
        Exercise       = exercise,
        Unit           = unit,
        `Working Wt`   = working_weight,
        `S1 Reps`      = set1_reps,
        `S2 Reps`      = set2_reps,
        `S3 Reps`      = set3_reps,
        `Set 4 Wt`     = set4_weight,
        `S4 Reps`      = set4_reps,
        `Next Session` = next_weight
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  observeEvent(input$clear_history, {
    showModal(modalDialog(
      title = "Clear All History",
      "Are you sure you want to delete all saved sessions? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Yes, Delete All", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_clear, {
    history(load_history()[FALSE, ])
    if (file.exists(history_file)) file.remove(history_file)
    removeModal()
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
