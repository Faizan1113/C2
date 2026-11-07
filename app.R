
# =====================================================================
#  Agarbatti Manufacturing Cost Calculator  (અગરબત્તી ખર્ચ કેલ્ક્યુલેટર)
#  IMPORTANT: Save this file with UTF-8 encoding
#  (RStudio: File > Save with Encoding... > UTF-8)
# =====================================================================

# ---- Check required package -----------------------------------------
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop('The package "shiny" is not installed. Run: install.packages("shiny")')
}
library(shiny)

RUPEE <- "\u20b9"   # the rupee sign, written as an escape so it never gets corrupted

# ---- Gujarati labels for every input (also used in error messages) ---
field_labels <- c(
  perfume_kg    = "\u0aaa\u0ab0\u0aab\u0acd\u0aaf\u0ac1\u0aae\u0aa8\u0ac1\u0a82 \u0ab5\u0a9c\u0aa8 (\u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe)",
  perfume_price = "\u0aaa\u0ab0\u0aab\u0acd\u0aaf\u0ac1\u0aae\u0aa8\u0acb \u0aad\u0abe\u0ab5 \u0aaa\u0acd\u0ab0\u0aa4\u0abf \u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe (\u20b9)",
  dep_kg        = "DEP \u0aa8\u0ac1\u0a82 \u0ab5\u0a9c\u0aa8 (\u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe)",
  dep_price     = "DEP \u0aa8\u0acb \u0aad\u0abe\u0ab5 \u0aaa\u0acd\u0ab0\u0aa4\u0abf \u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe (\u20b9)",
  packets       = "\u0aaa\u0ac7\u0a95\u0ac7\u0a9f\u0aa8\u0ac0 \u0ab8\u0a82\u0a96\u0acd\u0aaf\u0abe",
  packet_kg     = "\u0a8f\u0a95 \u0aaa\u0ac7\u0a95\u0ac7\u0a9f\u0aa8\u0ac1\u0a82 \u0ab5\u0a9c\u0aa8 (\u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe)",
  raw_price     = "\u0a95\u0abe\u0a9a\u0ac0 \u0a85\u0a97\u0ab0\u0aac\u0aa4\u0acd\u0aa4\u0ac0\u0aa8\u0acb \u0aad\u0abe\u0ab5 \u0aaa\u0acd\u0ab0\u0aa4\u0abf \u0a95\u0abf\u0a97\u0acd\u0ab0\u0abe (\u20b9)",
  pack_charge   = "\u0aaa\u0ac7\u0a95\u0abf\u0a82\u0a97 \u0a85\u0aa8\u0ac7 \u0ab2\u0ac7\u0aac\u0ab2\u0abf\u0a82\u0a97 \u0a9a\u0abe\u0ab0\u0acd\u0a9c \u0aaa\u0acd\u0ab0\u0aa4\u0abf \u0aaa\u0ac7\u0a95\u0ac7\u0a9f (\u20b9)"
)

# =====================================================================
#  PART 1: CALCULATION LOGIC (no Shiny code here)
# =====================================================================

# TRUE only for a single, real, finite number
is_valid_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
}

# Indian digit grouping, e.g. 1234567.5 -> ₹12,34,567.50
format_inr <- function(x) {
  x <- round(x, 2)
  negative <- x < 0
  parts <- strsplit(formatC(abs(x), format = "f", digits = 2), ".", fixed = TRUE)[[1]]
  whole <- parts[1]
  decimals <- parts[2]
  n <- nchar(whole)
  if (n > 3) {
    last_three <- substr(whole, n - 2, n)
    rest <- substr(whole, 1, n - 3)
    groups <- character(0)
    while (nchar(rest) > 2) {                       # group the rest in twos
      groups <- c(substr(rest, nchar(rest) - 1, nchar(rest)), groups)
      rest <- substr(rest, 1, nchar(rest) - 2)
    }
    if (nchar(rest) > 0) groups <- c(rest, groups)
    whole <- paste(c(groups, last_three), collapse = ",")
  }
  paste0(if (negative) "-" else "", RUPEE, whole, ".", decimals)
}

# Weight with 3 decimals, e.g. 25 -> "25.000 કિગ્રા"
format_kg <- function(x) {
  paste0(formatC(x, format = "f", digits = 3), " કિગ્રા")
}

# Returns a character vector of Gujarati error messages (empty = all OK)
validate_inputs <- function(v) {
  errors <- character(0)
  
  # 1) Every field must be filled with a real number
  missing_fields <- names(v)[!vapply(v, is_valid_number, logical(1))]
  if (length(missing_fields) > 0) {
    return(paste0("કૃપા કરીને બધી વિગતો ભરો. ખાલી અથવા ખોટું: ",
                  paste(field_labels[missing_fields], collapse=", ")))
  }
  
  # 2) No negative values
  for (id in names(v)) {
    if (v[[id]] < 0) {
      errors <- c(errors, paste0(field_labels[[id]], " નેગેટિવ (માઇનસ) ન હોઈ શકે."))
    }
  }
  
  # 3) Packets: whole number, at least 1
  if (v$packets >= 0 && (v$packets < 1 || v$packets != round(v$packets))) {
    errors <- c(errors, "પેકેટની સંખ્યા 1 કે તેથી વધુ પૂરી સંખ્યા (જેમ કે 200) હોવી જોઈએ.")
  }
  
  # 4) Packet weight must be above zero (this also prevents zero totals)
  if (v$packet_kg == 0) {
    errors <- c(errors, "એક પેકેટનું વજન 0 કરતાં વધુ હોવું જોઈએ.")
  }
  
  # 5) Raw agarbatti weight must not be negative (only checked if all above is fine)
  if (length(errors) == 0) {
    raw_kg <- v$packets * v$packet_kg - v$perfume_kg - v$dep_kg
    if (raw_kg < -1e-9) {
      errors <- c(errors, paste0(
        "ભૂલ: પરફ્યુમ અને DEP નું કુલ વજન તૈયાર માલના કુલ વજન કરતાં વધુ છે, ",
        "એટલે કાચી અગરબત્તીનું વજન માઇનસ આવે છે. ",
        "કૃપા કરીને પેકેટની સંખ્યા, પેકેટનું વજન, પરફ્યુમ અથવા DEP નું વજન ફરી તપાસો."))
    }
  }
  errors
}

# Exact business formulas. Call only after validate_inputs() returned no errors.
calculate_costs <- function(v) {
  total_weight <- v$packets * v$packet_kg                      # Step 1
  raw_weight   <- total_weight - v$perfume_kg - v$dep_kg       # Step 2
  if (abs(raw_weight) < 1e-9) raw_weight <- 0                  # ignore tiny rounding noise
  
  perfume_cost <- v$perfume_kg * v$perfume_price               # Step 3
  dep_cost     <- v$dep_kg * v$dep_price
  raw_cost     <- raw_weight * v$raw_price
  pack_cost    <- v$packets * v$pack_charge
  
  total_cost      <- perfume_cost + dep_cost + raw_cost + pack_cost   # Step 4
  cost_per_packet <- total_cost / v$packets                            # Step 5
  
  list(total_weight = total_weight, perfume_kg = v$perfume_kg, dep_kg = v$dep_kg,
       raw_weight = raw_weight, perfume_cost = perfume_cost, dep_cost = dep_cost,
       raw_cost = raw_cost, pack_cost = pack_cost, total_cost = total_cost,
       cost_per_packet = cost_per_packet)
}

# =====================================================================
#  PART 2: USER INTERFACE
# =====================================================================

app_css <- "
  body { font-family: 'Noto Sans Gujarati','Nirmala UI','Shruti',sans-serif;
         background:#f1f8f2; color:#1b3a24; }
  .app-wrap { max-width:560px; margin:0 auto; padding:12px 12px 40px 12px; }
  .app-header { background:#1b7a3d; color:#fff; border-radius:14px; padding:18px;
                text-align:center; margin-bottom:14px; }
  .app-header h1 { font-size:24px; font-weight:700; margin:0 0 4px 0; }
  .app-header p { margin:0; font-size:16px; opacity:.95; }
  .panel-box { background:#fff; border:2px solid #cfe8d5; border-radius:14px;
               padding:16px; margin-bottom:14px; }
  .panel-box h2 { font-size:20px; font-weight:700; color:#1b7a3d; margin:0 0 12px 0; }
  .form-group { margin-bottom:14px; }
  label.control-label { font-size:17px; font-weight:600; color:#1b3a24; }
  .form-control { height:54px; font-size:20px; border:2px solid #9fcfab;
                  border-radius:10px; }
  .form-control:focus { border-color:#1b7a3d; box-shadow:0 0 0 3px #cfe8d5; }
  .hint { font-size:14px; color:#4d6b57; margin-top:-4px; margin-bottom:6px; }
  .btn-calc, .btn-reset { width:100%; height:58px; font-size:21px; font-weight:700;
                          border-radius:12px; margin-top:6px; }
  .btn-calc { background:#1b7a3d; color:#fff; border:none; }
  .btn-calc:hover, .btn-calc:focus { background:#146030; color:#fff; }
  .btn-reset { background:#fff; color:#1b7a3d; border:2px solid #1b7a3d; }
  .btn-reset:hover, .btn-reset:focus { background:#e6f4e9; color:#146030; }
  .error-box { background:#fdecea; border:2px solid #d9534f; color:#8a1f1b;
               border-radius:12px; padding:14px; font-size:17px; margin-bottom:14px; }
  .error-box ul { margin:0; padding-left:20px; }
  .result-card { background:#1b7a3d; color:#fff; border-radius:16px; padding:22px 12px;
                 text-align:center; margin-bottom:14px; }
  .result-card .small-title { font-size:18px; }
  .result-card .big-number { font-size:46px; font-weight:800; line-height:1.2; }
  .line-item { display:flex; justify-content:space-between; gap:10px;
               padding:10px 0; border-bottom:1px solid #e3f1e6; font-size:17px; }
  .line-item:last-child { border-bottom:none; }
  .line-item .value { font-weight:700; white-space:nowrap; }
  .line-item.total { font-size:19px; color:#1b7a3d; }
"

make_inputs <- function() {
  tagList(
    numericInput("perfume_kg",    field_labels[["perfume_kg"]],    value = NA, step = 0.001, width = "100%"),
    numericInput("perfume_price", field_labels[["perfume_price"]], value = NA, step = 0.01,  width = "100%"),
    numericInput("dep_kg",        field_labels[["dep_kg"]],        value = NA, step = 0.001, width = "100%"),
    numericInput("dep_price",     field_labels[["dep_price"]],     value = NA, step = 0.01,  width = "100%"),
    numericInput("packets",       field_labels[["packets"]],       value = NA, step = 1,     width = "100%"),
    numericInput("packet_kg",     field_labels[["packet_kg"]],     value = NA, step = 0.001, width = "100%"),
    div(class = "hint", "કિલોગ્રામમાં લખો: 0.150 = 150 ગ્રામ, 0.100 = 100 ગ્રામ, 0.125 = 125 ગ્રામ"),
    numericInput("raw_price",     field_labels[["raw_price"]],     value = NA, step = 0.01,  width = "100%"),
    numericInput("pack_charge",   field_labels[["pack_charge"]],   value = 1,  step = 0.01,  width = "100%")
  )
}

result_line <- function(label, value, extra_class = "") {
  div(class = paste("line-item", extra_class),
      span(label), span(class = "value", value))
}

ui <- fluidPage(
  title = "અગરબત્તી ખર્ચ કેલ્ક્યુલેટર",
  lang = "gu",
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Noto+Sans+Gujarati:wght@400;600;700;800&display=swap"),
    tags$style(HTML(app_css))
  ),
  div(class = "app-wrap",
      div(class = "app-header",
          h1("અગરબત્તી ખર્ચ કેલ્ક્યુલેટર"),
          p("એક પેકેટની સાચી કિંમત જાણો")),
      
      div(class = "panel-box",
          h2("વિગતો ભરો"),
          uiOutput("input_panel"),
          actionButton("calc",  "ગણતરી કરો", class = "btn-calc"),
          actionButton("reset", "રીસેટ",      class = "btn-reset")),
      
      uiOutput("results")
  )
)

# =====================================================================
#  PART 3: SERVER
# =====================================================================

server <- function(input, output, session) {
  
  state       <- reactiveVal(NULL)   # NULL = nothing to show
  reset_count <- reactiveVal(0)      # changing this rebuilds (clears) the input boxes
  
  # The input boxes are rebuilt on Reset -> all blank, packaging charge back to 1
  output$input_panel <- renderUI({
    reset_count()
    make_inputs()
  })
  
  # Read the inputs into a list (missing input -> NA). Inputs are never modified.
  read_inputs <- function() {
    as_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
    list(perfume_kg    = as_num(input$perfume_kg),
         perfume_price = as_num(input$perfume_price),
         dep_kg        = as_num(input$dep_kg),
         dep_price     = as_num(input$dep_price),
         packets       = as_num(input$packets),
         packet_kg     = as_num(input$packet_kg),
         raw_price     = as_num(input$raw_price),
         pack_charge   = as_num(input$pack_charge))
  }
  
  observeEvent(input$calc, {
    v <- read_inputs()
    errors <- validate_inputs(v)
    if (length(errors) > 0) {
      state(list(ok = FALSE, errors = errors))
    } else {
      state(list(ok = TRUE, r = calculate_costs(v)))
    }
  })
  
  # If any input is edited after calculating, hide the old (now outdated) result
  observeEvent(
    list(input$perfume_kg, input$perfume_price, input$dep_kg, input$dep_price,
         input$packets, input$packet_kg, input$raw_price, input$pack_charge),
    state(NULL),
    ignoreInit = TRUE
  )
  
  # Reset: clear results and messages, and rebuild the inputs
  observeEvent(input$reset, {
    state(NULL)
    reset_count(reset_count() + 1)
  })
  
  output$results <- renderUI({
    s <- state()
    if (is.null(s)) return(NULL)
    
    if (!s$ok) {
      return(div(class = "error-box", tags$ul(lapply(s$errors, tags$li))))
    }
    
    r <- s$r
    tagList(
      div(class = "result-card",
          div(class = "small-title", "એક પેકેટની કિંમત"),
          div(class = "big-number", format_inr(r$cost_per_packet))),
      div(class = "panel-box",
          h2("વજનની વિગત"),
          result_line("કુલ તૈયાર માલનું વજન", format_kg(r$total_weight)),
          result_line("પરફ્યુમનું વજન", format_kg(r$perfume_kg)),
          result_line("DEP નું વજન", format_kg(r$dep_kg)),
          result_line("કાચી અગરબત્તીનું વજન (ગણેલું)", format_kg(r$raw_weight))),
      div(class = "panel-box",
          h2("ખર્ચની વિગત"),
          result_line("પરફ્યુમનો ખર્ચ", format_inr(r$perfume_cost)),
          result_line("DEP નો ખર્ચ", format_inr(r$dep_cost)),
          result_line("કાચી અગરબત્તીનો ખર્ચ", format_inr(r$raw_cost)),
          result_line("પેકિંગ અને લેબલિંગ ખર્ચ", format_inr(r$pack_cost)),
          result_line("કુલ બેચ ખર્ચ", format_inr(r$total_cost), "total"))
    )
  })
}

shinyApp(ui, server)


