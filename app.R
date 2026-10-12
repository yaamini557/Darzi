# app.R : Darzi, a cutting guide studio (India edition)
# Photo + size -> exact measurements -> cutting guide image, with smart shape nesting.
#
# One-time setup in R:
#   install.packages(c("shiny", "bslib", "httr2", "jsonlite", "base64enc", "reticulate"))
#   reticulate::py_install(c("shapely", "numpy"))          # optional, enables smart shape nesting
#   Sys.setenv(ANTHROPIC_API_KEY = "...")                   # optional, enables "Read style from photo"
# Then:  shiny::runApp()     (R 4.1+, all files in one folder)

library(shiny)
library(bslib)
source("nesting.R")
source("drafting.R")
source("guide.R")
source("vision.R")
source("shapely_nest.R")

shapely_ok <- shapely_available()

fabric_colours <- c("Cream" = "#F3E6C8", "Marigold" = "#F2B84B", "Indigo" = "#7C8CC8", "Maroon" = "#B5566A",
                    "Emerald" = "#5DB596", "Rose" = "#EBB1C2", "Charcoal" = "#8A8A96")

font_ok <- tryCatch(font_google("Poppins"), error = function(e) "sans-serif")
theme <- bs_theme(version = 5, primary = "#4B3F8F", secondary = "#E8A33D", success = "#1D9E75",
                  base_font = font_ok, heading_font = font_ok, "border-radius" = "0.8rem")

css <- "
  body { background: #f6f4fb; }
  .card { border: 0; box-shadow: 0 2px 14px rgba(59,47,122,.10); border-radius: 16px; }
  .hero { background: linear-gradient(110deg, #3b2f7a 0%, #7a3f91 55%, #e8a33d 120%); color: #fff;
          border-radius: 18px; padding: 22px 26px; margin-bottom: 14px; }
  .hero h2 { margin: 0 0 4px 0; font-weight: 700; letter-spacing: .2px; }
  .hero p { margin: 0; opacity: .88; }
  .chip { display: inline-block; padding: 3px 11px; border-radius: 999px; font-size: 12px;
          background: rgba(255,255,255,.18); margin: 8px 6px 0 0; }
  .accordion-button:not(.collapsed) { background: #efeafc; color: #3b2f7a; font-weight: 600; }
  .accordion-button { font-weight: 600; }
  .bslib-value-box .value-box-value { font-weight: 700; }
  .nav-link.active { font-weight: 600; }
  .btn-primary { border: 0; background: linear-gradient(90deg, #4b3f8f, #7a3f91); }
  .help-block, .form-text { color: #7a7690; }
  table.table { font-size: 14px; }
  .step-note { font-size: 12.5px; color: #7a7690; margin-top: 6px; }
"

ui <- page_sidebar(
  theme = theme,
  title = tagList(icon("scissors"), " Darzi"),
  tags$head(tags$style(HTML(css))),
  sidebar = sidebar(
    width = 410,
    accordion(
      id = "steps", multiple = TRUE, open = c("photo", "garment"),
      accordion_panel(
        "1 \u00B7 Reference photo", value = "photo", icon = icon("image"),
        fileInput("img", NULL, accept = c("image/png", "image/jpeg", "image/webp"),
                  buttonLabel = "Choose photo", placeholder = "Photo of the outfit"),
        imageOutput("ref", height = "auto"),
        actionButton("detect", "Read style from photo", icon = icon("wand-magic-sparkles"), class = "btn-primary"),
        div(class = "step-note", "The photo sets garment and style only. Exact sizes come from the measurements.")
      ),
      accordion_panel(
        "2 \u00B7 Garment & style", value = "garment", icon = icon("shirt"),
        selectInput("garment", "Garment", garment_choices),
        conditionalPanel("input.garment == 'Skirt' || input.garment == 'Lehenga'",
          selectInput("skirt_type", "Skirt type", c("Gathered", "Circular"))),
        conditionalPanel("input.garment == 'Custom piece list'",
          fileInput("custom", "Pieces CSV in cm (name, w, h, qty, grain_locked)", accept = ".csv")),
        layout_columns(col_widths = c(6, 6),
          selectInput("fit", "Fit", names(ease_in), selected = "Regular"),
          selectInput("flare", "Shape / flare", names(flare_cm), selected = "A-line")),
        layout_columns(col_widths = c(6, 6),
          selectInput("neck", "Neckline", names(neck_styles)),
          selectInput("sleeve", "Sleeve", sleeve_choices, selected = "Short")),
        selectInput("shaping", "Shaping", shaping_choices),
        selectInput("bottom", "Bottom wear", bottom_groups, selected = "None"),
        checkboxInput("dupatta", "Include dupatta", FALSE)
      ),
      accordion_panel(
        "3 \u00B7 Size (inches)", value = "size", icon = icon("ruler"),
        selectInput("size", "Standard size", size_chart$size, selected = "M"),
        layout_columns(col_widths = c(6, 6),
          numericInput("bust", "Bust", 36, step = 0.5), numericInput("waist", "Waist", 30, step = 0.5)),
        layout_columns(col_widths = c(6, 6),
          numericInput("hip", "Hip", 38, step = 0.5), numericInput("shoulder", "Shoulder", 14, step = 0.25)),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("kurti_len", "Garment length", 40, step = 0.5),
          numericInput("sleeve_len", "Full sleeve", 22, step = 0.5),
          numericInput("pal_len", "Bottom length", 38, step = 0.5))
      ),
      accordion_panel(
        "4 \u00B7 Fabric & cost", value = "fabric", icon = icon("indian-rupee-sign"),
        selectInput("fabric", "Fabric type", fabric_presets$fabric),
        layout_columns(col_widths = c(6, 6),
          selectInput("width", "Fabric width",
                      c("36 in (90 cm)" = 90, "44 in (112 cm)" = 112, "58-60 in (150 cm)" = 150), selected = 150),
          numericInput("price", "Price per metre (\u20B9)", 180, min = 1)),
        selectInput("fabric_col", "Fabric colour (for the drawing)", fabric_colours),
        checkboxInput("nap", "Nap or one-way motif", FALSE),
        sliderInput("shrink", "Shrinkage allowance (%)", 0, 10, 5),
        sliderInput("gap", "Gap between pieces (cm)", 0, 3, 1, step = 0.5),
        sliderInput("trials", "Layout attempts", 20, 300, 100, step = 20),
        input_switch("smart", "Smart shape nesting", value = shapely_ok),
        if (shapely_ok) span(class = "badge text-bg-success", "Shapely ready")
        else span(class = "badge text-bg-warning", "Shapely not found: using rectangles")
      )
    )
  ),

  div(class = "hero",
      h2("Cutting guide studio"),
      p("Upload a reference, enter the size, and get exact piece measurements with a ready-to-cut layout."),
      uiOutput("chips", inline = TRUE)),

  layout_column_wrap(
    width = "210px", fill = FALSE,
    value_box("Fabric to buy", textOutput("vb_len"), showcase = icon("ruler-horizontal"), theme = "primary"),
    value_box("Waste", textOutput("vb_waste"), showcase = icon("recycle"), theme = "success"),
    value_box("Estimated cost", textOutput("vb_cost"), showcase = icon("indian-rupee-sign"), theme = "secondary"),
    value_box("Saved vs manual layout", textOutput("vb_save"), showcase = icon("piggy-bank"), theme = "primary")
  ),

  navset_card_tab(
    title = "Your pattern",
    nav_panel("Cutting guide", icon = icon("scissors"),
      div(style = "text-align:center; overflow:auto;", plotOutput("guide", inline = TRUE)),
      layout_columns(col_widths = c(6, 6), fill = FALSE,
        downloadButton("dl_png", "PNG (share on WhatsApp)", class = "btn-primary w-100"),
        downloadButton("dl_svg", "SVG (print at 1:1)", class = "btn-primary w-100"))),
    nav_panel("Measurements", icon = icon("table"),
      p(class = "text-muted", "Exact cutting size of every piece, with seam and hem allowances included."),
      tableOutput("table")),
    nav_panel("Cost & savings", icon = icon("chart-column"),
      uiOutput("saving_text"),
      plotOutput("cmp", height = "300px")),
    nav_panel("Notes", icon = icon("circle-info"),
      tags$ul(
        tags$li("Formulas are common tailoring approximations. Cut a trial piece in cheap cloth first."),
        tags$li("The layout assumes the fabric is cut double (right sides together), so a turned piece is a valid pair."),
        tags$li("Red dashed triangles are darts. Dotted vertical lines are the selvedges."),
        tags$li("Neck and armhole curves are drawn on kurti, dress and bodice panels. Princess-seam, blouse and shirt pieces are shown as simple shapes."),
        tags$li("Photo reading sends the image to the Anthropic API. Get customer consent first.")))
  )
)

server <- function(input, output, session) {

  pending_len <- reactiveVal(NULL)   # length found in a photo, applied after a garment switch

  observeEvent(input$size, {
    r <- size_chart[size_chart$size == input$size, ]
    updateNumericInput(session, "bust", value = r$bust)
    updateNumericInput(session, "waist", value = r$waist)
    updateNumericInput(session, "hip", value = r$hip)
    updateNumericInput(session, "shoulder", value = r$shoulder)
  })

  observeEvent(input$garment, {                         # each garment has its own usual length and sleeve
    if (input$garment == "Custom piece list") return()
    len <- if (!is.null(pending_len())) pending_len() else garment_len[[input$garment]]
    pending_len(NULL)
    updateNumericInput(session, "kurti_len", value = len)
    if (input$garment == "Blouse") updateSelectInput(session, "sleeve", selected = "Short")
    if (input$garment == "Shirt") updateSelectInput(session, "sleeve", selected = "Full")
  }, ignoreInit = TRUE)

  observeEvent(input$bottom, {
    if (input$bottom != "None") updateNumericInput(session, "pal_len", value = bottoms[[input$bottom]]$len)
  }, ignoreInit = TRUE)

  observeEvent(input$fabric, {
    f <- fabric_presets[fabric_presets$fabric == input$fabric, ]
    updateNumericInput(session, "price", value = f$price_per_m)
    updateCheckboxInput(session, "nap", value = f$nap)
  })

  output$ref <- renderImage({
    req(input$img)
    list(src = input$img$datapath, contentType = input$img$type, width = "100%",
         style = "border-radius:12px; margin-bottom:10px;")
  }, deleteFile = FALSE)

  observeEvent(input$detect, {                          # fill garment and style from the photo
    req(input$img)
    res <- tryCatch(detect_style(input$img$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    if (is.null(res)) return()
    pick <- function(id, value, choices) {
      if (!is.null(value) && value %in% choices) updateSelectInput(session, id, selected = value)
    }
    garment <- if (!is.null(res$garment) && res$garment %in% garment_choices) res$garment else input$garment
    if (garment %in% c("Kurti", "Anarkali", "Dress") && !is.null(res$length) &&
        res$length %in% names(length_defaults)) {
      len <- length_defaults[[res$length]]
      if (garment != input$garment) pending_len(len) else updateNumericInput(session, "kurti_len", value = len)
    }
    pick("garment", garment, garment_choices)
    pick("fit", res$fit, names(ease_in))
    pick("flare", res$flare, names(flare_cm))
    pick("neck", res$neck, names(neck_styles))
    pick("sleeve", res$sleeve, sleeve_choices)
    pick("shaping", res$shaping, shaping_choices)
    pick("skirt_type", res$skirt_type, c("Gathered", "Circular"))
    pick("bottom", res$bottom, c("None", names(bottoms)))
    updateCheckboxInput(session, "dupatta", value = isTRUE(res$has_dupatta))
    showNotification("Garment and style filled in. Please check them against the photo.", duration = 6)
  })

  pieces <- reactive({
    if (input$garment == "Custom piece list") {
      validate(need(!is.null(input$custom), "Upload a CSV with columns name, w, h, qty, grain_locked (cm)."))
      return(normalise_custom(read.csv(input$custom$datapath, stringsAsFactors = FALSE)))
    }
    m <- list(bust = input$bust, waist = input$waist, hip = input$hip, shoulder = input$shoulder,
              kurti_len = input$kurti_len, sleeve_len = input$sleeve_len, pal_len = input$pal_len)
    st <- list(garment = input$garment, fit = input$fit, flare = input$flare, neck = input$neck,
               sleeve = input$sleeve, shaping = input$shaping, skirt_type = input$skirt_type,
               bottom = input$bottom, dupatta = input$dupatta)
    draft_garment(m, st, as.numeric(input$width))
  })

  smart <- reactive(isTRUE(input$smart) && shapely_ok)

  result <- reactive({
    tryCatch({
      pcs <- expand_pieces(pieces())
      W <- as.numeric(input$width)
      opt <- if (smart()) shapely_layout(pcs, W, input$nap, input$gap, input$trials)
             else optimise_layout(pcs, W, input$nap, input$gap, input$trials)
      base <- shelf_layout(pcs, W, input$gap)
      list(pcs = pcs, opt = opt, base = base,
           b_opt  = buy_summary(opt,  input$price, input$shrink),
           b_base = buy_summary(base, input$price, input$shrink))
    }, error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  guide_title <- reactive({
    r <- result()
    sprintf("%s cutting guide  |  width %g cm  |  cut length %.0f cm  |  buy %.2f m",
            input$garment, r$opt$W, r$opt$length_cm, r$b_opt$buy_m)
  })

  output$chips <- renderUI({
    items <- c(input$garment, paste(input$fit, "fit"), if (input$shaping != "None") input$shaping,
               if (input$bottom != "None") input$bottom, if (input$dupatta) "Dupatta",
               paste0(input$width, " cm ", tolower(input$fabric)),
               if (smart()) "Smart nesting" else "Quick nesting")
    lapply(items, function(x) span(class = "chip", x))
  })

  output$vb_len   <- renderText(sprintf("%.2f m", result()$b_opt$buy_m))
  output$vb_waste <- renderText(sprintf("%.1f%%", result()$opt$waste_pct))
  output$vb_cost  <- renderText(inr(result()$b_opt$cost))
  output$vb_save  <- renderText(inr(result()$b_base$cost - result()$b_opt$cost))

  guide_px <- reactive({                                 # about 3.4 screen pixels per cm of fabric
    r <- result()
    list(w = round(r$opt$W * 3.4 + 90), h = round(r$opt$length_cm * 3.4 + 90))
  })

  output$guide <- renderPlot(
    draw_guide(result()$opt, result()$pcs, guide_title(), input$fabric_col),
    res = 96, width = function() guide_px()$w, height = function() guide_px()$h)

  output$table <- renderTable(measurement_table(pieces()), striped = TRUE, hover = TRUE, width = "100%")

  output$saving_text <- renderUI({
    r <- result()
    s <- r$b_base$cost - r$b_opt$cost
    tagList(
      h4(sprintf("You save %s on this garment", inr(max(s, 0)))),
      p(class = "text-muted",
        sprintf("A manual left-to-right layout needs %.2f m (%s). This layout needs %.2f m (%s). Across 100 garments that is about %s.",
                r$b_base$buy_m, inr(r$b_base$cost), r$b_opt$buy_m, inr(r$b_opt$cost), inr(max(s, 0) * 100))))
  })

  output$cmp <- renderPlot({
    r <- result()
    par(mar = c(3, 7, 2, 1))
    barplot(c(r$b_base$cost, r$b_opt$cost), names.arg = c("Manual shelf", "Darzi layout"),
            col = c("#D3D1C7", "#4B3F8F"), border = NA, ylab = "", las = 1,
            main = "Fabric cost for this garment (\u20B9)", col.main = "#2b2550")
  })

  output$dl_png <- downloadHandler(
    filename = function() "cutting_guide.png",
    content = function(file) {
      r <- result(); d <- guide_dims(r$opt)
      png(file, width = d$w, height = d$h, units = "in", res = min(150, 3000 / d$h))
      on.exit(dev.off())
      draw_guide(r$opt, r$pcs, guide_title(), input$fabric_col)
    }
  )

  output$dl_svg <- downloadHandler(
    filename = function() "cutting_guide.svg",
    content = function(file) {
      r <- result(); d <- guide_dims(r$opt)
      svg(file, width = d$w, height = d$h)
      on.exit(dev.off())
      draw_guide(r$opt, r$pcs, guide_title(), input$fabric_col)
    }
  )
}

shinyApp(ui, server)
