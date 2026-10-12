# drafting.R : turn body measurements + garment type + style into exact cutting dimensions
# Inputs are in INCHES (how Indian tailors work). Internally everything is cm.
# The formulas are common tailoring approximations, not a universal standard.
# Always cut a trial piece (muslin or cheap cloth) before the real fabric.
#
# To add a garment: write draft_<name>(m, st) returning bind(mk(...), mk(...)), add one line to the
# switch() in draft_garment() and one name to garment_choices.
# To add a pant type: add one entry to the `bottoms` list below.

SA  <- 1.5   # seam allowance per side (cm)
HEM <- 4     # hem allowance (cm)

in2cm <- function(x) x * 2.54

# Show cm as inches to the nearest 1/8, e.g. 54.6 -> "21 1/2"
fmt_in <- function(cm) {
  x <- round(cm / 2.54 * 8) / 8
  whole <- floor(x)
  eighths <- round((x - whole) * 8)
  frac <- c("", "1/8", "1/4", "3/8", "1/2", "5/8", "3/4", "7/8")[eighths + 1]
  trimws(paste(whole, frac))
}

# Typical BODY measurements in inches. Replace with the customer's own tape measurements.
size_chart <- data.frame(
  size     = c("S", "M", "L", "XL", "XXL"),
  bust     = c(34, 36, 38, 40, 42),
  waist    = c(28, 30, 32, 34, 36),
  hip      = c(36, 38, 40, 42, 44),
  shoulder = c(13.5, 14, 14.5, 15, 15.5),
  stringsAsFactors = FALSE
)

# ---------- Options ----------
garment_choices <- c("Kurti", "Anarkali", "Dress", "Lehenga", "Skirt", "Blouse", "Shirt", "Custom piece list")
garment_len     <- c(Kurti = 40, Anarkali = 50, Dress = 44, Lehenga = 40, Skirt = 28, Blouse = 15, Shirt = 28)  # inches
ease_in         <- c(Fitted = 2, Regular = 4, Loose = 6)                    # extra room at bust (inches)
flare_cm        <- c(Straight = 0, "A-line" = 6, Flared = 10)               # extra hem width per side (cm)
sleeve_cm       <- c(Sleeveless = 0, Short = 17, Elbow = 30, ThreeQuarter = 42)
sleeve_choices  <- c(names(sleeve_cm), "Full")
shaping_choices <- c("None", "Darts", "Princess seams")
neck_styles     <- list(Round = c(w = 17, d = 10), V = c(w = 17, d = 18), Boat = c(w = 26, d = 4))  # cm
length_defaults <- c(Hip = 32, Knee = 40, Calf = 44, Ankle = 48)           # inches, adjust to height

# Pant types.  add   = inches added to hip before dividing by 4 (negative = stretch fabric)
#              hem   = hem width of one leg panel in cm   |  flare = extra width at hem instead of a fixed hem
#              mult  = length multiplier (churidar bunches at the ankle)   |  len = usual length in inches
#              band  = needs a waistband (otherwise elastic casing)   |  pockets = side pocket bags
bottoms <- list(
  "Straight Pant"    = list(add = 3,  hem = 24, flare = NA, mult = 1.0, len = 38, band = TRUE,  pockets = TRUE),
  "Cigarette Pant"   = list(add = 2.5, hem = 17, flare = NA, mult = 1.0, len = 36, band = TRUE,  pockets = FALSE),
  "Bootcut"          = list(add = 3,  hem = 27, flare = NA, mult = 1.0, len = 40, band = TRUE,  pockets = TRUE),
  "Flared Pant"      = list(add = 3,  hem = NA, flare = 14, mult = 1.0, len = 40, band = TRUE,  pockets = TRUE),
  "Wide-leg Trouser" = list(add = 4,  hem = 34, flare = NA, mult = 1.0, len = 40, band = TRUE,  pockets = TRUE),
  "Culottes"         = list(add = 4,  hem = NA, flare = 14, mult = 1.0, len = 26, band = TRUE,  pockets = TRUE),
  "Shorts"           = list(add = 3,  hem = 28, flare = NA, mult = 1.0, len = 14, band = TRUE,  pockets = TRUE),
  "Jogger"           = list(add = 10, hem = 15, flare = NA, mult = 1.0, len = 36, band = FALSE, pockets = FALSE),
  "Leggings"         = list(add = -3, hem = 14, flare = NA, mult = 1.0, len = 38, band = FALSE, pockets = FALSE),
  "Palazzo"          = list(add = 4,  hem = NA, flare = 20, mult = 1.0, len = 38, band = FALSE, pockets = FALSE),
  "Salwar"           = list(add = 14, hem = 26, flare = NA, mult = 1.0, len = 36, band = FALSE, pockets = FALSE),
  "Churidar"         = list(add = 10, hem = 17, flare = NA, mult = 1.3, len = 38, band = FALSE, pockets = FALSE),
  "Patiala"          = list(add = 24, hem = 28, flare = NA, mult = 1.0, len = 34, band = FALSE, pockets = FALSE),
  "Dhoti Pant"       = list(add = 12, hem = 19, flare = NA, mult = 1.0, len = 36, band = FALSE, pockets = FALSE),
  "Sharara"          = list(add = 4,  hem = NA, flare = 40, mult = 1.0, len = 40, band = FALSE, pockets = FALSE)
)
bottom_groups <- list(
  None    = "None",
  Western = c("Straight Pant", "Cigarette Pant", "Bootcut", "Flared Pant", "Wide-leg Trouser", "Culottes",
              "Shorts", "Jogger", "Leggings"),
  Ethnic  = c("Palazzo", "Salwar", "Churidar", "Patiala", "Dhoti Pant", "Sharara")
)

# ---------- Building blocks ----------

extra_cols <- c("top_w", "hem_w", "chest_w", "neck_w", "neck_d", "neck_type", "arm_d", "r0", "big_r", "theta",
                "ext", "rise", "dart_w", "dart_l", "dart_y", "dart_n")

# One row of the pieces table. w = across the fabric, h = along the grain, both in cm incl. allowances.
mk <- function(name, w, h, qty, grain_locked, shape = "rect",
               top_w = NA, hem_w = NA, chest_w = NA, neck_w = NA, neck_d = NA, neck_type = NA,
               arm_d = NA, r0 = NA, big_r = NA, theta = NA, ext = NA, rise = NA,
               dart_w = NA, dart_l = NA, dart_y = NA, dart_n = NA) {
  data.frame(name = name, w = w, h = h, qty = qty, grain_locked = grain_locked, shape = shape,
             top_w = top_w, hem_w = hem_w, chest_w = chest_w, neck_w = neck_w, neck_d = neck_d,
             neck_type = neck_type, arm_d = arm_d, r0 = r0, big_r = big_r, theta = theta,
             ext = ext, rise = rise, dart_w = dart_w, dart_l = dart_l, dart_y = dart_y, dart_n = dart_n,
             stringsAsFactors = FALSE)
}

# Stack pieces, ignoring NULLs (for optional pieces)
bind <- function(...) do.call(rbind, Filter(Negate(is.null), list(...)))

body_dims <- function(m, st) {
  ease <- ease_in[[st$fit]]
  chest_w <- in2cm((m$bust + ease) / 2) + 2 * SA
  list(ease = ease, chest_w = chest_w,
       hip_w = in2cm((m$hip + ease) / 2) + 2 * SA,
       top_w = min(in2cm(m$shoulder) + 2 * SA, chest_w),
       arm_d = in2cm(m$bust / 4 + 1.5))
}

sleeve_piece <- function(m, st, arm_d) {
  if (st$sleeve == "Sleeveless") return(NULL)            # conditional: no sleeve piece
  sl <- if (st$sleeve == "Full") in2cm(m$sleeve_len) else sleeve_cm[[st$sleeve]]
  mk("Sleeve", in2cm(m$bust * 0.30 + 2) + 2 * SA, sl + 0.4 * arm_d + 3, 2, TRUE)
}

neck_facing <- function(nk) mk("Neck facing", 6, 2 * nk[["w"]] + 2 * nk[["d"]] + 6, 1, FALSE)

# Front and back body pieces with the chosen shaping: plain, darts, or princess seams
bodice_pieces <- function(label, hem_w, len, b, nk, st, m, waist_y) {
  dart_w <- min(4, max(1, in2cm((m$bust - m$waist) / 6)))     # intake per dart
  dart_l <- in2cm(5)
  shaping <- st$shaping
  panel <- function(side) {
    darts <- identical(shaping, "Darts")
    mk(paste(label, side), hem_w, len, 1, TRUE, "panel", top_w = b$top_w, chest_w = b$chest_w,
       neck_w = nk[["w"]], neck_d = if (side == "front") nk[["d"]] else 5,
       neck_type = if (side == "front") st$neck else "Round", arm_d = b$arm_d,
       dart_w = if (darts) dart_w else NA, dart_l = if (darts) dart_l else NA,
       dart_y = if (darts) waist_y else NA, dart_n = if (darts) 2 else NA)
  }
  if (identical(shaping, "Princess seams")) {
    net_top <- b$chest_w - 2 * SA
    net_hem <- hem_w - 2 * SA
    part <- function(side, kind, share, qty) {
      mk(paste(label, side, kind), share * net_hem + 2 * SA, len, qty, TRUE, "trapezoid",
         top_w = share * net_top + 2 * SA, hem_w = share * net_hem + 2 * SA)
    }
    return(bind(part("front", "centre", 0.5, 1), part("front", "side", 0.25, 2),
                part("back", "centre", 0.5, 1), part("back", "side", 0.25, 2)))
  }
  bind(panel("front"), panel("back"))
}

# ---------- Garments ----------

draft_kurti <- function(m, st, label = "Kurti") {        # also used for straight/A-line dresses
  b <- body_dims(m, st)
  nk <- neck_styles[[st$neck]]
  hem_w <- max(b$chest_w, b$hip_w) + 2 * flare_cm[[st$flare]]
  len <- in2cm(m$kurti_len) + HEM + SA
  bind(bodice_pieces(label, hem_w, len, b, nk, st, m, waist_y = in2cm(16)),
       sleeve_piece(m, st, b$arm_d), neck_facing(nk))
}

# Fitted bodice (shoulder to waist) + circular-sector skirt panels
draft_anarkali <- function(m, st) {
  b <- body_dims(m, st)
  nk <- neck_styles[[st$neck]]
  bodice_len <- in2cm(16) + SA
  skirt_len <- in2cm(m$kurti_len) - in2cm(16) + HEM
  n <- 6
  sweep <- c(Straight = 120, "A-line" = 240, Flared = 360)[[st$flare]]   # degrees of circle used
  theta <- sweep / n
  r0 <- in2cm(m$waist + 3) / (sweep * pi / 180)          # inner radius so the waist edge fits the waist
  big_r <- r0 + skirt_len
  half <- theta / 2 * pi / 180
  bind(
    bodice_pieces("Bodice", b$chest_w, bodice_len, b, nk, st, m, waist_y = bodice_len),
    mk("Skirt panel", 2 * big_r * sin(half), big_r - r0 * cos(half), n, TRUE, "sector",
       r0 = r0, big_r = big_r, theta = theta),
    sleeve_piece(m, st, b$arm_d), neck_facing(nk)
  )
}

# Skirt and lehenga: gathered rectangles (width set by fabric width) or circular panels
draft_skirt <- function(m, st, W, fullness) {
  len <- in2cm(m$kurti_len) + HEM + 4                    # +4 cm for the waist seam
  waist <- in2cm(m$waist + 2)
  band <- mk("Waistband", waist + 10, 8, 1, FALSE)
  if (identical(st$skirt_type, "Circular")) {
    r0 <- waist / (2 * pi)
    big_r <- r0 + len
    return(bind(mk("Skirt panel", 2 * big_r * sin(pi / 4), big_r - r0 * cos(pi / 4), 4, TRUE, "sector",
                   r0 = r0, big_r = big_r, theta = 90), band))
  }
  sweep <- in2cm(m$hip + 2) * fullness                   # total gathered hem width
  n <- ceiling(sweep / (W - 2 * SA - 3.5))               # panels needed so each fits the fabric width
  bind(mk("Skirt panel", sweep / n + 2 * SA, len, n, TRUE), band)
}

draft_blouse <- function(m, st) {
  qw <- in2cm((m$bust + 3) / 4) + 2 * SA                 # quarter of bust + ease
  len <- in2cm(m$kurti_len) + SA + 2
  dart_w <- min(4, max(1, in2cm((m$bust - m$waist) / 6)))
  darts <- identical(st$shaping, "Darts")
  front <- if (identical(st$shaping, "Princess seams")) {
    net <- 2 * (qw - 2 * SA)                              # whole front width without seams
    bind(mk("Blouse front centre", 0.5 * net + 2 * SA, len, 1, TRUE),
         mk("Blouse front side", 0.25 * net + 2 * SA, len, 2, TRUE))
  } else {
    mk("Blouse front", qw, len, 2, TRUE,
       dart_w = if (darts) dart_w else NA, dart_l = if (darts) in2cm(6) else NA,
       dart_y = if (darts) len else NA, dart_n = if (darts) 1 else NA)
  }
  back <- mk("Blouse back", qw, len, 2, TRUE,
             dart_w = if (darts) dart_w else NA, dart_l = if (darts) in2cm(7) else NA,
             dart_y = if (darts) len else NA, dart_n = if (darts) 1 else NA)
  bind(front, back, sleeve_piece(m, st, in2cm(m$bust / 4 + 1.5)), neck_facing(neck_styles[[st$neck]]))
}

draft_shirt <- function(m, st) {
  b <- body_dims(m, st)
  len <- in2cm(m$kurti_len) + HEM + SA
  bind(
    mk("Shirt front", in2cm((m$bust + b$ease) / 4 + 1.5) + 2 * SA, len, 2, TRUE),   # +1.5 in placket
    mk("Shirt back", b$chest_w, len, 1, TRUE),
    sleeve_piece(m, st, b$arm_d),
    mk("Collar", 8, 42, 2, FALSE),
    if (st$sleeve %in% c("ThreeQuarter", "Full")) mk("Cuff", 8, 28, 2, FALSE)        # conditional piece
  )
}

# Any pant type: front and back leg panels with a crotch extension, plus waistband and pockets if needed
draft_bottom <- function(m, st) {
  sp <- bottoms[[st$bottom]]
  top <- in2cm((m$hip + sp$add) / 4) + 2 * SA
  hem <- if (!is.na(sp$flare)) top + sp$flare else sp$hem + 2 * SA
  len <- in2cm(m$pal_len) * sp$mult + HEM + (if (sp$band) 2 else 4)
  rise <- min(in2cm(m$hip / 4 + 1), 0.6 * len)           # crotch depth from the waist
  ext_f <- in2cm(m$hip / 16)                             # crotch extension, front
  ext_b <- in2cm(m$hip / 12)                             # crotch extension, back (deeper curve)
  leg <- function(side, ext) {
    mk(paste(st$bottom, side), max(top + ext, hem), len, 2, TRUE, "pant",
       top_w = top, hem_w = hem, ext = ext, rise = rise)
  }
  bind(
    leg("front", ext_f), leg("back", ext_b),
    if (sp$band) mk("Waistband", in2cm(m$waist + 2) + 8, 7, 1, FALSE),               # conditional pieces
    if (sp$pockets) mk("Pocket bag", 20, 28, 2, FALSE)
  )
}

# m: list(bust, waist, hip, shoulder, kurti_len, sleeve_len, pal_len) in inches
# st: list(garment, fit, flare, neck, sleeve, shaping, skirt_type, bottom, dupatta)
draft_garment <- function(m, st, W = 150) {
  if (is.null(st$shaping)) st$shaping <- "None"
  main <- switch(st$garment,
    Kurti    = draft_kurti(m, st, "Kurti"),
    Dress    = draft_kurti(m, st, "Dress"),
    Anarkali = draft_anarkali(m, st),
    Blouse   = draft_blouse(m, st),
    Shirt    = draft_shirt(m, st),
    Skirt    = draft_skirt(m, st, W, fullness = 2.5),
    Lehenga  = draft_skirt(m, st, W, fullness = 4),
    stop("Unknown garment: ", st$garment))
  bind(
    main,
    if (!is.null(st$bottom) && st$bottom != "None") draft_bottom(m, st),
    if (isTRUE(st$dupatta)) mk("Dupatta", 88, 225, 1, TRUE)
  )
}

# For garments not covered above: upload your own pieces (cm). Columns: name, w, h, qty, grain_locked
normalise_custom <- function(df) {
  if (!"qty" %in% names(df)) df$qty <- 1
  if (!"grain_locked" %in% names(df)) df$grain_locked <- TRUE
  df$shape <- "rect"
  for (col in extra_cols) df[[col]] <- NA
  df
}

# Table for the screen: exact cutting size of each piece in cm and inches
measurement_table <- function(df) {
  data.frame(
    Piece = df$name, Qty = df$qty,
    `Width (cm)` = round(df$w, 1), `Length (cm)` = round(df$h, 1),
    `Width (in)` = fmt_in(df$w), `Length (in)` = fmt_in(df$h),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
