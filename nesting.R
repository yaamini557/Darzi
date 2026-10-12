# nesting.R : core functions for the Fabric Cutting Optimizer (India edition)
# Units: centimetres. For each piece, w = across the fabric width, h = along the grain (fabric length).
# Pieces are treated as rectangles (starter version). Later you can swap in Shapely via reticulate.

# ---------- Indian currency helpers ----------

# Format a number with Indian digit grouping, e.g. 123456 -> "₹1,23,456"
inr <- function(x) {
  x <- round(x)
  s <- as.character(abs(x))
  if (nchar(s) > 3) {                                   # conditional: only group when needed
    head  <- substr(s, 1, nchar(s) - 3)
    last3 <- substr(s, nchar(s) - 2, nchar(s))
    head  <- gsub("(\\d)(?=(\\d{2})+$)", "\\1,", head, perl = TRUE)
    s <- paste0(head, ",", last3)
  }
  paste0(if (x < 0) "-" else "", "\u20B9", s)
}

# Indicative starting prices in rupees per metre. Edit these to match your local market.
fabric_presets <- data.frame(
  fabric      = c("Cotton", "Rayon", "Georgette", "Velvet", "Brocade (motifs)"),
  price_per_m = c(180, 220, 280, 550, 1200),
  nap         = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# ---------- Pieces ----------

sample_pieces <- function() {
  data.frame(
    name = c("Kurti front", "Kurti back", "Sleeve", "Palazzo leg", "Neck facing", "Waistband"),
    w    = c(54, 54, 38, 60, 30, 100),
    h    = c(110, 110, 48, 98, 14, 10),
    qty  = c(1, 1, 2, 2, 1, 1),
    grain_locked = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# Repeat each row qty times and number the copies
expand_pieces <- function(df) {
  rows <- list()
  for (i in seq_len(nrow(df))) {                        # loop over piece types
    n <- df$qty[i]
    for (k in seq_len(n)) {                             # loop over copies
      r <- df[i, setdiff(names(df), "qty"), drop = FALSE]
      if (n > 1) r$name <- paste(df$name[i], k)         # number the copies
      rows[[length(rows) + 1]] <- r
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Swap width and height for 90/270 degree turns
rotate_piece <- function(w, h, angle) {
  if (angle %in% c(90, 270)) c(w = h, h = w) else c(w = w, h = h)
}

# Rules: nap fabric or a locked grainline means no quarter turns.
# (A 180-degree turn has the same footprint for rectangles, so it is not tested.)
allowed_angles <- function(grain_locked, nap) {
  if (nap || grain_locked) 0 else c(0, 90)
}

# ---------- Placement ----------

empty_layout <- function() {
  data.frame(name = character(), x = numeric(), y = numeric(),
             w = numeric(), h = numeric(), rot = numeric(), stringsAsFactors = FALSE)
}

# Does a w x h rectangle at (x, y) stay inside the fabric and avoid every placed piece?
fits_at <- function(x, y, w, h, placed, W) {
  if (x + w > W + 1e-9) return(FALSE)                   # too wide for the fabric
  if (nrow(placed) == 0) return(TRUE)
  overlap <- x < placed$x + placed$w & x + w > placed$x &
             y < placed$y + placed$h & y + h > placed$y
  !any(overlap)
}

place_piece <- function(placed, name, x, y, w, h, rot) {
  rbind(placed, data.frame(name = name, x = x, y = y, w = w, h = h, rot = rot,
                           stringsAsFactors = FALSE))
}

# Lowest-then-leftmost corner where a piece fits, for one rotation
find_corner <- function(w, h, placed, W) {
  xs <- sort(unique(c(0, placed$x + placed$w)))
  ys <- sort(unique(c(0, placed$y + placed$h)))
  for (y in ys) {
    for (x in xs) {
      if (fits_at(x, y, w, h, placed, W)) return(c(x = x, y = y))
    }
  }
  NULL
}

# Try every allowed rotation and keep the one that ends highest on the fabric
best_spot <- function(w, h, grain_locked, placed, W, nap) {
  best <- NULL
  for (ang in allowed_angles(grain_locked, nap)) {
    d <- rotate_piece(w, h, ang)
    pos <- find_corner(unname(d["w"]), unname(d["h"]), placed, W)
    if (is.null(pos)) next
    bottom <- pos["y"] + d["h"]
    if (is.null(best) || bottom < best$bottom) {        # keep only if it beats the best so far
      best <- list(x = unname(pos["x"]), y = unname(pos["y"]),
                   w = unname(d["w"]), h = unname(d["h"]), rot = ang, bottom = unname(bottom))
    }
  }
  best
}

waste_percent <- function(used_area, W, len) 100 * (1 - used_area / (W * len))

summarise_layout <- function(placed, pcs, W, gap) {
  len <- max(placed$y + placed$h)
  # True area of each piece outline (needs guide.R); falls back to the bounding box
  used <- tryCatch(sum(vapply(seq_len(nrow(pcs)), function(i) piece_area(pcs[i, ]), numeric(1))),
                   error = function(e) sum(pcs$w * pcs$h))
  # Transform per piece: 0 degrees = shift only; 90 degrees = rotate counter-clockwise then shift
  orig_h <- ifelse(placed$rot == 90, placed$w - gap, placed$h - gap)
  turned <- placed$rot == 90
  tf <- data.frame(name = placed$name,
                   a = ifelse(turned, 0, 1), b = ifelse(turned, -1, 0),
                   c = ifelse(turned, 1, 0), d = ifelse(turned, 0, 1),
                   tx = placed$x + gap / 2 + ifelse(turned, orig_h, 0),
                   ty = placed$y + gap / 2, stringsAsFactors = FALSE)
  list(placed = placed, tf = tf, W = W, gap = gap, length_cm = len,
       waste_pct = waste_percent(used, W, len))
}

# Place pieces in the given order
nest_pieces <- function(pcs, W, nap = FALSE, gap = 1) {
  placed <- empty_layout()
  for (i in seq_len(nrow(pcs))) {
    spot <- best_spot(pcs$w[i] + gap, pcs$h[i] + gap, pcs$grain_locked[i], placed, W, nap)
    if (is.null(spot)) {
      stop(sprintf("'%s' does not fit on %g cm wide fabric", pcs$name[i], W))
    }
    placed <- place_piece(placed, pcs$name[i], spot$x, spot$y, spot$w, spot$h, spot$rot)
  }
  summarise_layout(placed, pcs, W, gap)
}

# Baseline: what most people do by hand. Pieces left to right in the listed order, new row when full.
shelf_layout <- function(pcs, W, gap = 1) {
  placed <- empty_layout()
  x <- 0; y <- 0; shelf_h <- 0
  for (i in seq_len(nrow(pcs))) {
    w <- pcs$w[i] + gap; h <- pcs$h[i] + gap; rot <- 0
    if (w > W) { tmp <- w; w <- h; h <- tmp; rot <- 90 }  # turn only if it cannot fit otherwise
    if (w > W) stop(sprintf("'%s' does not fit on %g cm wide fabric", pcs$name[i], W))
    if (x + w > W) {                                      # start a new row
      y <- y + shelf_h; x <- 0; shelf_h <- 0
    }
    placed <- place_piece(placed, pcs$name[i], x, y, w, h, rot)
    x <- x + w
    shelf_h <- max(shelf_h, h)
  }
  summarise_layout(placed, pcs, W, gap)
}

# Try many piece orders (largest first, tallest first, then random shuffles) and keep the shortest layout
optimise_layout <- function(pcs, W, nap = FALSE, gap = 1, trials = 100, seed = 1) {
  set.seed(seed)
  orders <- list(order(-pcs$w * pcs$h), order(-pcs$h), order(-pcs$w))
  for (t in seq_len(trials)) orders[[length(orders) + 1]] <- sample(nrow(pcs))
  best <- NULL
  for (o in orders) {
    cand <- nest_pieces(pcs[o, ], W, nap, gap)
    if (is.null(best) || cand$length_cm < best$length_cm) best <- cand
  }
  best
}

# ---------- Buying and cost (rupees) ----------

# Add shrinkage, then round up to the length a shop will actually cut (default 0.25 m steps)
buy_summary <- function(layout, price_per_m, shrink_pct = 5, round_to = 0.25) {
  need_m <- layout$length_cm / 100 * (1 + shrink_pct / 100)
  buy_m  <- ceiling(need_m / round_to) * round_to
  list(need_m = need_m, buy_m = buy_m, cost = buy_m * price_per_m)
}

# ---------- Drawing ----------

# 1:1 when saved to an SVG device of width W/2.54 inches and height length/2.54 + 0.5 inches
draw_layout <- function(layout, main = "") {
  p <- layout$placed; g <- layout$gap
  cols <- rep(c("#CECBF6", "#9FE1CB", "#F5C4B3", "#FAC775", "#F4C0D1", "#B5D4F4"), length.out = nrow(p))
  par(mai = c(0.25, 0, 0.25, 0))
  plot(NA, xlim = c(0, layout$W), ylim = c(layout$length_cm, 0), asp = 1,
       xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", main = main)
  rect(0, 0, layout$W, layout$length_cm, border = "grey50", lty = 2)
  rect(p$x + g / 2, p$y + g / 2, p$x + p$w - g / 2, p$y + p$h - g / 2, col = cols, border = "grey30")
  text(p$x + p$w / 2, p$y + p$h / 2, p$name, cex = 0.7)
  for (i in seq_len(nrow(p))) {                          # grainline arrow on every piece
    cx <- p$x[i] + p$w[i] / 2; cy <- p$y[i] + p$h[i] / 2
    if (p$rot[i] == 0) {
      arrows(cx, cy + 0.12 * p$h[i], cx, cy + 0.30 * p$h[i], code = 3, length = 0.04)
    } else {
      arrows(cx - 0.18 * p$w[i], cy + 0.15 * p$h[i], cx + 0.18 * p$w[i], cy + 0.15 * p$h[i], code = 3, length = 0.04)
    }
  }
  axis(1, at = c(0, layout$W), labels = c("0", paste(layout$W, "cm")))
}
