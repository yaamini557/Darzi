# guide.R : piece outlines and the cutting guide drawing
# Every piece has a local outline (x across, y along the grain, starting at 0,0). A layout stores, per piece,
# a 2D transform  x' = a*x + b*y + tx,  y' = c*x + d*y + ty  that puts the piece on the fabric.

# ---------- Outlines ----------

# Body panel: shoulder, scooped neck, curved armhole, side seam flaring to the hem
panel_outline <- function(bw, bh, top_w, chest_w, neck_w, neck_d, neck_type, arm_d) {
  cx <- bw / 2
  th <- seq(pi, 0, length.out = 25)
  neck <- if (neck_type == "V") {
    data.frame(x = c(cx - neck_w / 2, cx, cx + neck_w / 2), y = c(0, neck_d, 0))
  } else {
    data.frame(x = cx + neck_w / 2 * cos(th), y = neck_d * sin(th))
  }
  x0 <- cx + top_w / 2
  x1 <- cx + chest_w / 2
  t <- seq(0, 1, length.out = 15)
  arm <- data.frame(x = (1 - t)^2 * x0 + 2 * (1 - t) * t * x0 + t^2 * x1,
                    y = 2 * (1 - t) * t * (0.6 * arm_d) + t^2 * arm_d)
  right <- rbind(data.frame(x = x0, y = 0), arm, data.frame(x = bw, y = bh))
  left  <- data.frame(x = 2 * cx - right$x, y = right$y)
  rbind(neck, right, left[nrow(left):1, ])
}

trapezoid_outline <- function(bw, bh, top_w, hem_w) {
  data.frame(x = c((bw - top_w) / 2, (bw + top_w) / 2, (bw + hem_w) / 2, (bw - hem_w) / 2),
             y = c(0, 0, bh, bh))
}

# Slice of a circle (anarkali panel, circular skirt): waist arc at r0, hem arc at big_r
sector_outline <- function(r0, big_r, theta_deg) {
  th <- theta_deg * pi / 180
  a_out <- seq(-th / 2, th / 2, length.out = 30)
  a_in <- rev(a_out)
  x <- c(big_r * sin(a_out), r0 * sin(a_in))
  y <- c(big_r * cos(a_out), r0 * cos(a_in))
  bw <- 2 * big_r * sin(th / 2)
  data.frame(x = x + bw / 2, y = y - r0 * cos(th / 2))
}

# Pant leg: straight side seam (left), crotch extension on the inner seam (right), leg tapering or flaring to the hem
pant_outline <- function(top_w, ext, hem_w, rise, h) {
  xc <- (top_w + ext) / 2
  xs <- c(0, top_w, top_w + ext, xc + hem_w / 2, xc - hem_w / 2)
  ys <- c(0, 0, rise, h, h)
  data.frame(x = xs - min(0, xc - hem_w / 2), y = ys)
}

piece_polygon <- function(row) {
  shape <- if ("shape" %in% names(row) && !is.na(row$shape)) row$shape else "rect"
  switch(shape,
    panel     = panel_outline(row$w, row$h, row$top_w, row$chest_w, row$neck_w, row$neck_d, row$neck_type, row$arm_d),
    trapezoid = trapezoid_outline(row$w, row$h, row$top_w, row$hem_w),
    sector    = sector_outline(row$r0, row$big_r, row$theta),
    pant      = pant_outline(row$top_w, row$ext, row$hem_w, row$rise, row$h),
    data.frame(x = c(0, row$w, row$w, 0), y = c(0, 0, row$h, row$h)))
}

poly_area <- function(p) {
  n <- nrow(p)
  j <- c(2:n, 1)
  abs(sum(p$x * p$y[j] - p$x[j] * p$y)) / 2
}
piece_area <- function(row) poly_area(piece_polygon(row))

# Dart triangles (base on the line dart_y, tip pointing up toward the bust)
dart_tris <- function(row) {
  if (!"dart_w" %in% names(row) || is.na(row$dart_w)) return(list())
  xs <- if (row$dart_n == 2) row$w / 2 + c(-0.22, 0.22) * row$w else 0.6 * row$w
  lapply(xs, function(x) {
    data.frame(x = c(x - row$dart_w / 2, x + row$dart_w / 2, x),
               y = c(row$dart_y, row$dart_y, row$dart_y - row$dart_l))
  })
}

apply_tf <- function(p, t) {
  data.frame(x = t$a * p$x + t$b * p$y + t$tx, y = t$c * p$x + t$d * p$y + t$ty)
}

# Size in inches of the device that gives a true 1:1 print of the layout
guide_dims <- function(layout) {
  list(w = layout$W / 2.54 + 0.5, h = layout$length_cm / 2.54 + 0.75)
}

# ---------- Drawing ----------

piece_palette <- c("#CFC8F2", "#A8E0CF", "#F7C6B6", "#FBD28A", "#F5BED0", "#B9D6F6", "#D6E6A8", "#E5C7F0")

draw_guide <- function(layout, pcs, main = "", fabric_col = "#F3E6C8") {
  tf <- layout$tf
  fam <- sub(" [0-9]+$", "", tf$name)
  cols <- piece_palette[(match(fam, unique(fam)) - 1) %% length(piece_palette) + 1]

  par(mai = c(0.3, 0.45, 0.5, 0.05), family = "sans")
  plot(NA, xlim = c(0, layout$W), ylim = c(layout$length_cm, 0), asp = 1,
       xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
  title(main, adj = 0, font.main = 2, cex.main = 0.95, col.main = "#2b2550")
  rect(0, 0, layout$W, layout$length_cm, col = fabric_col, border = "#8a7f6a", lwd = 1.2)
  abline(v = c(1.5, layout$W - 1.5), lty = 3, col = "#b8ad94")          # selvedges

  for (i in seq_len(nrow(tf))) {                                        # loop over every placed piece
    t <- tf[i, ]
    row <- pcs[pcs$name == t$name, ][1, ]
    loc <- piece_polygon(row)
    p <- apply_tf(loc, t)
    polygon(p$x, p$y, col = adjustcolor(cols[i], 0.92), border = "#3d3560", lwd = 1.3)

    for (d in dart_tris(row)) {                                         # conditional: only shaped pieces have darts
      dp <- apply_tf(d, t)
      polygon(dp$x, dp$y, col = adjustcolor("#E24B4A", 0.35), border = "#B3261E", lty = 2)
    }

    ctr <- apply_tf(data.frame(x = row$w / 2, y = row$h * 0.42), t)
    lab <- paste0(sub(" (front|back) ([0-9]+)$", " \\1", row$name), "\n", round(row$w, 1), " x ", round(row$h, 1), " cm\n",
                  fmt_in(row$w), " x ", fmt_in(row$h), " in")
    fit <- 0.9 * min(row$w, row$h) / strwidth(lab, cex = 1)     # shrink text until it fits inside the piece
    text(ctr$x, ctr$y, lab, cex = min(0.75, max(0.3, fit)), col = "#1f1a38")

    g <- apply_tf(data.frame(x = rep(row$w / 2, 2), y = row$h * c(0.62, 0.84)), t)   # grainline arrow
    arrows(g$x[1], g$y[1], g$x[2], g$y[2], code = 3, length = 0.04, col = "#3d3560")
  }

  abline(h = layout$length_cm, col = "#c0392b", lty = 2)
  text(layout$W - 2, layout$length_cm - 4, sprintf("end of cut: %.0f cm", layout$length_cm),
       adj = c(1, 1), cex = 0.7, col = "#c0392b")
  axis(1, at = c(0, layout$W), labels = c("0", paste(layout$W, "cm")), cex.axis = 0.7, col = "#8a7f6a")
  axis(2, at = seq(0, layout$length_cm, by = 50), las = 1, cex.axis = 0.7, col = "#8a7f6a")
}
