# shapely_nest.R : call nest_shapely.py from R (smart shape nesting)
# One-time setup in R:
#   install.packages("reticulate")
#   reticulate::py_install(c("shapely", "numpy"))

.nest_env <- new.env()

shapely_available <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(tryCatch(reticulate::py_module_available("shapely") &&
                      reticulate::py_module_available("numpy"), error = function(e) FALSE))
}

load_nester <- function() {
  if (is.null(.nest_env$nest_polygons)) {
    reticulate::source_python("nest_shapely.py", envir = .nest_env)
  }
}

# pcs: expanded pieces (one row per physical piece). Returns the same layout structure as the
# rectangle engine, so the drawing and costing code does not care which engine produced it.
shapely_layout <- function(pcs, W, nap = FALSE, gap = 1, trials = 100, seed = 1) {
  load_nester()
  polys <- lapply(seq_len(nrow(pcs)), function(i) {
    p <- piece_polygon(pcs[i, ])
    matrix(c(p$x, p$y), ncol = 2)
  })
  r <- .nest_env$nest_polygons(polys, as.list(pcs$name), as.list(as.logical(pcs$grain_locked)),
                               isTRUE(nap), as.numeric(W), as.numeric(gap),
                               as.integer(trials), as.integer(seed))
  num <- function(v) as.numeric(unlist(v))
  tf <- data.frame(name = as.character(unlist(r$names)),
                   a = num(r$a), b = num(r$b), c = num(r$c), d = num(r$d),
                   tx = num(r$tx), ty = num(r$ty), stringsAsFactors = FALSE)
  len <- as.numeric(r$length)
  list(tf = tf, W = W, gap = gap, length_cm = len,
       waste_pct = waste_percent(as.numeric(r$area), W, len))
}
