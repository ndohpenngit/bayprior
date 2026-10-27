## Regenerate the curves and numbers in the welcome-page animation
## (inst/app/www/bp-live.html and bp-live.css) from bayprior's own functions.
##
## Run from the package root after devtools::load_all():
##   devtools::load_all(); source("dev/make_bp_live.R")
##
## Only the curve paths and the labels that quote a number are rewritten; the
## layout, colours and animation timing are left alone. The values that are
## printed at the end are the ones now shown on the page.
##
## Model behind each panel
##  1. Roulette chips: not rewritten here (a Beta(2.7, 14.2) fitted to 20 chips
##     with mean 16% and SD 8.7 points; checked separately).
##  2. Pooling: Expert A = Beta(3, 27) (mean 10%), Expert B = Beta(6, 14)
##     (mean 30%), equal weights. Linear pool = equal mixture. Logarithmic pool =
##     the normalised geometric mean of the two densities. "Agreement" is the
##     Bhattacharyya coefficient used by the Pooling tab (.bc_coef()).
##  3. Robust prior: the informative Beta from panel 1 plus the vague Beta(1, 1)
##     component that robust_prior() builds for a Beta prior, weights 80/20.
##     Data: 8 responders in 10. Posterior = prior x binomial likelihood,
##     evaluated numerically on a fine grid (an independent check of the exact
##     conjugate mixture update).

html_file <- "inst/app/www/bp-live.html"
css_file  <- "inst/app/www/bp-live.css"
stopifnot(file.exists(html_file), file.exists(css_file))

read_utf8 <- function(f) {
  x <- readChar(f, file.size(f), useBytes = TRUE)
  Encoding(x) <- "UTF-8"
  x
}
write_utf8 <- function(x, f) writeBin(charToRaw(x), f)   # bytes as they are: keeps UTF-8 and line endings
html <- read_utf8(html_file)
css  <- read_utf8(css_file)

# -- helpers ---------------------------------------------------------------------
fmt1 <- function(v) formatC(v, format = "f", digits = 1)

replace_path <- function(html, cls, d_new) {
  pat <- paste0('(<path class="[^"]*\\b', cls, '\\b[^"]*"[^>]*\\sd=")[^"]*(")')
  m <- regexpr(pat, html, perl = TRUE, useBytes = TRUE)
  if (m < 0) stop("path not found: ", cls)
  hit <- regmatches(html, m)
  new <- sub(pat, paste0("\\1", gsub("\\", "\\\\", d_new, fixed = TRUE), "\\2"),
             hit, perl = TRUE)
  regmatches(html, m) <- new
  html
}

# Replace the one match of a regular expression (so the script can be run again
# on a file it has already rewritten). `new` may use \\1, \\2 for groups.
replace_once <- function(txt, pattern, new) {
  n <- lengths(regmatches(txt, gregexpr(pattern, txt, perl = TRUE, useBytes = TRUE)))
  if (n != 1L) stop("expected exactly one match for: ", pattern, " (found ", n, ")")
  sub(pattern, new, txt, perl = TRUE, useBytes = TRUE)
}

path_xy <- function(xs, ys) paste0("L", fmt1(xs), " ", fmt1(ys), collapse = " ")
area_d  <- function(xs, ys) paste0("M30 140 ", path_xy(xs, ys), " L530 140 Z")
line_d  <- function(xs, ys) paste0("M30 140 ", path_xy(xs, ys))

xs <- seq(30, 530, by = 2.5)          # 200 steps across the drawing

# =================================================================================
# Panel 2: pooling   (axis 0-60%; common density scale, Expert A peak = 103 px)
# =================================================================================
A <- elicit_beta(mean = 3 / 30, sd = sqrt((3 * 27) / (30^2 * 31)), method = "moments")
B <- elicit_beta(mean = 6 / 20, sd = sqrt((6 * 14) / (20^2 * 21)), method = "moments")
stopifnot(isTRUE(all.equal(unname(unlist(A$params)), c(3, 27), tolerance = 1e-6)),
          isTRUE(all.equal(unname(unlist(B$params)), c(6, 14), tolerance = 1e-6)))

fine <- seq(1e-6, 1 - 1e-6, length.out = 20001); dfine <- fine[2] - fine[1]
fA <- function(t) stats::dbeta(t, A$params$alpha, A$params$beta)
fB <- function(t) stats::dbeta(t, B$params$alpha, B$params$beta)
fLin <- function(t) 0.5 * fA(t) + 0.5 * fB(t)
g_raw <- function(t) sqrt(fA(t) * fB(t))
g_norm <- sum(g_raw(fine)) * dfine
fLog <- function(t) g_raw(t) / g_norm

mean_of <- function(f) sum(fine * f(fine)) * dfine
mean_A <- mean_of(fA); mean_B <- mean_of(fB)
mean_lin <- mean_of(fLin); mean_log <- mean_of(fLog)
bc <- .bc_coef(A, B)

t2 <- (xs - 30) / 500 * 0.6
scale2 <- 103 / max(fA(fine))
h2 <- function(f) pmax(0, 140 - scale2 * f(pmin(pmax(t2, 1e-9), 1 - 1e-9)))
html <- replace_path(html, "bpl-sa", area_d(xs, h2(fA)))
html <- replace_path(html, "bpl-sb", area_d(xs, h2(fB)))
html <- replace_path(html, "bpl-sl", area_d(xs, h2(fLin)))
html <- replace_path(html, "bpl-sg", area_d(xs, h2(fLog)))

pct <- function(v) sprintf("%.0f%%", 100 * v)
html <- replace_once(html, "Expert A · mean [0-9]+%", paste0("Expert A · mean ", pct(mean_A)))
html <- replace_once(html, "Expert B · mean [0-9]+%", paste0("Expert B · mean ", pct(mean_B)))
html <- replace_once(html, "Linear pool · mean [0-9]+%", paste0("Linear pool · mean ", pct(mean_lin)))
html <- replace_once(html, "Logarithmic pool · mean [0-9]+%", paste0("Logarithmic pool · mean ", pct(mean_log)))
html <- replace_once(html, "(Overlap|Agreement) [0-9.]+ · they disagree",
                     sprintf("Agreement %.2f · they disagree", bc))

# =================================================================================
# Panel 3: robust prior   (axis 0-100%)
# =================================================================================
a0 <- 2.7; b0 <- 14.2                      # the prior fitted in panel 1
inf  <- elicit_beta(mean = a0 / (a0 + b0),
                    sd   = sqrt(a0 * b0 / ((a0 + b0)^2 * (a0 + b0 + 1))),
                    method = "moments")
rb   <- suppressWarnings(robust_prior(inf, vague_weight = 0.20))
v_a  <- rb$components$vague$params$alpha
v_b  <- rb$components$vague$params$beta
w_inf <- 0.80
x_obs <- 8; n_obs <- 10

th   <- seq(1e-6, 1 - 1e-6, length.out = 20001); dth <- th[2] - th[1]
f_inf  <- function(t) stats::dbeta(t, inf$params$alpha, inf$params$beta)
f_vag  <- function(t) stats::dbeta(t, v_a, v_b)
lik    <- function(t) stats::dbinom(x_obs, n_obs, t)

prior_mix <- function(t) w_inf * f_inf(t) + (1 - w_inf) * f_vag(t)
m_inf <- sum(f_inf(th) * lik(th)) * dth
m_vag <- sum(f_vag(th) * lik(th)) * dth
w_post <- w_inf * m_inf / (w_inf * m_inf + (1 - w_inf) * m_vag)

post_std <- function(t) stats::dbeta(t, inf$params$alpha + x_obs, inf$params$beta + n_obs - x_obs)
post_rob_raw <- function(t) prior_mix(t) * lik(t)
rob_norm <- sum(post_rob_raw(th)) * dth
post_rob <- function(t) post_rob_raw(t) / rob_norm

mean_std <- mean_of(post_std)
mean_rob <- sum(th * post_rob(th)) * dth

t3 <- (xs - 30) / 500
tt <- pmin(pmax(t3, 1e-9), 1 - 1e-9)
scale_prior <- 102.7 / max(prior_mix(th))
scale_post  <- 105.0 / max(post_std(th))     # both posteriors share one scale
html <- replace_path(html, "bpl-rp", area_d(xs, pmax(0, 140 - scale_prior * prior_mix(tt))))
html <- replace_path(html, "bpl-rc", line_d(xs, pmax(0, 140 - scale_post * post_std(tt))))
html <- replace_path(html, "bpl-rr", area_d(xs, pmax(0, 140 - scale_post * post_rob(tt))))

html <- replace_once(html, "Standard: (estimate|mean) [0-9]+%", paste0("Standard: mean ", pct(mean_std)))
html <- replace_once(html, "Robust: (estimate|mean) [0-9]+%", paste0("Robust: mean ", pct(mean_rob)))
w_lab <- if (w_post < 0.01) "&lt;1%" else sprintf("%.0f%%", 100 * w_post)
html <- replace_once(html, '(<span class="bpl-c2" style="grid-area:1/1">)[^<]*(</span>)',
                     paste0("\\1", w_lab, "\\2"))
html <- replace_once(html, "an informative part (near 10|centred near [0-9]+) percent with a wide vague part",
                     sprintf("an informative part centred near %.0f percent with a wide vague part", 100 * inf$fit_summary$mean))
css <- replace_once(css, "scaleX\\((\\.005|0\\.[0-9]{3})\\)", sprintf("scaleX(%.3f)", max(w_post, 0.005)))

write_utf8(html, html_file)
write_utf8(css,  css_file)

cat("Panel 2: means A/B/linear/log =", round(100 * c(mean_A, mean_B, mean_lin, mean_log), 1),
    "%; Bhattacharyya agreement =", round(bc, 3), "\n")
cat("Panel 3: informative Beta(", round(inf$params$alpha, 2), ",", round(inf$params$beta, 2),
    "), vague Beta(", v_a, ",", v_b, ")\n", sep = "")
cat("         standard posterior mean =", round(100 * mean_std, 1), "%; robust posterior mean =",
    round(100 * mean_rob, 1), "%; posterior weight on informative part =",
    round(100 * w_post, 2), "%\n")
