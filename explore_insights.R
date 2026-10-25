# ══════════════════════════════════════════════════════════════════
# EXPLORE-PAGE INSIGHTS for the quarterly report
# ══════════════════════════════════════════════════════════════════
# Charts and one-paragraph readings taken from the Explore page of
# https://kenyadebtcounter.vercel.app, using the same public API the page
# uses (seven small requests per render):
#
#   /api/debt/external-currency    External debt by currency     (OCOB, via the IEA sheet)
#   /api/debt/domestic-instrument  Domestic debt by instrument   (OCOB, via the IEA sheet)
#   /api/debt/service              Debt service by fiscal year   (OCOB, via the IEA sheet)
#   /api/debt/thresholds           Indicators vs thresholds      (CBK, OCOB, KNBS, Treasury PV)
#   /api/debt/deficit              12-month budget deficit       (CBK)
#   /api/debt/tax-revenue          Tax revenue and grants        (CBK)
#   /api/debt/spending             Spending by type              (CBK)
#
# Each fetch is independent: if one endpoint fails, only its figure is left
# out. Used by _explore_insights.qmd; needs kdc_api(), KDC_API_BASE and the
# iea_* colours / helpers defined in the main report.

ei_cache <- new.env()
ei_get <- function(path) {
  if (exists(path, envir = ei_cache, inherits = FALSE)) return(get(path, envir = ei_cache))
  d <- tryCatch({
    p <- kdc_api(paste0(KDC_API_BASE, path))
    if (!isTRUE(p$success) || is.null(p$data)) stop("no data")
    p$data
  }, error = function(e) {
    message("Explore insight ", path, " unavailable: ", conditionMessage(e))
    NULL
  })
  assign(path, d, envir = ei_cache)
  d
}
# TRUE when at least one of these endpoints returned data (fetches them once).
ei_has <- function(...) any(vapply(c(...), function(p) !is.null(ei_get(p)), logical(1)))

# ── formatting (all output is raw LaTeX, so % is escaped) ───────────────────
ei_pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f\\%%"), x)
ei_trn  <- function(ksh) sprintf("KSh %.2f trillion", ksh / 1e12)
ei_n    <- function(x) formatC(round(x), format = "d", big.mark = ",")
ei_bn   <- function(ksh) sprintf("KSh %s billion", ei_n(ksh / 1e9))
ei_amt  <- function(ksh) if (abs(ksh) >= 1e12) ei_trn(ksh) else ei_bn(ksh)
ei_fy   <- function(s) sub("^FY", "FY ", s)
ei_date <- function(iso) {
  d <- as.Date(substr(iso, 1, 10))
  sprintf("%d %s %s", as.integer(format(d, "%d")), month.name[as.integer(format(d, "%m"))], format(d, "%Y"))
}
ei_month <- function(ym) {
  sprintf("%s %s", month.name[as.integer(substr(ym, 6, 7))], substr(ym, 1, 4))
}

# Figure heading + chart + source + reading, in the report's existing style.
ei_fig_n <- 4L
ei_heading <- function(title) {
  ei_fig_n <<- ei_fig_n + 1L
  cat(sprintf("\n### Figure %d. %s\n\n", ei_fig_n, title))
}
ei_source <- function(text) {
  cat(sprintf("\n\\vspace{-0.5em}\n\\begin{flushleft}\n{\\scriptsize\\color{gray} Source: %s.}\n\\end{flushleft}\n\\vspace{-0.3em}\n",
              iea_tex_escape(text)))
}
ei_text <- function(...) cat(paste0(..., "\n\n"))
ei_plot <- function(p) print(p)

ei_theme <- function() {
  theme_minimal(base_size = 9) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text = element_text(colour = "gray25"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(0.35, "cm"),
          legend.text = element_text(size = 7.5),
          plot.margin = margin(4, 8, 2, 4))
}

# The site's chart colours (kenyadebtcounter.vercel.app/explore), so a
# figure here looks like the chart it comes from.
EI_SITE <- list(
  currency = "#eb6834", instrument = "#2a78d6",
  service = c("External principal" = "#eb6834", "External interest" = "#f5b08f",
              "Domestic principal" = "#2a78d6", "Domestic interest" = "#96bcea"),
  deficit = "#c2410c",
  tax = c("#2a78d6", "#1baf7a", "#eb6834", "#8b5cf6", "#94a3b8", "#db2777"),
  spend = c("#b91c1c", "#f87171", "#2a78d6", "#8b5cf6", "#94a3b8", "#eb6834", "#1baf7a")
)

# Shared palette for stacked categories (IEA greens and golds, then neutrals).
ei_pal <- c(iea_green, iea_gold, iea_blue, iea_orange, "#6D4C41", iea_light, iea_gray, "#9E9E9E")

# ══════════════════════════════════════════════════════════════════
# Breakdowns (external by currency, domestic by instrument)
# ══════════════════════════════════════════════════════════════════
# The API returns every snapshot; the latest is the last one.
ei_breakdown <- function(d) {
  if (is.null(d) || is.null(d$snapshots) || !length(d$snapshots)) return(NULL)
  snaps <- d$snapshots
  if (is.data.frame(snaps)) snaps <- lapply(seq_len(nrow(snaps)), function(i) list(asAt = snaps$asAt[i], total = snaps$total[i], rows = snaps$rows[[i]]))
  last <- snaps[[length(snaps)]]
  prev <- if (length(snaps) > 1) snaps[[length(snaps) - 1]] else NULL
  rows <- as.data.frame(last$rows)
  if (!nrow(rows)) return(NULL)
  rows$share <- rows$amount / sum(rows$amount) * 100
  list(asAt = last$asAt, total = sum(rows$amount), rows = rows[order(-rows$amount), ],
       prev = prev, source = d$source %||% "")
}
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

ei_bar_share <- function(rows, fill, top = 6) {
  r <- rows[, c("label", "amount", "share")]
  # Fold any "Other..." rows, and everything past the top few, into one "Others" bar.
  is_other <- grepl("^other", r$label, ignore.case = TRUE)
  if (any(is_other)) {
    r <- rbind(r[!is_other, ], data.frame(label = "Others", amount = sum(r$amount[is_other]), share = sum(r$share[is_other])))
  }
  if (nrow(r) > top) {
    other <- data.frame(label = "Others", amount = sum(r$amount[(top):nrow(r)]), share = sum(r$share[(top):nrow(r)]))
    r <- rbind(r[seq_len(top - 1), ], other)
  }
  r$label <- factor(r$label, levels = rev(r$label))
  ggplot(r, aes(x = share, y = label)) +
    geom_col(fill = fill, width = 0.62) +
    geom_text(aes(label = sprintf("%.1f%%  (KSh %s bn)", share, ei_n(amount / 1e9))),
              hjust = -0.05, size = 2.7, colour = "gray20") +
    scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.35))) +
    labs(x = NULL, y = NULL) +
    ei_theme() +
    theme(panel.grid.major.x = element_line(colour = "gray92"))
}

ei_render_currency <- function() {
  b <- ei_breakdown(ei_get("/api/debt/external-currency"))
  if (is.null(b)) return(invisible(FALSE))
  ei_heading(sprintf("External Debt by Currency, %s", ei_date(b$asAt)))
  ei_plot(ei_bar_share(b$rows, EI_SITE$currency))
  ei_source("National Treasury, via the Controller of Budget's full-year Budget Implementation Review Report")
  top <- b$rows[1, ]
  second <- if (nrow(b$rows) > 1) b$rows[2, ] else NULL
  ei_text(sprintf(
    "At %s, external public debt stood at \\textbf{%s}. \\textbf{%s} accounts for \\textbf{%s}%s. Because this debt is owed in foreign currency, every 1%% fall in the shilling adds about \\textbf{%s} to it in shillings, without any new borrowing.",
    ei_date(b$asAt), ei_amt(b$total), iea_tex_escape(top$label), ei_pct(top$share),
    if (!is.null(second)) sprintf(", followed by %s at %s", iea_tex_escape(second$label), ei_pct(second$share)) else "",
    ei_bn(b$total * 0.01)
  ))
  invisible(TRUE)
}

ei_render_domestic <- function() {
  b <- ei_breakdown(ei_get("/api/debt/domestic-instrument"))
  if (is.null(b)) return(invisible(FALSE))
  ei_heading(sprintf("Domestic Debt by Instrument, %s", ei_date(b$asAt)))
  ei_plot(ei_bar_share(b$rows, EI_SITE$instrument))
  ei_source("National Treasury, via the Controller of Budget's Budget Implementation Review Reports")
  bonds <- b$rows[grepl("bond", b$rows$label, ignore.case = TRUE), ]
  bills <- b$rows[grepl("bill", b$rows$label, ignore.case = TRUE), ]
  change <- ""
  if (!is.null(b$prev)) {
    d <- b$total - b$prev$total
    change <- sprintf(" That is %s %s than at %s.", ei_amt(abs(d)), if (d >= 0) "more" else "less", ei_date(b$prev$asAt))
  }
  ei_text(sprintf(
    "Domestic public debt stood at \\textbf{%s} at %s.%s%s%s",
    ei_amt(b$total), ei_date(b$asAt), change,
    if (nrow(bonds)) sprintf(" Treasury bonds make up \\textbf{%s}, locking in today's interest rates for years.", ei_pct(sum(bonds$share))) else "",
    if (nrow(bills)) sprintf(" Treasury bills, which must be rolled over within a year, make up \\textbf{%s}.", ei_pct(sum(bills$share))) else ""
  ))
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# Debt service by fiscal year
# ══════════════════════════════════════════════════════════════════
ei_render_service <- function() {
  d <- ei_get("/api/debt/service")
  y <- d$years
  if (is.null(y) || !is.data.frame(y) || !nrow(y)) return(invisible(FALSE))
  y <- y[order(y$fy_start), ]
  y <- tail(y, 8)
  gdp <- if (!is.null(y$gdp)) as.numeric(y$gdp) else rep(NA_real_, nrow(y))
  long <- rbind(
    data.frame(fy = y$fiscal_year, part = "External principal", v = y$external_principal, gdp = gdp),
    data.frame(fy = y$fiscal_year, part = "External interest",  v = y$external_interest,  gdp = gdp),
    data.frame(fy = y$fiscal_year, part = "Domestic principal", v = y$domestic_principal, gdp = gdp),
    data.frame(fy = y$fiscal_year, part = "Domestic interest",  v = y$domestic_interest,  gdp = gdp)
  )
  long$part <- factor(long$part, levels = c("External principal", "External interest", "Domestic principal", "Domestic interest"))
  long$fy <- factor(sub("^FY", "", long$fy), levels = sub("^FY", "", y$fiscal_year))
  ei_heading("Public Debt Service by Fiscal Year")
  ei_plot(ei_two_panel(long, EI_SITE$service))
  ei_source("Controller of Budget, full-year National Government Budget Implementation Review Reports; KNBS GDP (fiscal-year average)")
  last <- y[nrow(y), ]
  prev <- if (nrow(y) > 1) y[nrow(y) - 1, ] else NULL
  rev_share <- if (!is.null(last$revenue) && !is.na(last$revenue) && last$revenue > 0) last$total / last$revenue * 100 else NA
  ei_text(sprintf(
    "In %s the government paid \\textbf{%s} to service the public debt%s, %s of it on interest.%s%s",
    ei_fy(last$fiscal_year), ei_amt(last$total),
    if (!is.null(prev)) sprintf(" (%s %s than in %s)", ei_pct(abs(last$total / prev$total - 1) * 100),
                                 if (last$total >= prev$total) "more" else "less", ei_fy(prev$fiscal_year)) else "",
    ei_amt(last$interest),
    if (!is.na(rev_share)) sprintf(" That is \\textbf{%s} of the year's revenue: %s of every KSh 100 collected.", ei_pct(rev_share),
                                   sprintf("KSh %.0f", rev_share)) else "",
    sprintf(" Domestic debt took \\textbf{%s} of the total.", ei_pct(last$domestic / last$total * 100))
  ))
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# Indicators vs thresholds (as on the Explore page, Chart 13)
# ══════════════════════════════════════════════════════════════════
EI_INDICATORS <- data.frame(
  key   = c("debt_to_gdp", "external_debt_to_gdp", "external_debt_to_exports",
            "external_service_to_exports", "external_service_to_revenue", "deficit_to_gdp"),
  name  = c("Public debt / GDP", "External debt / GDP", "External debt / exports",
            "External debt service / exports", "External debt service / revenue", "Budget deficit / GDP"),
  limit = c(55, 40, 180, 15, 18, 3),
  by    = c("PFM Act anchor; IMF-WB", "IMF-WB", "IMF-WB", "IMF-WB", "IMF-WB", "EAC ceiling"),
  stock = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

ei_render_thresholds <- function() {
  d <- ei_get("/api/debt/thresholds")
  y <- d$years
  if (is.null(y) || !is.data.frame(y) || !nrow(y)) return(invisible(FALSE))
  y <- y[order(y$fy_start), ]
  pv <- d$pv
  pv <- if (is.data.frame(pv) && nrow(pv)) pv else NULL
  rows <- lapply(seq_len(nrow(EI_INDICATORS)), function(i) {
    ind <- EI_INDICATORS[i, ]
    if (!ind$key %in% names(y)) return(NULL)
    ok <- which(!is.na(y[[ind$key]]))
    if (!length(ok)) return(NULL)
    last <- y[max(ok), ]
    v <- last[[ind$key]]; basis <- "face value"; period <- last$fiscal_year
    if (ind$stock && !is.null(pv)) {   # stock ratios: the official present value, newest published
      p <- pv[pv$key == ind$key, ]
      if (nrow(p)) {
        p <- p[order(p$published, decreasing = TRUE), ][1, ]
        v <- p$value; basis <- "present value"; period <- p$period
      }
    }
    data.frame(Indicator = ind$name, Limit = ind$limit, Value = v, Period = period, Basis = basis, By = ind$by,
               stringsAsFactors = FALSE)
  })
  t <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(t) || !nrow(t)) return(invisible(FALSE))
  above <- t$Value > t$Limit
  tbl <- data.frame(
    Indicator = t$Indicator,
    Limit = sprintf("%s%%", format(t$Limit)),
    Latest = sprintf("%s%%", ifelse(t$Value >= 100, sprintf("%.0f", t$Value), sprintf("%.1f", t$Value))),
    Year = sub("^FY ", "", ei_fy(t$Period)),
    Basis = ifelse(t$Basis == "present value", "PV", "Face"),
    Status = ifelse(above, "\\textcolor{iearedflag}{\\textbf{Above}}", "\\textcolor{ieagreen}{\\textbf{Below}}"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cat(iea_fullwidth_table(tbl, raw_cols = "Status",
                          caption = "Kenya's debt indicators against their limits, latest figures (live)"))
  cat("\n\\vspace{0.2em}\n\\begin{flushleft}\n{\\scriptsize\\color{gray} PV = present value, the basis the limits are set on; Face = face value. Limits: IMF--World Bank Debt Sustainability Framework thresholds for a medium debt-carrying capacity country; Public Finance Management Act anchor (55\\% of GDP, present value); East African Community convergence ceiling (deficit). Source: CBK, Controller of Budget, KNBS, National Treasury (present-value ratios); \\href{https://kenyadebtcounter.vercel.app/explore\\#chart-thresholds}{kenyadebtcounter.vercel.app}.}\n\\end{flushleft}\n\\vspace{0.2em}\n")
  n <- sum(above)
  ei_text(sprintf(
    "On the latest figures Kenya is above the limit on \\textbf{%d of %d} indicators%s. The table above updates with the live data each day, whereas Table 4 reports the Treasury's own %s assessment.",
    n, nrow(t),
    if (n) paste0(": ", paste(t$Indicator[above], collapse = "; ")) else "",
    mtds_year
  ))
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# Budget: deficit, tax revenue, spending
# ══════════════════════════════════════════════════════════════════
ei_render_deficit <- function() {
  d <- ei_get("/api/debt/deficit")
  m <- d$months
  if (is.null(m) || !is.data.frame(m) || !nrow(m)) return(invisible(FALSE))
  m <- m[!is.na(m$gdp) & m$gdp > 0, ]
  if (nrow(m) < 12) return(invisible(FALSE))
  # CBK's own months only (the site estimates the latest ones), and the
  # deficit against ALL revenue: the API's `deficit` counts tax revenue and
  # grants only, so subtract non-tax revenue, as the site's "All revenue"
  # view and the EAC 3% ceiling do.
  m <- m[m$kind == "actual", ]
  if (nrow(m) < 12) return(invisible(FALSE))
  m$date <- as.Date(paste0(m$month, "-01"))
  m <- m[m$date >= max(m$date) - 365.25 * 10, ]
  m$deficit <- m$deficit - m$nontax
  m$pct <- m$deficit / m$gdp * 100
  last <- m[nrow(m), ]
  ei_heading("Budget Deficit over the Previous 12 Months (% of GDP)")
  ei_plot(
    ggplot(m, aes(x = date, y = pct)) +
      geom_hline(yintercept = 3, colour = "#0f172a", linetype = "dashed", linewidth = 0.4) +
      annotate("text", x = min(m$date), y = 3, label = "EAC ceiling 3%", hjust = 0, vjust = -0.5, size = 2.6, colour = "#0f172a") +
      geom_line(colour = EI_SITE$deficit, linewidth = 0.8) +
      geom_point(data = last, colour = EI_SITE$deficit, size = 1.8) +
      annotate("text", x = last$date, y = last$pct, label = sprintf("%.1f%%", last$pct),
               hjust = -0.2, size = 3, fontface = "bold", colour = EI_SITE$deficit) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.06))) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = NULL) +
      ei_theme() +
      theme(panel.grid.major.y = element_line(colour = "gray92"))
  )
  ei_source("Central Bank of Kenya, government finance statistics; KNBS GDP")
  tg <- d$targets
  fy_start <- if (as.integer(substr(last$month, 6, 7)) >= 7) as.integer(substr(last$month, 1, 4)) else as.integer(substr(last$month, 1, 4)) - 1L
  target <- if (is.data.frame(tg)) tg$pct_gdp[tg$fy_start == fy_start] else numeric(0)
  ei_text(sprintf(
    "Over the 12 months to %s the government spent \\textbf{%s} more than it collected in revenue and grants, \\textbf{%s of GDP}, %s the East African Community's 3%% ceiling.%s Interest alone was %s over the same months.",
    ei_month(last$month), ei_amt(last$deficit), ei_pct(last$pct),
    if (last$pct > 3) "above" else "within",
    if (length(target)) sprintf(" The budget for FY %d/%02d targets %s.", fy_start, (fy_start + 1) %% 100, ei_pct(target[1])) else "",
    ei_amt(last$interest)
  ))
  invisible(TRUE)
}

# Two panels side by side for a stacked fiscal-year chart:
#   (a) Nominal (KSh Billion)   (b) % of GDP
# with each bar's total printed on top. `long` has columns fy (factor),
# part (factor), v (KSh) and gdp (KSh, NA when unknown); years without GDP
# are left out of panel (b) only.
EI_PANEL_A <- "(a) Nominal (KSh Billion)"
EI_PANEL_B <- "(b) % of GDP"
ei_two_panel <- function(long, pal, legend_rows = 1) {
  a <- transform(long, panel = EI_PANEL_A, y = v / 1e9)
  b <- transform(long[!is.na(long$gdp) & long$gdp > 0, ], panel = EI_PANEL_B, y = v / gdp * 100)
  both <- rbind(a, b)
  both$panel <- factor(both$panel, levels = c(EI_PANEL_A, EI_PANEL_B))
  tot <- aggregate(y ~ fy + panel, data = both, FUN = sum)
  tot$label <- ifelse(tot$panel == EI_PANEL_A, formatC(round(tot$y), format = "d", big.mark = ","), sprintf("%.1f", tot$y))
  ggplot(both, aes(x = fy, y = y)) +
    geom_col(aes(fill = part), width = 0.68) +
    geom_text(data = tot, aes(label = label), vjust = -0.4, size = 2.15, colour = "gray15", fontface = "bold") +
    facet_wrap(~panel, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.13))) +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(nrow = legend_rows)) +
    ei_theme() +
    theme(panel.grid.major.y = element_line(colour = "gray92"),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold", size = 8.5, colour = "gray15"),
          axis.text.x = element_text(size = 6.3, angle = 45, hjust = 1),
          panel.spacing = unit(1.2, "lines"))
}

# Complete fiscal years only (the current year to date is left out), last `n`.
ei_years_long <- function(y, keys, labels, n = 8) {
  y <- y[y$complete %in% TRUE, ]
  y <- tail(y[order(y$fy_start), ], n)
  if (!nrow(y)) return(NULL)
  vals <- y$values
  gdp <- if (!is.null(y$gdp)) as.numeric(y$gdp) else rep(NA_real_, nrow(y))
  out <- do.call(rbind, lapply(seq_along(keys), function(i) {
    data.frame(fy = sub("^FY", "", y$fiscal_year), part = labels[i], v = vals[[keys[i]]] %||% 0, gdp = gdp)
  }))
  out$fy <- factor(out$fy, levels = sub("^FY", "", y$fiscal_year))
  out$part <- factor(out$part, levels = labels)
  out
}

ei_render_tax <- function() {
  d <- ei_get("/api/debt/tax-revenue")
  y <- d$years
  if (is.null(y) || !is.data.frame(y) || !nrow(y) || is.null(y$values)) return(invisible(FALSE))
  keys <- c("income_tax", "vat", "excise_duty", "import_duty", "other_tax", "grants")
  labs <- c("Income tax", "VAT", "Excise duty", "Import duty", "Other taxes", "Grants")
  long <- ei_years_long(y, keys, labs)
  if (is.null(long)) return(invisible(FALSE))
  ei_heading("Tax Revenue and Grants by Fiscal Year")
  ei_plot(ei_two_panel(long, setNames(EI_SITE$tax, labs)))
  ei_source("Central Bank of Kenya, government finance statistics; KNBS GDP (fiscal-year average). Complete fiscal years")
  yc <- y[y$complete, ]
  yc <- yc[order(yc$fy_start), ]
  if (!nrow(yc)) return(invisible(TRUE))
  last <- yc[nrow(yc), ]
  prev <- if (nrow(yc) > 1) yc[nrow(yc) - 1, ] else NULL
  shares <- sapply(keys[1:5], function(k) last$values[[k]]) / last$tax_total * 100
  top <- which.max(shares)
  gdp_pct <- if (!is.null(last$gdp) && !is.na(last$gdp) && last$gdp > 0) last$tax_total / last$gdp * 100 else NA
  ei_text(sprintf(
    "In %s tax revenue came to \\textbf{%s}%s%s. %s is the largest tax, at %s of the total; grants added %s.",
    ei_fy(last$fiscal_year), ei_amt(last$tax_total),
    if (!is.na(gdp_pct)) sprintf(", \\textbf{%s of GDP}", ei_pct(gdp_pct)) else "",
    if (!is.null(prev)) sprintf(", %s %s than in %s", ei_pct(abs(last$tax_total / prev$tax_total - 1) * 100),
                                 if (last$tax_total >= prev$tax_total) "more" else "less", ei_fy(prev$fiscal_year)) else "",
    labs[top], ei_pct(shares[top]), ei_amt(last$values$grants)
  ))
  invisible(TRUE)
}

ei_render_spending <- function() {
  d <- ei_get("/api/debt/spending")
  y <- d$years
  if (is.null(y) || !is.data.frame(y) || !nrow(y) || is.null(y$values)) return(invisible(FALSE))
  keys <- c("domestic_interest", "foreign_interest", "wages", "pensions", "other_recurrent", "county_transfers", "development")
  labs <- c("Domestic interest", "Foreign interest", "Wages", "Pensions", "Other recurrent", "Counties", "Development")
  long <- ei_years_long(y, keys, labs)
  if (is.null(long)) return(invisible(FALSE))
  ei_heading("Government Spending by Type")
  ei_plot(ei_two_panel(long, setNames(EI_SITE$spend, labs), legend_rows = 2))
  ei_source("Central Bank of Kenya, government finance statistics; KNBS GDP (fiscal-year average). Complete fiscal years")
  yc <- y[y$complete, ]
  yc <- yc[order(yc$fy_start), ]
  if (!nrow(yc)) return(invisible(TRUE))
  last <- yc[nrow(yc), ]
  interest <- last$values$domestic_interest + last$values$foreign_interest
  first <- yc[max(1, nrow(yc) - 9), ]
  int0 <- first$values$domestic_interest + first$values$foreign_interest
  ei_text(sprintf(
    "Interest on the public debt took \\textbf{%s} of government spending in %s (\\textbf{%s}), %s development spending (%s) and %s wages (%s).%s",
    ei_pct(interest / last$total * 100), ei_fy(last$fiscal_year), ei_amt(interest),
    if (interest > last$values$development) "more than" else "less than", ei_amt(last$values$development),
    if (interest > last$values$wages) "more than" else "less than", ei_amt(last$values$wages),
    if (first$fy_start < last$fy_start) sprintf(" In %s it was %s.", ei_fy(first$fiscal_year), ei_pct(int0 / first$total * 100)) else ""
  ))
  invisible(TRUE)
}
