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
  cat(sprintf("\n\\vspace{-1.2em}\n\\begin{flushleft}\n{\\scriptsize\\color{gray} Source: %s.}\n\\end{flushleft}\n\\vspace{-0.3em}\n",
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
  ei_plot(ei_bar_share(b$rows, iea_gold))
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
  ei_plot(ei_bar_share(b$rows, iea_green))
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
  long <- rbind(
    data.frame(fy = y$fiscal_year, part = "External principal", v = y$external_principal),
    data.frame(fy = y$fiscal_year, part = "External interest",  v = y$external_interest),
    data.frame(fy = y$fiscal_year, part = "Domestic principal", v = y$domestic_principal),
    data.frame(fy = y$fiscal_year, part = "Domestic interest",  v = y$domestic_interest)
  )
  long$part <- factor(long$part, levels = c("External principal", "External interest", "Domestic principal", "Domestic interest"))
  long$fy <- factor(sub("^FY", "", long$fy), levels = sub("^FY", "", y$fiscal_year))
  ei_heading("Public Debt Service by Fiscal Year (KSh Billion)")
  ei_plot(
    ggplot(long, aes(x = fy, y = v / 1e9, fill = part)) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c("External principal" = "#E6CF6B", "External interest" = iea_gold,
                                   "Domestic principal" = iea_light, "Domestic interest" = iea_green)) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = NULL) +
      ei_theme() +
      theme(panel.grid.major.y = element_line(colour = "gray92"))
  )
  ei_source("Controller of Budget, full-year National Government Budget Implementation Review Reports")
  last <- y[nrow(y), ]
  prev <- if (nrow(y) > 1) y[nrow(y) - 1, ] else NULL
  rev_share <- if (!is.null(last$revenue) && !is.na(last$revenue) && last$revenue > 0) last$total / last$revenue * 100 else NA
  ei_text(sprintf(
    "In %s the government paid \\textbf{%s} to service the public debt, %s on interest%s.%s%s",
    ei_fy(last$fiscal_year), ei_amt(last$total), ei_amt(last$interest),
    if (!is.null(prev)) sprintf(" (%s %s than in %s)", ei_pct(abs(last$total / prev$total - 1) * 100),
                                 if (last$total >= prev$total) "more" else "less", ei_fy(prev$fiscal_year)) else "",
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
  m$date <- as.Date(paste0(m$month, "-01"))
  m <- m[m$date >= max(m$date) - 365.25 * 10, ]
  m$pct <- m$deficit / m$gdp * 100
  m$est <- m$kind != "actual"
  last <- m[nrow(m), ]
  ei_heading("Budget Deficit over the Previous 12 Months (% of GDP)")
  ei_plot(
    ggplot(m, aes(x = date, y = pct)) +
      geom_hline(yintercept = 3, colour = iea_red, linetype = "dashed", linewidth = 0.4) +
      annotate("text", x = min(m$date), y = 3, label = "EAC ceiling 3%", hjust = 0, vjust = -0.5, size = 2.6, colour = iea_red) +
      geom_line(colour = iea_blue, linewidth = 0.8) +
      geom_point(data = last, colour = iea_blue, size = 1.8) +
      annotate("text", x = last$date, y = last$pct, label = sprintf("%.1f%%", last$pct),
               hjust = -0.2, size = 3, fontface = "bold", colour = iea_blue) +
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
    "Over the 12 months to %s the government spent \\textbf{%s} more than it collected in revenue and grants, \\textbf{%s of GDP}%s, %s the East African Community's 3%% ceiling.%s Interest alone was %s over the same months.",
    ei_month(last$month), ei_amt(last$deficit), ei_pct(last$pct),
    if (isTRUE(last$est)) " (CBK's latest months estimated)" else "",
    if (last$pct > 3) "above" else "within",
    if (length(target)) sprintf(" The budget for FY %d/%02d targets %s.", fy_start, (fy_start + 1) %% 100, ei_pct(target[1])) else "",
    ei_amt(last$interest)
  ))
  invisible(TRUE)
}

# Stacked bars of one category set per fiscal year, last `n` years.
ei_years_long <- function(y, keys, labels, n = 8) {
  y <- tail(y[order(y$fy_start), ], n)
  vals <- y$values
  out <- do.call(rbind, lapply(seq_along(keys), function(i) {
    data.frame(fy = sub("^FY", "", y$fiscal_year), complete = y$complete,
               part = labels[i], v = vals[[keys[i]]] %||% 0)
  }))
  out$fy <- factor(ifelse(out$complete, out$fy, paste0(out$fy, "*")),
                   levels = unique(ifelse(y$complete, sub("^FY", "", y$fiscal_year), paste0(sub("^FY", "", y$fiscal_year), "*"))))
  out$part <- factor(out$part, levels = labels)
  out
}

ei_stack_plot <- function(long, pal) {
  ggplot(long, aes(x = fy, y = v / 1e9, fill = part)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    ei_theme() +
    theme(panel.grid.major.y = element_line(colour = "gray92"))
}

ei_render_tax <- function() {
  d <- ei_get("/api/debt/tax-revenue")
  y <- d$years
  if (is.null(y) || !is.data.frame(y) || !nrow(y) || is.null(y$values)) return(invisible(FALSE))
  keys <- c("income_tax", "vat", "excise_duty", "import_duty", "other_tax", "grants")
  labs <- c("Income tax", "VAT", "Excise duty", "Import duty", "Other taxes", "Grants")
  long <- ei_years_long(y, keys, labs)
  ei_heading("Tax Revenue and Grants by Fiscal Year (KSh Billion)")
  ei_plot(ei_stack_plot(long, setNames(c(iea_green, iea_gold, iea_blue, iea_orange, iea_gray, "#BDBDBD"), labs)))
  ei_source("Central Bank of Kenya, government finance statistics. * Year to date")
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
  ei_heading("Government Spending by Type (KSh Billion)")
  ei_plot(ei_stack_plot(long, setNames(c(iea_red, "#EF9A9A", iea_blue, "#90CAF9", iea_gray, iea_gold, iea_green), labs)) +
            guides(fill = guide_legend(nrow = 2)))
  ei_source("Central Bank of Kenya, government finance statistics. * Year to date")
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
