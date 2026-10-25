# Quarterly Macro & Public Debt Report – Kenya

This repository contains a **Quarto-based quarterly analysis** of Kenya’s
macroeconomic performance and public debt statistics.

## 📄 Latest Output
- **Quarterly PDF report**:  
  `docs/quarterly_macro_debt_report_ieakenya.pdf`

The PDF is automatically generated and updated via GitHub Actions.

## ⚙️ How it works
- The report source is written in **Quarto (`.qmd`)**
- Data is read from trusted sources, including Google Sheets
- A GitHub Actions workflow:
  - Installs R, Quarto, and TinyTeX
  - Renders the PDF using XeLaTeX
  - Commits the updated PDF to the `docs/` folder

## 🔁 Automation
The report is rebuilt every day at 05:17 Nairobi time, whenever changes are
pushed to `main`, or when the workflow is run by hand. It always covers the
last completed quarter, so a new edition starts on 1 January, April, July
and October.

- **Today's copy** is force-pushed to the `report` branch (one commit, so the
  repository doesn't grow by a PDF a day) under readable names (quarter, not day, so links last all quarter), e.g.
  `IEA-Kenya_Quarterly-Debt-Report_Q3-2026.pdf` and
  `IEA-Kenya_Public-Debt-Data_Q3-2026.xlsx`; `latest.json` names them.
- **`docs/` on `main`** is updated only when a new quarter's edition appears.
- `latest.json` (quarter, period, publication date) feeds the report card on
  https://kenyadebtcounter.vercel.app/explore.

Debt, population and GDP come from the site's public API
(https://kenyadebtcounter.vercel.app/api/..., five requests per render);
other indicators come from the Google Sheet.

## 📁 Key files
- `quarterly_macro_debt_report_ieakenya.qmd` — report source
- `.github/workflows/render-pdf.yml` — CI workflow
- `docs/` — rendered PDF output

## 📜 License
This project is provided for analytical and research purposes.# regenerate pdf
