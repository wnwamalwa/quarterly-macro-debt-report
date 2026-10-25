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
The report is rebuilt on the 1st and 15th of each month (10:17 Nairobi time),
whenever changes are pushed to `main`, or when the workflow is run by hand.
It always covers the last completed quarter, so a new edition appears on
1 January, April, July and October.

Debt, population and GDP come from Supabase, the database behind
https://kenyadebtcounter.vercel.app (repository secrets `SUPABASE_URL` and
`SUPABASE_ANON_KEY`); other indicators come from the Google Sheet.
`docs/latest.json` records the edition (quarter, period, publication date)
for the report card on the website.

## 📁 Key files
- `quarterly_macro_debt_report_ieakenya.qmd` — report source
- `.github/workflows/render-pdf.yml` — CI workflow
- `docs/` — rendered PDF output

## 📜 License
This project is provided for analytical and research purposes.# regenerate pdf
