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
The report is automatically rebuilt when changes are pushed to the `main`
branch or when the workflow is manually triggered.

## 📁 Key files
- `quarterly_macro_debt_report_ieakenya.qmd` — report source
- `.github/workflows/render-pdf.yml` — CI workflow
- `docs/` — rendered PDF output

## 📜 License
This project is provided for analytical and research purposes.# regenerate pdf
