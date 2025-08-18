# GeneChem-Atlas
Webapp for Gene-Chemical Interactions

The **Chemical-Gene Atlas (CGA)** is an interactive Shiny web application for exploring how genes and chemicals interact in shaping prenatal and postnatal disease outcomes. It provides visual insights into gene effects, affected systems, lethality modes, and developmental timing, along with a searchable network and data table.

---

## 🌟 Features

- **Interactive Donut Charts** for:
  - Gene Effect
  - System Affected
  - Lethality Mode of Function
  - Lethality Timing (1st, 2nd, 3rd trimester, Postnatal)  
- **Gene–Chemical Network Visualization** with searchable and highlightable nodes.
- **Data Table** with global and column-level search, PubMed/OMIM/CTD hyperlinks, and export to CSV.
- **Reset & Zoom Options** for flexible exploration.
- **Institutional Branding** with linked partner logos (UCSF, Imperial, Stanford, HOPE, NIH).

---

## 📊 Data Input

The app requires a merged dataset in RDS format:

```bash
data/merged_data.rds

