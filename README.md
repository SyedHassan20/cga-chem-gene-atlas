# GeneChem-Atlas
Webapp for Gene-Chemical Interactions

The **Chemical-Gene Atlas (CGA)** is an interactive Shiny web application for exploring how genes and chemicals interact in shaping prenatal and postnatal disease outcomes. It provides visual insights into gene effects, affected systems, lethality modes, and developmental timing, along with a searchable network and data table.

---

## Features

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

## Data Input

The app requires a merged dataset in RDS format:

```bash
data/merged_data.rds
```

### Expected Columns
- **Genes**: `Gene_Name`, `Gene ID`, `OMIM Gene`, `Gene Effect`, `Variants`
- **Lethality & Phenotypes**: `Lethality.Mode.of.Function`, `1st`, `2nd`, `3rd`, `Postnatal`, `System.Affected`, `Disease`
- **Chemicals**: `X..ChemicalName`, `Chemical ID`, `Interaction`, `Interaction Actions`
- **Evidence**: `CTD PubMed IDs`, `Intolerome PubMed IDs`

---
## Install R dependencies

```bash
install.packages(c(
  "shiny", "dplyr", "tidyr", "stringr", "plotly",
  "visNetwork", "DT", "memoise", "shinycssloaders",
  "RColorBrewer", "viridis", "future", "promises"
))
```
---

## Install R dependencies
- Place your merged_data.rds file into the data/ directory.

