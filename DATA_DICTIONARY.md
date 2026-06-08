# Chemical-Gene Atlas Data Dictionary

## Dataset Description

The Chemical-Gene Atlas (CGA) dataset integrates developmental lethality-associated genes from the Intolerome with curated human chemical-gene interactions from the Comparative Toxicogenomics Database (CTD).

**Dataset file:**

* `merged_data.rds`

**Dataset summary:**

* Number of genes: 928
* Number of chemicals: 4,110
* Number of chemical-gene interactions: 73,256

## Data Sources

### Intolerome

https://rpldb.org/intolerome/

### Comparative Toxicogenomics Database (CTD)

https://ctdbase.org/

CTD revision used in this study: **17996**

## Column Definitions

| Column Name     | Description                                 | Example                  |
| --------------- | ------------------------------------------- | ------------------------ |
| GeneSymbol      | Official HGNC gene symbol                   | F5                       |
| OMIM_Gene_ID    | OMIM identifier for the gene                | 612309                   |
| ChemicalName    | Chemical name from CTD                      | Bisphenol A              |
| ChemicalID      | CTD chemical identifier                     | D001535                  |
| Interaction     | Curated CTD interaction description         | increases expression     |
| Organism        | Species associated with interaction         | Homo sapiens             |
| GeneEffect      | Developmental effect category               | Fetal                    |
| SystemAffected  | Biological system associated with phenotype | Cardiovascular           |
| LethalityMode   | Functional mechanism                        | Loss of Function         |
| LethalityTiming | Developmental timing                        | Second Trimester         |
| DiseaseName     | Associated developmental condition          | Recurrent Pregnancy Loss |
| Inheritance     | OMIM inheritance classification             | Autosomal Dominant       |

## Data Processing Notes

* All chemical-gene interactions were restricted to **Homo sapiens** records from CTD.
* Intolerome genes were matched to CTD interactions using **OMIM Gene IDs**.
* Duplicate records were removed following harmonization and dataset integration.
* Developmental timing annotations were transformed from wide to long format to facilitate filtering and visualization.
* The final merged dataset represents curated interactions between developmental lethality-associated genes and environmental chemical exposures.

## Repository

GitHub Repository:
https://github.com/SyedHassan20/cga-chem-gene-atlas

Web Application:
https://cgatlas.org/

## Citation

If you use the Chemical-Gene Atlas dataset or software in your work, please cite the associated publication and repository release.
