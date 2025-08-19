## Script Explanations

- `00_setup.R` - Creates needed local folders and downloads raw Excel file received from Dr. Zhiwei Zhong from my (NL's) personal Google Drive
- `01_tidy.R` - Wrangles the raw Excel file into a number of tidy-format CSVs (i.e., one row per observation, one column per variable)
- `02_process.R` - Combines outputs of `01` as needed to be ready for statistics / figure creation
- `03_stats.R` - Runs statistical tests as described in manuscript
- `04_figures.R` - Creates publication-quality figures demonstrating statistical results
