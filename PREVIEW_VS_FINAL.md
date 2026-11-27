# How the Final OAC relates to the Preview

The Final OAC uses the Preview clusters. `Code.R` doesn't run k-means again, so there are no separate Final centroids. Instead it:

1. copies the E&W assignments from the [Preview OAC](https://github.com/jakubwyszomierski/OAC2021-2), unchanged;
2. assigns each Scotland/NI OA to the nearest Preview subgroup centroid, using the newly calculated Final inputs and excluding `ex_variables` ([Code.R](Code.R#L980-L1023));
3. combines the two into `data/UK_OAC_Final.parquet`.

Every class in the Final OAC is therefore defined by a Preview centroid. The new part is the Scotland/NI inputs and their assignments.

| | E&W | Scotland/NI |
| --- | --- | --- |
| Labels | Preview, unchanged | Final, reassigned |
| Inputs | Preview inputs | Final inputs from `Code.R` |
| Centroids | Preview | Preview |

`Code.R` also recalculates the E&W inputs, but those values don't reproduce the E&W labels, because the labels were built from the Preview inputs ([#1](https://github.com/safer-streets/Output_Area_Classification/issues/1)).

## Consistent files

These files match each other and reproduce every label in `UK_OAC_Final.parquet`:

- **`data/UK_OAC_Final.parquet`**: the supergroup, group and subgroup of every UK OA.
- **`data/OAC_Input_IHS.parquet`**: the 60 inputs `v01`–`v60` per OA (inverse hyperbolic sine, then range-standardised). The E&W rows are the Preview inputs, renamed to the Final variable numbering. The Scotland/NI rows are the Final inputs from `Code.R`, and `v40`/`v41` are empty for them. Built by [extra_code/preview_ew_inputs.py](extra_code/preview_ew_inputs.py), which also runs the checks below. Re-running `Code.R` overwrites this file with the Final values.
- **`data/preview/OAC_centroids.csv`**, **`data/preview/OAC_Groups_centroids.csv`**, **`data/preview/OAC_Subgroups_centroids.csv`**: the Preview k-means centroids for the 8 supergroups, 21 groups and 52 subgroups, in the same space as `OAC_Input_IHS.parquet`. Copied unchanged from [OAC2021-2](https://github.com/jakubwyszomierski/OAC2021-2/tree/c058eee98c8a1abe5a197a3c79125c0204a18e15/Data/Clustering) at commit `c058eee`.

To reproduce the labels from these files:

- **E&W:** classify top-down (nearest supergroup, then nearest group within it, then nearest subgroup within that) using all 60 variables.
- **Scotland/NI:** take the nearest subgroup centroid, excluding `ex_variables` (`v07`, `v09`, `v11`, `v23`, `v32`, `v40`, `v41`, `v42`, `v60`).

All of these files already use the Final variable numbering. `data/lookup/old_v_new.csv` maps the Preview numbering to the Final numbering, and is only needed to rename the raw Preview inputs.

## Files that are not consistent

`data/OAC_Input.parquet` and the `Index_Scores_Final_*.csv` files are built from the inputs `Code.R` recalculates. For E&W these don't exactly match the inputs the clusters were built from, so the index scores don't exactly describe the clusters. [Variable labels](#variable-labels) lists where they differ.

## Variable labels

There are two variable numberings:

- **Preview numbering**: used by the Preview repository and the GeoDS documentation (for example, `v01` is "Lives in a communal establishment").
- **Final numbering**: used by every file in this repository (for example, `v01` is "Usual residents per square kilometre").

`data/lookup/old_v_new.csv` maps one to the other. The names in `data/lookup/Variables_OAC.csv`, which the `Index_Scores_Final_*.csv` files copy, match the values in the consistent files except for these variables:

| Final | Label in `Variables_OAC.csv` | In the consistent files | In `OAC_Input.parquet` and `Index_Scores_Final_*.csv` (E&W) |
| --- | --- | --- | --- |
| `v11` | Country of birth: Africa | Born in Africa | Census code `ts0040010`, "Europe: Non-EU countries: All other non-EU countries", which is the same as `v10` at OA level. The code for Africa is `ts0040011`. |
| `v23` | Other religion | Every religion other than Christian: Buddhist, Hindu, Jewish, Muslim, Sikh and other religions | Census code `ts0300009`, the "Other religion" category only (about 0.6% of people) |
| `v40`, `v41` | Occupancy rating of rooms: +1 or more / −1 or less | Under-occupancy / overcrowding | Missing: `Code.R` doesn't calculate them, so their index scores are empty |
| `v42` | SIR | Called "Standardised Illness Ratio" in the Preview lookups and "Standardised Disability Ratio" in the GeoDS documentation; it's the same variable | Calculated slightly differently from the Preview (rank correlation 0.90 at OA level) |
| `v43` | Provides unpaid care | Provides unpaid care | Calculated slightly differently from the Preview (rank correlation 0.97) |

For the other 54 variables, the E&W values in `OAC_Input.parquet` rank the OAs exactly as the consistent files do. All of the variables above except `v43` are in `ex_variables`, so none of these differences affected the Scotland/NI assignments.

## The GeoDS pen portraits document

GeoDS publishes [UK Output Area Classification 2021/22](https://data.geods.ac.uk/dataset/73a16168-f2b0-4732-b36a-3ec0253e34fe/resource/33fd0618-b594-4451-9705-9f1e9dae2c66/download/pen_portraits_ukoac.pdf) (28 January 2025), with the typology, a glossary of variables, distribution statistics, pen portraits and a technical note on the update. Checked against the files in this repository, it is correct except for two errors:

- **Glossary of Variables: correct.** It uses the Preview numbering. Its definitions match the consistent files, including `v11` Africa and `v32` "Other religion" (Final `v23`), which it correctly describes as Buddhist, Hindu, Jewish, Muslim, Sikh and other religions.
- **Distribution Statistics: wrong caption.** The table says it covers England and Wales, but the counts are for the whole UK. All 52 rows match `data/descriptive_stats.csv`, which `Descriptions.R` calculates over all 239,023 UK OAs. For example, the table gives 10,758 OAs for `1a1`; the UK count is 10,758 and the E&W count is 7,790. The local authority counts are UK-wide too.
- **Pen portraits: consistent.** The E&W clusters didn't change, so the portraits still describe them. Spot checks of the statements about Africa and religion (Groups `3b` and `4a`, Subgroup `6b3`) agree with the consistent files, not with the `Index_Scores_Final_*.csv` files. Not every statement has been checked.
- **Update Technical Note: variables in the wrong numbering.** The method it describes matches `Code.R`. But it lists the variables excluded from the Scotland/NI reassignment (`v07, v09, v11, v23, v42, v60, v32, v40 and v41`) in the Final numbering, while the glossary in the same document uses the Preview numbering. Read with the glossary, the list wrongly says that, for example, families with dependent children (`v07`) and Pakistani ethnic group (`v23`) were excluded. In the document's own numbering the excluded variables are:

  | Preview (document) | Final (this repository) | Variable |
  | --- | --- | --- |
  | `v18` | `v07` | Aged 85 years and over |
  | `v09` | `v09` | Country of birth: Europe: EU countries |
  | `v11` | `v11` | Country of birth: Africa |
  | `v32` | `v23` | Other religion |
  | `v33` | `v42` | Standardised Disability Ratio |
  | `v57` | `v60` | Economically active: Unemployed |
  | `v19` | `v32` | Address one year ago is the same as the address of enumeration |
  | `v40` | `v40` | Under-occupancy |
  | `v41` | `v41` | Overcrowding |
