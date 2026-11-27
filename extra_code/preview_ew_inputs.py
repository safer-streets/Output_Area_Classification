"""Replace the E&W rows of data/OAC_Input_IHS.parquet with the Preview OAC inputs.

The E&W cluster assignments in UK_OAC_Final.parquet are the Preview OAC assignments
(jakubwyszomierski/OAC2021-2), copied unchanged by Code.R. The E&W inputs written by
Code.R were recalculated afterwards and don't reproduce those assignments (see issue #1).

This script:
- takes the Preview inputs (asinh + range-standardised) for E&W and renames them from the
  Preview to the Final variable numbering (data/lookup/old_v_new.csv)
- adds v40/v41, which the Preview clustering used; they are NaN for Scotland/NI, which were
  assigned without them
- keeps the Scotland/NI rows unchanged, since those are the values Code.R assigned from
- checks every row against UK_OAC_Final.parquet:
  - E&W: top-down nearest centroid (supergroup -> group -> subgroup), all 60 variables
  - S/NI: flat nearest subgroup centroid, excluding ex_variables (the Code.R rule)

The Preview centroids are vendored in data/preview/ (copied unchanged from OAC2021-2 at the
commit pinned in PREVIEW). The Preview inputs (OAC_assignment.csv, ~100 MB) are still read
from that commit.

Run from the repository root: python extra_code/preview_ew_inputs.py
Requires pandas, numpy and pyarrow.
"""

from pathlib import Path

import numpy as np
import pandas as pd

PREVIEW = (
    "https://raw.githubusercontent.com/jakubwyszomierski/OAC2021-2/"
    "c058eee98c8a1abe5a197a3c79125c0204a18e15/Data/Clustering/"
)
EX_VARIABLES = ["v07", "v09", "v11", "v23", "v42", "v60", "v32", "v40", "v41"]
DATA = Path("data")


def nearest(X: np.ndarray, C: pd.DataFrame) -> np.ndarray:
    Cv = C.to_numpy()
    d2 = (X**2).sum(axis=1)[:, None] - 2 * X @ Cv.T + (Cv**2).sum(axis=1)[None, :]
    return C.index.to_numpy()[d2.argmin(axis=1)]


def top_down(X: np.ndarray, sup: pd.DataFrame, grp: pd.DataFrame, sub: pd.DataFrame) -> np.ndarray:
    s = nearest(X, sup)
    g = np.empty(len(X), dtype=object)
    u = np.empty(len(X), dtype=object)
    for k in np.unique(s):
        m = s == k
        g[m] = nearest(X[m], grp[grp.index.str[0] == k])
    for k in np.unique(g):
        m = g == k
        u[m] = nearest(X[m], sub[sub.index.str[:2] == k])
    return u


def main() -> None:
    current = pd.read_parquet(DATA / "OAC_Input_IHS.parquet").set_index("OA")
    labels = pd.read_parquet(DATA / "UK_OAC_Final.parquet").set_index("Geography_Code")["Subgroup"]

    preview_to_final = pd.read_csv(DATA / "lookup/old_v_new.csv", encoding="utf-8-sig").set_index("Preview")["Final"]
    preview = pd.read_csv(PREVIEW + "OAC_assignment.csv").set_index("Geography_Code").rename(columns=preview_to_final)
    variables = sorted(c for c in preview.columns if c.startswith("v"))
    assert len(variables) == 60

    is_ew = current.index.str[0].isin(["E", "W"])
    ew_codes = current.index[is_ew]
    assert ew_codes.isin(preview.index).all()

    # keep the existing column order, with v40/v41 inserted after v39
    columns = list(current.columns)
    columns[columns.index("v39") + 1 : columns.index("v39") + 1] = ["v40", "v41"]

    fixed = current.reindex(columns=columns)
    fixed.loc[ew_codes, variables] = preview.loc[ew_codes, variables].to_numpy()

    # checks
    centroids = DATA / "preview"
    sup = pd.read_csv(centroids / "OAC_centroids.csv", dtype={"Supergroup8": str}).set_index("Supergroup8")[variables]
    grp = pd.read_csv(centroids / "OAC_Groups_centroids.csv").set_index("Group")[variables]
    sub = pd.read_csv(centroids / "OAC_Subgroups_centroids.csv").set_index("Subgroup")[variables]

    ew = fixed.loc[ew_codes, variables]
    assert not ew.isna().any().any()
    ew_match = top_down(ew.to_numpy(), sup, grp, sub) == labels.loc[ew_codes].to_numpy()

    s_ni_codes = current.index[~is_ew]
    kept = [v for v in variables if v not in EX_VARIABLES]
    s_ni_match = nearest(fixed.loc[s_ni_codes, kept].to_numpy(), sub[kept]) == labels.loc[s_ni_codes].to_numpy()

    print(f"E&W:  {ew_match.mean():.4%} of {len(ew_codes)} reproduce UK_OAC_Final")
    print(f"S/NI: {s_ni_match.mean():.4%} of {len(s_ni_codes)} reproduce UK_OAC_Final")
    assert ew_match.all() and s_ni_match.all()

    fixed.reset_index().to_parquet(DATA / "OAC_Input_IHS.parquet", index=False)
    print(f"wrote {DATA / 'OAC_Input_IHS.parquet'}: {fixed.shape[0]} rows, {fixed.shape[1]} variables")


if __name__ == "__main__":
    main()
