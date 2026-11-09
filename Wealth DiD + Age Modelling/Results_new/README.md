# Working outputs (`Results_new`)

This is the output directory targeted by the three current trajectory/DiD scripts under `../Scripts/Current/`. It retains the Stata-expected folder name and the original result filenames so new runs can write their expected output paths once the local `global resultspath` is corrected.

## Main results

- **Child observed LOWESS fan workbook:** `Child_wealth_byage_lowess_fan_2026_26_05.xlsx` (child script, final export at lines 1706-1709).
- **Parent observed + extrapolated LOWESS fan workbook:** `Parent_wealth_byage_lowess_fan_extrapolated100_2026_26_05.xlsx` (parent script, final export at lines 1732-1735).

The first was formerly under `Coefficients/`; moving it here matches the destination explicitly written in the child script. The parent workbook was already here. Other workbooks, `Coefficients/` and `Figures/` include supporting analysis exports, not necessarily the preferred final inputs for downstream work.

Older variants, exact duplicates and prior `_old` subfolders are under `../Archive/Outputs/`. The former `From Elisa` files are retained unchanged under `../Archive/Shared_with_Elisa_snapshot/`. Dates embedded in older export filenames do not necessarily identify their last execution date. Figures have not been visually compared or regenerated.
