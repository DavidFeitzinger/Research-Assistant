# Wealth DiD + Age Modelling

Repository layout checked on **2026-11-09**. The reorganization changes filenames and Git locations only; no Stata estimators, data, or numeric results were modified or rerun.

## Where to start

- **[`Scripts/Current/`](Scripts/Current)** contains the three working Stata scripts, saved as of 2026-11-09: child wealth trajectories, parent wealth trajectories, and child inheritance DiD.
- **[`Results_new/`](Results_new)** is the live output destination used by the current scripts. See its README for primary workbooks and intermediate outputs.
- **[`Scripts/Other_analyses/`](Scripts/Other_analyses)** has *separate* intergenerational transition and alternative inheritance-DiD analyses, not sequential revisions of the trajectory models.
- **[`Results/`](Results)** holds outputs for the separate transition and inheritance-DiD workstreams. Its path is preserved because those scripts write there.
- **[`Data/`](Data)** contains input and intermediate data; it was not reorganized.
- **[`Scripts/Archive/`](Scripts/Archive)** and **[`Archive/`](Archive)** retain early scripts, prior outputs, and the historical copy of files shared with Elisa. No original file contents were deleted.

## Main output files

| File | Meaning | Script |
| --- | --- | --- |
| [`Results_new/Child_wealth_byage_lowess_fan_2026_26_05.xlsx`](Results_new/Child_wealth_byage_lowess_fan_2026_26_05.xlsx) | Observed child LOWESS fan trajectories | `Scripts/Current/Wealth_Child_Traj_2026-11-09.do`, lines 1593-1709 |
| [`Results_new/Parent_wealth_byage_lowess_fan_extrapolated100_2026_26_05.xlsx`](Results_new/Parent_wealth_byage_lowess_fan_extrapolated100_2026_26_05.xlsx) | Parent LOWESS fan trajectories extrapolated to age 100 | `Scripts/Current/Wealth_Parent_Traj_2026-11-09.do`, lines 1562-1735 |

These two workbooks are byte-identical to the corresponding final copies in the archived shared-with-Elisa snapshot. The child script also performs extrapolations and generates plots; the named child workbook above is its **observed-only** fan export.

## Versioning conventions

- Script snapshots and archive folders use ISO dates (`YYYY-MM-DD`). **2026-11-09** is the date of the current script snapshot, **not** evidence that estimates were recalculated then.
- Older scripts have the original date as recorded in their filename. `early_undated` and `undated` intentionally avoid guessing an unrecorded creation date.
- Output filenames are preserved where they are referenced by Stata code. Legacy suffixes such as `_2026_26_05` mean **26 May 2026**, while `_2026_08_05` mean **8 May 2026**. They are not ISO-formatted.
- A file in `Archive/Outputs` is an older, duplicate, or alternative stored output, not necessarily proven statistically inferior. The original relative paths are recorded in [`Archive/FILE_MOVES_2026-11-09.tsv`](Archive/FILE_MOVES_2026-11-09.tsv).

## Re-running

The Stata scripts retain their **original absolute Windows/Dropbox paths**, including `global datapath` and `global resultspath`. If your local project is now inside `RA/Research-Assistant-Git/Wealth DiD + Age Modelling`, update those path settings before executing a script. Archived scripts may also need output-path adjustments because historic result folders were moved. Files here were checked by matching script exports and Git blob hashes, **not** by rerunning Stata.
