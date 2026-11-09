# Stata scripts and version history

## Current (saved on 2026-11-09)

- `Current/Wealth_Child_Traj_2026-11-09.do`: child wealth trajectories, outlier handling, regressions, LOWESS smoothing, fan bands, observed-only Excel export; also extrapolation to 100 in intermediate calculations and figures.
- `Current/Wealth_Parent_Traj_2026-11-09.do`: parent wealth trajectories, trimming, LOWESS fan bands, age-100 extrapolation, final Excel export.
- `Current/Wealth_Child_DiD_2026-11-09.do`: current available child inheritance DiD. Relative to `Archive/2026-05-02/Wealth_Child_DiD_2026-05-02.do`, the **only** code difference is line 13: output directory `Results_old` changed to `Results_new`. No estimation procedure was updated.

## Prior versions

- `Archive/2026-05-02/`: the three earlier May 2 trajectory/DiD scripts, which precede the LOWESS fan-chart exports in the current trajectory scripts.
- `Archive/early_undated/`: original undated child and parent trajectory scripts, which reference April 21 input/output names. Their exact creation dates are unknown.

## Different analyses, not versions

- `Other_analyses/Intergenerational_transitions/`: parent-to-child wealth quartile transition matrices, May 19 and weighted May 20 variants.
- `Other_analyses/Inheritance_DiD_alternatives/`: distinct, historically undated inheritance-DiD/age-model code paths (`Samuel_Wealth_Inh_DiD` and `Wealth_Inh_DiD_age_model`). These were **not** established to be direct versions of `Wealth_Child_DiD`.

Note: dates on script snapshots reflect filenames, and paths in the code remain as originally written. Moving the scripts does not change their Stata logic. Output locations remain configured inside each script.
