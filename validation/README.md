# validation/

Reproducible simulation studies for bayprior (not part of the built package; listed in `.Rbuildignore`).

- `sim_mixture.R`: Part D, calibration for mixture priors (robust, pooled), deterministic. Run `Rscript sim_mixture.R`.
- `sim_numerical.R`: Part E, calibration of the numerical path (lognormal, Weibull, exponential priors on a rate), with an independent predictive calculation and a Monte Carlo spot check. Slow (about 30 minutes). Run `Rscript sim_numerical.R`.
- `sim_power.R`: Part F, size-adjusted power (exact, mid-p, Normal approximation, boundary-safe approximation, equal-tail, likelihood-ratio oracle). Deterministic, about 1 minute.
- `sim_boxp.R`: Parts A and B of the analysis plan (calibration of the Box prior predictive p-value, exact vs Normal approximation; S-value summaries) plus two independent verification checks. Run `Rscript sim_boxp.R` from this directory. `BAYPRIOR_SRC` selects the package source (default `../bayprior`; when this folder sits in the package repo root use `BAYPRIOR_SRC=.. Rscript sim_boxp.R`). `REPS` sets replications per cell (default 20000).
- `results_boxp.csv` / `.rds`: one row per cell x alpha.
- `verification_mc.csv`: exact p-values vs brute-force Monte Carlo from the generative model.
- `fig_type1_error.png`: type I error heatmap.
- `REPORT_boxp.md`: results and interpretation.
- `sessionInfo_boxp.txt`: software versions used.

Seeds: `RNGkind("L'Ecuyer-CMRG"); set.seed(20261013)`.

## Provenance: what Parts A-F are and where they come from

The Parts are the sections of an analysis plan written **before** the simulations were run, to check whether the exact prior predictive Box p-value (new default in the development version) is better calibrated than the Normal approximation used in 0.4.0. They are labelled A-F in the order the questions were posed; the letters are not file names. Each Part maps to one script, one results file and one section of `REPORT_boxp.md`:

| Part | Question | Script | Output | Status |
|---|---|---|---|---|
| A | Type I error of the Box p-value, exact vs Normal approximation (Beta-Binomial, Gamma-Poisson) | `sim_boxp.R` | `results_boxp.csv/.rds`, `fig_type1_error.png` | run 2026-10-13 on bayprior 0.4.0 |
| B | S-value summaries and agreement between the methods, plus two independent verification checks | `sim_boxp.R` | `results_boxp.csv`, `verification_mc.csv` | run 2026-10-13 on bayprior 0.4.0 |
| C | MAP-prior benchmarks | none | none | **planned, not run, not reported** |
| D | Calibration for mixture priors (robust, pooled) | `sim_mixture.R` | `results_mixture.csv` | run 2026-10-15, development version |
| E | Calibration of the numerical (quadrature) path for lognormal, Weibull, exponential priors; independent predictive calculation; Monte Carlo spot check | `sim_numerical.R` | `results_numerical.csv`, `verification_numerical_mc.csv` | run 2026-10-15, development version |
| F | Size-adjusted power of exact, mid-p, Normal approximation, boundary-safe approximation, equal-tail and likelihood-ratio variants | `sim_power.R` | `results_power.csv`, `verification_power_mc.csv` | run 2026-10-15, development version |

Notes for a reviewer:
- Parts A and B ran on the released 0.4.0 code; D-F ran on the development version (0.4.0.9000). Check `sessionInfo_boxp.txt` and the header of each script for the exact source used.
- Part C is listed only so the lettering stays stable; there is nothing to review for it.
- Every method compared is computed with the package's own functions, except where a script says otherwise (the independent predictive calculations in Parts B and E, and the candidate "boundary-safe" approximation, which is not implemented in the package).
- Seeds are fixed in each script; deterministic Parts (D, F) need no seed.
- `../validation_pilot/pilot.R`, if present, is the small pilot used to size the grid; it is not part of the reported results.
- The report is a working document and has not been peer reviewed.