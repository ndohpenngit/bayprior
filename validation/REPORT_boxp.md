# Calibration of the Box prior predictive p-value in bayprior

_Working report, not peer reviewed. Parts A and B were first run on 2026-10-13 on bayprior 0.4.0 (the released CRAN version); Parts D to F were added on 2026-10-15 and run on the development version. Code: `sim_*.R`. Part C (MAP benchmarks) is not included._

## Design
- **Families:** Beta prior with binary data (Beta-Binomial); Gamma prior with Poisson counts (Gamma-Poisson). These are the two combinations with an exact prior predictive check.
- **Grid:** Beta: prior mean 0.1/0.3/0.5, prior strength (effective sample size) 2/5/10/20/50, n = 5/10/20/50/100/500. Gamma: prior mean 0.1/1, shape 1/2/5/20/50, exposure n = 5/10/20/50/100/500. 150 no-conflict cells.
- **Scenarios:** no conflict (true parameter drawn from the prior); conflict (true parameter fixed at the prior mean +/- 0.5, 1, 2, 3 prior SDs; cells where the shifted value is outside the parameter space are skipped).
- **Replications:** 20,000 per cell (Monte Carlo SE of a rejection rate near 0.05: 0.0015).
- **Methods compared** (all computed with the package's own functions; the harness reproduced `prior_conflict()` exactly on 200 random cells):
  1. *Exact*: prior predictive p-value with outcomes ordered by probability (`exact = TRUE`).
  2. *Normal approximation*: the 0.4.0 default (`exact = FALSE`).
  3. *Boundary-safe approximation* (candidate only, not implemented in the package): same approximation but with the sampling SE computed from (x + 0.5)/(n + 1), or sqrt(x + 0.5)/n for counts.
- **Verification:** exact p-values agreed with a brute-force Monte Carlo from the generative model (4 million draws per configuration; outcomes ordered by an independently coded closed-form pmf) to within 0.0006 across 63 outcomes (MC SE <= 0.00025).

## Main result: type I error (no conflict, alpha = 0.05)
| Method | Mean rejection rate | Worst cell | Cells above 0.05 + 3 MC SE |
|---|---|---|---|
| Exact | 0.037 | 0.052 | 0 of 150 |
| Normal approximation (0.4.0 default) | 0.079 | 0.625 | 52 of 150 (35%) |
| Boundary-safe approximation (candidate) | 0.046 | 0.225 | 36 of 150 (24%) |

At alpha = 0.10 the pattern is the same (exact worst cell 0.105; approximation worst cell 0.628; 40 of 150 cells above 0.10 + 3 MC SE). See `fig_type1_error.png`.

Where the approximation fails: informative priors (Beta prior strength >= 10; Gamma shape >= 5) with small samples. The failures concentrate in cells with a low prior mean, where zero events or counts are probable. For Gamma-Poisson with shape >= 5, the worst-cell rejection rates under no conflict were 0.62 at exposure 5, 0.41 at 10 and 0.19 at 20 (other cells in that range are much closer to nominal, e.g. 0.05 at exposure 5 for prior mean 1). For Beta-Binomial with strength 50 the worst cells were 0.62 (n = 5), 0.39 (n = 10) and 0.18 (n = 20), all at prior mean 0.1 or 0.3. From n = 50 onward no method exceeds 0.075 in any cell tested (largest: the approximation, Gamma-Poisson, n = 100).

Mechanism (verified on one cell: Beta(5, 45), n = 10): zero events has prior predictive probability 0.38. The approximation's SE is estimated from the observed proportion, so it is zero at x = 0 and the approximate p-value is 0.017; the exact p-value is 1. That single outcome (probability 0.38) accounts for essentially all of the approximation's 38.7% rejection rate in this cell (exact: 2.2%). The boundary-safe SE removes most of this for Gamma-Poisson but not for Beta-Binomial with strength 50, prior mean 0.3, at n = 5 (0.225).

The exact version is conservative (mean 0.037 at nominal 0.05) because the data are discrete; it never exceeded nominal beyond Monte Carlo error.

## Power under conflict
At small shifts (up to 1 prior SD) every method has little power, as expected for a prior predictive check. At 2 SDs the average rejection rates across cells were 0.27 (exact) and 0.34 / 0.25 (approximation, Beta / Gamma); at 3 SDs 0.58 / 0.41 (exact) vs 0.75 / 0.67 (approximation) for downward shifts. **These are not like-for-like**: the approximation is not size-controlled, so some of its apparent power is spurious. Example (Beta(5, 45), n = 10, true value 2 SD below the prior mean, 0.016): approximation "power" 0.85, exact 0.00, because 0 events in 10 is entirely consistent with both the prior and the true value; the approximation flags it for the wrong reason. A fair power comparison needs size-adjusted power; see Part F below.

## Agreement between the methods
Proportion of datasets where the exact and approximate versions disagree on rejection at 0.05 (no-conflict scenarios, mean over cells):

| n | 5 | 10 | 20 | 50 | 100 | 500 |
|---|---|---|---|---|---|---|
| Beta-Binomial | 0.121 | 0.064 | 0.031 | 0.012 | 0.007 | 0.006 |
| Gamma-Poisson | 0.217 | 0.144 | 0.068 | 0.025 | 0.017 | 0.005 |

Part of the difference at large n is definitional: the exact p orders outcomes by probability, the approximation by distance from the mean, so they differ for skewed priors at any n (see the package documentation of the exact check). Mean absolute p-value differences should therefore not be read as an error measure.

## S-value (exploratory)
Under no conflict, the median S-value (S = -log2 p) from the exact p ranged from 0 to about 1.0 bit across cells (0 when the median p is 1, which happens with discrete data; the 1 bit of a continuous uniform p is reached only approximately). From the approximation, the median ranged from 0.5 to 5.9 bits for Beta-Binomial and from 0.8 to 39 bits for Gamma-Poisson (the largest values at exposure 5, the zero-count artefact: the approximate p is essentially 0 at x = 0). These are cell-level medians over the 150 no-conflict cells. Use S-values descriptively; they inherit the calibration of the p-value they are derived from.

## Limitations
- Parts A and B cover Beta-Binomial and Gamma-Poisson; mixtures in Part D, lognormal/Weibull/exponential priors in Part E, size-adjusted power in Part F (all below). Not covered: continuous data with non-Normal priors, sequential settings, Part C.
- Fixed shifts in prior SD units; no continuous data type; no sequential or interim-analysis settings.
- Discrete data: the exact test is conservative (mean size 0.0375 at nominal 0.05 over the Part F grid), so its unadjusted power is lower than a randomised or mid-p version would give; mid-p is not size-controlled (see Part F).
- The boundary-safe approximation is one of several possible corrections; it was not tuned.
- Parts A and B were run on the released 0.4.0 code (`exact = TRUE`/`FALSE` as then implemented); Parts D to F on the development version (0.4.0.9000), whose default and implementation differ (see NEWS).
- Part C (MAP prior against RBesT/bayesmeta) has not been run.

## Consequence for the package
In the development version (0.4.0.9000), `prior_conflict()` uses the prior predictive p-value (probability-ordered, the Box / Evans-Moshonov check) by default for discrete data: closed form for Beta-Binomial and Gamma-Poisson and for linear mixtures of them (robust and pooled priors), and numerical integration for other proper priors on the parameter space (Parts D and E). The result reports `box_pvalue_method` ("exact", "numerical" or "normal_approx") and, when the Normal approximation is used with discrete data in a boundary or small-count situation, `box_pvalue_caution`. Severity uses one rule on either path: severe if p < 2*pnorm(-3) (about 0.0027), mild if p < alpha. `exact = FALSE` gives the Normal approximation of 0.4.0 (with the caution flag). Continuous data keep the Normal approximation.

## Part D: mixture priors (added in the development version)
Script `sim_mixture.R`, results `results_mixture.csv`. Deterministic calculation (no Monte Carlo error): under theta drawn from the mixture, the rejection probability is the sum of the prior predictive pmf over outcomes whose p-value is below alpha. 240 cells: robust prior (Beta + Normal vague component, weight 0.2) and a 50/50 pooled consensus of two Betas, prior means 0.1/0.3/0.5, strengths 5-50, n 5-100, alpha 0.05/0.10.

- Prior predictive p-value (exact for the Beta mixture, quadrature with a truncated Normal component for the robust prior): maximum rejection 0.0499 at alpha 0.05 and 0.1000 at alpha 0.10; mean 0.038 / 0.079. Never above nominal.
- Normal approximation: pooled consensus up to 0.433 at alpha 0.05 (24 of 60 cells above 0.05; 0.235 at n = 10); robust prior up to 0.057 (4 of 60 cells), because the wide vague component inflates the mixture SD.
- Caveat: for the robust prior the generative model is the package's own prior (Normal vague component truncated to [0, 1]); calibration is against that reading.

## Part E: lognormal, Weibull and exponential priors (numerical path)
Script `sim_numerical.R`; results `results_numerical.csv`, `verification_numerical_mc.csv`. Priors on a rate with Poisson/survival data, prior mean 0.1 or 1, CV 0.25/0.5/1.5 (lognormal, Weibull; exponential has CV 1), exposure 5-500: 84 cells. Rejection probability under no conflict = sum over outcomes of the prior predictive probability times the indicator that p < alpha. The predictive probabilities used as truth were computed independently of the package (log-scale trapezoid rule, 40,001 points).

- Prior predictive p-value (package numerical path): maximum rejection 0.0500 (0.04996 at the largest) at alpha 0.05 and 0.1000 at alpha 0.10; never above nominal in any of the 84 cells. Mean 0.044 / 0.089.
- Normal approximation: above nominal in 25 of 84 cells at alpha 0.05 (18 at 0.10); worst 0.626 (Weibull, prior mean 0.1, CV 0.5, exposure 5; lognormal 0.624), 0.41 at exposure 10, 0.21 at exposure 20 (maxima over cells). Exponential priors are close to nominal (largest 0.052).
- Agreement with the independent calculation: the package predictive pmf differs by at most 1.5e-5 (absolute). The resulting p-values differ by a median of 3e-5 per cell, but by up to 0.005 in the broadest predictive (lognormal, CV 1.5, exposure 500), where many outcomes have nearly equal probability and a 1e-5 error reorders them. A decision at alpha is affected only if p lies within that distance of alpha.
- `prior_conflict()` reproduces the table (30 random draws, difference 0, method "numerical" in all). Monte Carlo (4 million draws, three cells) agrees with the deterministic rejection probability within 0.0001 (about 1 Monte Carlo SE).
- Cost: the quadrature grid has 4,000 points, so run time grows with the number of possible counts. Measured in the sandbox: about 0.07 s for exposure 100 with a mean count near 10; 2-4 s when the count support reaches several thousand; 8.7 s for a Weibull prior with CV 1.5 at exposure 500 (support 10,177). In that regime the Normal approximation is also adequate (Part E table above: within about 0.01 of nominal from exposure 100).
- Scope: Poisson/survival data, priors on a rate. A Normal or lognormal prior on a probability with binary data uses the same code, truncated to [0, 1], but was not simulated here.

## Part F: size-adjusted power
Script `sim_power.R`; results `results_power.csv`, `verification_power_mc.csv`. Same 150-prior x 6-n grid as Part A/B, shifts of +/-0.5, 1, 2, 3 prior SDs (984 valid cell-shift combinations per alpha), alpha 0.05 and 0.10. All quantities are deterministic sums over outcomes; the script's predictive pmf and p-values match `prior_conflict()` (maximum differences 1e-14 and 5e-13) and a Monte Carlo spot check agrees (power and size within Monte Carlo error).

Methods. exact (package default), mid-p (exact p minus half the point probability), approx (0.4.0 default), approxB (boundary-safe approximation), tail (equal-tail p = 2 min(P(X<=x), P(X>=x)), an alternative definition), and an oracle that orders outcomes by the likelihood ratio for the specific alternative (the best possible test of that alternative; an upper bound, not a usable method). "Size-adjusted" means each method's ordering of outcomes with a randomised cutoff so that its size under no conflict equals alpha exactly, which compares the orderings at equal size.

**Size under no conflict (as the package would flag, alpha 0.05, mean / max, share of rows above nominal):** exact 0.0375 / 0.0500 / 0%; mid-p 0.0440 / 0.0679 / 45%; approx 0.0857 / 0.6215 / 49%; approxB 0.0482 / 0.2196 / 38%; equal-tail 0.0297 / 0.0499 / 0%. At alpha 0.10: exact 0.0810 / 0.1000, mid-p max 0.1318, approx max 0.6245, approxB max 0.2490, equal-tail 0.0649 / 0.1000. Mid-p is closer to nominal on average but is not size-controlled.

**Unadjusted power at alpha 0.05 (mean over all shifts):** exact 0.178, mid-p 0.193, approx 0.233, approxB 0.194, equal-tail 0.140. The approximation's apparent advantage is mostly its inflated size.

**Size-adjusted power at alpha 0.05 (mean over 984 combinations):**

| | exact | approx | approxB | equal-tail | oracle |
|---|---|---|---|---|---|
| all | 0.202 | 0.199 | 0.202 | 0.190 | 0.360 |
| Beta-Binomial | 0.204 | 0.200 | 0.202 | 0.195 | 0.371 |
| Gamma-Poisson | 0.199 | 0.198 | 0.201 | 0.183 | 0.344 |

(alpha 0.10: exact 0.295, approx 0.283, approxB 0.288, equal-tail 0.282, oracle 0.502.)

By shift (alpha 0.05, size-adjusted):

| shift (prior SD) | -3 | -2 | -1 | -0.5 | +0.5 | +1 | +2 | +3 |
|---|---|---|---|---|---|---|---|---|
| exact | 0.529 | 0.303 | 0.014 | 0.009 | 0.021 | 0.041 | 0.326 | 0.671 |
| approx | 0.593 | 0.374 | 0.027 | 0.013 | 0.013 | 0.027 | 0.299 | 0.615 |
| equal-tail | 0.543 | 0.361 | 0.017 | 0.008 | 0.018 | 0.036 | 0.235 | 0.645 |
| oracle | 0.648 | 0.517 | 0.196 | 0.156 | 0.196 | 0.255 | 0.495 | 0.702 |

What this shows.
1. **At equal size the exact p-value costs no power overall** (0.202 vs 0.199 for the approximation; 0.202 for the boundary-safe version). The approximation's higher unadjusted power comes from its excess false alarms.
2. **The comparison is direction dependent.** For shifts below the prior mean, the approximation's ordering has more power (+0.064 at -3 SD, +0.071 at -2 SD on average); for shifts above, the exact ordering has more (-0.027 at +2, -0.056 at +3). For the symmetric Beta prior with mean 0.5 the two are identical (difference 0.000). The difference comes from skewed priors: the exact p orders outcomes by predictive probability, and for a right-skewed predictive low counts (including zero) are probable, so they are not treated as extreme even when the true value is far below the prior mean. Example: Beta(5, 45), n = 10, true value 2 SD below the prior mean: exact power 0.000, size-adjusted approximation 0.102, oracle 0.112. For such cases the check has almost no power whatever the method; the approximation's apparent 0.85 was spurious.
3. **The equal-tail alternative is not better overall** (0.190; smaller size, mean 0.030, so more conservative). It recovers part of the downward-shift power (0.361 vs 0.303 at -2 SD) and loses a lot upward (0.235 vs 0.326 at +2 SD).
4. **Power is low in absolute terms and far from the oracle** (0.20 vs 0.36 on average; at |shift| <= 1 SD the mean size-adjusted power of every method is below 0.05, the maximum in any row is 0.11, while the oracle averages 0.16-0.26). A non-significant prior predictive check is weak evidence that a prior is adequate; this should be stated in the documentation and the paper. In rows where the oracle power exceeds 0.5 (266 of 984), exact 0.563, approximation 0.573, oracle 0.819.
5. **Mid-p** gives higher unadjusted power than exact (0.193 vs 0.178) with a size above nominal in 45% of rows (maximum 0.068). Its ordering is identical to exact, so its size-adjusted power is the same. It is a trade of calibration for sensitivity, not a free gain.

Decision implications (not implemented). The exact probability-ordered p-value is a sound default: size-controlled, no overall power loss. Its main weakness is directional (downward shifts against skewed priors). Options if that matters: report a one-sided/tail-area companion, or document the limitation. Switching the definition to equal-tail is not supported by these results.

## Part E/F limitations
- Fixed shifts in prior-SD units against a point alternative; no composite or sequential alternatives.
- The oracle depends on the alternative being known and is not attainable; the gap to it is a ceiling on what any check could do, not a defect of the package.
- Part E covers Poisson/survival data only; Normal/lognormal priors on a probability with binary data were not simulated.
- Part F uses the Part A/B grid, so conclusions apply to those prior strengths and sample sizes.
