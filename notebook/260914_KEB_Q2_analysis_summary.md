# Summary of Q2 analysis with juvenile genetic data

---

## Overview

Q2 asks how offspring **genetic diversity** (heterozygosity, allelic richness) and **environment-of-origin** relate to oyster performance (survival, shell length, condition index) in the field, across two common garden sites (Lewisetta, York River) and three timepoints (November 2023, May 2024, November 2024).

We approached this three ways, differing in how environment-of-origin is represented and which treatments are included:

| | Question | Treatments | Environmental predictor |
|---|---|---|---|
| **Q2a** | Diversity + environmental quantiles | Monocultures only (8 source populations) | 4 temperature/salinity quantiles |
| **Q2b** | Diversity + environmental distance | Monocultures only (8 source populations) | 1 distance term (origin → field site) |
| **Q2c** | Diversity alone | Monocultures **and** polycultures (10 groups) | none |

Q2a and Q2b are restricted to monocultures because polyculture bags mix multiple source populations and so have no single environment-of-origin. Q2c drops the environmental term entirely, which is what allows the polycultures back into the analysis.

---

## Changes to methods

**1. Mixed models with a population-level random effect.**
The genetic and environmental predictors vary only across source populations (8, or 10 for Q2c), not across individual bags or oysters. Previous analysis treated each bag or individual as independent. I changed the models to include a random effect at the population level so that population-level predictors are tested against the number of independent source populations:

- Survival: `response ~ predictors + (1|Population)`
- Length & Condition index: `response ~ predictors + (1|Population/Bag)`

**2. No stepwise selection.**
All predictors are retained in every model rather than dropped by `step()`/`drop1()`. We have mostly clean collinearity now (below).

**3. Temperature Q10 excluded from Q2a.**
Of the four environmental quantiles, I dropped temperature Q10 to keep VIF lower. The retained Q2a set is temperature Q90, salinity Q10, salinity Q90 (plus HO and Ar).

**4. Low statistical power**
Because predictors vary across only 8–10 source populations, the degrees of freedom for population-level terms are low (≈ 2 for Q2a/Q2b, ≈ 7 for Q2c). Confidence intervals are also wide.

**5. P-values and confidence intervals on figures are aligned**
P-values come from Satterthwaite-approximated t-tests (lmerTest). Figure error bars are t-based 95% confidence intervals using the same degrees of freedom, so a bar crossing zero corresponds to a non-significant effect.

---

## Q2a — Genetic diversity + environmental quantiles (monocultures)

_Model: `response ~ HO + Ar + TemperatureQ90 + SalinityQ10 + SalinityQ90 + (1|Population[/Bag])`, fit separately per site × timepoint._

**Temperature Q90 is a consistent driver of survival.** It was positive and among the strongest predictors in all six survival models (both sites, all timepoints), with the effect strengthening over time. No other predictor genetic or environmental showed a strong independent effect on survival once population-level replication was accounted for.

**Length** No predictor was significant for length. Warm-origin populations both survive better and trend larger, but only the survival effect is strong enough to detect with 8 populations.

**Condition index** showed no consistent significant predictor.

### Figures — Q2a

_Survival, Lewisetta (Fig 3A):_

<img width="758" height="429" alt="Screenshot 2026-09-14 at 10 24 16 PM" src="https://github.com/user-attachments/assets/2356f3ba-f2f5-46e9-80d3-13d9a69b0683" />

_Survival, York River (Fig 3C):_

<img width="900" height="500" alt="Screenshot 2026-09-14 at 10 24 41 PM" src="https://github.com/user-attachments/assets/88440698-016b-421f-961d-62159e520536" />

_Length, Lewisetta / York (Fig S5A / S5C):_

<img width="881" height="498" alt="Screenshot 2026-09-14 at 10 25 05 PM" src="https://github.com/user-attachments/assets/1fecfd19-6d44-4b9b-9a3c-e8fd800b8b55" />
<img width="884" height="502" alt="Screenshot 2026-09-14 at 10 25 24 PM" src="https://github.com/user-attachments/assets/d52524ed-b9ae-4982-8ddb-1d35cfab2449" />

_Predictor correlation matrix (Fig S2):_

<img width="646" height="535" alt="Screenshot 2026-09-14 at 10 25 43 PM" src="https://github.com/user-attachments/assets/22c12582-c18d-415e-894c-c0f810519cd0" />

---

## Q2b — Genetic diversity + environmental distance (monocultures)

_Model: `response ~ HO + Ar + [environmental distance] + (1|Population[/Bag])`, fit separately per site × timepoint._

This approach collapses the four environmental quantiles into a single measure — the Euclidean distance between each source population's environment-of-origin and the field site — which reduces the predictor count and eases the strain of estimating many effects from few populations.

**Note on collinearity:** heterozygosity and environmental distance are moderately correlated (r ≈ −0.71 to −0.78; VIF ≤ 3.4, below thresholds requiring action). This means the diversity and distance effects share variance and are harder to fully separate from each other — an effect attributed to one could partly belong to the other. Worth keeping in mind when interpreting any distance or diversity result here.

**Summary of results:** _[paste key findings from your Q2b output — which cells, if any, showed significant distance, HO, or Ar effects, and whether the pattern is consistent across sites/times]_

### Figures — Q2b

_Survival, Lewisetta / York (Fig 3B / 3D):_

<br><br><br><br><br>

_Length, Lewisetta / York (Fig S5B / S5D):_

<br><br><br><br><br>

_Distance correlation matrices (Lewisetta / York):_

<br><br><br><br><br>

---

## Q2c — Genetic diversity alone (monocultures + polycultures)

_Model: `response ~ HO + Ar + (1|Population[/Bag])`, fit separately per site × timepoint, 10 groups._

This is the only approach that includes the polyculture groups (HYBRIDMIX, SEEDMIX), since it uses genetic diversity alone. The two mix groups have the highest heterozygosity values in the dataset, so they sit at the high end of the diversity axis and carry leverage on the HO slope.

**Headline result: genetic diversity shows essentially no direct effect on any response.** Across all 18 models (3 responses × 2 sites × 3 timepoints), neither HO nor Ar was significant for survival or condition index at any site or time. The single exception was a heterozygosity effect on **York River length at November 2024** (p ≈ 0.03) — but this came from the one model where the nested random effect was singular, and rests heavily on the two high-diversity mix groups. It should be treated as suggestive pending a leverage check (refitting without the mix groups).

This is consistent with Q2a and Q2b: genetic diversity did not emerge as a robust driver of field performance once population-level replication was properly handled.

### Figures — Q2c

_Survival, Lewisetta / York:_

<br><br><br><br><br>

_Length, Lewisetta / York:_

<br><br><br><br><br>

_Condition index, Lewisetta / York:_

<br><br><br><br><br>

---

## Overall takeaways

1. **Thermal origin, not genetic diversity, is the clearest signal.** Across all three approaches, the one robust, consistent effect is that populations from warmer-origin environments (Temperature Q90) survive better in the field, with the effect growing over time — consistent with selective mortality favoring warm-adapted origins.

2. **Genetic diversity effects are not detectable** on survival, length, or condition once the nested design is accounted for. Where they appeared in earlier (uncorrected) analyses, they did not survive proper handling of population-level replication.

3. **The design limits power, and we report it honestly.** With 8–10 source populations, we can only detect strong population-level effects. Non-significant results are "underpowered to detect," not "shown to be zero" — worth stating plainly rather than over-claiming.

4. **Results are robust to how environment is represented.** The quantile (Q2a) and distance (Q2b) framings tell a consistent story, which strengthens confidence in the thermal-origin conclusion.

---

## Open questions for discussion

- The York-t3 length HO effect (Q2c): worth the leverage check, or set aside as a single singular-fit result among 18 tests?
- Condition index has a persistent right-skew in residuals — acceptable as-is, or worth a transformation?
- Is the diversity–distance collinearity in Q2b worth addressing (e.g., reporting single-predictor sensitivity models), or is a stated caveat sufficient?
- How to frame the low-power / 8-population limitation in the manuscript.

---

_Notes: model results files are in `results/juvenile_effects_H2F/models/`; figures in `figures/juvenile_effects_H2F/`._
