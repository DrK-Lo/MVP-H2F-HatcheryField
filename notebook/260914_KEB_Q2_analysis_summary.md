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
The genetic and environmental predictors vary only across source populations (8, or 10 for Q2c), not across individual bags or oysters. Previous analysis treated each bag or individual as independent. I changed the models to include a random effect at the population level so that population-level predictors are tested against the number of independent source populations.

- Survival: `response ~ predictors + (1|Population)`
- Length & Condition index: `response ~ predictors + (1|Population/Bag)`

**2. No stepwise selection.**
All predictors are retained in every model rather than dropped by `step()`/`drop1()`. We have mostly clean collinearity now (below).

**3. Temperature Q10 excluded from Q2a.**
Of the four environmental quantiles, I dropped temperature Q10 to keep VIF lower. The retained Q2a set is temperature Q90, salinity Q10, salinity Q90 (plus HO and Ar).

**4. Low statistical power**
Because predictors vary across only 8–10 source populations, the degrees of freedom for population-level terms are low (= 2 for Q2a/Q2b, = 7 for Q2c). Confidence intervals are also wide.

**5. P-values and confidence intervals on figures are aligned**
P-values come from Satterthwaite-approximated t-tests (lmerTest). Figure error bars are t-based 95% confidence intervals using the same degrees of freedom, so a bar crossing zero corresponds to a non-significant effect.

---

## Q2a — Genetic diversity + environmental quantiles (monocultures)

_Model: `response ~ HO + Ar + TemperatureQ90 + SalinityQ10 + SalinityQ90 + (1|Population[/Bag])`

**Temperature Q90.** It was positive and among the strongest predictors in all six survival models (both sites, all timepoints). No other predictor showed a strong effect on survival once population-level replication was accounted for.

**Length** No predictor was significant for length. Warm-origin populations both survive better and trend larger, but only the survival effect is strong enough to detect.

**Condition index** showed no consistent significant predictor.

### Figures — Q2a

_Survival, Lewisetta (Fig 3A):_

<img width="758" height="429" alt="Screenshot 2026-09-14 at 10 24 16 PM" src="https://github.com/user-attachments/assets/2356f3ba-f2f5-46e9-80d3-13d9a69b0683" />

_Survival, York River (Fig 3C):_

<img width="900" height="500" alt="Screenshot 2026-09-14 at 10 24 41 PM" src="https://github.com/user-attachments/assets/88440698-016b-421f-961d-62159e520536" />

_Length, Lewisetta / York (Fig S5A / S5C):_

<img width="881" height="498" alt="Screenshot 2026-09-14 at 10 25 05 PM" src="https://github.com/user-attachments/assets/1fecfd19-6d44-4b9b-9a3c-e8fd800b8b55" />
<img width="884" height="502" alt="Screenshot 2026-09-14 at 10 25 24 PM" src="https://github.com/user-attachments/assets/d52524ed-b9ae-4982-8ddb-1d35cfab2449" />

_Correlation matrix (Fig S2):_

<img width="646" height="535" alt="Screenshot 2026-09-14 at 10 25 43 PM" src="https://github.com/user-attachments/assets/22c12582-c18d-415e-894c-c0f810519cd0" />

---

## Q2b — Genetic diversity + environmental distance (monocultures)

_Model: `response ~ HO + Ar + [environmental distance] + (1|Population[/Bag])`

This approach collapses the four environmental quantiles into a single measure (Euclidean distance between each source population's environment-of-origin and the field site).

**Collinearity:** heterozygosity and environmental distance are moderately correlated (r = −0.71 to −0.78; VIF ≤ 3.4). This means the diversity and distance effects share variance and are harder to fully separate from each other.

### Figures — Q2b

_Survival, Lewisetta / York (Fig 3B / 3D):_

<img width="897" height="506" alt="Screenshot 2026-09-14 at 10 29 22 PM" src="https://github.com/user-attachments/assets/d8a5550e-7f0f-45f0-89d6-dd46f628f65a" />
<img width="884" height="497" alt="Screenshot 2026-09-14 at 10 29 36 PM" src="https://github.com/user-attachments/assets/46462783-3595-4aee-a357-01041912782a" />

_Length, Lewisetta / York (Fig S5B / S5D):_

<img width="882" height="501" alt="Screenshot 2026-09-14 at 10 29 58 PM" src="https://github.com/user-attachments/assets/a9c74b5e-654e-4301-b2d7-ea6789916c75" />
<img width="887" height="495" alt="Screenshot 2026-09-14 at 10 30 11 PM" src="https://github.com/user-attachments/assets/3c53b578-e626-48ee-b7f6-7bda3b42b6d1" />

_Distance correlation matrices (Lewisetta / York):_

<img width="677" height="652" alt="Screenshot 2026-09-14 at 10 30 41 PM" src="https://github.com/user-attachments/assets/c498dd64-b956-4c33-86f7-abc739469afb" />
<img width="674" height="628" alt="Screenshot 2026-09-14 at 10 30 58 PM" src="https://github.com/user-attachments/assets/c11a7dae-6c5d-4b7d-bc80-5b8fc5b35e56" />

---

## Q2c — Genetic diversity alone (monocultures + polycultures)

_Model: `response ~ HO + Ar + (1|Population[/Bag])`

This is the only approach that includes the polyculture groups (HYBRIDMIX, SEEDMIX), since it uses genetic diversity alone. The two mix groups have the highest heterozygosity values in the dataset.

**Genetic diversity shows no direct effect on any response.** Across all 18 models (3 responses × 2 sites × 3 timepoints), neither HO nor Ar was significant for survival or condition index at any site or time. This is consistent with Q2a and Q2b: genetic diversity was not a driver of field performance once population-level replication was handled.

### Figures — Q2c

_Survival, Lewisetta / York:_

<img width="885" height="498" alt="Screenshot 2026-09-14 at 10 32 24 PM" src="https://github.com/user-attachments/assets/5174dd64-a9b1-412d-b454-03923825e801" />
<img width="884" height="504" alt="Screenshot 2026-09-14 at 10 32 48 PM" src="https://github.com/user-attachments/assets/e85208e9-cdcf-4f4f-9396-cba68335e3db" />

_Length, Lewisetta / York:_

<img width="882" height="498" alt="Screenshot 2026-09-14 at 10 33 06 PM" src="https://github.com/user-attachments/assets/d8b93391-72ec-4ff7-b16c-53dcbdeabe38" />
<img width="883" height="495" alt="Screenshot 2026-09-14 at 10 33 18 PM" src="https://github.com/user-attachments/assets/1dc26b4f-3621-47a8-a261-b0cb5b9ed62c" />

_Condition index, Lewisetta / York:_

<img width="894" height="504" alt="Screenshot 2026-09-14 at 10 33 43 PM" src="https://github.com/user-attachments/assets/cbaec290-b358-41ca-bd79-618cb8721a18" />
<img width="882" height="503" alt="Screenshot 2026-09-14 at 10 33 55 PM" src="https://github.com/user-attachments/assets/67e14192-fe86-47dc-a389-23a05823d455" />

---

## Overall takeaways

1. **High temperature at environment-of-origin is the clearest signal.**

2. **Genetic diversity effects are not detectable.**

3. **The design limits power.**

4. **Results are robust to how environment is represented.** The quantile (Q2a) and distance (Q2b) models are consistent.

---

_Model results files are in `results/juvenile_effects_H2F/models/`; figures in `figures/juvenile_effects_H2F/`._
