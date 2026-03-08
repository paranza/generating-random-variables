# Gaussian–Gaussian Bayesian Analysis (R)

This project implements a **Bayesian analysis of the mean of a Gaussian distribution with known variance**, using a **Gaussian prior**.  
It serves as a didactic example of **conjugate Bayesian inference**, posterior updating, and posterior predictive analysis.

---

## Model Setup

We observe i.i.d. samples:
\[
X_1, \dots, X_n \sim \mathcal{N}(\theta, \sigma^2)
\]

- The variance \(\sigma^2\) is assumed **known**
- The mean \(\theta\) is **unknown**

A Gaussian prior is placed on \(\theta\):
\[
\theta \sim \mathcal{N}(v, \tau^2)
\]

Because the prior is conjugate to the likelihood, the posterior distribution of \(\theta\) is also Gaussian and can be computed analytically.

---

## What the Script Does

The R script performs the following steps:

1. **Simulates data** from a Gaussian distribution with unknown mean
2. **Computes the sample mean** (frequentist estimator)
3. **Defines a Gaussian prior** (informative or weakly informative)
4. **Computes the posterior distribution**:
   - Posterior mean
   - Posterior variance
5. **Derives the posterior predictive distribution** for a new observation
6. **Finds the value maximizing the predictive density**
7. **Visualizes**:
   - True data-generating distribution
   - Prior distribution
   - Posterior distribution

---

## Key Insights
![](output_gg.png)
- The posterior mean is a **weighted average** of the prior mean and the sample mean
- As the sample size increases:
  - The posterior variance shrinks
  - The posterior mean approaches the true parameter
- The influence of the prior decreases with more data
- The posterior predictive distribution incorporates both parameter uncertainty and observation noise

---

## Requirements

- R
- `ggplot2`

---

## Purpose

This script is intended for:
- Learning Bayesian statistics
- Understanding conjugate priors
- Exploring posterior and predictive distributions analytically and visually

It is not meant as a production library, but as a **clear, minimal, and reproducible example**.

---

## File

- `Anaylisis_Gaussian_Gaussian.R`
