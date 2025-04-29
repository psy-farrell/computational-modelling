---
layout: page
title: Errata
excerpt: "Computational modeling book by Farrell and Lewandowsky."
logo: true
---

This page lists errata organised by chapter and page, with credit to error-spotters. If you spot and error, please email [Simon Farrell](mailto:simon.farrell@uwa.edu.au).

### 1. Introduction

p. 8 and p. 16: In the captions of both the Figures 1.4 (on p.8) and 1.8 (on p.16), there is a typo in the title of the Nosofsky (1991) publication. "Tests of an exemplar mode..." should read: "Tests of an exemplar model..." [Thanks to Paolo Petta]

### 4. Maximum Likelihood Parameter Estimation

p. 83: The caption to Figure 4.5 is incorrect: the top panel of Figure 4.5 plots the probability density *p(t \| m)*. [Thanks to Daniel Kraußer]

p. 95: The first line should refer to parameters *m* and *a* of the Wald model (not parameters *m* and *s*). [Thanks to Daniel Kraußer]

### 5. Combining Information from Multiple Participants

pp. 110--111: Listing 5.1 and surrounding text refer to `dat` as having participants in rows, and data in columns. The reverse is true: each column is a participant, so that `dat` has `nobs` rows and `nsubj` columns. [Thanks to Chris Street of University of Huddersfield]

p. 116 "We initialize the mixing proportion (the proportion of data thought to belong to the first distribution, ppi)...": ppi is the proportion of data thought to belong to the *second* distribution. [Thanks to Kristin Javaras, McLean Hospital Harvard]

### 7. Bayesian Parameter Estimation

Listings 7.3 and 7.4 contain an error. Instead of `g*dunif(data, -180, 180)` in Listing 7.3, line 35, and Listing 7.4, lines 6-7, it should be `g*rep(1,length(data))/(2*pi)`. [A full explanation and correction]({{ site.url }}/errata/listing7374.pdf). [Thanks to Grant Shields of UC Davis]

### 9. Multilevel or Hierarchical Modelling

p. 213, Figure 9.5: The arrow from $$k_{ij}$$ to $$\theta_{ij}$$ in the figure should be reversed [Thanks to Víthor Rosa Franco]: 

![]({{ site.url }}/images/fig9.5corrected.png)

### References
The correct reference for Nosofsky (1986) is Nosofsky, R. M. (1986). Attention, similarity, and the identification-categorization relationship. *Journal of Experimental Psychology: General, 115,* 39--57. [Thanks to Paolo Petta]

