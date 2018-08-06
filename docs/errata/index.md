---
layout: page
title: Errata
excerpt: "Computational modeling book by Farrell and Lewandowsky."
logo: true
---

This page lists errata organised by chapter and page, with credit to error-spotters. If you spot and error, please email [Simon Farrell](mailto:simon.farrell@uwa.edu.au).

### 7. Bayesian Parameter Estimation

Listings 7.3 and 7.4 contain an error. Instead of `g*dunif(data, -180, 180)` in Listing 7.3, line 35, and Listing 7.4, lines 6-7, it should be `g*rep(1,length(data))/(2*pi)`. [A full explanation and correction]({{ site.url }}/errata/listing7374.pdf). [Thanks to Grant Shields of UC Davis]

### 9. Multilevel or Hierarchical Modelling

p. 213, Figure 9.5: The arrow from $$k_{ij}$$ to $$\theta_{ij}$$ in the figure should be reversed [Thanks to Víthor Rosa Franco]: 

![]({{ site.url }}/images/fig9.5corrected.png)
