---
title: "SCtools: Tools for Synthetic Controls Analysis"
tags:
  - r
  - Causal Inferences	
  - Synthetic Control
  - Econometrics
  - Political Science
  - Economics
authors:
 - name: Bruno Castanho Silva
   orcid: 0000-0001-9363-4704
   affiliation: 1
 - name: Michael E. DeWitt Jr
   orcid: 0000-0001-8940-1967
   affiliation: 2
affiliations:
 - name:  Cologne Center for Comparative Politics, University of Cologne, Germany
   index: 1
 - name: Cone Health
   index: 2
date: 1 October 2019
bibliography: paper.bib
---

# Introduction

The Synthetic Control Method was first proposed by [@abadie_economic_2003] to evaluate the impact of a specific event or policy, implemented in one geographical area (city, country, region, and so on) on an outcome of interest. It works by generating a synthetic unit which is as similar as possible to the so-called "treated" unit, except for the intervention or event from which one wants to estimate a causal effect. This synthetic unit is constructed as a weighed average of similar units, which have not received the treatment. 

<!-- 
Add the math and the optimization problem
-->

It has since become a popular method in economics and public policy evaluation, due to its fitness for rigorous causal inference with a single treated observation, and intuitiveness.  The method as described by [@abadie_economic_2003] is implemented in R in the `Synth` [@Synth] package. 


However, `Synth` has not kept up to date with recent developments in Synthetic Controls. It has no automatic feature to implement the placebo tests suggested by [@abadie_comparative_2015] or [@abadie_synthetic_2010], nor more recent features such as synthetic controls for multiple treated units [@Kreif2015]. `SCtools` comes in to fill these gaps providing these extensions to `Synth`. 

<!--
* What is SCM?
* Application Areas
* Introduce Synth
  * Limitations
  * How SCtools fills in
-->

# Examples of Features

[@abadie_synthetic_2010] suggests using "placebos" to test the significance of the effects identified with synthetic controls: the approach consists in creating a synthetic control for each unit in the donor pool. Given that none of them was treated, the estimated "effects" from these units, in relation to their synthetic controls, is the expected difference between a unit and its synthetic control one could expect by chance, under the null hypothesis of no treatment effect. Therefore, if the observed effect for the treated unit is larger than that for the placebos, this is considered evidence of an actual treatment effect, where the pseudo p-value is the proportion of placebos having an effect as large as or larger than the observed for the treated unit.

## Generation of Placebos

`SCtools` automates the generation of placebos from the donor pool, based on objects created when running synthetic control analysis with `Synth`. It returns the estimated synthetic control for each donor unit, and a `gaps plot` with the respective curves comparing each donor pool unit to its synthetic control, along with the treated unit. 

Placebos can then be used for another test proposed by [@abadie_comparative_2015], the post/pre Mean Square Prediction Error (MSPE) test. It is the difference between the observed outcome of a unit and its synthetic control, before and after treatement. A higher ratio means a small  pre-treatment prediction error (a good synthetic control), combined with a high post-treatment MSPE, meaning a large difference between the unit  and its synthetic control after the intervention. By calculating this ratio for all placebos, the test can be interpreted  as looking at how likely the result obtained for a single treated case  with a synthetic control analysis could have occurred by chance given no treatement. `SCtools` also provides a post/pre MSPE plot and its associated pseudo p-value.

## Multiple Synthetic Control

An important advance in Synthetic Controls has been the estimation of causal effects for interventions with multiple treated units [@Kreif2015,@Cavallo2013]. For these, one synthetic control is estimated for each treated unit. Then, the average distance between each treated unit and its synthetic control, before and after the intervention, is taken to indicate the goodness-of-fit (before treatment) and the esitmated average treatment effect (after treatment). `SCtools` implements a change to the familiar `dataprep()` function from `Synth`, in order to accommodate multiple treated units and return a plot with the estimated average path for treated units and their synthetic controls.

Inference of causal effects with multiple treated units is also done using placebos. In this case, once again one synthetic control is created for each unit in the donor pool. `SCtools` implements a bootstrap approach is used to calculate a p-value for the average treatment effect estimate. It works by sampling `k` placebos (where k = the number of treated units) `n` times, calculating the estimated average placebo effect each time, and at the end having a distribution of average placebo effects. Comparing the observed average treatment effect to this distribution of average placebo effects gives an estimated p-value for the observed ATT. 

# Future Work



# Licensing and Availability

**SCtools** is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/bcastanho/SCtools), and with a corresponding issue tracker for bug reporting and feature enhancements.   

# References
