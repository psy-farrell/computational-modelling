---
layout: post
title: "Prior problems"
modified:
categories: blog
excerpt: "A blog on developing priors"
tags: [Bayesian, priors]
image:
  feature:
date: 2018-03-08
---

> A modified version of this post previously appeared as a blog post on the Psychonomics website, at https://featuredcontent.psychonomic.org/we-often-know-more-than-we-think-using-prior-knowledge-to-avoid-prior-problems-bayesinpsych/. See the related (and fun) post by Clintin Davis-Stober: https://featuredcontent.psychonomic.org/bayesinpsych-spiking-a-slab-with-sleepless-pillow-talk-and-prior-inequalities/

One of the unique features of Bayesian statistical and computational modelling is the prior distribution. A prior distribution is both conceptually and formally necessary to do any sort of Bayesian modelling. If we are estimating the values of model parameters (e.g., regression coefficients), we do this by updating our prior beliefs about the parameter values using the information from our experiment---without priors, we'd have nothing to update!

Priors also play a major role when performing model selection by Bayes Factors, which includes the hypothesis testing described in several of the papers in the Special Issue. A key quantity in Bayesian model selection is the *marginal likelihood*, which tells us how consistent our observed data are with a target model. One problem is that models will usually make different quantitative predictions for different parameter values, and we don't know the values of the parameters. The marginal likelihood solves this problem by calculating a weighted average of goodness of fit across all possible parameter values (that is, across the entire parameter space), the weights being determined by our the prior distribution of parameter values.

So whether we are estimating parameters, or performing model selection, the prior distribution on the model parameters needs to be specified. But where do the prior distributions come from, and what makes a good prior? In their paper [in the Special Issue](http://www.springer.com/psychology/cognitive+psychology/journal/13423)

, Lee and Vanpaemel tackle these questions by discussing how we can specify informative priors for cognitive models.

Cognitive models---as opposed to statistical models---specify psychological theories as mathematical equations or computational algorithms. A default technique used by many modellers is to specify a non-informative or weakly informative prior. For example, if we have a parameter in our model that varies between 0 and 1---for example, forgetting rate---it seems reasonable to assume that all values between 0 and 1 are equally plausible. However, when we do so, we are arguably throwing away information that is relevant to our modelling!

Lee and Vanpaemel argue that modellers should be more dedicated to specifying informative priors. Informative priors are those that specify that some parameter values are clearly more likely than others. Before applying a model to some data, we almost always have some good idea about the data we are going to see. We all have some notion that some results in an experiment would be more surprising than others, and we usually know enough about our experiments to detect anomalies in our results.

The issue is that those developing models invest large amounts of time into developing their model (the *likelihood* in Bayesian inference) to capture key effects in their domain, only to helplessly throw up their arms when it comes to specifying the prior. The view advanced by Lee and Vanpaemel, and others, is that we shouldn't shy away from specifying an informative prior.

Indeed, Lee and Vanpaemel identify several benefits of specifying informative priors. Informative priors can solve issues with statistical ambiguity (dealing with issues of parameter identifiability) and theoretical ambiguity. Importantly, models can be made simpler by using prior distributions to emphasise certain regions of parameter space as being more plausible. This acts to narrow the range of predictions the model makes, and so makes the theory less flexible and more falsifiable.

As noted by Lee and Vanpaemel, this can work both ways. A falsifiable model is more easily rejected, but if that model's predictions match the data despite the constraints introduced by an informative prior, that model should receive more support. Several philosophers of science (Popper, Lakatos, and Meehl are just a few) have proposed that the "riskiness" of predictions should be taken into account when a model accounts for one or more sets of data. A well known maxim due to Wesley Salmon is that it would be a "damn strange coincidence" if a tight range of predictions corresponds to the data, the implication that being that data should be taken as giving greater support for a model if its predictions are more precise.

Bayesian model selection is one formalisation of the idea of risky predictions giving greater support. As mentioned earlier, the marginal likelihood is calculated by working out how well the model predicts the data for each possible set of parameter values, and then averaging all those values (this is a simplification---the parameter space is usually continuous and so we use exact or approximate integration). Critically, this averaging is weighted by the prior distribution. If we place lots of weight (i.e., much of the mass of the prior) in a part of parameter space that doesn't give good fits to the data, the average fit of the model will be poor: we've made a risky prediction that failed. Alternatively, if we place lots of weight in a part of parameter space that captures the data well, those good fits will be highly weighted, and the model will be better supported than one where we used a more diffuse prior: the model has made a successful risky prediction. 

But how do we build our informative priors? There often won't be an obvious prior of parameters that have complex and indirect effects on model performance. Lee and Vanpaemel discuss several ways in which we can obtain these priors. One is eliciting priors from experts (e.g., researchers in the field). This can either be done directly (What rates of forgetting are plausible?), or working backwards from the predictions: by have experts determine which predictions are more or less plausible a priori, modellers can work backwards to find those regions of parameter space that generate the endorsed predictions. One can also use previous model applications; if a model has been fit to a variety of experiment sets, modellers will have some basis to construct a prior based on the parameter estimates from those previous applications.

Another approach, used by Kary et al. (2016), is to obtain a data-informed prior. Under this approach, the model is first fit to a subset of the data so as to obtain a posterior distribution on the parameters. This posterior can then be used as a prior for fitting the remaining data; this is especially useful when performing model selection on this second part of the data.

It is notable that many of Lee and Vanpaemel's comments also apply to statistical modelling. Recent years have seen the introduction of easy-to-use methods for calculating Bayes Factors for t-tests and ANOVAs, such as [JASP](https://jasp-stats.org/) and the [BayesFactor package](https://cran.r-project.org/package=BayesFactor) of Morey, Rouder, and Jamil. Any models which specify an effect must specify a prior on that effect. These packages typically come with default priors for effects that are centered on 0, and with some default spread. 

These default priors are reasonable for many problems, especially for effects that have not been well studied. However, in many situations we have good information about the possible distribution for effect sizes. For example, if we run a standard recognition memory experiment, we can be fairly confident that people can tell apart "old" and "new" items; the effect of old vs new should be positive. Accordingly, a symmetric 0-centered distribution---in which we give equal weight to positive and negative effect sizes, and the most weight to the effect size of 0---is inappropriate.

As for cognitive models, psychological researchers can spend a fair amount of time working out our statistical models, especially when using more complicated methods such as mixed effects modelling or structural equation modelling. A message from Lee & Vanpaemel and others (e.g., Dienes, 2014) is that we might also spend a bit more time thinking about our priors.

Dienes, Z. (2014). Using Bayes to get the most out of non-significant results. Frontiers in Psychology, 5, 781.

Kary, A., Taylor, R., & Donkin, C. (2016). Using Bayes factors to test the predictions of models: A case study in visual working memory. *Journal of Mathematical Psychology, 72*, 210-219.