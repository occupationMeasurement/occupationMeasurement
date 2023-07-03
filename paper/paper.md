---
title: 'occupationMeasurement: A Comprehensive Toolbox for Interactive Occupation Coding in Surveys'
tags:
  - R
  - occupation coding
  - machine learning
  - classification
  - KldB
  - ISCO
authors:
  - name: Jan Simson
    corresponding: true
    orcid: 0000-0002-9406-7761
    affiliation: 1
  - name: Olga Kononykhina
    affiliation: 1
  - name: Malte Schierholz
    orcid: 0000-0003-4058-1543
    affiliation: 1
affiliations:
 - name: Department of Statistics, Ludwig-Maximilians-Universität München, Germany
   index: 1
date: 24 Jan 2023
bibliography: paper.bib
---

# Summary

People earn a living a multitude of ways which is why the occupations they pursue are almost as diverse as people themselves. This makes quantitative analyses of free-text occupational responses from surveys hard to impossible, especially since people may refer to the same occupations with different terms. To address this problem, a variety of different classifications have been developed, such as the International Standard Classification of Occupations 2008 (ISCO) [@ilo2012] and the German Klassifikation der Berufe 2010 (KldB) [@bundesagenturfürarbeit2011], narrowing down the amount of occupation categories into more manageable numbers in the mid hundreds to low thousands and introducing a hierarchical ordering of categories. This leads to a different problem, however: Coding occupations into these standardized categories is usually expensive, time-intensive and plagued by issues of reliability.

Here we present a new instrument that implements a faster, more convenient and interactive occupation coding workflow where respondents are included in the coding process. Based on the respondent's answer, a novel machine learning algorithm generates a list of suggested occupational categories from the [Auxiliary Classification of Occupations](https://github.com/occupationMeasurement/auxiliary-classification) [@schierholz2018], from which one is chosen by the respondent (see \autoref{fig:app_flow_screenshots}). Issues of ambiguity within occupational categories are addressed through clarifying follow-up questions. We provide a comprehensive toolbox including anonymized German training data and pre-trained models without raising privacy issues, something not possible yet with other algorithms due to the difficulties of anonymizing free-text data.

# Statement of Need

Assigning occupations to standardized codes is a critical task frequently encountered in research, public administration and beyond: They are used in government censuses (e.g. USA, UK, Germany) and administrative data to better understand economic activity, in epidemiology to estimate exposure to health hazards, and in sociology to obtain a person's socio-economic status, to name a few examples.

To date, the standard approach to coding occupations is to collect one or two free-text responses and later hand-coding these descriptions by trained personnel with a classification manual, possibly assisted by computer software. Since coding typically occurs after data collection, based on the responses only and without the ability to request clarifying information from the respondent, the assignment of categories is often inaccurate. This approach to occupational coding is costly due to the experts' time needed and often suffers from low inter-coder reliability[^1].

![**Typical flow of the interactive application.** 1. A respondent provides a free text response describing her occupation. 2. The machine learning model then generates a list of suggested categories, from which the respondent will select one. 3. As a result, the associated occupational category codes from both the German KldB-2010 and the international ISCO-08 are returned. \label{fig:app_flow_screenshots}](./occupationMeasurement-figure.pdf)

Given the limitations of manual coding, a technical solution that can generate a suggested code fast enough to elicit immediate feedback from respondents would be a boon. However, implementations of this idea are few, and none are openly available. Technological solutions using machine learning have been proposed [@gweon2017; @russ2014; @russ2016; @creecy1992; @schierholz2021] but face problems obtaining training data and sharing trained models due to privacy issues, as training data often contains sensitive free-text responses that may personally identify respondents.

Our toolbox addresses these issues by implementing an occupation coding workflow during the interview as discussed in @peycheva2021 and @schierholz2018a. Difficulties of data and model sharing are resolved by using a novel machine learning algorithm specifically crafted to work with anonymized occupational data.

[^1]: An international review found rates of agreement between 44% and 89% when different coders code the same answers across different studies [@mannetje2003]. For a more in-depth discussion of the factors affecting the reliability of occupation coding, see @conrad2016 and @massing2019.

# Functionalities

We provide an open-source implementation of a machine learning algorithm for occupation coding with immediate feedback and verification, available as an R [@R] package on CRAN[^2]. An introductory ["Getting Started"](https://occupationmeasurement.github.io/occupationMeasurement/articles/occupationMeasurement.html) guide is available for anyone looking to use the package. To make it widely useful, our toolkit can be readily integrated in both self-administered web surveys as well as interviewer-administered (telephone) surveys using the included questionnaires. Programmers can [adapt these questionnaires](https://occupationmeasurement.github.io/occupationMeasurement/articles/app-questionnaire.html) to fit a wide array of requirements. The toolbox includes [custom survey software](https://occupationmeasurement.github.io/occupationMeasurement/articles/app.html) built on top of the shiny [@winston2023] framework to integrate machine learning predictions into surveys. On the off-chance that further flexibility is needed, we offer [direct API access](https://occupationmeasurement.github.io/occupationMeasurement/articles/api.html) for completely custom data collection and integration into existing survey software. As we built the toolbox on top of the shiny [@winston2023] and plumber [@schloerke2022] frameworks, [deployments on the Web](https://occupationmeasurement.github.io/occupationMeasurement/articles/app-deployment.html) are easy. We further provide [pre-built container images](https://github.com/orgs/occupationMeasurement/packages?repo_name=occupationMeasurement) for even easier deployment in production environments.

The toolbox uses a specifically developed list of occupational task descriptions, the [Auxiliary Classification of Occupations](https://github.com/occupationMeasurement/auxiliary-classification), designed to be easier to understand and less ambiguous than existing lists of job titles [@schierholz2018]. Alongside this list, it provides matching follow-up questions to enable a fine-grained assignment into existing classification systems.

The machine-learning algorithm used in our instrument is able to work with anonymized training data while retaining predictive performance on-par with other machine learning and non-machine-learning algorithms [@schierholz2019; @schierholz2021]. This enables sharing training data as well as trained models, allowing on the one hand out-of-the-box usage of our instrument without the need for labeled data or pre-training, but also the further sharing of newly trained models by users of the toolbox. Anonymized training data and pre-trained models in German are included with the package.

The toolbox is released under the MIT license alongside extensive [online documentation](https://occupationMeasurement.github.io/occupationMeasurement). Quality of the software is ensured using automated testing via continuous integration.

[^2]: Package "occupationMeasurement" on CRAN: <https://cran.r-project.org/web/packages/occupationMeasurement/index.html>.

# Related Work

This project is not the first to apply technology to assist in the coding of occupations, although it is the first tool to be released as open-source software and to offer this level of convenience and flexibility. Notable examples of prior work include the WISCO[^3] database [@tijdens2010], providing a search tree with levels of nested categories of occupations for use in Web surveys. Another prominent tool is CASCOT [@elias2014], short for Computer Assisted Structured Coding Tool. CASCOT uses a mixture of a coding index, which requires manual editing, text distances and manually specified rules to code responses into occupational categories. A promising tool has also been developed for the US Standard Occupational Classification System (SOC) [@u.s.bureauoflaborstatistics], called SOCcer [@russ2014; @russ2016]. Similar to the software presented here, SOCcer relies on using a machine learning algorithm. Unfortunately, neither SOCcer nor CASCOT are open-sourced, with the former offering coding via a free online version[^4] and the latter requiring payment for use.

[^3]: The WISCO database used different names over time, but kept the same acronym. The latest description is: "World database of occupations, coded ISCO". The database is available at <https://surveycodings.org>.

[^4]: Online version available at: <https://soccer.nci.nih.gov/soccer/>.

# Acknowledgements

This project is funded by the Deutsche Forschungsgemeinschaft (DFG, German Research Foundation) -- Project numbers 290773872 and 460037581.

# References
