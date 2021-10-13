---
title: Methods used by WHO to estimate the global burden of TB disease
author: WHO
geometry: "left=3cm,right=3cm,top=2cm,bottom=2cm"
papersize: a4
fontsize: 12pt
output: 
  pdf_document:
    latex_engine: xelatex
---




# Methods used by WHO to estimate the global burden of TB disease



<center>11 October 2021</center>




<center>Glaziou P<sup>1</sup>, Arinaminpathy N<sup>2</sup>, Dodd P.J.<sup>3</sup>, Floyd K<sup>1</sup></center> 



<sup>1</sup> Global TB Programme, World Health Organization, Geneva, Switzerland

<sup>2</sup> Imperial College London, UK

<sup>3</sup> School of Health and Related Research, University of Sheffield, UK




## Abstract

This paper describes methodological details used by WHO in 2021 to estimate TB incidence and mortality for the period 2000-2020. Incidence and mortality are disaggregated by HIV status, age and sex. Four main methods are used to derive incidence over the period 2000-2019: (_i_) results from TB prevalence surveys; (_ii_) notifications in high-income countries adjusted by a standard factor to account for under-reporting and underdiagnosis and (_iii_) national inventory studies; (_iv_) case notification data combined with expert opinion about case detection gaps.  Mortality is obtained from national vital registration systems of mortality surveys. In other countries, mortality is derived indirectly from incidence and case fatality ratio. For the year 2020, TB incidence and mortality are estimated using new dynamic and statistical models in 16 and 111 countries, respectively. These new methods were required to produce estimates that account for the major disruptions to the provision of and access to TB diagnostic and treatment services that have occurred in the context of the COVID-19 pandemic.

<div style="page-break-after: always; break-after: page;"></div>

## 1. Introduction

Estimates of the burden of disease caused by TB and measured in terms of incidence, prevalence and mortality are produced annually by WHO using information gathered through surveillance systems (case notifications and death registrations), special studies (including surveys of the prevalence of disease), mortality surveys, “inventory studies” of under-reporting of detected TB, in-depth analysis of surveillance and other data, expert opinion and consultations with countries. In June 2006, the WHO Task Force on TB Impact Measurement was established<sup><a href="https://paperpile.com/c/uyCckg/ZoHVx">1</a></sup>, with the aim of ensuring that WHO’s assessment of whether global targets were achieved should be as rigorous, robust and consensus-based as possible. The Task Force reviewed methods and provided recommendations in 2008, 2009, 2015, 2016 and most recently in April 2019. 



## 2. Historical background

Historically, a major source of data to derive incidence estimates were results from tuberculin surveys conducted in children<sup><a href="https://paperpile.com/c/uyCckg/mvYyS">2</a></sup>. Early studies showed the following relationship between the annual risk of infection denoted λ and the incidence of smear positive TB denoted _I<sub>s+</sub>_: one smear positive case infects on average 10 individuals per year for a period of 2 years and a risk of infection of $10^{-2}y^{-1}$ corresponds approximately to an incidence rate of $50 \times 10^{-5}y^{-1}$.  However, this relationship no longer holds in the context of modern TB control and in high HIV prevalence settings<sup><a href="https://paperpile.com/c/uyCckg/dRJdL">3</a></sup>. In addition to uncertainty about the relationship between λ and _I<sub>s+</sub>_, estimates of incidence obtained from tuberculin surveys suffer from other sources of uncertainty and bias, including unpredictable diagnostic performance of the tuberculin test<sup><a href="https://paperpile.com/c/uyCckg/rKdTK">4</a></sup>, digit preference when reading and recording the size of tuberculin reactions<sup><a href="https://paperpile.com/c/uyCckg/SUD9x">5</a></sup>, sensitivity to assumptions about reaction sizes attributed to infection<sup><a href="https://paperpile.com/c/uyCckg/fpTJi">6</a></sup>, sensitivity to the common assumption that the annual risk of infection is age invariant, and lastly, sensitivity of overall TB incidence estimates to the assumed proportion of TB incidence that is smear positive.

A first global and systematic estimation exercise led by WHO in the early 1990s estimated that there were approximately 8 million incident TB cases in 1990 ($152 \times 10^{-5}y^{-1}$) and 2.6-2.9 million deaths ($46-55 \times 10^{-5}y^{-1}$)<sup><a href="https://paperpile.com/c/uyCckg/bEfPf">7</a></sup>. A second major reassessment was published in 1999<sup><a href="https://paperpile.com/c/uyCckg/LGN3b">8</a></sup>, with an estimated 8 million incident  cases for the year 1997 ($136 \times 10^{-5}y^{-1}$), and 1.9 million TB deaths ($32 \times 10^{-5}y^{-1}$). The most important sources of information were case notification data for which gaps in detection and reporting were obtained from expert opinion. In addition, data from 24 tuberculin surveys were translated into incidence and 14 prevalence surveys of TB disease were used.

<div style="page-break-after: always; break-after: page;"></div>


## 3. Incidence, 2000-2019

TB incidence has never been measured through population based surveys at national level because this would require long-term studies among large cohorts of people (hundreds of thousands), involving high costs and challenging logistics. Notifications of TB cases provide a good proxy indication of TB incidence in countries that have both high-performance surveillance systems (for example, there is little under-reporting of diagnosed cases) and where the quality of and access to health care means that few cases remain undiagnosed and overdiagnosis is limited. In the large number of countries where these criteria are not yet met, better estimates of TB incidence can be obtained from an inventory study. An inventory study aims at quantifying the level of under-reporting of detected TB cases; if certain conditions are met, capture-recapture methods can also be used to estimate TB incidence <sup><a href="https://paperpile.com/c/uyCckg/1MqlV">9</a></sup>.

The ultimate goal of TB surveillance is to directly measure TB incidence from national case notifications in all countries. This requires a combination of strengthened surveillance, better quantification of under-reporting and over-reporting (i.e. the number of newly diagnosed cases that are missed by surveillance systems and the number of cases over-diagnosed with TB) and universal access to quality health care (to minimize under-diagnosis of cases and overdiagnosis). A TB surveillance checklist developed by the WHO Global Task Force on TB Impact Measurement defines the standards that need to be met for notification data to provide a direct measure of TB incidence<sup><a href="https://paperpile.com/c/uyCckg/XExTU">10</a></sup>. 

Methods currently used for the period 2000-2019 by WHO to estimate TB incidence can be grouped into four major categories. Figure 1 shows the distribution of countries according to the four categories - in reality, methods are often combined to estimate entire time series and the shown distribution of countries reflects the dominant method used to estimate incidence over the most recent years up to 2019 (see also Global TB Report 2021, section 2.1):

1. **Results from TB prevalence surveys**. Incidence is estimated using prevalence survey results and estimates of the distribution characteristics of duration of disease, accounting for the impact of HIV coinfection and ART. This method is used for 29 countries, of which 28 have national survey data and one – India – has a survey in one state. The 29 countries accounted for 66% of the estimated global number of incident cases in 2019.

2. **Notifications in high-income countries adjusted by a standard factor to account for under-reporting, under-diagnosis and overdiagnosis/overreporting**. This method is used for 139 countries that comprise all high-income countries except Germany, the Netherlands and the United Kingdom, plus selected upper-middle income countries with low levels of underreporting, including Brazil, China and the Russian Federation. For three countries (France, Republic of Korea and Turkey) the adjustment was country specific, based on results from reports of underreporting. These 139 countries accounted for 6% of the estimated global number of incident cases in 2019.

3. **Results from inventory/capture-recapture studies**. This method is used for 8 countries: China, Egypt, Germany, Indonesia, Iraq, the Netherlands, the United Kingdom and Yemen. They accounted for 17% of the estimated global number of incident cases in 2019.  

4. **Case notification data combined with expert opinion about case detection gaps**. Expert opinion, elicited in regional workshops or country missions, is used to estimate levels of under-reporting and under-diagnosis. Trends are estimated using either mortality data, surveys of the annual risk of infection or exponential interpolation using estimates of case detection gaps for three years. This method is considered generally unreliable and used when other methods are not applicable due to missing or poor quality data. In this report, this method is used for 39 countries that accounted for 11% of the estimated global number of incident cases in 2019.

The code implementing the different estimation methods is available at the following public repository: [TB estimation methods](http://github.com/GTB-TME/gtbreport2021).


### **Four main methods**


#### **Method 1 - Case notification data combined with expert opinion about case detection gaps.**

Expert opinion, elicited in regional workshops, national consensus workshops or country missions, is used to estimate levels of under-reporting, over-reporting (false positive diagnoses that may occur particularly in the context of systematic screening in populations with relatively low probability of TB disease) and under-diagnosis. Trends are estimated using either mortality data, national repeat surveys of the annual risk of infection or exponential interpolation using estimates of case detection gaps for specific years. The estimation of case detection gaps is essentially based on an in-depth analysis of surveillance data; experts provide their educated best guess about the range of the plausible detection gap _g_ and incidence _I_ is obtained from

$$I=\frac{f(N)}{1-g}, g\in[0,1[$$

where _N_ denotes case notifications, _f_ denotes a cubic spline function in countries with large year-to-year fluctuations in _N_, or else, the identity function. The incidence series are completed using assumptions about changes in CFR over time in countries with evidence of improvements in TB prevention and care, such as increased detection coverage over time or improved treatment outcomes, ensuring that the following inequality holds

$$0 \leq \left| \frac{\Delta I}{\Delta t} \right| \leq \left| \frac{\Delta M}{\Delta t} \right|$$

where _M_ denotes mortality.

A full description of the methods used in regional workshops where expert opinion was systematically elicited following an in-depth analysis of surveillance data is publicly available in a report of the workshop held for countries in the African Region (in Harare, Zimbabwe, December 2010)<sup><a href="https://paperpile.com/c/uyCckg/y5mi9">11</a></sup>. In some countries, case reporting coverage changed significantly during the period 2000-2019 as a result of disease surveillance reforms (e.g. disease surveillance was thoroughly reformed after the SARS epidemic in China, the Ministry of Justice sector notified cases among prisoners in Russia starting in the early 2000s). Trends in incidence are derived from repeat tuberculin survey results in Bhutan, India and Yemen and from trends in mortality or case notifications. 

Case proportions are assumed to follow a beta distribution, with parameters _α_ and _β_ obtained from the expected value _E_ and variance _V_ using the method of moments<sup><a href="https://paperpile.com/c/uyCckg/hyytO">12</a></sup>, as follows

$$\alpha = E \left(\frac{E(1-E)}{V} - 1 \right)$$

$$\beta  = (1-E)\left(\frac{E(1-E)}{V} - 1 \right)$$

Time series are built according to the characteristics of the levels of under-reporting and under-diagnosis that were estimated for specific reference years (three reference years in regional workshops conducted around 2010). A cubic spline extrapolation of _V_ and _E_, with knots set at the reference years, is used for countries with low-level or concentrated HIV epidemics. In countries with a generalized HIV epidemic, the trajectory of incidence is based on the annual rate of change in HIV prevalence and time changes in the fraction _F_ of incidence attributed to HIV, determined as follows

$$F = \frac{h(\rho - 1)}{h(\rho - 1) + 1}= \frac{\vartheta - h}{1 - h}$$

where _h_ is the prevalence of HIV in the general population, _ρ_ is the TB incidence rate ratio among HIV-positive individuals over HIV-negative individuals and _ϑ_ is the prevalence of HIV among new TB cases.

If there is insufficient data to determine the factors leading to time-changes in case notifications, incidence is assumed to follow a horizontal trend going through the most recent estimate of incidence.

Limitations of the method based on eliciting expert opinion about gaps in case detection and reporting include a generally small number of interviewed experts; lack of clarity about vested interests when eliciting expert opinion; lack of recognition of over-reporting (due to over-diagnosis, e.g. in some countries implementing a large-scale systematic population screening policy that may result in many people with abnormal chest X-ray but no bacteriological confirmation of TB disease being notified and treated as new TB cases) or in countries where cases with confirmed non-TB mycobacteria were not systematically reviewed and those judged non-TB were not de-notified; incomplete data on laboratory quality and high proportion of patients with no bacteriological confirmation of diagnosis are a potential source of error in estimates.


#### **Method 2 - Results from TB prevalence surveys.**

Two approaches were used to derive incidence from prevalence.

In a first approach, incidence is estimated using measurements from national surveys of the prevalence of TB disease combined with estimates of the duration of disease. Incidence is estimated as the prevalence of TB divided by the average duration of disease assuming epidemic equilibrium: let _N_ denote the size of a closed population with the number of birth and deaths the same for a period Δ_t_>0, let _C_ be the number of prevalent TB cases, _P_ the prevalence rate so that _P_=_C_/_N_. Let _m_ denote the rate of exit from the pool of prevalent cases through mortality, spontaneous self-cure or cure from treatment, and _I_ the rate at which new cases are added to the pool. At equilibrium during the time period Δ_t_ and further assuming exponentially distributed durations _d_ such that _d_=_m<sup>-1</sup>_

$$I(N - C) = m C$$

$$I = \frac{m C}{N - C} = \frac{P}{d(1 - P)} \approx \frac{P}{d}$$

In practice, the average duration of presence in the pool of prevalent cases cannot be directly measured. For example, measurements of the duration of symptoms in prevalent TB cases that are detected during a prevalence survey are systematically biased towards lower values, since survey investigations truncate the natural history of undiagnosed disease. Measurements of the duration of disease in notified cases ignore the duration of disease among non-notified cases and are affected by recall biases.

Literature reviews have provided estimates of duration of disease in untreated TB cases from the pre-chemotherapy era (before the 1950s). The best estimate of the mean duration of untreated disease (for smear-positive cases and smear-negative cases combined) in HIV-negative individuals is about three years. There are few data on the duration of disease in HIV-positive individuals. The assumed distributions of disease durations are shown in Table 1.

A second approach consists of estimating disease duration using three compartments: susceptibles (_S_), untreated for TB (_U_) and treated for TB (_T_). The size of _U_ and _T_ is obtained from the results of the prevalence survey. Transition rates from _U_ to _T_ are determined as follows

$$\frac{\mathrm{d}U}{\mathrm{d}t} = I S - (\mu_u + \theta_u + \delta)U$$

$$\frac{\mathrm{d}T}{\mathrm{d}t}= \delta U - (\mu_t + \theta_t) T$$

Where _I_ denotes Incidence, _μ_ and _θ_ denote mortality and self-cure (remission) or cure (with subscripts _u_ and _t_ indicating untreated and treated cases), respectively, _δ_ denotes the rate of removal from _U_ through detection and treatment. At equilibrium, the above two equations simplify to

$$I = \frac{U}{d_U}$$

$$\delta U=\frac{T}{d_T}$$

Disease duration (untreated) is obtained from 

$$d_U = (1-\pi)\frac{U}{T}d_T$$

where

$$\pi = 1 - \frac{\delta U}{IS}$$ 

is the proportion of incidence that dies or self-cures before treatment. π is assumed to be a distributed uniform with bounds 0 and 0.05<sup><a href="https://paperpile.com/c/uyCckg/gAIve">13</a></sup>. Table 2 shows estimates of incidence from four recent prevalence surveys using this method.

Limitations of this method include the insufficient power of disease prevalence surveys to estimate the number of prevalent TB cases on treatment with sufficient precision. Further, in most surveys, cases found on treatment during the survey do not have a bacteriological status at onset of treatment documented based on the same criteria as survey cases (particularly when culture or Xpert were not performed routinely). The method, however, provides more robust estimates of incidence compared with those obtained from expert opinion (method 1). 

In countries with high-level HIV epidemics that completed a prevalence survey, the prevalence of HIV among prevalent TB cases was found systematically lower than the prevalence of HIV among newly notified TB cases, with an HIV prevalence rate ratio among prevalent TB over notified cases ranging from 0.07 in Rwanda (2012) to 0.5 in Malawi (2013). The HIV rate ratio was pooled using random-effects model fitting data from countries with data collected over the period 2012-2019, including Kenya, Malawi, Rwanda, Tanzania, Uganda, Zambia and Zimbabwe, using the R package metafor<sup><a href="https://paperpile.com/c/uyCckg/elMmC">14</a></sup>. The pooled ratio value is used to predict HIV prevalence in prevalent cases from HIV prevalence in notified cases in African countries that were not able to measure the prevalence of HIV among survey cases.

The above two methods to derive incidence from prevalence are compared in Table 3. It is not clear a priori which method will perform better. The second method requires a sufficient number of cases on treatment at the time of the survey (as a rule of thumb, at least 30 cases) to generate relatively stable estimates. When both methods can be applied (so far only in selected low-HIV settings), results from two methods may be combined in a statistical ensemble approach as follows:

The incidence rate obtained using method _i_ is assumed distributed Beta with shape and scale parameters _α<sub>i</sub>_+1 and _β<sub>i</sub>_+1, respectively, and determined using the method of moments: _I<sub>i</sub>∼B_(_α<sub>i</sub>_+1,_β<sub>i</sub>_+1) so that

$$\textrm{Prob}(x = \textrm{TB})= \int_{0}^{1} x B(\alpha_i, \beta_i)\, \mathrm{d}x = \frac{\alpha_i+1}{a_i+\beta_i+2}$$

The combined probability is then expressed as

$$\textrm{Prob}(x = \textrm{TB}) = \frac{\sum{\alpha_i}+1}{\sum{\alpha_i}+\sum{\beta_i}+2}$$

$$\textrm{Var} = \frac{\left(\sum{\alpha} +1\right)\left(\sum{\beta} +1\right)}{\left(\sum{\alpha} + \sum{\beta} + 2\right)^2 \left(\sum{\alpha} + \sum{\beta} + 3\right)}$$

Indirect estimation of incidence from prevalence relies on a number of assumptions difficult to verify, including (i) epidemic in a stable state of equilibrium; (ii) correctly assumed distribution of disease duration for each case category; (iii) size of the unmeasured prevalence of clinically diagnosed TB and childhood TB correctly estimated.


#### **Method 3 - Notifications in high-income countries adjusted by a standard factor to account for under-reporting and under-diagnosis.**

TB surveillance systems from countries in the high-income group and other selected countries in the upper-middle income group are assumed to perform similarly well on average in terms of under-diagnosis and under-reporting. Exceptions include the Republic of Korea, where the under-reporting of TB cases has recently been measured using annual inventory studies and France, where the estimated level of under-reporting was communicated by public health authorities, based on unpublished survey results. In the United Kingdom and the Netherlands, incidence was obtained using capture-recapture modeling (see next section). Surveillance data in this group of countries are usually internally consistent. Consistency checks include detection of rapid fluctuations in notification rates and in the ratio of TB deaths / TB notifications (_M_/_N_ ratio), which may be indicative of reporting problems.


#### **Method 4 - Inventory studies, capture-recapture modelling.**

This method was used for 7 countries: China, Egypt<sup><a href="https://paperpile.com/c/uyCckg/raOQE">15</a></sup>, Indonesia, Iraq<sup><a href="https://paperpile.com/c/uyCckg/iYMM6">16</a></sup>, the Netherlands<sup><a href="https://paperpile.com/c/uyCckg/aaMNg">17</a></sup>, the United Kingdom<sup><a href="https://paperpile.com/c/uyCckg/Fcm8a">18</a></sup> and Yemen<sup><a href="https://paperpile.com/c/uyCckg/ajM98">19</a></sup>. Capture-recapture modelling is considered in studies with at least 3 sources (lists) and estimation of between source dependences<sup><a href="https://paperpile.com/c/uyCckg/1MqlV">9</a></sup>. The surveillance gap (proportion of unreported incident cases) in the UK and the Netherlands was assumed time invariant. In Yemen, trends in incidence were derived from results of two consecutive tuberculin surveys<sup><a href="https://paperpile.com/c/uyCckg/q7Vw6">20</a></sup>. In Egypt, Indonesia and Iraq, trends were derived using methods described in section describing method 1.

Capture recapture modelling for estimating TB incidence requires the following six assumptions: (i) all cases should be observable (preclinical stages are rarely detected before they become symptomatic); (ii) low proportion of mismatches and matching failures, which typically requires a large sampling fraction; (iii) closed population during the study period (typically 3-6 months); (iv) dependences between S data sources (S ≥ 3) accounted for in the model design but S-way interaction assumed null - referrals between sources  (e.g. clinic to lab) may imply an S-way interaction, invalidating the approach (of note, in many high-burden countries, there will not be 3 sources meeting requirements); (v) homogeneity of within-source observation probabilities across subpopulation groups such as defined by socio demographic characteristics; (vi) consistent case definitions across sources.  It is anticipated that capture recapture may only be successfully implemented in very few high-burden countries planning an inventory study.


### HIV-positive TB incidence

Provider-initiated testing and counselling with at least 50% HIV testing coverage is the most widely available source of information on the prevalence of HIV in TB patients. However, this source of data is affected by selection biases, particularly when coverage is closer to 50% than to 100%. As coverage of HIV testing continues to increase globally, biases will decrease. Other sources of information on the prevalence of HIV among new TB cases include sero-surveys of a random sample of newly diagnosed TB cases and HIV sentinel surveillance systems when they include TB as a sentinel group. The different data sources were combined using local polynomial regression fitting by weighted least squares, using weight values of 1 for data from a nationally representative survey, 0.2 for data based on HIV sentinel surveillance, and a value equal to testing coverage in the case of data from provider-initiated HIV testing with coverage greater than 50%, and zero weights when testing coverage was less than 50%. In countries with no surveillance data on HIV among TB cases, the prevalence of HIV was derived indirectly from the prevalence of HIV in the general population, based on the relationship between the prevalence of HIV in TB and the prevalence of HIV in the general population shown in Annex 2. 


### Disaggregation by age and sex<sup>21</sup>

Estimates for men (males aged ≥15 years), women (females aged ≥15 years) and children (aged &lt;15 years) are derived as follows. Age and sex disaggregation of smear-positive tuberculosis case notifications has been requested from countries since the establishment of the data collection system in 1995, but with few countries actually reporting these data to WHO. In 2006, the data collection system was revised to additionally monitor age disaggregated notifications for smear-negative and extrapulmonary tuberculosis. The revision also included a further disaggregation of the 0–14 age group category to differentiate the very young (0–4) from the older children (5–14). While reporting of age disaggregated data was limited in the early years of the data collection system, reporting coverage kept improving. For 2012 case notifications, age-specific data reached 99%, 83% and 83% of total smear-positive, smear-negative and extrapulmonary tuberculosis global case notifications. Finally in 2013, another revision of the recording and reporting system was necessary to allow for the capture of cases diagnosed using WHO-approved rapid diagnostic tests (such as Xpert MTB/RIF)<sup>22</sup>. This current revision requests the reporting of all new and relapse case notifications by age and sex. 

While there are some nationwide surveys that have quantified the amount of under-reporting of cases diagnosed in the health sector outside the network of the NTPs<sup>15,17,23</sup>, none have produced precise results by age. Small-scale, convenience-sample studies indicate that under-reporting of childhood tuberculosis can be very high<sup><a href="https://paperpile.com/c/uyCckg/oTglF+U3DpD">24,25</a></sup> but extrapolation to national and global levels is not yet possible. Plans for implementation of nationwide surveys are under way in selected countries to measure under-reporting of tuberculosis in children<sup><a href="https://paperpile.com/c/uyCckg/9PIyY">26</a></sup>. However, producing estimates of TB incidence among children remains challenging primarily due to the lack of well-performing diagnostics to confirm childhood TB and the lack of age-specific, nationwide, robust survey and surveillance data.

In order to maintain consistency with the total TB incidence and its uncertainty, the approach to estimating TB incidence by age and sex sought to disaggregate the total incidence into the incidence in each age group and sex. For countries where incidence was based on either a capture-recapture study or a standard factor adjustment of notification, and the implied case detection ratio was over 85%, we disaggregated total tuberculosis incidence by age and sex in proportion to the notifications. For these countries, surveillance systems were assumed to function well enough to inform patterns by age and sex directly. We also disaggregated incidence proportionally to notifications in countries where fewer than 1,000 TB cases were reported in total. In these countries, the stochasticity is strong and modelled estimates less appropriate.

For other countries, one million samples were from a country ‘prior’ for the proportion of incidence in each age and sex group, and the mean over samples where the implied incidence in every category exceeded the corresponding notifications was used. Where no samples had this property, the 100 samples with the smallest undershoot were used.

The prior for each country was based for adults on a hierarchical analysis for prevalence risk ratios was developed based on prevalence survey data and Horton et al’s systematic review of prevalence sex ratios<sup><a href="https://paperpile.com/c/uyCckg/60Hxl">27</a></sup>. This prior closely followed age and sex patterns for prevalence in countries with surveys, and made predictions (with greater uncertainty) for countries without prevalence surveys informing the age patterns with prevalence surveys in the same WHO region, and sex ratios from Horton’s WHO region specific meta-analysis. The prior for children was based on based on a mathematical modelling approach that simulates the course of natural history of TB in children, starting from estimates of tuberculous infection in children as a function of demographic and adult TB prevalence and subsequently modelling progression to pulmonary and extra-pulmonary tuberculosis disease taking into account country-level BCG vaccination coverage and HIV prevalence<sup><a href="https://paperpile.com/c/uyCckg/hwHfb">28</a></sup>. The disaggregation by sex in children was based on a random-effects meta-analysis of the sex ratio in notification data for children (0-14 years).

Finally, for a small number of countries the approach above generated results lacked face validity and a standard factor adjustment of notifications was used instead.


### Incidence of TB due to _Mycobacterium bovis_

_Mycobacterium bovis_ is the most common cause of bovine TB in cattle and zoonotic TB in people, globally. Incidence of zoonotic TB is calculated by applying TB incidence estimates from 2019 to the regional proportions of all TB cases that are estimated to be caused by _M. bovis<sup><a href="https://paperpile.com/c/uyCckg/rO1B">29</a></sup>_. A standard deviation of 50% relative to the best estimate of each regional proportion was assumed when propagating uncertainty. Due to an absence of routine reporting in most countries where bovine TB is endemic, these proportions were drawn from scientific studies <sup><a href="https://paperpile.com/c/uyCckg/CWyD2+rO1B">29,30</a></sup> that lack regional representativeness. As a result, estimates have a large uncertainty range. Mortality (excluding TB deaths in HIV-infected individuals) was similarly calculated based on the same proportions but this time applied to aggregated estimates of TB mortality by WHO region, reduced by a 20% factor to account for a higher proportion of extra-pulmonary TB with lower case fatality. Given that other mycobacterial species can also cause zoonotic TB, the true incidence and mortality may be higher than these estimations. There is a need to strengthen surveillance of zoonotic TB in order to understand the true burden of disease. One of the major barriers for diagnosis is that the most commonly used laboratory tests for TB do not differentiate the _M. tuberculosis_ complex into its separate species. 


### Bacteriologically confirmable TB incidence

Country-specific estimates of the number of incident cases of pulmonary TB that could be bacteriologically confirmed are produced as the product of a) estimated TB incidence b) observed country-specific proportion of notified cases diagnosed with pulmonary TB and c) expected proportion of pulmonary cases that could be bacteriologically confirmed if the best tests were used. The latter parameter is derived from the weighted average (83%) and standard deviation (5.9%) of the proportion of bacteriologically confirmed cases among pulmonary cases notified in the group of high-income countries in 2019.



## 4. Mortality, 2000-2019

The best sources of data about deaths from TB (excluding TB deaths among HIV-positive people) are vital registration (VR) systems in which causes of death are coded according to ICD-10 (although the older ICD-9 and ICD-8 classification are still in use in several countries), using ICD-10: A15-A19 and B90 codes, equivalent to ICD-9: 010-018, and 137. When people with AIDS die from TB, HIV is registered as the underlying cause of death and TB is recorded as a contributory cause. Since one third of countries with VR systems report to WHO only the underlying causes of death and not contributory causes, VR data usually cannot be used to estimate the number of TB deaths in HIV-positive people. Two methods were used to estimate TB mortality among HIV-negative people (see also Global TB report 2021, section 2.2):

* direct measurements of mortality from VR systems or mortality surveys;
* indirect estimates derived from multiplying estimates of TB incidence by estimates of the CFR.


### Estimating TB mortality among HIV-negative people from vital registration data and mortality surveys up to 2019

As of July 2019, mortality data from 123 countries were used, representing 60% of the estimated number of TB deaths (among HIV-negative TB) globally in 2019. 

Estimates for 21 countries, including India and for South Africa (adjusted for HIV/TB miscoding) were obtained from the Institute of Health Metrics and Evaluation at http://ghdx.healthdata.org/gbd-results-tool, readjusted to fit WHO mortality envelopes (the estimated number of deaths in total) by using a multiplication factor equal to the ratio of WHO to IHME envelopes. The median country-year envelope ratio (WHO/IHME) was 1.03 (interquartile range, 0.94-1.11) among 391 country-year data points.

Among the countries for which VR or mortality survey data could be used, there were 1586 country-year data points 2000–2019, after removing 120 country-year data points with insufficient data quality as estimated by WHO<sup>31</sup>. 

Reports of TB mortality are adjusted upwards to account for incomplete coverage (estimated deaths with no cause documented) and ill-defined causes of death (ICD-9: B46, ICD-10: R00–R99). It is assumed that the proportion of TB deaths among deaths not recorded by the VR system was the same as the proportion of TB deaths in VR-recorded deaths. For VR-recorded deaths with ill-defined causes, it is assumed that the proportion of deaths attributable to TB is the same as the observed proportion in recorded deaths. The adjusted number of TB deaths _κ<sub>a</sub>_ is obtained from the VR report _κ_ as follows:

$$\kappa_a = \frac{\kappa}{v(1-g)}$$

where _v_ denotes coverage (i.e. the number of deaths with a documented cause divided by the total number of estimated deaths) and _g_ denotes the proportion of ill-defined causes. The uncertainty related to the adjustment was estimated as follows:

$$\hat{\sigma} = \frac{\kappa}{4} \left[\frac{1}{v(1-g) -1}\right]$$

The uncertainty calculation does not account for miscoding, such as HIV deaths miscoded as deaths due to TB, except in South Africa.

Missing data between existing adjusted data points are interpolated. Trailing missing values are predicted using a Kalman smoother or using the last observation carried forward or in the case of leading missing values, the next observation carried backwards. 

In 2019, 58% of global TB mortality (excluding HIV) was directly measured from VR or survey data (or imputed from survey or VR data from previous years). The remaining mortality was estimated using the indirect methods described in the next section.


### Estimating TB mortality among HIV-negative people from estimates of case fatality rates and TB incidence

In countries lacking mortality data of the necessary coverage and quality, TB mortality is estimated as the product of TB incidence and the case fatality rate (CFR) after disaggregation by case type as shown in Table 4, following a literature review of CFRs by the TB Modelling and Analysis Consortium (TB-MAC):

$$M^{\textrm{-}} = (I^{\textrm{-}} -T^{\textrm{-}})f_u^{\textrm{-}} + T^{\textrm{-}}f_t^{\textrm{-}}$$	(1)

where _M_ denotes mortality, _I_ incidence. _f<sub>u</sub>_ and _f<sub>t</sub>_ denote CFRs untreated and treated, respectively and the superscript denotes HIV status. _T_ denotes the number of treated TB cases. In countries where the number of treated patients that are not notified (under-reporting) is known from an inventory study, the number of notified cases is adjusted upwards to estimate _T<sup>-</sup>_ accounting for under-reporting.


### Estimating TB mortality among HIV-positive people

TB mortality among HIV-positive is calculated exchanging superscripts - with + (Eq. 1). The case fatality ratios were obtained in collaboration with the TB Modeling and Analysis Consortium (TB-MAC), and are shown in Table 5. The disaggregation of incident TB into treated and not treated cases is based on the numbers of notified cases adjusted for under-reporting. 

Direct measurements of HIV-associated TB mortality are urgently needed. This is especially the case for countries such as South Africa and Zimbabwe, where national VR systems are already in place. In other countries, more efforts are required to initiate the implementation of sample VR systems as an interim measure.



### Disaggregation of TB mortality by age and sex

TB mortality is disaggregated by age and sex using the age- and sex-specific adjusted (for coverage and ill-defined causes) number of deaths from VR data in countries with high-quality vital registration systems in place (ie, where these data have been used to estimate the TB mortality envelope)<sup>21</sup>. For other countries, adult mortality is disaggregated by age, sex and HIV-infection status by applying CFRs to disaggregated incidence estimates, distinguishing CFR by anti-TB treatment status and HIV/ART status (see Tables 4 and 5). TB mortality in children for these countries is also estimated from TB incidence in children using a case-fatality based approach<sup>32</sup>. This approach distinguishes case fatality children by age, anti-TB treatment status, and HIV/ART status. HIV-positive TB deaths in adults are distributed by age and sex proportional to age- and sex-specific HIV prevalence from UNAIDS estimates in such a way as to maintain the estimated total number of HIV-positive TB deaths.

## 6. Estimation of the impact of COVID-19 on the burden of TB, 2020

New dynamic and statistical models to produce country-specific estimates of TB incidence and mortality in 2020. These new methods were required to produce estimates that account for the major disruptions to the provision of and access to TB diagnostic and treatment services that have occurred in the context of the coronavirus (COVID-19) pandemic.

## 6.1 Dynamic models

Dynamic country-specific models were developed for 16 countries. These countries – prioritized based on the size of their contribution to the global drop in TB case notifications between 2019 and 2020 – were Angola, Bangladesh, Brazil, China, India, Indonesia, Kenya, Myanmar, Pakistan, Peru, Philippines, Russian Federation, South Africa, Uganda, Ukraine and Viet Nam (Global TB Report 2021, Section 2.1). Collectively, they accounted for 93% of the drop in global TB notifications between 2019 and 2020.

*Model description*

Figure 1 illustrates schematically the deterministic, compartmental model of TB transmission dynamics that was used. A key part of the model is the per-capita rate of treatment initiation, *d*, representing the rate at which untreated, prevalent TB is diagnosed and initiated on treatment. Disruptions were modelled through a time-dependent factor $k(t)$, multiplying $d$. In the pre-pandemic period *k(t)* was fixed at 1, and the value of $d$ was calibrated, along with other model parameters, in order to match WHO estimates for incidence, mortality and notifications. This calibration was performed using Bayesian adaptive Markov Chain Monte Carlo methods, allowing uncertainty in WHO estimates to be propagated to uncertainty in model projections.

To capture different drivers of TB epidemiology in each of the priority countries, the basic structure shown in Figure 1 was further refined to give the following three models:

(i)      Public/private model. Many countries in South- and South-East Asia have a strong role for the private healthcare sector, in managing TB. For these countries the compartment labelled ‘on TB treatment’ was bifurcated into public and private sectors, allowing different rates, $d_{pu}$ and $d_{pr}$, for both (to be calibrated to the available data). Monthly data for notifications in 2020 for most countries is not disaggregated by public and private sectors; accordingly, it was assumed for simplicity that the same disruptions applied to both. The exception is India, where different disruptions were modelled for each sector, to match the disaggregated notification data.

(ii)      TB/HIV model. For countries where HIV coinfection plays a strong role in the TB epidemic, the basic structure shown in Figure 1 was further divided into three strata: those without HIV; those with HIV and who are not on ART; and those on ART. HIV incidence was modelled through a transition from the first to the second of these strata, and ART uptake through a transition from the second to the third. The available, monthly notification data for 2020 does not include stratification by HIV status; accordingly, it was assumed for simplicity that any disruptions to diagnosis apply equally to HIV-negative and HIV-positive TB. Ignoring disruptions to HIV services, this analysis is conservative with respect to the potential effects of such disruptions.

 (iii)     DS/DR-TB model. For countries where rifampicin-resistant TB accounts for a substantial share of TB incidence, the basic structure shown in Figure 1 was divided into two strata: those with drug-susceptible TB, and those with rifampicin-resistant TB. We also incorporate second-line treatment, and the potential for patients with DR-TB to be inappropriately initiated on first-line treatment. The available, monthly notification data for 2020 does not include stratification by drug resistance status, nor does it provide information on the proportion of notifications that had a DST result; accordingly, it was assumed for simplicity that any disruptions to diagnosis applied equally to DS and RR-TB; disruptions in provision of DST were not modelled.



 ![model](C:\Users\glazioup\Dropbox\gtb2021\report\doc\model.png)

**Figure 1. Schematic illustration of the basic model structure**. For simplicity, transitions omitted from this diagram (but included in the model) are: spontaneous cure from TB; exogenous reinfection; and population turnover. As described in the text, this structure was further refined for settings where: (i) the private healthcare sector plays a strong role in managing TB, (ii) there is a high burden of rifampicin-resistant TB, (iii) there are high rates of HIV/TB coinfection. Rates shown in the diagram are as follows: $\lambda$, time-dependent force of infection; $u$, per-capita hazard of breakdown to active disease in the years after infection; $v$, per-capita rate of reactivation thereafter; $\mu_{TB}$, per-capita hazard of TB mortality; $\rho$, per-capita rate of relapse; $d$, per-capita rate of diagnosis and treatment initiation; *k(t)* time-dependent adjustment to diagnosis and treatment initiation due to disruptions. 

 *Modelling disruptions to TB services*

This analysis concentrated on delays to diagnosis and treatment initiation. The analysis ignored disruptions to treatment continuity amongst those already on TB treatment, partly for lack of systematic data, but also because previous modelling analysis<sup>33</sup> suggests that these types of disruptions are likely to have a weaker effect on incidence, than disruptions to diagnosis and treatment initiation.

The intensity and duration of disruptions was informed by monthly notifications (quarterly where monthly data is unavailable), as reported to WHO. It was assumed that any reduction in notifications, compared to an extrapolation of pre-2020 trends, arises from delays to diagnosis and treatment initiations, rather than shortfalls in reporting. In turn, these delays may arise from patient-related factors (e.g. symptomatic patients being less willing or able to seek care during periods of anti-COVID restrictions), or from health system related factors (e.g. TB programmes having less diagnostic or HR capacity than usual times). The model structure shown above is agnostic to either of these factors, as the whole patient careseeking journey is made implicit in the rate $d$.

 Assuming that treatment initiations are a reasonable proxy for notifications, the number of notifications in a given month *n* is:

$$\begin{equation} \text{Notifications in month } n = \int_n^{n+1} k(t) d I dt, \end{equation}$$

where $I$ is the number of individuals having active, infectious disease in Figure 1. Using the full transmission model, the monthly value of $k(t)$ was therefore adjusted in such a way as to yield treatment initiations consistent with the monthly notification data. The timeseries for $k$ determined in this way, then formed the basis for model projections for incidence. 

*Lockdown-related reductions in TB transmission*

As much as lockdowns and social restrictions can control transmission of COVID, they may also have had similar effects on TB transmission. Given uncertainty about the strength of these effects in different settings, it was assumed that in any setting experiencing a country-wide lockdown, there was a 50% reduction in TB transmission during that period of lockdown (with transmission returning to pre-lockdown levels as soon as restrictions were lifted). For any country implementing subnational lockdowns, this reduction was scaled in proportion to the share of the country’s population undergoing those lockdowns. 

 *Quantifying excess TB incidence and mortality*

For model projections between 2020 – 2025, a ‘best-case recovery scenario’ was adopted, assuming that TB services return to normal in the month immediately after the last available data point.  Although artificial, this scenario serves as a lower bound for the excess burden that may occur over the next five years, in the absence of any remedial measures such as accelerated case-finding. 

 *Identifying priority countries*

The analysis focused on the 16 countries listed in Table 1, that together accounted for >94% of the drop in global notifications in 2020 (relative to what would have been expected in that year through a linear extrapolation from the previous two years). The rationale for this approach is that countries with the greatest absolute drop in notifications are also those most likely to see changes in incidence and mortality that are meaningful for global TB burden. 

**Table 1. List of countries modelled**, and the corresponding model structures used. 

| **Public/private model**                                     | **TB/HIV model**                            | **DS/DR-TB model**                |
| ------------------------------------------------------------ | ------------------------------------------- | --------------------------------- |
| Bangladesh  China  India  Indonesia  Myanmar  Pakistan  Philippines  Viet Nam | Angola  Brazil  Kenya  South Africa  Uganda | Peru  Russian Federation  Ukraine |

 

**Model equations**

***Public-private sector model\***

Uninfected ($U$):

$$\begin{equation}
\frac{dU}{dt} = b – (\lambda + \mu)U
\end{equation}$$

where $b$ is the influx into the population due to births; $\lambda$ is the force-of-infection (defined below); and $\mu$ is the per-capita rate of background mortality.

Latent ‘fast’ progression, $L_f$:

$$\begin{equation}
\frac{dL_f}{dt} = \lambda U + c\lambda(L_s + R_{lo} + R_{hi} + R_{LT}) – (u + w + \mu)L_f,
\end{equation}$$

where $u$ is the rate of breakdown to active disease; c$ is protection from reinfection; and $w$ is the rate of stabilisation to latent ‘slow’ progression. 

Latent ‘slow’ progression, $L_s$ 

$$\begin{equation}
\frac{dL_s}{dt} = wL_f – (v + \mu + c\lambda) L_s
\end{equation}$$

where $v$ is the rate of reactivation to active disease. 

Active, infectious disease, $I$

$$\begin{equation}
\frac{dI}{dt} = uL_f + vL_s + \rho_{lo} R_{lo} + \rho_{hi} R_{hi} + \rho_{LT} R_{LT} - \left[ d_{pu} k_{pu} (t) + d_{pr} k_{pr} (t) + \mu_{TB} + \sigma \right] I 
\end{equation}$$

where $d_{pu}, d_{pr}$ are respectively the rates of diagnosis and treatment initiation by the public and private sectors; $k_{pu}(t), k_{pr}(t)$ are respectively the time-dependent modifiers to these rates arising from COVID-related disruptions; $\mu_{TB}$ is the per-capita hazard of mortality for untreated TB; and $\sigma$ is the per-capita rate of self-cure. Note that most countries (apart from India) do not have separate notification data for public and private sectors; in these cases we assume that $k_{pu} = k_{pr}$, i.e. that both sectors see the same extent of relative disruptions.

On TB treatment in public (NTP) sector, $T_{pu}$:

$$\begin{equation}
\frac{dT_{pu}}{dt} = d_{pu} k_{pu} (t) – (\tau + g_{pu} + \mu) T_{pu},
\end{equation}$$

where $1/\tau$ is the duration of first-line treatment, and $g_{pu}$ is the rate of treatment drop-off in the public sector.

On TB treatment in private (non-NTP) sector, $T_{pr}$:

$$\begin{equation}
\frac{dT_{pr}}{dt} = d_{pr} k_{pr} (t) – (\tau + g_{pr} + \mu) T_{pu},
\end{equation}$$

where $g_{pr}$ is the rate of treatment drop-off in the private sector. 

Recovered, low relapse risk (following successful treatment completion), $R_{lo}$:

$$\begin{equation} 
\frac{dR_{lo}}{dt} = \tau T_{pu} + \tau T_{pr} – (\mu + \rho_{lo} + c\lambda + s) R_{lo}, 
\end{equation}$$

Where $\rho_{lo}$ is the rate of endogenous relapse amongst those with low relapse risk; $c$ is the protection from reinfection; and $s$ is the stabilisation of relapse risk.

Recovered, high relapse risk (following treatment non-completion or self cure), $R_{hi}$:

$$\begin{equation}
\frac{dR_{hi}}{dt} = g_{pu} T_{pu} + g_{pr} T_{pr} – (\mu + \rho_{hi} + c\lambda + s)R_{hi},
\end{equation}$$

where $\rho_{hi}$ is the rate of endogenous relapse amongst those with high relapse risk.

Recovered, long term stabilisation, $R_{LT}$

$$\begin{equation}
\frac{dR_{LT}}{dt} = s(R_{lo} + R_{hi}) – (\mu + \rho_{LT} + c\lambda) R_{LT},
\end{equation}$$

Force of infection

$$\begin{equation}
\lambda = \beta I / N,
\end{equation}$$

where $\beta$ is the average number of secondary infections per year per infectious case in a fully susceptible population, and $N$ is the overall population size.


 ***TB/HIV model\***

In the following equations, the superscript $h$ denotes HIV status, which is split into three different categories: HIV-negative (denoted 0), HIV-positive and not on ART (denoted 1), HIV-positive and on ART (denoted 2). 

Uninfected, $U^{(h)}$:

$$\begin{equation}
\frac{dU^{(h)}}{dt} = b – (\lambda + \mu) U^{(h)} + f(h,U),
\end{equation}$$

where $b$ is the influx into the population due to births; $\lambda$ is the force-of-infection (defined below); $\mu$ is the per-capita rate of background mortality; and $f$ denotes transitions between different HIV statuses, as described below.

Latent ‘fast’ progression, $L_f^{(h)}$:

$$\begin{equation}
\frac{dL_f^{(h)}}{dt} = \lambda U^{(h)} + c\lambda \left(L_s^{(h)} + R_{lo}^{(h)} + R_{hi}^{(h)} + R_{LT}^{(h)} \right) – (u + w + \mu) L_f^{(h)} + f(h, L_f),
\end{equation}$$

where $c$ is protection from reinfection; $u$ is the rate of breakdown to active disease; and $w$ is the rate of stabilisation to latent ‘slow’ progression.

Latent ‘slow’ progression, $L_s^{(h)}$:

$$\begin{equation}
\frac{dL_s^{(h)}}{dt} = w L_f^{(h)} – (v + \mu + c\lambda)L_s^{(h)} + f(h, L_s)
\end{equation}$$

where $v$ is the rate of reactivation to active disease.

Active, infectious disease, $I^{(h)}$:

$$\begin{equation}
\frac{dI^{(h)}}{dt} = uL_f^{(h)} + vL_s^{(h)} – [d k(t) + \mu_{TB} + \sigma] I^{(h)} + f(h, I)
\end{equation}$$

where $d$ is the rate of diagnosis and treatment initiation; $k(t)$ is the time-dependent modifier to this rate arising from COVID-related disruptions; $\mu_{TB}$ is the per-capita hazard of mortality for untreated TB; and $\sigma$ is the per-capita rate of self-cure. 

On TB treatment, $T^{(h)}$:

$$\begin{equation}
\frac{dT^{(h)}}{dt} = d k(t) I^{(h)} – (\tau + g + \mu) T^{(h)} + f(h,T),
\end{equation}$$

where $1/\tau$ is the duration of first-line treatment, and $g$ is the rate of treatment discontinuation, amongst those not completing treatment.

Recovered, low relapse risk (following successful treatment completion), $R_{lo}^{(h)}$: 

$$\begin{equation}
\frac{d R_{lo}^{(h)}}{dt} = \tau T^{(h)} – (\mu + \rho_{lo} + c\lambda + s)R_{lo}^{(h)} + f(h, R_{lo}),
\end{equation}$$

where $\rho_{lo}$ is the rate of endogenous relapse amongst those with low relapse risk; $c$ is the protection from reinfection; and $s$ is the stabilisation of relapse risk.

Recovered, high relapse risk (following treatment non-completion or self cure), $R_{hi}^{(h)}$:

$$\begin{equation}
\frac{d R_{hi}^{(h)}}{dt} = gT^{(h)} – (\mu + \rho_{hi} + c\lambda + s) R_{hi}^{(h)} + f(h, R_{hi}),
\end{equation}$$

where $\rho_{hi}$ is the rate of endogenous relapse amongst those with high relapse risk.

Recovered, long term stabilisation, $R_{LT}^{(h)}$:

$$\begin{equation}
\frac{R_{LT}^{(h)}}{dt} = s \left( R_{lo}^{(h)} + R_{hi}^{(h)}\right) – (\mu + \rho_{LT} + c\lambda) R_{LT}^{(h)} + f(h, R_{LT}),
\end{equation}$$

Force of infection

$$\begin{equation}
\lambda = \beta \sum_h I^{(h)}/N,
\end{equation}$$

where $\beta$ is the average number of secondary infections per year per infectious case in a fully susceptible population, and $N$ is the overall population size.

In the following equations, we denote $X$ as any of the model compartment listed above. Transitions between HIV statuses are modelled as follows.

HIV-uninfected $(h = 0)$:

$$\begin{equation}
f(0,X) = -r_{HIV} X^{(0)}
\end{equation}$$

HIV infected, not on ART $(h = 1)$:

$$\begin{equation}
f(1,X) = r_{HIV} X^{(0)} – r_{ART} X^{(1)}
\end{equation}$$

On ART $(h = 2)$:

$$\begin{equation}
f(2,X) = r_{ART} X^{(1)}
\end{equation}$$


***DS/DR-TB model\***

In the following equations, the superscript *r* denotes rifampicin resistance status, which is split into two different categories: drug sensitive (denoted 0), and drug resistance (denoted 1). 

Uninfected, $U$:

$$\begin{equation}
\frac{dU}{dt} = b - \left( \sum_r \lambda^{(r)} + \mu\right) U
\end{equation}$$

where $b$ is the influx into the population due to births; $\lambda^{(r)}$ is the force-of-infection associated with drug resistance status $r$ (defined below); and $\mu$ is the per-capita rate of background mortality.

Latent ‘fast’ progression, $L_f^{(r)}$

$$\begin{equation}
\frac{dL_f^{(r)}}{dt} = \lambda^{(r)}U + c\lambda^{(r)} \sum_r \left(R_{lo}^{(r)} + R_{hi}^{(r)} + R_{LT}^{(r)}\right) – (u + w+ \mu)L_f^{(r)}
\end{equation}$$

where $c$ is protection from reinfection; $u$ is the rate of breakdown to active disease; and $w$ is the rate of stabilisation to latent ‘slow’ progression.

Latent ‘slow’ progression, $L_f^{(r)}$

$$\begin{equation}
\frac{dL_s^{(r)}}{dt} = wL_f^{(r)} – (v + \mu + c\lambda) L_s^{(r)}
\end{equation}$$

where $v$ is the rate of reactivation to active disease.

Active, infectious disease, $I^{(r)}$:

$$\begin{equation}
\frac{dI^{(r)}}{dt} = uL_f^{(r)} + vL_s^{(r)} + \delta (r,1) \tau_{FL} (1 – p_{transf}) T_{FL}^{(1)} – [dk(t) + \mu_{TB} +\sigma] I^{(r)}
\end{equation}$$

where $d$ is the rates of diagnosis and treatment initiation, and $k(t)$ is the time-dependent modifier to this rate arising from COVID-related disruptions. Amongst other terms, $\delta (r,1)$ is an indicator function taking value 1 when $r = 1$ and 0 otherwise; $1/\tau_{FL}$ is the duration of first-line treatment; $p_{transf}$ is the proportion of drug-resistant patients that are switched to second-line treatment after failing first-line treatment; $\mu_{TB}$ is the per-capita hazard of mortality for untreated TB; and $\sigma$ is the per-capita rate of self-cure. 

On first-line TB treatment, $T_{FL}^{(r)}$:

$$\begin{equation}
\frac{dT_{FL}^{(r)}}{dt} = 
\begin{cases}
d k(t) I^{(0)} – (\tau_{FL} + g + \mu + a) T_{FL}^{(0)} & r = 0 \\
​      d k(t) (1 – p_{rec}) I^{(1)} + a T_{FL}^{(1)} – (\tau_{FL} + g + \mu)T_{FL}^{(1)} & r = 1
\end{cases}
\end{equation}$$

where $1/\tau_{FL}$ is the duration of first-line treatment; $g$ is the rate of treatment drop-off; $a$ is the per-capita rate of acquisition of rifampicin resistance; and amongst those with drug-resistant TB, $p_{rec}$ is the proportion that are recognised as such at the point of TB diagnosis, and initiated on appropriate, second-line treatment.

On second-line TB treatment, $T_{SL}^{(1)}$ ($r = 1$ only):

$$\begin{equation}
\frac{dT_{SL}^{(1)}}{dt} = d k(t) p_{rec} I^{(1)} + \tau_{FL} p_{transf} T_{FL}^{(1)} – (\tau_{SL} + g + \mu) T_{SL}^{(1)}
\end{equation}$$

where $1/\tau_{SL}$ is the duration of second-line treatment.

Recovered, low relapse risk (following successful treatment completion), $R_{lo}^{(r)}$:

$$\begin{equation}
\frac{dR_{lo}^{(r)}}{dt} = 
\begin{cases}
\tau_{FL} T_{FL}^{(0)} – \left(\mu + \rho_{lo} + \sum_r c\lambda^{(r)} + s\right)R_{lo}^{(0)} & r = 0 \\
\tau_{SL} T_{SL}^{(1)} – \left(\mu + \rho_{lo} + \sum_r c\lambda^{(r)} + s\right)R_{lo}^{(1)} & r = 1
\end{cases}
\end{equation}$$

where $\rho_{lo}$ is the rate of endogenous relapse amongst those with low relapse risk; $c$ is the protection from reinfection; and $s$ is the stabilisation of relapse risk.

Recovered, high relapse risk (following treatment non-completion or self cure), $R_{hi}^{(r)}$: 

$$\begin{equation}
\frac{dR_{hi}^{(r)}}{dt} = gT^{(r)} – \left(\mu + \rho_{hi} + \sum_r c\lambda^{(r)} + s\right) R_{hi}^{(r)}
\end{equation}$$
where $\rho_{hi}$ is the rate of endogenous relapse amongst those with high relapse risk.

Recovered, long term stabilisation, $R_{LT}^{(r)}$:

$$\begin{equation}
\frac{dR_{LT}^{(r)}}{dt} = s\left(R_{lo}^{(r)} + R_{hi}^{(r)}\right) - \left(\mu + \rho_{LT} + \sum_r c\lambda^{(r)}\right) R_{LT}^{(r)}
\end{equation}$$

Force of infection

$$\begin{equation}
\lambda^{(r)} = 
      \begin{cases}
      \beta^{(0)} I^{(0)}/N & r = 0 \\
      \beta^{(1)} \left(I^{(1)} + T_{FL}^{(1)}\right)/N & r = 1     
      \end{cases}
\end{equation}$$

where $\beta^{(r)}$ is the average the number of secondary infections per year per infectious case with drug resistance status $r$ in a fully susceptible population, and $N$ is the overall population size.



## 6.2 Statistical model

The incidence and mortality rate ratios for 2020 (estimate under COVID-19 disruptions over a counterfactual estimate based on pre-2020 trends) in the 16 countries modelled according to methods described in section 6.1 is predicted using linear regression models including the 2019 case detection ratio (notifications / incidence), the relative drop in case detection between 2019 and 2020 and the 2019 mortality / incidence ratio as predictors, along with interaction terms. Models were selected based on the AIC criterion. The 2020 incidence rate ratio was not found significantly associated with any of the tested predictor and a pooled ratio of 1.02 (SD 0.026) was used to predict incidence in the set of 111 countries with a 2020 shortfall in case detection compared with 2019. In contrast, the mortality model fit the data satisfactorily. Predicted ratios were constrained within the interval [1, 1.5]. Estimates for 2020 are considered provisional.



## 7. Estimation of uncertainty

There are many potential sources of uncertainty associated with estimates of TB incidence, prevalence and mortality, as well as estimates of the burden of HIV-associated TB and MDR-TB. These include uncertainties in input data, in parameter values, in extrapolations used to impute missing data, and in the models used. Uncertainty in population estimates is not accounted for. 

Notification data are of uneven quality. Cases may be under-reported (for example, missing quarterly reports from remote administrative areas are not uncommon), misclassified (in particular, misclassification of recurrent cases in the category of new cases is common), or over-reported as a result of duplicated entries in TB information systems or due to over-diagnosis. The latter issues can only be addressed efficiently in countries with case-based nationwide TB databases that include patient identifiers. Sudden changes in notifications over time are often the result of errors or inconsistencies in reporting.

Uncertainty bounds and ranges are defined as the 2.5th and 97.5th percentiles of outcome distributions. The general approach to uncertainty analyses is to propagate errors in _m_ real-valued random variables _X_ by approximating a function _h_(_X)_ using second-order Taylor series expansion about its moments<sup>34, 35</sup>. Using matrix notation, the expected value _E_[_h_(_X_)] and variance of _h_(_X_) were approximated as follows:

$$E[h(X)] \approx h(E[X]) + \frac{1}{2!} tr H(h)\Sigma(X)$$

$$Var(h(X)) \approx \nabla(h) \Sigma(X) \nabla(h)^T + \frac{1}{2!} tr ((H(h)) \Sigma(X))^2$$

where _tr_ denotes the trace, _H(h)_ the Hessian matrix of partial second-order derivatives of _h_(_X_) with respect to each _X<sub>i=1..m</sub>_, $$\nabla(h)$$ the gradient matrix of partial first-order derivatives  and $$\Sigma(X)$$ the joint covariance matrix of _X_. 

<div style="page-break-after: always; break-after: page;"></div>


## 8. Conclusion

The measurement methods described here can be combined to assess tuberculosis incidence and mortality, to evaluate progress towards targets for tuberculosis control and the SDGs for TB. Alternative TB burden estimation methods have been developed by the Institute of Health Metrics and Evaluation<sup><a href="https://paperpile.com/c/uyCckg/16mDd">36</a></sup>, with generally consistent results at the global level compared with WHO, but with marked differences in specific countries. Discrepancies in estimates from different agencies reflect the questionable quality and completeness of the underlying data. Further convergence in estimates will result from improvements in measurements at country level. National control programmes should be able to measure the level and time trends in incidence through well-performing TB surveillance with universal access to health. In countries with incomplete routine surveillance, prevalence surveys of TB disease provide estimates of TB burden that do not heavily rely on expert opinion. The performance of TB surveillance should be assessed periodically<sup><a href="https://paperpile.com/c/uyCckg/XExTU">10</a></sup> and the level of under-reporting should be measured<sup><a href="https://paperpile.com/c/uyCckg/1MqlV">9</a></sup> and minimized. Tuberculosis mortality will ideally be measured by counting deaths in a comprehensive vital registration system<sup>36/sup>. 

WHO’s post-2015 global TB strategy, known as the End TB Strategy<sup>37</sup>, has the goal of ending the global TB epidemic, with corresponding targets of a 90% reduction in TB deaths and an 80% reduction in the TB incidence rate by 2030, compared with 2015. Improved measurements through substantial investments in health information systems, TB surveillance and the broader SDG agenda will provide a firmer basis for monitoring progress towards the End TB Strategy targets and ultimate TB elimination.


## Acknowledgements

Ibrahim Abubakar, Sandra Alba, Elisabeth Allen, Martien Borgdorff, Jaap Broekmans, Ken Castro, Frank Cobelens, Ted Cohen, Charlotte Colvin, Sarah Cook-Scalise, Liz Corbett, Simon Cousens, Katherine Fielding, Peter Godfrey-Faussett, Yohhei Hamada, Rein Houben, Helen Jenkins, Aviansh Kanshar, Li Liu, Mary Mahy, Valérie Schwoebel, Cherise Scott, James Seddon, Babis Sismanidis, Andrew Thomson, Edine Tiemersma, Hazim Timimi, Theo Vos, Emilia Vynnycky and Richard White reviewed the described methods to derive TB incidence, prevalence and mortality with disaggregation by age and sex and provided specific recommendations to improve them. 



<div style="page-break-after: always; break-after: page;"></div>


## Annex 1 - Definitions

**Incidence** is defined as the number of new and recurrent (relapse) episodes of TB (all forms) occurring in a given year. Recurrent episodes are defined as a new episode of TB in people who have had TB in the past and for whom there was bacteriological confirmation of cure and/or documentation that treatment was completed.

**Prevalence** is defined as the number of TB cases (all forms) at the middle of the year.

**Mortality** from TB is defined as the number of deaths caused by TB in HIV-negative people occurring in a given year, according to the latest revision of the International classification of diseases (ICD-10). TB deaths among HIV-positive people are classified as HIV deaths in ICD-10. For this reason, estimates of deaths from TB in HIV-positive people are presented separately from those in HIV-negative people.

The **case fatality rate** is the risk of death from TB among people with active TB disease.

The **case notification** rate refers to new and recurrent episodes of TB notified for a given year. Patients reported in the _unknown history_ category are considered incident TB episodes (new or recurrent).

**Population estimates** were obtained from the World Population Prospects, which is produced by the United Nations Population Division (UNPD, [http://esa.un.org/unpd/wpp/](http://esa.un.org/unpd/wpp/)). The UNPD estimates sometimes differ from those made by countries.





<div style="page-break-after: always; break-after: page;"></div>


## Annex 2 - Relationship between HIV prevalence in new TB cases and HIV prevalence in the general population

Let _I_ and _N_ denote incident cases and the total population, respectively, superscripts + and - denote HIV status, _ϑ_ is the prevalence of HIV among new TB cases, _h_ is the prevalence of HIV in the general population and _ρ_ is the incidence rate ratio (HIV-positive over HIV-negative).

$$\rho = \frac{I^{\textrm{+}}/N^{\textrm{+}}}{I^{\textrm{-}}/N^{\textrm{-}}} > 1$$

$$\rho \frac{I^{\textrm{-}}}{I^{\textrm{+}}} = \frac{N^{\textrm{-}}}{N^{\textrm{+}}}$$

$$\rho \frac{I - I^{\textrm{+}}}{I^{\textrm{+}}} = \frac{N - N^{\textrm{+}}}{N^{\textrm{+}}}$$

$$\frac{I^{\textrm{+}}}{I} = \frac{\rho \frac{N^{\textrm{+}}}{N}}{1 + (\rho - 1)\frac{N^{\textrm{+}}}{N}} = \vartheta$$

$$\vartheta = \frac{h \rho}{1 + h(\rho - 1)}$$

The TB incidence rate ratio _ρ_ can be estimated by fitting the following linear model with a slope constrained to 1

$$\log(\hat{\rho}) = \log \left(\frac{\vartheta}{1-\vartheta}\right) - \log \left(\frac{h}{1-h}\right), (\vartheta, h) \in {]0,1[}$$




<div style="page-break-after: always; break-after: page;"></div>


## Annex 3 - Estimating the number of household contacts of a bacteriologically confirmed pulmonary cases less than 5 years old

In low TB burden countries (113 high-income or upper middle-income countries with an estimated incidence rate less than 100 per 100 000 population), the number of child household contacts eligible for LTBI treatment is defined as the number of children under 5 years of age who are household contacts of bacteriologically confirmed pulmonary TB cases and have LTBI defined as a positive result to a standard tuberculin test or an IGRA test. In high TB burden countries, the number eligible is defined as the number of child household contacts without active TB, based on the current WHO recommendations that do not require LTBI testing among child household contacts &lt;5 years prior to the provision of preventive treatment in these countries.<sup>1,2</sup>  

The estimated number _n_ of child household contacts eligible for LTBI treatment is 

$$n=\frac{b}{c}HpL(1-t) $$

where _b_ is the number of notified bacteriologically confirmed pulmonary TB, _c_ is the average number of TB cases per household, _H_ is the average household size, _p_ is national proportion of children &lt;5 years, _t_ is proportion of child household contacts with active TB, and _L_ is prevalence of LTBI among child household contacts &lt; 5 years old. In high TB burden countries, _L_ is set to 1 (testing for LTBI is not required). The following sources of uncertainty are accounted for: prevalence of LTBI, variance in the count of TB cases per household, and in the proportion of child household contacts with active TB. Uncertainty about United Nations Population Division (UNPD) population size is not documented. Errors were propagated using methods described in chapter 7. 



<table>
  <tr>
   <td>Parameters
   </td>
   <td>Values
   </td>
   <td>Sources
   </td>
  </tr>
  <tr>
   <td>
    Number of notified bacteriologically confirmed pulmonary TB in 2015
   </td>
   <td>
    Differ by country
   </td>
   <td>
    WHO global TB database
   </td>
  </tr>
  <tr>
   <td>
    National proportion of children &lt;5 years of age in 2015
   </td>
   <td>
    Differ by country
   </td>
   <td>
    2015 Revision of World Population, United Nations Population Division  (https://esa.un.org/unpd/wpp/)
   </td>
  </tr>
  <tr>
   <td>
    National average household size
   </td>
   <td>
    Differ by country
   </td>
   <td>
    National censuses, DHS  statistical year books, or official websites of the national statistical authorities
   </td>
  </tr>
  <tr>
   <td>
    Prevalence of LTBI among child household contacts &lt;5years old in LBC
   </td>
   <td>
    Constant across countries = 27.6% (19.2%-38.0%)
   </td>
   <td>
    Systematic review of literature from LBC up to Dec 2015 (unpublished)
   </td>
  </tr>
  <tr>
   <td>
    Average cluster size of active TB per household
   </td>
   <td>
    Constant across countries =1.06 (95%CI 1.04-1.08)
   </td>
   <td>
    Systematic review of literature between Jan 2005 and Dec 2015 (unpublished)
   </td>
  </tr>
  <tr>
   <td>
    Proportion of children &lt; 5 years old with active TB  among those who had a household contact with TB cases
   </td>
   <td>
    Constant across countries =6.1% (95%CI 1.0%-16.3%)
   </td>
   <td>Dodd et al, Lancet Glob Health. 2014<sup>3</sup>
      </td>
</table>


1. World Health Organization Recommendations for Investigating Contacts of Persons with Infectious Tuberculosis in Low- and Middle-Income CountriesGeneva, Switzerland: WHO, 2012.

2. World Health Organization. Guidelines for intensified tuberculosis case-finding and isoniazid preventive therapy for people living with HIV in resource-constrained settings. Geneva, Switzerland: WHO, 2011.

3. Dodd PJ, Gardiner E, Coghlan R, Seddon JA. Burden of childhood tuberculosis in 22 high-burden countries: a mathematical modelling study. _Lancet Glob Health_ 2014; **2**(8): e453-9.

   

<div style="page-break-after: always; break-after: page;"></div>


## Tables

**Table 1**: Distribution of disease duration by case category


<table>
  <tr>
   <td>Case category
   </td>
   <td>Distribution of disease duration  \
(year)
   </td>
  </tr>
  <tr>
   <td>Treated, HIV-negative
   </td>
   <td>Uniform (0.2−2)
   </td>
  </tr>
  <tr>
   <td>Not treated, HIV-negative
   </td>
   <td>Uniform (1−4)
   </td>
  </tr>
  <tr>
   <td>Treated, HIV-positive
   </td>
   <td>Uniform(0.01−1)
   </td>
  </tr>
  <tr>
   <td>Not treated, HIV-positive
   </td>
   <td>Uniform (0.01−0.2)
   </td>
  </tr>
</table>


**Table 2**: Incidence estimation based on _U_/_T_ 


<table>
  <tr>
   <td>
   </td>
   <td>U
<p>
(<em>n</em>)
   </td>
   <td>T
<p>
(<em>n</em>)
   </td>
   <td>Prevalence
<p>
(10<sup>-3</sup>)
   </td>
   <td>Duration
<p>
(year)
   </td>
   <td>Incidence
<p>
(10<sup>-3</sup>y<sup>-1</sup>)
   </td>
  </tr>
  <tr>
   <td>Cambodia 2002
   </td>
   <td>260
   </td>
   <td>42
   </td>
   <td>12 (10-15)
   </td>
   <td>2.9 (1.9-4)
   </td>
   <td>4 (2.5-5.8)
   </td>
  </tr>
  <tr>
   <td>Cambodia 2011
   </td>
   <td>205
   </td>
   <td>80
   </td>
   <td>8.3 (7.1-9.8)
   </td>
   <td>1.2 (0.8-1.6)
   </td>
   <td>6.7 (4.5-9.3)
   </td>
  </tr>
  <tr>
   <td>Myanmar 2009
   </td>
   <td>300
   </td>
   <td>79
   </td>
   <td>6.1 (5-7.5)
   </td>
   <td>1.8 (1.1-1.6)
   </td>
   <td>3.3 (2-4.8)
   </td>
  </tr>
  <tr>
   <td>Thailand 2012
   </td>
   <td>136
   </td>
   <td>60
   </td>
   <td>2.5 (1.9-3.5)
   </td>
   <td>1.1 (0.5-1.6)
   </td>
   <td>2.3 (1-3.5)
   </td>
  </tr>
</table>


<div style="page-break-after: always; break-after: page;"></div>

**Table 3**: Estimates of incidence derived from prevalence survey results, based on two estimation methods.


<table>
  <tr>
   <td>
   </td>
   <td>Prevalence
<p>
(<em>10<sup>-3</sup></em>)
   </td>
   <td>Incidence - Method 1
<p>
(<em>10<sup>-3</sup>y<sup>-1</sup>)</em>
   </td>
   <td>Incidence - Method 2
<p>
(<em>10<sup>-3</sup>y<sup>-1</sup>)</em>
   </td>
  </tr>
  <tr>
   <td>Cambodia 2002
   </td>
   <td>12 (10-15)
   </td>
   <td>4 (2.5-5.8)
   </td>
   <td>2.2 (1.5-2.9)
   </td>
  </tr>
  <tr>
   <td>Cambodia 2011
   </td>
   <td>8.3 (7.1-9.8)
   </td>
   <td>6.7 (4.5-9.3)
   </td>
   <td>3.8 (2.2-5.8)
   </td>
  </tr>
  <tr>
   <td>Myanmar 2009
   </td>
   <td>6.1 (5-7.5)
   </td>
   <td>3.3 (2-4.8)
   </td>
   <td>3.5 (2-5.1)
   </td>
  </tr>
  <tr>
   <td>Thailand 2012
   </td>
   <td>2.5 (1.9-3.5)
   </td>
   <td>2.3 (1-3.5)
   </td>
   <td>1.1 (0.7-1.6)
   </td>
  </tr>
</table>


**Table 4**: Distribution of CFRs by case category

<table>
  <tr>
   <td>
   </td>
   <td>CFR
   </td>
   <td>Sources
   </td>
  </tr>
  <tr>
   <td>Not on TB treatment <em>f<sub>u</sub></em>
   </td>
   <td>0.43 (0.28-0.53)
   </td>
   <td><sup>38,39</sup>
   </td>
  </tr>
  <tr>
   <td>On TB treatment <em>f<sub>t</sub></em>
   </td>
   <td>0.03 (0-0.07)
   </td>
   <td><sup>40</sup>
   </td>
  </tr>
</table>


**Table 5**: Distribution of CFR in HIV-positive individuals

<table>
  <tr>
   <td><strong>ART</strong>
   </td>
   <td><strong>TB treatment</strong>
   </td>
   <td><strong>CFR</strong>
   </td>
   <td><strong>Sources</strong>
   </td>
  </tr>
  <tr>
   <td>off
   </td>
   <td>off
   </td>
   <td>0.78 (0.65-0.94)
   </td>
   <td><sup>38</sup>
   </td>
  </tr>
  <tr>
   <td>off
   </td>
   <td>on
   </td>
   <td>0.09 (0.03-0.15)
   </td>
   <td>  <sup>40,41</sup>
   </td>
  </tr>
  <tr>
   <td>&lt; 1 year
   </td>
   <td>off
   </td>
   <td>0.62 (0.39-0.86)
   </td>
   <td>Data from review + assumptions
   </td>
  </tr>
  <tr>
   <td>&lt; 1 year
   </td>
   <td>on
   </td>
   <td>0.06 (0.01-0.13)
   </td>
   <td>Data from review + assumptions
   </td>
  </tr>
  <tr>
   <td>≥ 1 year
   </td>
   <td>off
   </td>
   <td>0.49 (0.31-0.70)
   </td>
   <td>Assumptions
   </td>
  </tr>
  <tr>
   <td>≥ 1 year
   </td>
   <td>on
   </td>
   <td>0.04 (0.00-0.10)
   </td>
   <td>Assumptions
   </td>
  </tr>
</table>



<div style="page-break-after: always; break-after: page;"></div>



## References

1	Dye C, Bassili A, Bierrenbach AL, et al. Measuring tuberculosis burden, trends, and the impact of control programmes. Lancet Infect Dis 2008; 8: 233–43.

2	Styblo K. Styblo K. The Relationship between the risk of tuberculous infection and the risk of developing infectious tuberculosis. Bulletin of the International Union against Tuberculosis and Lung Diseases 1985; : 117–9.

3	Van leth F. Prevalence of tuberculous infection and incidence of tuberculosis; a re-assessment of the Styblo rule. Bull World Health Organ 2008; 86: 20–6.

4	Dinnes J, Deeks J, Kunst H, et al. A systematic review of rapid diagnostic tests for the detection of tuberculosis infection. Health Technol Assess 2007; 11: 1–196.

5	Eilers PHC, Borgdorff MW. Modeling and correction of digit preference in tuberculin surveys. Int J Tuberc Lung Dis 2004; 8: 232–9.

6	Rieder HL. Methodological issues in the estimation of the tuberculosis problem from tuberculin surveys. Tuber Lung Dis 1995; 76: 114–21.

7	Sudre P, ten Dam G, Kochi A. Tuberculosis: a global overview of the situation today. Bull World Health Organ 1992; 70: 149–59.

8	Dye C, Scheele S, Dolin P, Pathania V, Raviglione MC, Others. Global burden of tuberculosis: estimated incidence, prevalence, and mortality by country. JAMA 1999; 282: 677–86.

9	WHO. Assessing tuberculosis under-reporting through inventory studies. World Health Organization, 2012.

10	WHO. Standards and Benchmarks for tuberculosis surveillance and vital registration systems. WHO, 2014 http://www.who.int/tb/publications/standardsandbenchmarks/.

11	WHO. Improving estimation of TB disease burden via systematic assessment of surveillance data. http://www.who.int/tb/advisory_bodies/impact_measurement_taskforce/meetings/improving_estimates/en/.

12	Renyi A. Probability Theory. New York: Dover Publications Inc, 2007.

13	Dye C. The Population Biology of Tuberculosis. Princeton University Press, 2015.

14	Viechtbauer W. Conducting Meta-Analyses in R with the metafor Package. Journal of Statistical Software 2010; 36: 1–48.

15	Bassili A, Grant AD, El-Mohgazy E, et al. Estimating tuberculosis case detection rate in resource-limited countries: a capture-recapture study in Egypt. Int J Tuberc Lung Dis 2010; 14: 727–32.

16	Huseynova S, Hashim DS, Tbena MR, et al. Estimating tuberculosis burden and reporting in resource-limited countries: a capture-recapture study in Iraq. Int J Tuberc Lung Dis 2013; 17: 462–7.

17	van HEST NAH, Smit F, Baars HWM, et al. Completeness of notification of tuberculosis in The Netherlands: how reliable is record-linkage and capture-recapture analysis? Epidemiol Infect 2007; 135: 1021–9.

18	Anderson L, Moore J, Pedrazzoli D, et al. Tuberculosis in the UK. 2010.

19	Bassili A, Al-Hammadi A, Al-Absi A, et al. Estimating the tuberculosis burden in resource-limited countries: a capture-recapture study in Yemen. Int J Tuberc Lung Dis 2013; 17: 456–61.

20	Al-Absi A, Bassili A, Abdul Bary H, et al. The decline of tuberculosis in Yemen: evaluation based on two nationwide tuberculin surveys. Int J Tuberc Lung Dis 2009; 13: 1100–5.

21	Dodd PJ, Sismanidis C, Glaziou P. Methods for estimating tuberculosis incidence and mortality by age and sex. Int J Epidemiol 2021; 50: 570–7.

22	WHO. Definitions and reporting framework for tuberculosis 2013 revision WHO. WHO, 2013 http://www.who.int/tb/publications/definitions/.

23	Van Hest NAH, Story A, Grant AD, Antoine D, Crofts JP, Watson JM. Record-linkage and capture–recapture analysis to estimate the incidence and completeness of reporting of tuberculosis in England 1999–2002. Epidemiol Infect 2008; 136: 1606.

24	Lestari T, Probandari A, Hurtig A-K, Utarini A. High caseload of childhood tuberculosis in hospitals on Java Island, Indonesia: a cross sectional study. BMC Public Health 2011; 11: 784.

25	Coghlan R, Gardiner E, Amanullah F, et al. Understanding Market Size and Reporting Gaps for Paediatric TB in Indonesia, Nigeria and Pakistan: Supporting Improved Treatment of Childhood TB in the Advent of New Medicines. PLoS One 2015; 10: e0138323.

26	WHO. WHO Global Task Force on TB Impact Measurement - Sub-group on strengthening surveillance. 2014; published online Nov 6. http://www.who.int/tb/advisory_bodies/impact_measurement_taskforce/meetings/sep14indonesia_inventorystudyworkshop/en/ (accessed Oct 15, 2015).

27	Horton KC, MacPherson P, Houben RMGJ, White RG, Corbett EL. Sex Differences in Tuberculosis Burden and Notifications in Low- and Middle-Income Countries: A Systematic Review and Meta-analysis. PLoS Med 2016; 13: e1002119.

28	Dodd PJ, Gardiner E, Coghlan R, Seddon JA. Burden of childhood tuberculosis in 22 high-burden countries: a mathematical modelling study. The Lancet Global Health 2014; 2: e453–9.

29	Müller B, Dürr S, Alonso S, et al. Zoonotic Mycobacterium bovis-induced tuberculosis in humans. Emerg Infect Dis 2013; 19: 899–908.

30	World Health Organization. Who Estimates of the Global Burden of Foodborne Diseases: Foodborne Disease Burden Epidemiology Reference Group 2007-2015. 2016.

31	Mathers CD, Fat DM, Inoue M, Rao C, Lopez AD. Counting the dead and what they died from: an assessment of the global status of cause of death data. Bull World Health Organ 2005; 83: 171–7.

32	Dodd PJ, Yuen CM, Sismanidis C, Seddon JA, Jenkins HE. The global burden of tuberculosis mortality in children: a mathematical modelling study. Lancet Glob Health 2017; 5: e898–906.

33	Cilloni L, Fu H, Vesga JF, et al. The potential impact of the COVID-19 pandemic on the tuberculosis epidemic a modelling analysis. EClinicalMedicine 2020; 28: 100603.

34	Ku HH. Notes on the use of propagation of error formulas. Journal of Research of the National Bureau of Standards, Section C: Engineering and Instrumentation 1966; 70C: 263.

35	Lab AS. An Introduction To Error Propagation: Derivation, Meaning and Examples of Equation C_Y=F_X C_X F^X_T. Swiss Federal Institute of Technology, Lausanne, 1998 http://www.nada.kth.se/~kai-a/papers/arrasTR-9801-R3.pdf.

36	Murray CJL, Ortblad KF, Guinovart C, et al. Global, regional, and national incidence and mortality for HIV, tuberculosis, and malaria during 1990–2013: a systematic analysis for the Global Burden of Disease Study 2013. Lancet 2014; 6736: 1–66.

37	Uplekar M, Weil D, Lonnroth K, et al. WHO’s new end TB strategy. Lancet 2015; 385: 1799–801.

38	Corbett EL, Watt CJ, Walker N, et al. The growing burden of tuberculosis: global trends and interactions with the HIV epidemic. Arch Intern Med 2003; 163: 1009–21.

39	Tiemersma EW, van der Werf MJ, Borgdorff MW, Williams BG, Nagelkerke NJD. Natural history of tuberculosis: duration and fatality of untreated pulmonary tuberculosis in HIV negative patients: a systematic review. PLoS One 2011; 6: e17601.

40	Straetemans M, Glaziou P, Bierrenbach AL, Sismanidis C, van der Werf MJ. Assessing tuberculosis case fatality ratio: a meta-analysis. PLoS One 2011; 6: e20755.

41	Mukadi YD, Maher D, Harries A. Tuberculosis case fatality rates in high HIV prevalence populations in sub-Saharan Africa. AIDS 2001; 15: 143–52.

