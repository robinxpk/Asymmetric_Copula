# Asymmetric Copula

Repo used for reproducing and extending the paper: 
[Asymmetric copula in multivariate flood frequency analysis](https://doi.org/10.1016/j.advwatres.2005.09.005)

## TODOs
- Summary on tail dependence 
- Relevancy of asymmetry in my copula (i.e. non-symmetric BIVARAITE copula)? 
- Simulate data according to Li's paper on copula
- Try to recover DGP modelling KENDALLS TAU! \
Do not model parameters themselves bc then things might get weird:\
Effect of parameters on the dependence structure depends on the generator function, but Kendall's tau is constant for all the copula types! Considering this, non-parametric estimation of thetas via Kendall's tau sounds pretty interesting... Zhangp.150. That is: 
I model Kendalls tau, then I select the most appropriate Copula from which I derive the "link" function i.e. parameter as function of kendall's tau. Then, I can interpret model on kendall level and translate that even onto parameter level (probably limited tho). It still might be interesting to compare the parametric (ML) estimates of the parameter with Kendall's tau estimate. That is, compare the fit suggested by both.
Note: I think parametric modelling of the parameter and then fit a model to it might get weird (again) due to different generator function i.e. parameters imply different dependence strucutre. Thus, modelling Kendall's tau is valid for any copula, while modelling theta depends on exact copula.
> Introduce connection parameter and tau in Paper. 
> If I want to compare non-parametric and parametric fit, maybe use p153 Zhang? Here, the comparison of fits is used to identify a best fitting copula
(Estimation p99)
- Tail dependency:
Tail dependence dependson on the type of copula and its parameter, i.e.: 
> Gaussian copula: No Taildependence (LTD = UTD = 0)
> Clayton: strong LTD (i.e. > 0) 
> Gumbel: strong UTD (i.e. > 0)
> Frank: LTD = UTD = 0
In practice: Captures co-occurence of extreme event --> HIGHLY relevant! If two rivers exhibit strong upper-tail dependence, extreme floos are more likely to occure together (and vice versa for droughts)
For modelling: Choose appropriate copula and estimate parameter. Tail dependence is implicitly given (is function of this parameter)
USING FITTED COPULA: Simulate extreme event scenarios to assess worst-case scenario! (Sounds very interesting)
- !!! Archimedean copulas cannot model negative dependence strucutre (really? Kinda read this on a thread only) This is due to the generator function being completely monotone --> Positive dependence only; What makes it more clear: Archimedean copulas are radially symmetric, that is, only positive correlation. BUT work around is modelling u' = 1-u instead of u itself I think? Then u' is enforced to have positive dependence while this implies negative dependence for u. That means the Kendall's tau I model MUST NOT be negative! (Should I use Beta distribution then? Would ensure meaningful values for tau) ALSO, to ensure to condition of thetas having this required order, I can model nested Copula: tau, outer level: tau_outer - tau_inner ~ beta (Not sure if Beta is reasonable bc sum of two beta). 

## Approach
### Given Data
#### For each station
1.1) Kendall's tau\
1.2) Fit copula\
1.3) Goodness-of-fit, etc\
1.4?) Anything else interesting here? Can I use fitted copula for something? 
Check paper for what they did here
#### Between stations
2.1) Model tau~. (Bayes?)\
2.2) How does tau bzw. betas influence the stations level behavior? What else to conclude?\
Hydrology point of view: Interesting in how e.g. slope affects extreme events. How could I visualize this? Maybe:\
tail dependence as function of parameter = as function of kendall's tau = as function of lin. pred --> plot as function of lin. pred\
--> "Sensitivity analysis"?
### Simulation
1) Model Kendall's tau using 

## Qs
- Due to the nested nature in NACs, can I consider each copula independently? i.e.: 
For trivariate, I consider the most inner copula independent in the sense of goodness of fit / modelling kendall's tau based on other coviariates. For the outer copula, all I need to consider is the condition on the dependence parameter thea. Thus, given the inner fit, can I consider goodness of fit between 3rd variable and inner copula (i.e. joint distribution / inner copula) independent of the inner fit? 
To the point: Is it valid to fit, consider goodness of fit and modelling from inside out? The next higher level then uses the inner fit and thus is kind of this "conditioned on a given inner fit" 
I mean, the R package _HAC_ estimates its parameters like this, but I cannot find a source stating this explicitly
- Want to model 
1) inner kendall's tau using GLM with beta response bc non-negative and between 0 and 1
2) outer kendall's tau by using inner - outer >= 0 to ensure the condition that inner must be larger than outer. Can I again use a beta distribution here? I mean, both Kendall's tau are dependent / correlated for sure. Is sum of correlated beta distributed RV still beta bzw. still reasonable to apply beta?
-> What are even the parameters estimated of the beta? I mean, I can easily model skewness using beta. Location and scale both for difference? No idea... !! Check paper, what do they use??

### References
- Bayesian Copula idea: Li - Improving forecasting performance using covariate-dependent copula models





### Notes during Data wrangling
- Discharge highly depends on what river and where at a given river we look at (i.e. at which station for a given river). 
Thus, the discharge threshold at which a flood event occures should vary. 
The flood event dependence structure / the copula is independent of exact values however.\
Idea: Use p-quantile of yearly discharge to identify when the flood event begins. Of all flood events, we choose the one with the highest peak discharge. 
Thereby we can\
a) Can compare dependence strucutre between rivers of different size since the dependence structure is scale independent\
b) We could also filter by p-quantile values (e.g. p-quantile > 100 discharge --> only look at large rivers)\
c) Let p in p-quantile vary to move the threshold i.e. see how dependence strucutre changes for varying threshold (like paper did)\
Visualization of different discharge behaviors: For each station, determine mean / quantile / peak of a year. Plot boxplot of distribution over the years for each station to compare distribution between stations / rivers / river spots.\
Issue: quantile flood indicator is sometimes quite off. Maybe it is more reasonable to use the slope as indicator? Like "at steepest point, flood event beginns" -> Value is threshold -> flood ends when threshold reached again. Potential issue I see here is when the steepest slope is at a very low threshold. Maybe I can use: Steepest slope as indicator when flood begins, but then I use mean / quantile as threshold for duration of flood event?
NEVERMIND: Choosing steepest slope and then another statistic to determine the threshold is meaningless. wtf. The peak of discharge is used to identify flood. That is, using the steepest slope and then still using another statistic is the same as just using another statistic.\
i.e. improve quantile approach by using a SMALLER quantile!\
--> Adjusting quantiles works fine! 
FOR LATER: Moving threshold (as paper does) could floods to be not well identified. Thus, changing dependence structure here might not be too meaningful since the flood event is not meaningful identified. Too large p (p = 0.95):
![p too high](./READMEpics/ptooghigh16000708_25.png)
![good p](./READMEpics/16000708_25.png)
![p too high](./READMEpics/ptooghigh16000708_35.png)
![good p](./READMEpics/16000708_35.png)
![p too high](./READMEpics/ptooghigh16001303_1.png)
![good p](./READMEpics/16001303_1.png)
![p too high](./READMEpics/ptooghigh16001303_2.png)
![good p](./READMEpics/16001303_2.png)


- Not all years contain meaningful data. The copula data set should be fine even with some missing years in between...\
Mostly the first year of the data is iffy. (For now) I drop the first year which usually contains only little observations (e.g. Moosburg in 2015 only 1 observation, Freising ~3000 observations (one every 15minutes) within 1 year..)

