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
- BRO HAS A WEBSITE:
https://tu-dresden.de/bu/verkehr/ivw/osv/die-professur/inhaber-in





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
Mostly the first year of the data is iffy. (For now) I drop the first year which usually contains only little observations (e.g. Moosburg in 2015 only 1 observation, Freising ~3000 observations (one every 15minutes) within 1 year..)\
There are also cases where years contain no data at all or very little:
![shit data](./READMEpics/16000708_40.png)
This is garbage and could lead to outliers in further analysis causing headaches.. Don't need those.\
Here is the completeness distribution for Donau and Isar. 
![completeness](./READMEpics/CompletenessBoxplots.png)
For these two the majority of years are complete (we have 30 stations / observations for each year). Thus, for years to be included in our analysis, they are required to be $90% $ complete.\
Note: Ratio larger than 1 due to leap year bc I calculated ratio by non leap year days\
I arbitrarily conclude that $85% $ is good threshold (blue line)

### Notes during single analysis (Station: Munich, River: Isar)
#### Paper
- Practical applications mentioned (p.1160):\
trivariate info required for design of expansion basin / diversion channel.\
building synthetic design hydrographs -> HOW?\
conditional joint density / distribution of volume and duration given peak
- Varying thresholds (p. 1162); They use FIXED thresholds tho. And given table2, their rivers do not seem too compareable tbh...\
I could try both: quantile and fixed, but I think quantile is more meaningful bc dependence structure is scale invariant anyway so relative consideration of flood event seems appropriate! 

Following is based on _fixed_ threshold. Paper does not mention how they decided on this fixed value (p. 1163, section 5.2 mentions this fixed threshold tho)

- Estimate Kendall's tau and test for significance (test not really necessary IMO)
- Identifying univariate distribution; ONLY if I use some parametric approach. Else I just use empirical copula. IF I use parametric approach too, I should check behavior of missspecification in simulation
- Estimate parameter for all considered copulas / generators: inner and outer generator as well as assumptions of symmetric trivariate AC\
(Using Canonical ML; This is also implemented in HAC; TODO: Read up on what that is and if its equivalent to iterative nested ML)\
For each estimate, also give the 95% likelihood profile CI (What is that?)
- From all fitted copulas, select best fitted\
Selection via: 
Visual comparison\
superimpose 5000 pairs of generated samples\
Hypothesis test\
(proposed by Chen in source 3)
- Comparison copula performance vs traditional model in flood analysis (WONT DO THIS), BUT the comparison is interesting: \
Compare two models via conditioning on values of Peak and plotting MOST PROBABLE pairs of volume and duration, i.e. mode of conditional cdf 

Considered Copulas: M3, M4, M6, M12

#### Ideas
- Initial plan: Model tau in dependence of other variable; Now: Estimates based on tau do not seem to perform that well in copula estimation process? 
##### Estimation
- Kendall's tau
- Use functions form to estimate parameters based on tau --> Don't bc estimates seem to be not as good..
- Copula selection (see also goodness-of-fit)
(- Kendall's tau as response --> estimate coefficients of linear predictor; POSTPONED until I am done with main copula modellling)
- CANNOT use any available package to estimate with mixed generator functions. Would have to implement this myself. Cannot just estimate nestedly bc then joint variable is considered as a fixed variable which biases the estimates...
What I would need is to determine the likelihood for all the different generator functions (I think) and then just run constraint opimizer to ensure the conditions on the parameters
FOR NOW, use SAME copula families I guess.... IF I HAVE TIME, I can implement constraint optimization on the copula likelihood... Check the paper Okhrin - on the structure and estimation AND Hofert - Densities of nested Archimedean copulas

Note: I cannot estimate recursively, but I can apply GOF recursively bc then I use the unbiased estimates anyway..
##### Goodness of fit
- Comparison observed and generated
##### Practical application
- Tail dependence
- ? "Synthetic design hydrographs" (What ever Grimaldi paper means by that)




# Vine copulas
- Naglers paper for my purpose is a banger. BUT they also model tau? That seems odd given that the other paper mentions bias due to Jensen's inequality. Can I run simulation modelling both and check if there is a bias or not?? Also, how does estimate behave if I change response function? The closer to linear, the less bias, right? For identity, we should not have bias but the model would then be wrong, no? Evaluate true MSE in simulation
For simulation I'd like to evaluate true MSE and some goodness of fit statistic and simultaneously check if that works 
sim: https://tvatter.github.io/gamCopula/
- TIME-VARYING MIXTURE COPULA MODELS WITH COPULA SELECTION; I think they allow both: Time varying weights on mixed copula structure AND estimation of copula params as function of covariates. Not too sure tho. Doesn't seem too hard either(?) BUT I think I would have to implement everything myself. So postpone this for now. 
- I have coordinates of stations; I could use L2 norm to determine distance and use as input for time. Issue is that number of estimates increases 
Maybe not advisable bc only 50 observations each (50 years with 1 flood event each)

- Trees are visual representation of the complex product 



!! For now, focus on vine copulas as alternative to NACs bc the issue at hand is the violated assumption of NACs. Regression task is more relevant later....
TODO: Write functions during simulation s.t. they are widely applicable to my data.
I can even compare tau-modelled NACs with tau modelled Vines
