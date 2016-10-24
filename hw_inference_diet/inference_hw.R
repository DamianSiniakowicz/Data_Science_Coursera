# inference hw


# title: the exponential distribution and the sampling distribution of its mean

### summary
# I am going to show you the exponential distribution
# Then I will show you the sampling distribution of the mean for the exp distro

### exponential distribution
# this is a histogram of 1000 random values from the exponential distribution with rate = 0.2
hist(rexp(n=1000,rate = .2))


### sampling distibution of the mean 
means <- NULL
for(i in 1:1000){
        the_mean <- mean(rexp(n=40, rate = .2))
        means <- c(means, the_mean)
}
# this is a histogram the means of 1000 size 40 samples taken from the exponential distribution with rate = 0.2
hist(means)

### mean
sam_mean <- mean(means)
# the theoretical mean of a sampling distribution of the mean is equal to the mean of the distribution from which the samples were drawn
# the theoretical mean of an exponential distro is 1/rate
# so the expected mean of our sampling distro is 1/.2, which equals 5
# our observed mean of the sampling distro is `r sam_mean`


### variance
# the theoretical variance of the exponential distribution is 1/rate = 1/.2 = 5
# and for the sampling distribution...
# the law of large numbers say that as sample size increases, the mean of the samples approaches the population mean
# therefore the variance of the sampling distro decreases as sample size increases
# to be exact, the theoretical variance of the sampling distribution of the mean equals: 
# (Population_Standard_Deviation / sqrt(Sample_Size)) ^ 2
theo_var <- (5/sqrt(40))^2
# in our case the theoretical variance of the sampling distro is `r theo_var`
sam_var <- var(means)*(40/39)
# the observed variance of our sampling distro was `r sam_var`

### normality
# normal distributions follow the 68-95-99 rule
# about 68% of values should fall within one standard deviation of the mean, etc.
sam_sd <- sqrt(sam_var)
one_sd <- mean(means > sam_mean - sam_sd & means < sam_mean + sam_sd)
two_sd <- mean(means > sam_mean - 2*sam_sd & means < sam_mean + 2*sam_sd)
three_sd <- mean(means > sam_mean - 3*sam_sd & means < sam_mean + 3*sam_sd)
# in our case
# `r one_sd*100`% of values fall within one standard deviation of the mean
# `r two_sd*100`% of values fall within two standard deviations of the mean
# `r three_sd*100`% of values fall within three standard deviations of the mean
# so the sampling distro is approximately normal, as expected



## title: Vitamin C and Tooth Growth in Guinea Pigs
# load the data
library(ggplot2)
library(datasets)
data("ToothGrowth")
## Exploration
# I crossed the dose and supplement factors, and plotted tooth length for each level
ToothGrowth$DS <- interaction(ToothGrowth$supp, ToothGrowth$dose)
ggplot(data = ToothGrowth, aes(y=len, x=sort(DS), fill = supp)) + geom_boxplot() + scale_x_discrete(labels=c("0.5","1.0","2.0","0.5","1.0","2.0")) + labs(x="Dose", title="Effect of Dose and Supplement on Length")

## Analysis

# I wrote a function that would do a two-sample t-test on pairs of tooth length vectors
# the null hypothesis for these tests will be that the difference of the means is 0 

do_t_tests <- function(a_list,alt_hyp){
        # performs two-sample t-tests on all list items
        for(group in seq_along(1:(length(a_list) - 1))){
                for(other_group in (group + 1):length(a_list)){
                    t_test <- t.test(a_list[[other_group]],a_list[[group]],alternative=alt_hyp)    
                    t_tests[[paste((names(a_list))[other_group]," vs. ",names(a_list)[group])]] <<- t_test
        }
        }
}


# I made a list of 6 tooth-length vector, each for one dose-supplement factor leel
supp_dose_combos <- list()
for(supp_dose_pair in levels(ToothGrowth$DS)){
        supp_dose_combos[[supp_dose_pair]] <- ToothGrowth[(ToothGrowth$DS == supp_dose_pair),]$len
}


# I split the 6 vectors into 2 groups, one for guinea pigs that got OJ, one for gp's that got VC
OJ_groups <- supp_dose_combos[grepl(pattern="OJ",x=names(supp_dose_combos))]
VC_groups <- supp_dose_combos[grepl(pattern="VC",x=names(supp_dose_combos))]

supp_group <- list(OJ_groups=OJ_groups, VC_groups=VC_groups)

# I also split the 6 vectors into 3 groups, one for each dose level: 0.5, 1, and 2
half_groups <- supp_dose_combos[grepl(pattern="0.5",x=names(supp_dose_combos))]
one_groups <- supp_dose_combos[grepl(pattern="1",x=names(supp_dose_combos))]
two_groups <- supp_dose_combos[grepl(pattern="2",x=names(supp_dose_combos))]

dose_group <- list(half_groups=half_groups,one_groups=one_groups,two_groups=two_groups)

# I performed two-sample t-tests on all samples that had the same level for exactly one factor 
t_tests <- list()

for(each_group in supp_group){
    do_t_tests(each_group,alt_hyp = "greater")
}
for(each_group in dose_group){
    do_t_tests(each_group,alt_hyp = "two.sided")
}

# I displayed preliminary results
# if 0 did not fall in the 95% confidence interval then the groups were said to be significantly different
for(ind in 1:length(t_tests)) {ifelse(sum(0 > t_tests[[ind]]$conf.int)==0 | sum(0 > t_tests[[ind]]$conf.int)==2 , print(paste(names(t_tests)[[ind]],"are significantly different")) ,print(paste(names(t_tests)[[ind]],"are not significantly different")))}

# I applied the Benjamini-Hochberg correction to fix the FDR at 5%
p_scores <- numeric()
for(t_tesst in t_tests){p_scores <<- c(p_scores,t_tesst$p.value)}
adjusted_p_scores <- p.adjust(p_scores,method="BH")
for(ind in 1:length(t_tests)) {ifelse(adjusted_p_scores[ind] < .05 , print(paste(names(t_tests)[[ind]],"are significantly different")) ,print(paste(names(t_tests)[[ind]],"are not significantly different")))}


# I plotted the p-scores of two sample t-tests on samples with fixed dose
the_names <- names(t_tests)

Supp_P <- adjusted_p_scores[7:9]
Supp_name <- the_names[7:9]


sup_df <- data.frame(Supp_P,Supp_name)


par(mfrow=c(1,1))
with(sup_df,plot(Supp_P~Supp_name,main="probability of no significant difference between supplement effectiveness at different doses"))


    
## Conclusion
# it appears that larger vitamin C doses significantly increase tooth growth in guinea pigs
# OJ is a more effective supplement at low doses, at high doses it seems to be about as effective as VC
# Assumptions
# all environmental factors other than supplement type and dosage were kept constant across groups
# all guinea pigs were from the same population
# the guinea pigs in the study are representative of the larger guinea pig population my reader wishes to apply my inferences to 
