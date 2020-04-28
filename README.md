# Carbajal_Capstone2.0
---
title: "Carbajal_Darwin_Capstone"
author: "Darwin Carbajal"
---


```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(ggplot2)
```

1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**In order to advance heart-on-a-chip technologies to better study disease modelling we want to develop a chip that can pace iteself using pacemaker cells instead of the the traditional way of using an external electrical source. The research question that interests me is to investigate which ion channel genes are better at pacing cardiac human-induced pluripotent cells (h-IPSCs)**


2) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**It is not known if heart-on-a-chip systems can be paced using cardiac cells that have been functionally re-engineered to express hyperpolarized-activated cyclic nucleotide-gated 4 (HCN4) or HCN2 ion channel genes. To be specific, it is not known if expressing HCN4 on cardiac h-IPSCs can drive increased self-pacing as measured by beats per minute compared to expressing HCN2.**

3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If cardiac human induced pluripotent stem cells (h-IPSCs) express HCN4 genes, then they will have increased beats/min compared to h-IPSCs expressing HCN2.**

4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The dependent variable that will be observed are beats/min. The predictor values that will be manupilated is expression of HCN4 gene vs HCN2 in cardiac h-IPSCs.**

5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

$H_0$**: Expressing HCN4 gene will have no difference in beats/min in cardiac h-IPSCs compared to HCN2 cardiac h-IPSCs.** 

$H_1$**:Expressing HCN4 gene will lead to increased beats/min in cardiac h-IPSCs compared to HCN2 cardiac h-IPSCs.**

6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**To to test my hypothesis I will use a one-sided unpaired student t-test. Another option would be test my hypothesis using a  Welch's t-test, However, I believe that a student t-test is more appropriate for the following reasons: The data collected will be continuous, i.e. beats per minute, I expect the rate (beats/min) to have a normal distribution, my sample replicates will be independent of every other sample replicate, and I expect my data(groups) to have equal varience.**


7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**In order to make sure that I have independent replicates I have collected cardiac h-IPSCs from different laboratories from our different collaborators. Furthermore, If my indepedent replicate number needs to be higher to come to conclusions, my procedure will be to thaw out my h-IPSCs on different days and/or at different hours in order to make it as independent as possible.**

**My time course will be as follows: Day 0 all h-IPSCs will receive either treatment of lentivirus with HCN4 gene or HCN2. On day 5 I will measure beats/min on the two different groups.**

**Based on my on my scientific experience and literatue research, I expect HCN4 h-IPSCs to have more beats/min; this is why I will perform a one-sided student t-test.  My alpha will be 0.05 which corresponds to a 95% confidence interval. This means that I will reject my null if less then my preset alpha.**
**My beta will be set at 20% since this accepted in biomedical research and I also don't have an intuitive sense to make this threshold any different.**

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.
```{r}
n<-10
b<-50 #basal beats per minute
a<-1.3 #30% increase 
f<- 1.10
sd<- 10


CRdataMaker <- function(n, b, a, f, sd) { 
  
  
  
  HCN2_treated<- rnorm(n, b, sd) #basal or negative ctrl
  HCN4_treated <- rnorm(n, (b*a), sd) #treatment
  
    
    Beats_per_min<- c(HCN2_treated, HCN4_treated)
    h_IPSCs <- c(rep(c("HCN2_treated", "HCN4_treated"), each = n))
    ID <- as.factor(c(1:length(h_IPSCs)))
    df <-data.frame(ID, h_IPSCs, Beats_per_min)
    }

data <- CRdataMaker(n,b,a,f,sd)

ggplot(data, aes(h_IPSCs,Beats_per_min))+
  geom_jitter(width=0.1,size = 4, alpha=0.5)+
ggtitle("HCN4+ Cardiac h-IPSCs Have Increased Beats/min:") +
  geom_jitter(width=0.15, size=4)+stat_summary(fun.data= mean_sdl, 
              fun.args = list(mult=1), 
              geom="crossbar", 
               width=0.2, 
              color="red"
               )  
 
```

9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.
```{r}
# If you feed t.pwr a sample size, it will calculate power

t.pwr <- function(n){
  # Intitializers. Means and SD's of populations compared.
  # Change these values to the units of what you expect.
  HCN2=50; sd1=10; HCN4= 80; sd2=10
  
  ssims=1000
  p.values <- c()
  i <- 1
  
  # this next step is THE monte carlo, it resamples based 
  # upon the initializer settings 
  repeat{
    x=rnorm(n, HCN2, sd1); 
    y=rnorm(n, HCN4, sd2);
    p <- t.test(x, y, 
                paired=F, 
                alternative="less", 
                var.equal=T,
                conf.level=0.95)$p.value
    p.values[i] <- p
    if (i==ssims) break
    i = i+1
    pwr <- length(which(p.values<0.05))/ssims
  }
  return(pwr)
}
# Run t.pwr over a range of sample sizes and plot results
frame <- data.frame(n=2:50)
data <- bind_cols(frame, 
                  power=apply(frame, 1, t.pwr))
#plot
ggplot(data, aes(n, power))+
  geom_point() +
  scale_y_continuous(breaks=c(seq(0, 1, 0.1)))+
  scale_x_continuous(breaks=c(seq(0,50,2)))+
  labs(x="n per group")
```

**Running a montecarlo 1,000 times indicates that at n=4 I am already at over 80% power. To be specific, at n=4, I run a risk of 10% type II errors.**

**In summary: According to my montecarlo simulations. I can expect to see a statistical significance (p<0.5) between my group means of HCN4 and HCN2 treated cardiac h-IPSCs at n=4 (with a tolerance of beta ~ less than 10%). At this point, I should be able to reject my null. However, I will proably do n=6 to get as close as possible to 99% power.**

10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.
