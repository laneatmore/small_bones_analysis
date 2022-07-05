library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(caret)
library(viridis)
#for assessing prediction ability of linear models
library(olsrr)
#for cross tabs
library(gmodels)
#for Cramer's V
library(rcompanion)

#first, input the data from excel
data <- read.csv('Supplementary_Data_S1.csv', sep = ';', header = TRUE)
head(data)

#filter any unnecessary information
data <- data %>%
  select(ID, Country, Site, element, Age_ybp, Weight_mg, 
         QUBIT, nu_cov, nu_end, nu_length, nu_clon, mt_cov,
         mt_end, mt_length, mt_clon, Site_Code)
head(data)

nrow(data)
#121 observations

#visualize the distribution of weight bins
weights <- ggplot(data, aes(Weight_mg)) + geom_bar() +
  theme_bw() + xlab('Weight (mg)') + ylab('Number of Samples')
weights 

#Count the number of bones in each weight class
grouped <- data %>%
  group_by(Weight_mg) %>% tally()
grouped

#Weight_mg     n
#<int> <int>
#  1         1    20
#2        10    26
#3        20    18
#4        30    27
#5        40    19
#6        50     9
#7        60     1
#8        70     1

#Count the number of bones per site
site_grouped <- data %>%
  group_by(Site) %>% tally()
site_grouped 

# A tibble: 16 × 2
#Site                           n
#<chr>                      <int>
#  1 Basel. Museum der Kulturen     5
#2 Basel. Schnabelgasse 6         5
#3 Biddinghuizen Colfschip       18
#4 Blue Bridge Lane               9
#5 Coppergate                    10
#6 Giecz                          5
#7 Huis de Struys                 5
#8 Kadriorg Wreck                 2
#9 Kaldus                         1
#10 Kaupang                       16
#11 Kolowbrzeg Budzistowo         11
#12 Lyminge                        9
#13 Mała Nieszawka 1               4
#14 Selsø-Vestby                   9
#15 Tallinn. Valbabuse 1           1
#16 Truso (Janow Pomorski)        11

#drop any bones that did not yield successful libraries
data_filtered <- data %>%
  drop_na()
nrow(data_filtered)
#90 observations remaining

#31 samples were not sequenced due to dimers and/or libraries that
#did not look high enough quality for some reason

#visualize weight class distribution for successful libraries
weights_filtered <- ggplot(data_filtered, aes(Weight_mg)) + geom_bar() +
  theme_bw() + xlab('Weight (mg)') + ylab('Number of Samples')
weights_filtered

#count number of samples left per weight class
grouped_filtered <- data_filtered %>%
  group_by(Weight_mg) %>% tally()
grouped_filtered

# A tibble: 9 × 2
#Weight_mg     n
#<int> <int>
#  1         1     9
#2        10    22
#3        20    13
#4        30    22
#5        40    16
#6        50     6
#7        60     1
#8        70     1

#1 55% removed
#10 15.4% removed
#20 23.3% removed
#30 18.5% removed
#40 15.8% removed
#50 33.3% removed
#60 0% removed
#70 0% removed

#Is there a difference between the number of samples in each dataset?
t.test(grouped$n,grouped_filtered$n)

#no
#data:  grouped$n and grouped_filtered$n
#t = 0.8227, df = 13.483, p-value = 0.425
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -6.263702 14.013702
#sample estimates:
#  mean of x mean of y 
#15.125    11.250 

#make dataframe with weight and percentage removed
weight <- c(1,10,20,30,40,50,60,70)
percent_removed <- c(55,15.4,33.3,18.5,15.8,33.3,0,0)
df <- data.frame(weight, percent_removed)
head(df)

num_removed <- c(11, 4, 5, 5, 3, 3, 0, 0)
df2 <- data.frame(weight, num_removed)

#look at relationship between weight class and percent removed with chi squared test
chisq.test(df$weight,df$percent_removed)
#Do the same but using the raw count data instead and fisher's exact test
fisher.test(df2$weight, df2$num_removed)

#no significant association between weight class and library success

#
#Pearson's Chi-squared test

#data:  df$weight and df$percent_removed
#X-squared = 40, df = 35, p-value = 0.2578

#Fisher's Exact Test for Count Data

#data:  df2$weight and df2$num_removed
#p-value = 1
#alternative hypothesis: two.sided


#now do the same for archaeological site
#count the number of samples per site
groupsite <- data %>%
  group_by(Site) %>% tally()
groupsite

# A tibble: 16 × 2
#Site                           n
#<chr>                      <int>
#  1 Basel. Museum der Kulturen     5
#2 Basel. Schnabelgasse 6         5
#3 Biddinghuizen Colfschip       18
#4 Blue Bridge Lane               9
#5 Coppergate                    10
#6 Giecz                          5
#7 Huis de Struys                 5
#8 Kadriorg Wreck                 2
#9 Kaldus                         1
#10 Kaupang                       16
#11 Kolowbrzeg Budzistowo         11
#12 Lyminge                        9
#13 Mała Nieszawka 1               4
#14 Selsø-Vestby                   9
#15 Tallinn. Valbabuse 1           1
#16 Truso (Janow Pomorski)        11

#count the remaining bones per site
groupsite_filtered <- data_filtered %>%
  group_by(Site) %>% tally()
groupsite_filtered

# A tibble: 16 × 2
#Site                           n
#<chr>                      <int>
#  1 Basel. Museum der Kulturen     4
#2 Basel. Schnabelgasse 6         5
#3 Biddinghuizen Colfschip       18
#4 Blue Bridge Lane               4
#5 Coppergate                     5
#6 Giecz                          5
#7 Huis de Struys                 2
#8 Kadriorg Wreck                 1
#9 Kaldus                         1
#10 Kaupang                        8
#11 Kolowbrzeg Budzistowo          8
#12 Lyminge                        8
#13 Mała Nieszawka 1               4
#14 Selsø-Vestby                   7
#15 Tallinn. Valbabuse 1           1
#16 Truso (Janow Pomorski)         9

#percent removed
#1 20
#2 0
#3 0
#4 55.55
#5 50
#6 0
#7 40
#8 50
#9 0
#10 50
#11 27.27
#12 11.11
#13 0
#14 22.22
#15 0
#16 18.18

sites <- c('Basel. Museum der Kulturen', 'Basel. Schnabelgasse 6', 'Biddinghuizen Colfschip',
           'Blue Bridge Lane','Coppergate','Giecz','Huis de Struys',
           'Kadriorg Wreck','Kaldus','Kaupang','Kolowbrzeg Budzistowo',
           'Lyminge','Mała Nieszawka 1','Selsø-Vestby','Tallinn. Valbabuse 1','Truso (Janow Pomorski)')

percent_removed <- c(20,0,0,55.55,50,0,40,50,0,50,27.27,11.11,0,22.22,0,18.18)
percent_sites <- data.frame(sites, percent_removed)

num_removed <- c(1,0,0,5,5,0,3,1,0,8,3,1,5,2,0,2)
num_sites <- data.frame(sites, num_removed)

chisq.test(percent_sites$sites, percent_sites$percent_removed)
fisher.test(num_sites$sites, num_sites$num_removed)

#
#Pearson's Chi-squared test

#data:  percent_sites$sites and percent_sites$percent_removed
#X-squared = 128, df = 120, p-value = 0.2918

#Fisher's Exact Test for Count Data

#data:  percent_sites$sites and percent_sites$percent_removed
#p-value = 1
#alternative hypothesis: two.sided

#
#Fisher's Exact Test for Count Data

#data:  num_sites$sites and num_sites$num_removed
#p-value = 1
#alternative hypothesis: two.sided

groupelement <- data %>%
  group_by(element) %>% tally()
groupelement


element_filtered <- data_filtered %>%
  group_by(element) %>% tally()
element_filtered

#element          n
#<fct>        <int>
#  1 ceratohyal      10
#2 cleithrum        2
#3 dentary         22
#4 prooticum       16
#5 unidentified     4
#6 vertebra        36

elements <- c('ceratohyal','cleithrum','dentary','prooticum','unidentified',
              'vertebra')

num_removed <- c(16.6,50,18.18,38.46,42.86,34.55)
num_element <- data.frame(elements, num_removed)
fisher.test(num_element$elements, num_element$num_removed)


groupage <- data %>%
  group_by(Age_ybp) %>% tally()
groupage


age_filtered <- data_filtered %>%
  group_by(Age_ybp) %>% tally()
age_filtered

levels(factor(data_filtered$Age_ybp))

age <- c("500", "550","600","650","665","700","800","900","925","950",
         "1000","1025","1050","1058","1175","1200","1250")

num_removed <- c(0,20,57.14,0,33.33,0,55.55,0,60,20,0,60,0,40,50,18.18,9.09)
num_age <- data.frame(age, num_removed)
fisher.test(num_age$age, num_age$num_removed)


###########Using Filtered Dataset to explore relationship between weight and quality

head(data_filtered)

#first, let's look at weight without any conditionals
model <- lm(nu_end~Weight_mg, data = data_filtered)
summary(model)

#Call:
#  lm(formula = nu_end ~ Weight_mg, data = data_filtered)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.10874 -0.09606 -0.05583  0.05668  0.36085 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.1091103  0.0244401   4.464 2.37e-05 ***
#  Weight_mg   -0.0003320  0.0008393  -0.396    0.693    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1234 on 88 degrees of freedom
#Multiple R-squared:  0.001775,	Adjusted R-squared:  -0.009569 
#F-statistic: 0.1564 on 1 and 88 DF,  p-value: 0.6934

#no direct effect

#How well does this model predict the observed data?
predictions <- model %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.1219737
R2(predictions, data_filtered$nu_end)
#0.0177461

mse <- mean(model$residuals^2)
mse

#However, we know that End DNA content could also be impacted
#by site, element, and age so we need to stratify the model by these parameters
#(see DAGs in supplement)

#Let's look at an additive model that takes the other confounding variables into account
model <- lm(nu_end~Weight_mg + Site + element + Age_ybp, data = data_filtered)
summary(model)

#This model DOES explain the End DNA content with ~49% of the variation explained, 
#which means something in the data is explaining at least half of our results
#However, the only significantly-associated variables are Sites: Coppergate, Giecz, 
#Mala Nieszawka

#Call:
#  lm(formula = nu_end ~ Weight_mg + Site + element + Age_ybp, data = data_filtered)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.190763 -0.029494 -0.002008  0.023337  0.248957 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                  0.2798276  0.2446309   1.144  0.25675   
#Weight_mg                   -0.0003869  0.0011369  -0.340  0.73469   
#SiteBasel. Schnabelgasse 6   0.1704188  0.1423690   1.197  0.23552   
#SiteBiddinghuizen Colfschip -0.0253639  0.0820038  -0.309  0.75805   
#SiteBlue Bridge Lane         0.1994501  0.1130560   1.764  0.08226 . 
#SiteCoppergate               0.3960512  0.1778415   2.227  0.02931 * 
#  SiteGiecz                    0.3793128  0.1789933   2.119  0.03779 * 
#  SiteHuis de Struys           0.1549968  0.1019721   1.520  0.13322   
#SiteKadriorg Wreck           0.2421445  0.1259589   1.922  0.05881 . 
#SiteKaldus                   0.1902712  0.1583958   1.201  0.23389   
#SiteKaupang                  0.2247138  0.2384893   0.942  0.34946   
#SiteKolowbrzeg Budzistowo    0.2033444  0.2036177   0.999  0.32155   
#SiteLyminge                  0.4363716  0.2654423   1.644  0.10487   
#SiteMała Nieszawka 1         0.3371448  0.1028416   3.278  0.00166 **
#  SiteSelsø-Vestby             0.2402914  0.1809639   1.328  0.18874   
#SiteTallinn. Valbabuse 1     0.0656530  0.1284876   0.511  0.61105   
#SiteTruso (Janow Pomorski)   0.3661671  0.2699629   1.356  0.17954   
#elementcleithrum             0.0033779  0.1696362   0.020  0.98417   
#elementdentary              -0.0445902  0.0987914  -0.451  0.65319   
#elementprooticum            -0.0380326  0.1065011  -0.357  0.72213   
#elementunidentified          0.0285638  0.1126311   0.254  0.80058   
#elementvertebra             -0.0315079  0.1223313  -0.258  0.79753   
#Age_ybp                     -0.0003967  0.0003712  -1.069  0.28901   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.08762 on 67 degrees of freedom
#Multiple R-squared:  0.6165,	Adjusted R-squared:  0.4906 
#F-statistic: 4.896 on 22 and 67 DF,  p-value: 2.384e-07

#How well does this model predict the observed data?
predictions <- model %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.07559878
R2(predictions, data_filtered$nu_end)
#0.616535

mse <- mean(model$residuals^2)
mse

#what are the VIFs for the variables in question?
VIFs <- ols_vif_tol(model)
VIFs

#Variables   Tolerance        VIF
#1                    Weight_mg 0.274996221   3.636414
#2   SiteBasel. Schnabelgasse 6 0.080208381  12.467525
#3  SiteBiddinghuizen Colfschip 0.079280533  12.613437
#4         SiteBlue Bridge Lane 0.157142705   6.363643
#5               SiteCoppergate 0.051402527  19.454297
#6                    SiteGiecz 0.050743125  19.707103
#7           SiteHuis de Struys 0.377541197   2.648718
#8           SiteKadriorg Wreck 0.489319245   2.043656
#9                   SiteKaldus 0.309429954   3.231749
#10                 SiteKaupang 0.018518172  54.001010
#11   SiteKolowbrzeg Budzistowo 0.025404164  39.363625
#12                 SiteLyminge 0.014948437  66.896628
#13        SiteMała Nieszawka 1 0.189908070   5.265706
#14            SiteSelsø-Vestby 0.036314470  27.537232
#15    SiteTallinn. Valbabuse 1 0.470248778   2.126534
#16  SiteTruso (Janow Pomorski) 0.013004812  76.894616
#17            elementcleithrum 0.136423841   7.330097
#18              elementdentary 0.047322742  21.131489
#19            elementprooticum 0.051449384  19.436579
#20         elementunidentified 0.158330471   6.315904
#21             elementvertebra 0.023750242  42.104834
#22                     Age_ybp 0.007970241 125.466723

#There are serious signs of multicollinearity here
#Further exploration is required

#Collinearity diagnostics
ols_coll_diag(model)

ols_plot_resid_fit_spread(model)

ols_correlations(model)
#This shows that site is contributing the majority of the 
#variation in the dataset

ols_plot_obs_fit(model)

#The model overestimates endogenous DNA content !

#It looks like we need to separate out some variables, since
#there is collinearity

#So how does this look if we separate out each variable?
#Age appeared to be at least somewhat related to endogenous DNA content
#Although it showed very high collinearity -- likely interacting with
#Site

agemodel <- lm(nu_end~Age_ybp, data = data_filtered)
summary(agemodel)

#Call:
#  lm(formula = nu_end ~ Age_ybp, data = data_filtered)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.12677 -0.07965 -0.05976  0.05918  0.35586 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) 2.033e-02  4.259e-02   0.477   0.6342  
#Age_ybp     9.060e-05  4.569e-05   1.983   0.0505 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1208 on 88 degrees of freedom
#Multiple R-squared:  0.04278,	Adjusted R-squared:  0.0319 
#F-statistic: 3.933 on 1 and 88 DF,  p-value: 0.05046

#Not significant! Age does not explain variation in endogenous
#DNA on its own

#How well does this model predict the observed data?
predictions <- agemodel %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.1194421
R2(predictions, data_filtered$nu_end)
#0.04278157

mse <- mean(agemodel$residuals^2)
mse

#How about skeletal element?
elementmodel <- lm(nu_end~element, data = data_filtered)
summary(elementmodel)

#Call:
#lm(formula = nu_end ~ element, data = data_filtered)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.18000 -0.08804 -0.03893  0.07225  0.34080 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          0.14450    0.03775   3.828 0.000248 ***
#  elementcleithrum     0.10550    0.09246   1.141 0.257089    
#elementdentary      -0.05476    0.04552  -1.203 0.232402    
#elementprooticum    -0.10384    0.04812  -2.158 0.033783 *  
#  elementunidentified -0.08900    0.07062  -1.260 0.211038    
#elementvertebra     -0.02530    0.04267  -0.593 0.554858    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1194 on 84 degrees of freedom
#Multiple R-squared:  0.1078,	Adjusted R-squared:  0.05467 
#F-statistic: 2.029 on 5 and 84 DF,  p-value: 0.08271

#How well does this model predict the observed data?
predictions <- elementmodel %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.1153157
R2(predictions, data_filtered$nu_end)
#0.1077781

mse <- mean(elementmodel$residuals^2)
mse

data_filtered$element <- as.factor(data_filtered$element)
levels(data_filtered$element)

#Overall, there is no significant association. However, there does
#Appear to be a significant but weak positive relationship with SOME elements and endogenous
#DNA content, including ceratohyal (here "intercept") and prootica. 
#There is a MUCH stronger relationship between ceratohyal and endogenous DNA content than
#There is for prootica
#Let's plot this

bone_end <- ggplot(data_filtered, aes(element, nu_end)) + geom_point() +
  theme_bw() +
  xlab('Skeletal element') +
  ylab('Endogenous DNA Content') +
  #scale_x_discrete(labels = reorder(data_sorted$Site, -data_sorted$Age_ybp)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
bone_end

#How about the relationship between weight and bone element? These are likely
#collinear

elementmodel_ <- lm(nu_end~element + Weight_mg, data = data_filtered)
summary(elementmodel_)

#Residual standard error: 0.1199 on 83 degrees of freedom
#Multiple R-squared:  0.1106,	Adjusted R-squared:  0.04627 
#F-statistic:  1.72 on 6 and 83 DF,  p-value: 0.1266


#How well does this model predict the observed data?
predictions <- elementmodel_ %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.1153157
R2(predictions, data_filtered$nu_end)
#0.1077781

mse <- mean(elementmodel_$residuals^2)
mse

#Let's look at the distribution of skeletal elements per site
data_sorted <- data_filtered %>%
  arrange(Age_ybp)

element_by_site <- ggplot(data_sorted, aes(fill=element, y=Site)) + 
  geom_bar() + theme_bw() + scale_fill_viridis_d() 

head(data_sorted)

data_sorted <- data_sorted %>%
  select('ID', 'Country', 'Site', 'element', 'Age_ybp', 'Weight_mg',
         'QUBIT','nu_cov','nu_end', 'nu_length','nu_clon','mt_cov',
         'mt_end','mt_length','mt_clon','Site_Code')

#What about the relationship between weight and site/element?
#Weight is a continuous variable usually, but here is classed as categorical 
#So we need to look at chi-squared test

chisq.test(data_filtered$Site, data_filtered$element, simulate.p.value = TRUE)
#p-value 0.0004998 

chisq.test(data_filtered$element, data_filtered$Weight_mg, simulate.p.value = TRUE)
#p-value 0.002999

chisq.test(data_filtered$Site, data_filtered$Weight_mg, simulate.p.value = TRUE)
#p-value 0.04648

chisq.test(data_filtered$Site, data_filtered$Age_ybp, simulate.p.value = TRUE)
#p-value 0.0004998

weight_by_site <- ggplot(data_sorted, aes(reorder(Site, -Age_ybp), fill = factor(Weight_mg))) +
  geom_bar() + theme_bw() + scale_fill_viridis_d(option='magma') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Weight (mg)') +
  xlab('Site')

weight_by_site

#Ceratohyals basically only show up at Truso, with one at Kolowbrzeg-Budzistowo
#Dentaries are more spread out, but cluster in specific sites
#Likely there is an interaction between site and element -- how can we tease this apart?
#Let's look at the impact of Site and then at an additive model

sitemodel <- lm(nu_end~Site, data = data_filtered)
summary(sitemodel)

#How well does this model predict the observed data?
predictions <- sitemodel %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.07960714
R2(predictions, data_filtered$nu_end)
#0.5747932
mse <- mean(sitemodel$residuals^2)
mse
#0.006337297

#Call:
#  lm(formula = nu_end ~ Site, data = data_filtered)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.20800 -0.02600 -0.00165  0.01924  0.30429 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  0.026250   0.043896   0.598 0.551665    
#SiteBasel. Schnabelgasse 6   0.030790   0.058893   0.523 0.602667    
#SiteBiddinghuizen Colfschip -0.023200   0.048529  -0.478 0.634014    
#SiteBlue Bridge Lane         0.103750   0.062079   1.671 0.098893 .  
#SiteCoppergate               0.211750   0.058893   3.596 0.000581 ***
#  SiteGiecz                    0.196550   0.058893   3.337 0.001326 ** 
#  SiteHuis de Struys           0.113750   0.076031   1.496 0.138878    
#SiteKadriorg Wreck           0.193750   0.098155   1.974 0.052124 .  
#SiteKaldus                   0.103750   0.098155   1.057 0.293949    
#SiteKaupang                 -0.024106   0.053762  -0.448 0.655190    
#SiteKolowbrzeg Budzistowo   -0.002125   0.053762  -0.040 0.968577    
#SiteLyminge                  0.159000   0.053762   2.957 0.004161 ** 
#  SiteMała Nieszawka 1         0.273750   0.062079   4.410 3.45e-05 ***
#  SiteSelsø-Vestby             0.099464   0.055027   1.808 0.074739 .  
#SiteTallinn. Valbabuse 1    -0.004250   0.098155  -0.043 0.965580    
#SiteTruso (Janow Pomorski)   0.128194   0.052757   2.430 0.017528 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.08779 on 74 degrees of freedom
#Multiple R-squared:  0.5748,	Adjusted R-squared:  0.4886 
#F-statistic: 6.669 on 15 and 74 DF,  p-value: 8.771e-09

#Site has a strong association with endogenous DNA content
#So it's probably the site and not the skeletal element that is driving
#the positive association between ceratohyal and endogenous DNA

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.07960714
R2(predictions, data_filtered$nu_end)
#0.5747932

mse <- mean(sitemodel$residuals^2)
mse
#0.006337297

sitemodel_element2 <- lm(nu_end~Site+element, data = data_filtered)
summary(sitemodel_element2)

#Residual standard error: 0.08721 on 69 degrees of freedom
#Multiple R-squared:  0.6087,	Adjusted R-squared:  0.4953 
#F-statistic: 5.367 on 20 and 69 DF,  p-value: 7.529e-08

#How well does this model predict the observed data?
predictions <- sitemodel_element2 %>% predict(data_filtered)

#prediction error
RMSE(predictions, data_filtered$nu_end)
#0.07636453
R2(predictions, data_filtered$nu_end)
#0.6087273

mse <- mean(sitemodel_element2$residuals^2)
mse
#0.005831541

#This explains it better but only by <1% difference in adjusted R2
#Call:
#lm(formula = nu_end ~ Site + element, data = data_filtered)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.208000 -0.025812 -0.000333  0.019125  0.244750 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  0.050439   0.129102   0.391 0.697228    
#SiteBasel. Schnabelgasse 6   0.030790   0.058505   0.526 0.600383    
#SiteBiddinghuizen Colfschip -0.008116   0.078934  -0.103 0.918400    
#SiteBlue Bridge Lane         0.103750   0.061670   1.682 0.097023 .  
#SiteCoppergate               0.211750   0.058505   3.619 0.000559 ***
#  SiteGiecz                    0.201507   0.063320   3.182 0.002191 ** 
#  SiteHuis de Struys           0.129602   0.098836   1.311 0.194107    
#SiteKadriorg Wreck           0.213061   0.121514   1.753 0.083977 .  
#SiteKaldus                   0.103894   0.138574   0.750 0.455961    
#SiteKaupang                 -0.022556   0.053941  -0.418 0.677124    
#SiteKolowbrzeg Budzistowo    0.004561   0.095189   0.048 0.961923    
#SiteLyminge                  0.159000   0.053408   2.977 0.004011 ** 
#  SiteMała Nieszawka 1         0.293061   0.095189   3.079 0.002982 ** 
#  SiteSelsø-Vestby             0.069061   0.093170   0.741 0.461065    
#SiteTallinn. Valbabuse 1     0.008143   0.114777   0.071 0.943643    
#SiteTruso (Janow Pomorski)   0.104005   0.132335   0.786 0.434601    
#elementcleithrum             0.130500   0.121788   1.072 0.287664    
#elementdentary              -0.043500   0.097509  -0.446 0.656912    
#elementprooticum            -0.036583   0.105356  -0.347 0.729478    
#elementunidentified         -0.024333   0.100707  -0.242 0.809787    
#elementvertebra             -0.024189   0.121514  -0.199 0.842798    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.08721 on 69 degrees of freedom
#Multiple R-squared:  0.6087,	Adjusted R-squared:  0.4953 
#F-statistic: 5.367 on 20 and 69 DF,  p-value: 7.529e-08

#Again, sites are the only variables that are coming up as significantly associated
#with endogenous DNA content
#but this model does seem to predict the data better than the one just using
#site as an explanatory categorical variable

#Is there collinearity between site and element?
#Since element sampling was *not* done randomly, we likely can't use element 
#to tell use how good the data is without bias

hist_element <- ggplot(data_filtered, aes(fill = element, x = element)) +
  geom_bar() + theme_bw() + scale_fill_viridis_d()

hist_element

#CrossTab of element per site
site_element_table <- CrossTable(data_filtered$Site, data_filtered$element)
#Cramer's V to assess association between categorical variables
cramerV(site_element_table$t)
#0.7577 -- there is a high degree of collinearity
#Given this, we will use only site as the driving factor

#Make sure the dataset only contains necessary columns
data_filtered <- data_filtered %>%
  select('ID', 'Country', 'Site', 'element', 'Age_ybp', 'Weight_mg',
         'QUBIT','nu_cov','nu_end', 'nu_length','nu_clon','mt_cov',
         'mt_end','mt_length','mt_clon','Site_Code')

data_filtered$pred_nu_end <- predict(sitemodel)
predslm <- predict(sitemodel, interval = 'confidence')
head(predslm)

data_filtered <- cbind(data_filtered, predslm)
head(data_filtered)

site_end <- ggplot(data_filtered, aes(as.integer(reorder(factor(Site), -Age_ybp)), 
                                    nu_end)) + 
  geom_point(aes(as.integer(reorder(factor(Site), -Age_ybp)), nu_end, fill = element),
             color = 'black', pch=21, size=5) +
  theme_bw() +
  xlab('Archaeological Site') +
  ylab('Endogenous DNA Content') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #geom_smooth(method = 'loess', ) +
  geom_line(aes(y=pred_nu_end), size = 0.5) +
  geom_ribbon( aes(ymin = lwr, ymax = upr, color = NULL), alpha = .15) +
  scale_fill_viridis_d() +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))

site_end

library(reshape2)
data_2 <- melt(data_filtered, id.vars = c('Site','Age_ybp'),
               measure.vars = 'nu_end')

head(data_2)

levels(factor(data_2$Site))

mean(data_filtered$nu_end)
sd(data_filtered$nu_end)

site_end <- ggplot(data_filtered, aes(reorder(factor(Site), -Age_ybp), 
                                      nu_end)) + 
  geom_boxplot(aes(reorder(factor(data_2$Site), -data_2$Age_ybp), 
                   data_2$value)) +
  geom_point(aes(reorder(factor(Site), -Age_ybp), nu_end, fill = element),
             color = 'black', pch=21, size=3) +
  geom_hline(yintercept = 0.101, linetype = 'dashed', col = 'red') +
  theme_bw() +
  xlab('Archaeological Site') +
  ylab('Endogenous DNA Content') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #geom_smooth(method = 'loess', ) +
  #geom_line(aes(y=pred_nu_end), size = 0.5) +
  #geom_ribbon( aes(ymin = lwr, ymax = upr, color = NULL), alpha = .15) +
  scale_fill_viridis_d()
  #scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))

site_end

weight_end <- ggplot(data_sorted, aes(Weight_mg, nu_end)) + geom_point() +
  theme_bw() +
  xlab('Weight (mg)') +
  ylab('Endogenous DNA Content')
weight_end

#####Stratified regression weight vs site?

model1 <- lm(nu_end~Site + Weight_mg, data=data_filtered)
model2 <- lm(nu_end~Site * Weight_mg, data=data_filtered)
anova(model2, test="Chisq")

#no significant association between weight and site in the regression analysis

#how about site and element?
model1 <- lm(nu_end~Site + element, data=data_filtered)
model2 <- lm(nu_end~Site * element, data=data_filtered)
anova(model2, test="Chisq")
#also no significant association
#p=0.9525


#############CLONALITY

model1 <- lm(nu_clon ~ Site + element + Age_ybp, Weight_mg, data = data_filtered)
summary(model1)

#r2=1
#p<2.2e-16
#In summary.lm(model1) : essentially perfect fit: summary may be unreliable

model2 <- lm(nu_clon ~ Site, data = data_filtered)
summary(model2)
#r2=19.99
#p=0.0052

model3 <- lm(nu_clon ~ element, data = data_filtered)
summary(model3)
#r2=-0.04733
#p=0.9634

model4 <- lm(nu_clon ~ Age_ybp, data = data_filtered)
summary(model4)
#r2=-0.01
#p=0.7314

model5 <- lm(nu_clon ~ Weight_mg, data = data_filtered)
summary(model5)
#r2=-0.01123
#p=0.9133
