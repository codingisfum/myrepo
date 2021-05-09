Health, Language, and Income in Chicago, Illinois
================

Nathan Eckert

## Introduction

The data included in this exploratory analysis are from the official
City of Chicago website. The first data set called Languages Spoken in
Chicago was collected from 2008 to 2012. It includes data about the
predominant non-English language spoken in each Chicago community. It
also includes the percent of residents over five, who speak English
“less than very well” in addition to the counts of the spoken languages.
The data was compiled from U.S. Census Bureau’s 2008-2012 American
Community Survey 5-year estimates. The second data set is named Public
Heath Statistics from 2005 to 2011 and includes various health
indicators organized by community area of Chicago. It was collected from
multiple sources including census data, Chicago Dept. of Public Health,
and reports accessible through local infectious disease laws. The data
were tidied and combined into one data set.

I was interested in this data set because it contains a sizable amount
of data across multiple health variables for a specific area. The
addition of a language aspect may reveal ethnic or racial health
differences among communities. The restriction to only Chicago
communities may serve as a control for my findings. I will describe my
variables when they are used. Specific information about variables can
be found at the provided sources.

*Source URLs*
<https://data.cityofchicago.org/Health-Human-Services/Census-Data-Languages-spoken-in-Chicago-2008-2012/a2fk-ec6q>
<https://data.cityofchicago.org/Health-Human-Services/Public-Health-Statistics-Selected-public-health-in/iqnk-2tcu>

``` r
#Opened data sets and renamed to simpler names.
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.0.4

``` r
Chicago_Health_short_ <- read_excel("Chicago Health (short).xlsx")
Chicago_Languages_short_ <- read_excel("Chicago Languages (short).xlsx")

chic_lang <- Chicago_Languages_short_
chic_health <- Chicago_Health_short_
```

## Tidy

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.5     v dplyr   1.0.3
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)

head(chic_lang)
```

    ## # A tibble: 6 x 42
    ##   Community `Community Name` `NON-ENG LANG (~ `AFRICAN LANGUA~ ARABIC ARMENIAN
    ##       <dbl> <chr>            <chr>                       <dbl>  <dbl>    <dbl>
    ## 1         1 Rogers Park      SPANISH (9.9%)                332    165       65
    ## 2         2 West Ridge       SPANISH (8.7%)                374   1500        2
    ## 3         3 Uptown           SPANISH (4.9%)                750     58        0
    ## 4         4 Lincoln Square   SPANISH (8.4%)                127    106       31
    ## 5         5 North Center     SPANISH (4.2%)                  0      0        0
    ## 6         6 Lake View        SPANISH (1.9%)                425    128        0
    ## # ... with 36 more variables: `CAMBODIAN (MON-KHMER)` <dbl>, CHINESE <dbl>,
    ## #   CREOLE <dbl>, FRENCH <dbl>, GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>,
    ## #   HEBREW <dbl>, HINDI <dbl>, HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>,
    ## #   JAPANESE <dbl>, KOREAN <dbl>, LAOTIAN <dbl>, NAVAJO <dbl>, `OTHER
    ## #   ASIAN` <dbl>, `OTHER INDIC` <dbl>, `OTHER INDO EURPOEAN` <dbl>, `OTHER
    ## #   NATIVE NORTH AMERICAN` <dbl>, `OTHER PACIFIC ISLAND` <dbl>, `OTHER
    ## #   SLAVIC` <dbl>, `OTHER WEST GERMANIC` <dbl>, PERSIAN <dbl>, POLISH <dbl>,
    ## #   PORTUGUESE <dbl>, RUSSIAN <dbl>, SCANDINAVIAN <dbl>,
    ## #   `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>, THAI <dbl>,
    ## #   UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>

``` r
#separate Non-English Lang (%) into two separate columns for predominant non-English language spoken and percent that speak English less than very well.
chic_lang <- chic_lang%>%
  separate("NON-ENG LANG (%)", into = c("NON-ENGLISH LANG", "Percent under well"))
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 78 rows [1, 2, 3, 4,
    ## 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
head(chic_lang)
```

    ## # A tibble: 6 x 43
    ##   Community `Community Name` `NON-ENGLISH LA~ `Percent under ~ `AFRICAN LANGUA~
    ##       <dbl> <chr>            <chr>            <chr>                       <dbl>
    ## 1         1 Rogers Park      SPANISH          9                             332
    ## 2         2 West Ridge       SPANISH          8                             374
    ## 3         3 Uptown           SPANISH          4                             750
    ## 4         4 Lincoln Square   SPANISH          8                             127
    ## 5         5 North Center     SPANISH          4                               0
    ## 6         6 Lake View        SPANISH          1                             425
    ## # ... with 38 more variables: ARABIC <dbl>, ARMENIAN <dbl>, `CAMBODIAN
    ## #   (MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>, FRENCH <dbl>,
    ## #   GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>, HINDI <dbl>,
    ## #   HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>, KOREAN <dbl>,
    ## #   LAOTIAN <dbl>, NAVAJO <dbl>, `OTHER ASIAN` <dbl>, `OTHER INDIC` <dbl>,
    ## #   `OTHER INDO EURPOEAN` <dbl>, `OTHER NATIVE NORTH AMERICAN` <dbl>, `OTHER
    ## #   PACIFIC ISLAND` <dbl>, `OTHER SLAVIC` <dbl>, `OTHER WEST GERMANIC` <dbl>,
    ## #   PERSIAN <dbl>, POLISH <dbl>, PORTUGUESE <dbl>, RUSSIAN <dbl>,
    ## #   SCANDINAVIAN <dbl>, `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>,
    ## #   THAI <dbl>, UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>

``` r
#This code turns all spaces into periods and all commas into spaces.

names(chic_health)<-str_replace_all(names(chic_health), c(" " = "." , "," = "" ))
names(chic_lang)<-str_replace_all(names(chic_lang), c(" " = "." , "," = "" ))

#This changes any missing values like "." into NAs.
chic_health <- na_if(chic_health, ".")
chic_lang <- na_if(chic_lang, ".")
```

The data sets are now tidy and ready to be joined.

## Join/ Merge

``` r
chic_lang
```

    ## # A tibble: 78 x 43
    ##    Community Community.Name `NON-ENGLISH.LA~ Percent.under.w~ AFRICAN.LANGUAG~
    ##        <dbl> <chr>          <chr>            <chr>                       <dbl>
    ##  1         1 Rogers Park    SPANISH          9                             332
    ##  2         2 West Ridge     SPANISH          8                             374
    ##  3         3 Uptown         SPANISH          4                             750
    ##  4         4 Lincoln Square SPANISH          8                             127
    ##  5         5 North Center   SPANISH          4                               0
    ##  6         6 Lake View      SPANISH          1                             425
    ##  7         7 Lincoln Park   SPANISH          1                              53
    ##  8         8 Near North Si~ SPANISH          1                              51
    ##  9         9 Edison Park    POLISH           2                               0
    ## 10        10 Norwood Park   POLISH           6                               0
    ## # ... with 68 more rows, and 38 more variables: ARABIC <dbl>, ARMENIAN <dbl>,
    ## #   `CAMBODIAN.(MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>, FRENCH <dbl>,
    ## #   GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>, HINDI <dbl>,
    ## #   HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>, KOREAN <dbl>,
    ## #   LAOTIAN <dbl>, NAVAJO <dbl>, OTHER.ASIAN <dbl>, OTHER.INDIC <dbl>,
    ## #   OTHER.INDO.EURPOEAN <dbl>, OTHER.NATIVE.NORTH.AMERICAN <dbl>,
    ## #   OTHER.PACIFIC.ISLAND <dbl>, OTHER.SLAVIC <dbl>, OTHER.WEST.GERMANIC <dbl>,
    ## #   PERSIAN <dbl>, POLISH <dbl>, PORTUGUESE <dbl>, RUSSIAN <dbl>,
    ## #   SCANDINAVIAN <dbl>, `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>,
    ## #   THAI <dbl>, UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>

``` r
#I will join the two data sets chic_lang and chic_health by community name and remove a redundant column "community.y".
chicago <- left_join(chic_health, chic_lang, by = "Community.Name")%>%
  select(!"Community.y")

#Fixed percent and gonorrhea.m being chr, now is numeric.
chicago <- chicago%>%
  mutate_at(vars(`Percent.under.well`),as.numeric)%>%
  mutate_at(vars(`Gonorrhea.M`),as.numeric)
```

    ## Warning: Problem with `mutate()` input `Percent.under.well`.
    ## i NAs introduced by coercion
    ## i Input `Percent.under.well` is `.Primitive("as.double")(Percent.under.well)`.

``` r
chicago
```

    ## # A tibble: 77 x 70
    ##    Community.x Community.Name Birth.Rate General.Fert.Ra~ Low.Birth.Weight
    ##          <dbl> <chr>               <dbl>            <dbl>            <dbl>
    ##  1           1 Rogers Park          16.4             62               11  
    ##  2           2 West Ridge           17.3             83.3              8.1
    ##  3           3 Uptown               13.1             50.5              8.3
    ##  4           4 Lincoln Square       17.1             61                8.1
    ##  5           5 North Center         22.4             76.2              9.1
    ##  6           6 Lake View            13.5             38.7              6.3
    ##  7           7 Lincoln Park         13.2             38.7              6.6
    ##  8           8 Near North Si~       10.7             35.9              8.6
    ##  9           9 Edison Park          11.3             59.5              7.9
    ## 10          10 Norwood Park         10.4             59.6              4.9
    ## # ... with 67 more rows, and 65 more variables: Prenatal.Care.First.Trim <dbl>,
    ## #   Preterm.Births <dbl>, Teen.Birth.Rate <dbl>, `Assault.(Homicide)` <dbl>,
    ## #   Breast.Cancer.F <dbl>, `Cancer.(All.Sites)` <dbl>, Colorectal.Cancer <dbl>,
    ## #   `Diabetes-related` <dbl>, `Firearm-related` <dbl>, Infant.Mort.Rate <dbl>,
    ## #   Lung.Cancer <dbl>, Prostate.Cancer.M <dbl>, Stroke <dbl>,
    ## #   Childhood.Blood.Lead.level <dbl>, Childhood.Lead.Poisoning <dbl>,
    ## #   Gonorrhea.F <dbl>, Gonorrhea.M <dbl>, TB <dbl>, Below.Poverty.Level <dbl>,
    ## #   Crowded.Housing <dbl>, Dependency <dbl>, No.HS.Diploma <dbl>,
    ## #   Per.Capita.Income <dbl>, Unemployment <dbl>, `NON-ENGLISH.LANG` <chr>,
    ## #   Percent.under.well <dbl>, AFRICAN.LANGUAGES <dbl>, ARABIC <dbl>,
    ## #   ARMENIAN <dbl>, `CAMBODIAN.(MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>,
    ## #   FRENCH <dbl>, GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>,
    ## #   HINDI <dbl>, HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>,
    ## #   KOREAN <dbl>, LAOTIAN <dbl>, NAVAJO <dbl>, OTHER.ASIAN <dbl>,
    ## #   OTHER.INDIC <dbl>, OTHER.INDO.EURPOEAN <dbl>,
    ## #   OTHER.NATIVE.NORTH.AMERICAN <dbl>, OTHER.PACIFIC.ISLAND <dbl>,
    ## #   OTHER.SLAVIC <dbl>, OTHER.WEST.GERMANIC <dbl>, PERSIAN <dbl>, POLISH <dbl>,
    ## #   PORTUGUESE <dbl>, RUSSIAN <dbl>, SCANDINAVIAN <dbl>,
    ## #   `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>, THAI <dbl>,
    ## #   UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>

A redundant column and data for the city as a whole were removed. The
output is the joined data set called “chicago”.

## Exploratory Data Analysis (EDA)

``` r
library(ggplot2)
#made a new dataset with some categorical variables of interest
chicagoplus<- chicago%>%
mutate(proficiency = case_when(Percent.under.well>5.7 ~ "above",
                                Percent.under.well<=5.7 ~ "below"),
       income = case_when(Per.Capita.Income>27915 ~ "upper",
                          Per.Capita.Income<=27915 ~ "lower"),
       cancer.death = case_when(`Cancer.(All.Sites)`>178.4 ~ "high",
                                `Cancer.(All.Sites)`<= 178.4 ~ "low"),
       gon.rate = case_when(Gonorrhea.F>285 ~ "above",
                            Gonorrhea.F<=285 ~ "below"))
```

The variable ‘proficiency’ divided communities into “above” and “below”
the percent of people who speak English less than very well in relation
to the percent for the US as a whole, which was 5.7 (as determined by
the US census). The variable ‘income’ is the per capita income per
community separated into “above” and “below” the US baseline. Note that
baselines come from the data sources and are determined by the US
government’s Healthy People objective. Cancer.death and gon.rate break
communities up by above and below US baselines for total cancer deaths
(per 100,000 people) and gonorrhea cases in females (per 100,000),
respectively.

``` r
#graph of cancer deaths by language proficiency
chicagoplus%>%
  filter(!is.na(proficiency))%>%
  ggplot(aes(x=proficiency, y=`Cancer.(All.Sites)`, ))+geom_boxplot()+
  ggtitle("Community cancer deaths by proficiency in English")+
  xlab("Relation to US percent who speak English < very well")+
  ylab("Deaths from cancer (per 100,000)")
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

It appears that there is a difference in dying of cancer in communities
where more people speak poor English than in in communities where
English is spoken well. This will be further investigated with a
logistic regression model.

``` r
# relationship of predominate non-English language across variables
chicagoplus %>%
  group_by(`NON-ENGLISH.LANG`) %>%
  summarize(mean(Per.Capita.Income), mean(Stroke), mean(Low.Birth.Weight), mean(Infant.Mort.Rate), mean(Teen.Birth.Rate), mean(Prenatal.Care.First.Trim))
```

    ## # A tibble: 4 x 7
    ##   `NON-ENGLISH.LA~ `mean(Per.Capit~ `mean(Stroke)` `mean(Low.Birth~
    ## * <chr>                       <dbl>          <dbl>            <dbl>
    ## 1 AFRICAN                    18626.           71.8            13.8 
    ## 2 CHINESE                    38757.           43.0             8.68
    ## 3 POLISH                     32422.           38.2             6.22
    ## 4 SPANISH                    23316.           46.9            10.5 
    ## # ... with 3 more variables: `mean(Infant.Mort.Rate)` <dbl>,
    ## #   `mean(Teen.Birth.Rate)` <dbl>, `mean(Prenatal.Care.First.Trim)` <dbl>

``` r
# counts of each predominate non-English language communities
chicagoplus%>%
  group_by(`NON-ENGLISH.LANG`)%>%
  count()
```

    ## # A tibble: 4 x 2
    ## # Groups:   NON-ENGLISH.LANG [4]
    ##   `NON-ENGLISH.LANG`     n
    ##   <chr>              <int>
    ## 1 AFRICAN                2
    ## 2 CHINESE                6
    ## 3 POLISH                 6
    ## 4 SPANISH               63

I checked the means of different variables across predominate
non-English language. Chinese speaking communities seem to make the most
per capita income. African language speaking communities seemed to have
comparatively very high rates of stroke-related deaths , highest rates
of low birth rate, infant mortality, and teen birthrate. Rates of
prenatal care was the highest among Polish-speaking communities though,
there was little variation in this category. These statistics are all
qualified by extremely low sample sizes except for Spanish which takes
up the majority of observations. Significant differences will be
determined with MANOVA and subsequent tests.

``` r
# relationship between high school diplomas, per capita income, and gonorrhea rates
chicagoplus%>%
  select(Gonorrhea.F, Per.Capita.Income, No.HS.Diploma)%>%
  ggplot(aes(x=No.HS.Diploma, y=Gonorrhea.F, color=Per.Capita.Income)) + geom_point()+
  #set middle color at US baseline per capita income
  scale_color_gradient2(low="red",mid = "yellow",high="dark green", midpoint = 27915)+
  ggtitle("Female gonorrhea rates by per capita income and high school diploma rates") +
  xlab("Percent no high school diploma (over 25 y/o)")+
  ylab("Gonorrhea cases in females (per 100,000)")+
  theme_light()
```

    ## Warning: Removed 12 rows containing missing values (geom_point).

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This graph suggests that communities with a greater per capita income
have fewer gonorrhea cases (in females). It also appears that those
communities that make the US baseline per capita income (yellow) have
about 20 percent of residents over 25 who do not have a high school
diploma. There appears to be a normal distribution between having a high
school diploma and gonorrhea. I would like to further investigate the
relationship between shown variables in a linear regression model.

``` r
# Distribution of per capita income
hist(chicagoplus$Per.Capita.Income)
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

The per capita income of Chicago communities appears to not be normal
and have a right skew. As this is a variable I will use frequently, the
lack of normality should be considered when evaluating accuracy of
models and assumptions.

## MANOVA

``` r
# Does per capita income or stroke death rate differ by predominate non-English language
# Perform MANOVA with 2 response variables listed in cbind()
manova_chic <- manova(cbind(Per.Capita.Income, Stroke) ~ `NON-ENGLISH.LANG`, data = chicagoplus)
# Output of MANOVA
summary(manova_chic)
```

    ##                    Df  Pillai approx F num Df den Df  Pr(>F)  
    ## `NON-ENGLISH.LANG`  3 0.19999   2.7036      6    146 0.01613 *
    ## Residuals          73                                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Whether or not stroke deaths or per capita income vary between groups of
predominate non-English language was investigated as it may reveal
ethnic differences in cardiovascular health or financial status. It
appears that the groups of predominate non-English languages do differ
by at least one selected variable.

``` r
#ANOVA for each varible
summary.aov(manova_chic)
```

    ##  Response Per.Capita.Income :
    ##                    Df     Sum Sq   Mean Sq F value  Pr(>F)  
    ## `NON-ENGLISH.LANG`  3 1.7252e+09 575080758  2.7498 0.04881 *
    ## Residuals          73 1.5267e+10 209137271                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response Stroke :
    ##                    Df  Sum Sq Mean Sq F value  Pr(>F)  
    ## `NON-ENGLISH.LANG`  3  1769.9  589.98  3.0554 0.03366 *
    ## Residuals          73 14095.9  193.09                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

It appears that both rates of stroke-related deaths and per capita
income were significantly different between predominate non-English
languages.

``` r
# post-hoc testing for per capita income
pairwise.t.test(chicagoplus$Per.Capita.Income,chicagoplus$`NON-ENGLISH.LANG`, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  chicagoplus$Per.Capita.Income and chicagoplus$`NON-ENGLISH.LANG` 
    ## 
    ##         AFRICAN CHINESE POLISH
    ## CHINESE 0.092   -       -     
    ## POLISH  0.246   0.450   -     
    ## SPANISH 0.653   0.015   0.145 
    ## 
    ## P value adjustment method: none

``` r
# post-hoc testing for stroke rates
pairwise.t.test(chicagoplus$Stroke,chicagoplus$`NON-ENGLISH.LANG`, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  chicagoplus$Stroke and chicagoplus$`NON-ENGLISH.LANG` 
    ## 
    ##         AFRICAN CHINESE POLISH
    ## CHINESE 0.0134  -       -     
    ## POLISH  0.0042  0.5501  -     
    ## SPANISH 0.0147  0.5223  0.1502
    ## 
    ## P value adjustment method: none

``` r
# Bonferroni alpha
# 1 MANOVA + 2 ANOVA + 12 t-tests...15 total tests
0.05/15
```

    ## [1] 0.003333333

A total of 15 tests were performed. For this reason the new probability
of a type I error was multiplied by 15 (from 0.05 to 0.75). To account
for this, the Bonferroni adjusted alpha of 0.0033 was used. After using
the adjusted alpha, none of the groups appeared significantly different
despite the significant MANOVA and ANOVA tests. This unexpected result
may be due to the extremely small sample sizes.

On that note, it is important to note that many of the MANOVA
assumptions have likely not been met. The sample sizes for the groups of
non-English languages are as low as 2. Judging from the lack of
normality of per capita income, I expect much of the data to be
non-normal. Since the data was not transformed I expect there could be
unequal variance among other failed assumptions. A PERMANOVA test may be
more appropriate for this reason.

## Randomization Test

``` r
# Does percent preterm births change with income?
chic.rando<- chicagoplus%>%
  select(Preterm.Births, income)
# Calculate the mean difference between the two conditions
true_diff <- chic.rando %>%
  group_by(income) %>%
  summarize(means = mean(Preterm.Births)) %>%
  summarize(mean_diff = diff(means)) %>%
  pull
true_diff
```

    ## [1] -1.988095

``` r
# true diff is -1.99

# loop of sampled data
mean_diff <- vector()
for(i in 1:5000){ 
  temp <- data.frame(income = chic.rando$income, Preterm.Births = sample(chic.rando$Preterm.Births)) 
  
  mean_diff[i] <- temp %>% 
    group_by(income) %>%
    summarize(means = mean(Preterm.Births)) %>%
    summarize(mean_diff = diff(means)) %>%
    pull
}

# Represent the distribution of the mean differences with a vertical line showing the true difference
{hist(mean_diff, main="Distribution of the mean differences"); abline(v = -1.988095, col="red")}
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Calculate the corresponding two-sided p-value
mean(mean_diff > -true_diff | mean_diff < true_diff)
```

    ## [1] 0.0106

I selected preterm birth percent to compare because preterm births are
caused by things such as maternal disease, unhealthy habits (like
smoking), and stress. These are predicted to be related to lower income,
thus the means may differ.

Ho: The group means of percent pre-term births between higher income and
lower income are not significantly different. HA: The group means of
percent pre-term births between higher income and lower income are
significantly different.

After performing a randomization test, I reject the null hypothesis and
conclude that the group means of percent pre-term births between higher
income and lower income are significantly different. My evidence is a p
value of 0.008 which is less than my alpha of 0.05.

## Linear Regression

``` r
# centering numeric variables of interaction
chicagoplus$No.HS.Diploma_c <- chicagoplus$No.HS.Diploma - mean(chicagoplus$No.HS.Diploma)

# linear regression of centered variables with interactions
# note: centering may change coefficients, but not interactions
fit.gon_c <- lm(Gonorrhea.F ~ income * No.HS.Diploma_c, data = chicagoplus)
summary(fit.gon_c)
```

    ## 
    ## Call:
    ## lm(formula = Gonorrhea.F ~ income * No.HS.Diploma_c, data = chicagoplus)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1260.80  -561.36   -99.75   397.66  2070.02 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  1261.53     126.64   9.962 2.06e-14 ***
    ## incomeupper                  -749.51     620.17  -1.209  0.23150    
    ## No.HS.Diploma_c               -30.03      10.37  -2.896  0.00523 ** 
    ## incomeupper:No.HS.Diploma_c    47.70      45.28   1.054  0.29621    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 789 on 61 degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## Multiple R-squared:  0.2592, Adjusted R-squared:  0.2228 
    ## F-statistic: 7.114 on 3 and 61 DF,  p-value: 0.0003558

``` r
#graph of interaction
ggplot(chicagoplus, aes(x = No.HS.Diploma_c, y = Gonorrhea.F, color = income)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")+
  ggtitle("Gonorrhea rates and centered no high school degree by income")+
  ylab("Female gonorrhea cases (per 100,000)")+
  xlab("Centered percent no high school degree (over 25 y/o)")+
  labs(color = "Income in upper or lower\n half of US baseline")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 12 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 12 rows containing missing values (geom_point).

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

If income is in the upper half above the US baseline, then predicted
female gonorrhea rates decrease by 750 cases per 100,000 from the lower
income half, controlling for the centered percent with no high school
diploma. As the percent of no high school degrees in a community
increases above the sample mean by one percent, the female gonorrhea
rate is predicted to decrease by 30 cases. Additionally, if the
community is in the upper income half, the model predicts gonorrhea
cases increase by an extra 47 cases for every one percent increase above
the mean percent of no high school diplomas.

The model shows that only centered percent of no high school diploma is
a significant predictor of female gonorrhea rates.

The amount of variation explained by the model is revealed by the
coefficient of determination or r-squared. Because multiple predictors
were used, the adjusted r-squared is preferred. The adjusted r-squared
for the model is 0.22 meaning only 22% of the variation can be explained
in the model.

*Assumptions*

``` r
# Normality
# Q-Q plot to check for normality of the residuals
plot(fit.gon_c, which = 2)
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Residuals against fitted values plot to check for any problematic patterns (nonlinear, equal variance)
plot(fit.gon_c, which = 1)
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

According to the qq plot the residuals appear normal and meet the
normality assumption. The data meets the linearity and independence
assumptions as there is no non-linear pattern in the residuals-fitted
plot. The residuals-fitted plot suggests however that the data fails the
equal variance assumption as there is visible funneling.

*Robust Standard Errors*

``` r
# install.packages (sandwich wouldn't open coeftest, but lmtest did)
library(sandwich)
```

    ## Warning: package 'sandwich' was built under R version 4.0.4

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.0.4

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.0.4

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
coeftest(fit.gon_c, vcov = vcovHC(fit.gon_c))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)                 1261.5343   143.4821  8.7923  1.93e-12 ***
    ## incomeupper                 -749.5054   312.2059 -2.4007 0.0194300 *  
    ## No.HS.Diploma_c              -30.0288     7.5052 -4.0011 0.0001732 ***
    ## incomeupper:No.HS.Diploma_c   47.7049    18.1228  2.6323 0.0107275 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

After correcting for failed assumptions, the model now suggests that
both the effect from upper income and the interaction from the two
predictors is significant, in addition to the effect of centered percent
no high school diploma. Considering my earlier model failed some
assumptions, it is likely that this is a more appropriate way to view
the model.

*Bootstrapped SE and confidence interval*

``` r
# resampling loop
samp_SEs <- replicate(5000, {
  # Bootstrap your data (resample observations)
  boot_data <- sample_frac(chicagoplus, replace = TRUE)
  # Fit regression model
  fitboot <- lm(Gonorrhea.F ~ income * No.HS.Diploma_c, data = boot_data)
  # Save the coefficients
  coef(fitboot)
})

# Estimated SEs
samp_SEs %>%
  # Transpose the obtained matrices
  t %>%
  # Consider the matrix as a data frame
  as.data.frame %>%
  # Compute the standard error (standard deviation of the sampling distribution)
  summarize_all(sd)
```

    ##   (Intercept) incomeupper No.HS.Diploma_c incomeupper:No.HS.Diploma_c
    ## 1     142.435    322.2765        7.919512                    19.02677

At the change from original to robust standard error (SE) the SEs seemed
to decrease by a considerable amount. This was unexpected as I figured
the error would increase for failed assumptions. A reason for this might
be the fact that the original model suffered from failed assumptions
that were accounted for in the robust SEs. Another unknown reason may
also be the cause. As expected from decreased standard error, the p
values decreased from original to robust. The change even caused two
more predictors to appear significant. From robust SE to bootstrapped
SE, the standard errors increased in all areas, though slightly. This
makes sense as the bootstrapped SE assumes the fewest assumptions met,
so it presumes the greatest amount of error.

## Logistic Regression

``` r
# Logistic regression predicting high or low cancer rate than average based on % English and per capita income
chic.glm <- chicagoplus%>%
  
  #make cancer.death a binary response
  mutate(y = ifelse(cancer.death == "high", 1, 0))%>%
  filter(!is.na(Percent.under.well))

# chic.glm will the the dataset I use
chic.glm
```

    ## # A tibble: 75 x 76
    ##    Community.x Community.Name Birth.Rate General.Fert.Ra~ Low.Birth.Weight
    ##          <dbl> <chr>               <dbl>            <dbl>            <dbl>
    ##  1           1 Rogers Park          16.4             62               11  
    ##  2           2 West Ridge           17.3             83.3              8.1
    ##  3           3 Uptown               13.1             50.5              8.3
    ##  4           4 Lincoln Square       17.1             61                8.1
    ##  5           5 North Center         22.4             76.2              9.1
    ##  6           6 Lake View            13.5             38.7              6.3
    ##  7           7 Lincoln Park         13.2             38.7              6.6
    ##  8           8 Near North Si~       10.7             35.9              8.6
    ##  9           9 Edison Park          11.3             59.5              7.9
    ## 10          10 Norwood Park         10.4             59.6              4.9
    ## # ... with 65 more rows, and 71 more variables: Prenatal.Care.First.Trim <dbl>,
    ## #   Preterm.Births <dbl>, Teen.Birth.Rate <dbl>, `Assault.(Homicide)` <dbl>,
    ## #   Breast.Cancer.F <dbl>, `Cancer.(All.Sites)` <dbl>, Colorectal.Cancer <dbl>,
    ## #   `Diabetes-related` <dbl>, `Firearm-related` <dbl>, Infant.Mort.Rate <dbl>,
    ## #   Lung.Cancer <dbl>, Prostate.Cancer.M <dbl>, Stroke <dbl>,
    ## #   Childhood.Blood.Lead.level <dbl>, Childhood.Lead.Poisoning <dbl>,
    ## #   Gonorrhea.F <dbl>, Gonorrhea.M <dbl>, TB <dbl>, Below.Poverty.Level <dbl>,
    ## #   Crowded.Housing <dbl>, Dependency <dbl>, No.HS.Diploma <dbl>,
    ## #   Per.Capita.Income <dbl>, Unemployment <dbl>, `NON-ENGLISH.LANG` <chr>,
    ## #   Percent.under.well <dbl>, AFRICAN.LANGUAGES <dbl>, ARABIC <dbl>,
    ## #   ARMENIAN <dbl>, `CAMBODIAN.(MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>,
    ## #   FRENCH <dbl>, GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>,
    ## #   HINDI <dbl>, HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>,
    ## #   KOREAN <dbl>, LAOTIAN <dbl>, NAVAJO <dbl>, OTHER.ASIAN <dbl>,
    ## #   OTHER.INDIC <dbl>, OTHER.INDO.EURPOEAN <dbl>,
    ## #   OTHER.NATIVE.NORTH.AMERICAN <dbl>, OTHER.PACIFIC.ISLAND <dbl>,
    ## #   OTHER.SLAVIC <dbl>, OTHER.WEST.GERMANIC <dbl>, PERSIAN <dbl>, POLISH <dbl>,
    ## #   PORTUGUESE <dbl>, RUSSIAN <dbl>, SCANDINAVIAN <dbl>,
    ## #   `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>, THAI <dbl>,
    ## #   UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>,
    ## #   proficiency <chr>, income <chr>, cancer.death <chr>, gon.rate <chr>,
    ## #   No.HS.Diploma_c <dbl>, y <dbl>

``` r
#new dataset has relevant variables and binary variable
head(chic.glm)
```

    ## # A tibble: 6 x 76
    ##   Community.x Community.Name Birth.Rate General.Fert.Ra~ Low.Birth.Weight
    ##         <dbl> <chr>               <dbl>            <dbl>            <dbl>
    ## 1           1 Rogers Park          16.4             62               11  
    ## 2           2 West Ridge           17.3             83.3              8.1
    ## 3           3 Uptown               13.1             50.5              8.3
    ## 4           4 Lincoln Square       17.1             61                8.1
    ## 5           5 North Center         22.4             76.2              9.1
    ## 6           6 Lake View            13.5             38.7              6.3
    ## # ... with 71 more variables: Prenatal.Care.First.Trim <dbl>,
    ## #   Preterm.Births <dbl>, Teen.Birth.Rate <dbl>, `Assault.(Homicide)` <dbl>,
    ## #   Breast.Cancer.F <dbl>, `Cancer.(All.Sites)` <dbl>, Colorectal.Cancer <dbl>,
    ## #   `Diabetes-related` <dbl>, `Firearm-related` <dbl>, Infant.Mort.Rate <dbl>,
    ## #   Lung.Cancer <dbl>, Prostate.Cancer.M <dbl>, Stroke <dbl>,
    ## #   Childhood.Blood.Lead.level <dbl>, Childhood.Lead.Poisoning <dbl>,
    ## #   Gonorrhea.F <dbl>, Gonorrhea.M <dbl>, TB <dbl>, Below.Poverty.Level <dbl>,
    ## #   Crowded.Housing <dbl>, Dependency <dbl>, No.HS.Diploma <dbl>,
    ## #   Per.Capita.Income <dbl>, Unemployment <dbl>, `NON-ENGLISH.LANG` <chr>,
    ## #   Percent.under.well <dbl>, AFRICAN.LANGUAGES <dbl>, ARABIC <dbl>,
    ## #   ARMENIAN <dbl>, `CAMBODIAN.(MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>,
    ## #   FRENCH <dbl>, GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>,
    ## #   HINDI <dbl>, HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>,
    ## #   KOREAN <dbl>, LAOTIAN <dbl>, NAVAJO <dbl>, OTHER.ASIAN <dbl>,
    ## #   OTHER.INDIC <dbl>, OTHER.INDO.EURPOEAN <dbl>,
    ## #   OTHER.NATIVE.NORTH.AMERICAN <dbl>, OTHER.PACIFIC.ISLAND <dbl>,
    ## #   OTHER.SLAVIC <dbl>, OTHER.WEST.GERMANIC <dbl>, PERSIAN <dbl>, POLISH <dbl>,
    ## #   PORTUGUESE <dbl>, RUSSIAN <dbl>, SCANDINAVIAN <dbl>,
    ## #   `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>, THAI <dbl>,
    ## #   UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>,
    ## #   proficiency <chr>, income <chr>, cancer.death <chr>, gon.rate <chr>,
    ## #   No.HS.Diploma_c <dbl>, y <dbl>

``` r
#Logistic model
fit1 <- glm(y ~ Per.Capita.Income * proficiency, data = chic.glm, family = "binomial")
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = y ~ Per.Capita.Income * proficiency, family = "binomial", 
    ##     data = chic.glm)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3328  -0.9768   0.2324   0.8454   1.4799  
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)                        -1.131e-02  1.106e+00  -0.010  0.99184   
    ## Per.Capita.Income                  -2.137e-05  5.130e-05  -0.417  0.67701   
    ## proficiencybelow                    5.159e+00  1.889e+00   2.731  0.00632 **
    ## Per.Capita.Income:proficiencybelow -1.078e-04  6.718e-05  -1.605  0.10843   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 102.889  on 74  degrees of freedom
    ## Residual deviance:  72.854  on 71  degrees of freedom
    ## AIC: 80.854
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
#more interpretable odds
exp(coef(fit1))
```

    ##                        (Intercept)                  Per.Capita.Income 
    ##                          0.9887499                          0.9999786 
    ##                   proficiencybelow Per.Capita.Income:proficiencybelow 
    ##                        174.0668048                          0.9998922

By controlling for income, I attempted to determine if a lack of
proficiency in English led to people being more hesitant to visit a
health care provider without the effects of high health care costs. This
hesitancy may arise from a language barrier between communities and
health care providers.

The results of the logistic model suggest that proficiency in English
does significantly predict if a community will have cancer deaths higher
or lower than the US baseline. The interpretation is those communities
who have English proficiency of “below” the US baseline have odds of
living in a community with cancer deaths higher than the US baseline 174
times the odds of those who have proficiency of “above”, when
controlling for per capita income.

The insignificant effect of per capita income was having the odds of
living in a community with cancer deaths higher than the US baseline be
multiplied by 0.99 for each one dollar increase in per capita income,
controlling for English proficiency. The interactions were insignificant
as well.

#### Confusion Matrix

``` r
# create probabilities variable
chic.glm$prob1 <-  predict(fit1, type = "response")

# call probabilities of greater than 0.5 true... i.e. "high" cancer, otherwise "low"
chic.glm$predicted <- ifelse(chic.glm$prob1 > .5, "high", "low")

# Confusion matrix: compare true to predicted condition in table
table(true_condition = chic.glm$cancer.death, predicted_condition = chic.glm$predicted) %>% 
  addmargins
```

    ##               predicted_condition
    ## true_condition high low Sum
    ##           high   26  16  42
    ##           low     4  29  33
    ##           Sum    30  45  75

``` r
# Accuracy (correct/total)
(26+29)/75
```

    ## [1] 0.7333333

``` r
# Sensitivity (proportion true positives identified)
26/42
```

    ## [1] 0.6190476

``` r
# Specificity (proportion true negatives identified)
29/33
```

    ## [1] 0.8787879

``` r
# Precision (proportion of true positive predictions)
26/30
```

    ## [1] 0.8666667

According to the confusion matrix, my models accuracy was 0.73, the
sensitivity 0.62, the specificity 0.89, and the precision 0.86. The
worst predictor was for identifying communities higher-than-US-baseline
cancer deaths (sensitivity). It was better at correctly identifying
communities with lower-than-US-baseline cancer deaths (specificity).
Overall the accuracy and precision of the model are good.

``` r
# Predicting log-odds from model
chic.glm$logit <- predict(fit1, type = "link") 
chic.glm
```

    ## # A tibble: 75 x 79
    ##    Community.x Community.Name Birth.Rate General.Fert.Ra~ Low.Birth.Weight
    ##          <dbl> <chr>               <dbl>            <dbl>            <dbl>
    ##  1           1 Rogers Park          16.4             62               11  
    ##  2           2 West Ridge           17.3             83.3              8.1
    ##  3           3 Uptown               13.1             50.5              8.3
    ##  4           4 Lincoln Square       17.1             61                8.1
    ##  5           5 North Center         22.4             76.2              9.1
    ##  6           6 Lake View            13.5             38.7              6.3
    ##  7           7 Lincoln Park         13.2             38.7              6.6
    ##  8           8 Near North Si~       10.7             35.9              8.6
    ##  9           9 Edison Park          11.3             59.5              7.9
    ## 10          10 Norwood Park         10.4             59.6              4.9
    ## # ... with 65 more rows, and 74 more variables: Prenatal.Care.First.Trim <dbl>,
    ## #   Preterm.Births <dbl>, Teen.Birth.Rate <dbl>, `Assault.(Homicide)` <dbl>,
    ## #   Breast.Cancer.F <dbl>, `Cancer.(All.Sites)` <dbl>, Colorectal.Cancer <dbl>,
    ## #   `Diabetes-related` <dbl>, `Firearm-related` <dbl>, Infant.Mort.Rate <dbl>,
    ## #   Lung.Cancer <dbl>, Prostate.Cancer.M <dbl>, Stroke <dbl>,
    ## #   Childhood.Blood.Lead.level <dbl>, Childhood.Lead.Poisoning <dbl>,
    ## #   Gonorrhea.F <dbl>, Gonorrhea.M <dbl>, TB <dbl>, Below.Poverty.Level <dbl>,
    ## #   Crowded.Housing <dbl>, Dependency <dbl>, No.HS.Diploma <dbl>,
    ## #   Per.Capita.Income <dbl>, Unemployment <dbl>, `NON-ENGLISH.LANG` <chr>,
    ## #   Percent.under.well <dbl>, AFRICAN.LANGUAGES <dbl>, ARABIC <dbl>,
    ## #   ARMENIAN <dbl>, `CAMBODIAN.(MON-KHMER)` <dbl>, CHINESE <dbl>, CREOLE <dbl>,
    ## #   FRENCH <dbl>, GERMAN <dbl>, GREEK <dbl>, GUJARATI <dbl>, HEBREW <dbl>,
    ## #   HINDI <dbl>, HMONG <dbl>, HUNGARIAN <dbl>, ITALIAN <dbl>, JAPANESE <dbl>,
    ## #   KOREAN <dbl>, LAOTIAN <dbl>, NAVAJO <dbl>, OTHER.ASIAN <dbl>,
    ## #   OTHER.INDIC <dbl>, OTHER.INDO.EURPOEAN <dbl>,
    ## #   OTHER.NATIVE.NORTH.AMERICAN <dbl>, OTHER.PACIFIC.ISLAND <dbl>,
    ## #   OTHER.SLAVIC <dbl>, OTHER.WEST.GERMANIC <dbl>, PERSIAN <dbl>, POLISH <dbl>,
    ## #   PORTUGUESE <dbl>, RUSSIAN <dbl>, SCANDINAVIAN <dbl>,
    ## #   `SERBO-CROATIAN` <dbl>, SPANISH <dbl>, TAGALOG <dbl>, THAI <dbl>,
    ## #   UNSPECIFIED <dbl>, URDU <dbl>, VIETNAMESE <dbl>, YIDDISH <dbl>,
    ## #   proficiency <chr>, income <chr>, cancer.death <chr>, gon.rate <chr>,
    ## #   No.HS.Diploma_c <dbl>, y <dbl>, prob1 <dbl>, predicted <chr>, logit <dbl>

``` r
# use predicted log-odds to build density plot
chic.glm %>%
  ggplot() + 
  geom_density(aes(logit, color = cancer.death, fill = cancer.death), alpha = .4) +
    geom_rug(aes(logit, color = cancer.death)) +
  geom_text(x = -5, y = .07, label = "TN = 431") +
  geom_text(x = -1.75, y = .008, label = "FN = 19") +
  geom_text(x = 1, y = .006, label = "FP = 13") +
  geom_text(x = 5, y = .04, label = "TP = 220") +
  theme(legend.position = c(.85,.85)) +
  geom_vline(xintercept = 0) + 
  xlab("logit (log-odds)")+
  ggtitle("Density plot of log-odds by higher or lower than US baseline cancer deaths")
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# ROC and AUC of model
library(plotROC) 
```

    ## Warning: package 'plotROC' was built under R version 4.0.4

``` r
# Plot ROC depending on values of y and its probabilities displaying some cutoff values
cancer.ROC <- ggplot(chic.glm) + 
  geom_roc(aes(d = y, m = prob1))
cancer.ROC
```

![](comp_bio_project_2_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# AUC
calc_auc(cancer.ROC)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.8253968

The area under the curve (AUC) of my ROC plot is about 83% of the total
area. The interpretation of the AUC is a randomly selected community
from the “high” cancer death category would have a test value greater
than a randomly selected community from the “low” cancer death category
83 percent of the time. This AUC suggests the model is “good” at
predicting if a community would have higher or lower cancer deaths than
the US baseline.
