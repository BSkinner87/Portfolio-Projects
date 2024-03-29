Hate Crime Stats
================
Brett Skinner
2023-12-26

# Load and separate different groups

## Separate LGBTQ groups

``` r
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ───────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
HateCrimes <- read_csv("C:/Users/brett/Desktop/Employment Stuff/Portfolio/Kaggle Data Sets/Hate Crime (complete)/Hate_Crimes_filtered_cols.csv")
```

    ## Rows: 209442 Columns: 12
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): incident_date, agency_name, state_name, division_name, population_group, offender_race, offense_name, locat...
    ## dbl  (2): offender_count, victim_count
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
HateCrimes$offender_race[is.na(HateCrimes$offender_race)] <- "Unknown"
HateCrimes$Anti_Trans <- NA
HateCrimes$Anti_Trans[str_detect(HateCrimes$bias_desc, "Anti-Transgender")] <-"Y"
HateCrimes$Anti_Trans[!str_detect(HateCrimes$bias_desc, "Anti-Transgender")] <-"N"

HateCrimes$Anti_Gay <- NA
HateCrimes$Anti_Gay[str_detect(HateCrimes$bias_desc, 'Anti-Gay \\(Male\\)')] <-"Y"
HateCrimes$Anti_Gay[!str_detect(HateCrimes$bias_desc, "Anti-Gay \\(Male\\)")] <-"N"

HateCrimes$Anti_Lesbian <- NA
HateCrimes$Anti_Lesbian[str_detect(HateCrimes$bias_desc, "Anti-Lesbian \\(Female\\)")] <-"Y"
HateCrimes$Anti_Lesbian[!str_detect(HateCrimes$bias_desc, "Anti-Lesbian \\(Female\\)")] <-"N"

HateCrimes$Anti_Bi <- NA
HateCrimes$Anti_Bi[str_detect(HateCrimes$bias_desc, "Anti-Bisexual")] <-"Y"
HateCrimes$Anti_Bi[!str_detect(HateCrimes$bias_desc, "Anti-Bisexual")] <-"N"

HateCrimes$Anti_Fluid <- NA
HateCrimes$Anti_Fluid[str_detect(HateCrimes$bias_desc, "Anti-Gender Non-Conforming")] <-"Y"
HateCrimes$Anti_Fluid[!str_detect(HateCrimes$bias_desc, "Anti-Gender Non-Conforming")] <-"N"

HateCrimes$Anti_LGBTQ <- NA
HateCrimes$Anti_LGBTQ[str_detect(HateCrimes$bias_desc, "Anti-Gay \\(Male\\)") |
                       str_detect(HateCrimes$bias_desc, "Anti-Lesbian \\(Female\\)") |
                       str_detect(HateCrimes$bias_desc, "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)") |
                       str_detect(HateCrimes$bias_desc, "Anti-Bisexual") |
                       str_detect(HateCrimes$bias_desc, "Anti-Gender Non-Conforming") |
                       str_detect(HateCrimes$bias_desc, "Anti-Transgender")] <- "Y"

HateCrimes$Anti_LGBTQ[!(str_detect(HateCrimes$bias_desc, "Anti-Gay \\(Male\\)") | 
                        str_detect(HateCrimes$bias_desc, "Anti-Lesbian \\(Female\\)") | 
                        str_detect(HateCrimes$bias_desc, "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)") |
                        str_detect(HateCrimes$bias_desc, "Anti-Bisexual") |
                        str_detect(HateCrimes$bias_desc, "Anti-Gender Non-Conforming") |
                        str_detect(HateCrimes$bias_desc, "Anti-Transgender"))] <- "N"

HateCrimes$Anti_Hetero <- NA
HateCrimes$Anti_Hetero[str_detect(HateCrimes$bias_desc, "Anti-Heterosexual")] <- "Y"
HateCrimes$Anti_Hetero[!str_detect(HateCrimes$bias_desc, "Anti-Heterosexual")] <- "N"
```

## Separate by Race

``` r
HateCrimes$Anti_Black <- NA
HateCrimes$Anti_Black[str_detect(HateCrimes$bias_desc, "Anti-Black or African American")] <- "Y"
HateCrimes$Anti_Black[!str_detect(HateCrimes$bias_desc, "Anti-Black or African American")] <- "N"

HateCrimes$Anti_White <- NA
HateCrimes$Anti_White[str_detect(HateCrimes$bias_desc, "Anti-White")] <- "Y"
HateCrimes$Anti_White[!str_detect(HateCrimes$bias_desc, "Anti-White")] <- "N"

HateCrimes$Anti_Asian <- NA
HateCrimes$Anti_Asian[str_detect(HateCrimes$bias_desc, "Anti-Asian")] <- "Y"
HateCrimes$Anti_Asian[!str_detect(HateCrimes$bias_desc, "Anti-Asian")] <- "N"

HateCrimes$Anti_Pac_Isl <- NA
HateCrimes$Anti_Pac_Isl[str_detect(HateCrimes$bias_desc, "Anti-Native Hawaiian or Other Pacific Islander")] <- "Y"
HateCrimes$Anti_Pac_Isl[!str_detect(HateCrimes$bias_desc, "Anti-Native Hawaiian or Other Pacific Islander")] <- "N"

HateCrimes$Anti_Native_Am <- NA
HateCrimes$Anti_Native_Am[str_detect(HateCrimes$bias_desc, "Anti-American Indian or Alaska Native")] <- "Y"
HateCrimes$Anti_Native_Am[!str_detect(HateCrimes$bias_desc, "Anti-American Indian or Alaska Native")] <- "N"

HateCrimes$Anti_Multiple_race <- NA
HateCrimes$Anti_Multiple_race[str_detect(HateCrimes$bias_desc, "Anti-Multiple Races")] <- "Y"
HateCrimes$Anti_Multiple_race[!str_detect(HateCrimes$bias_desc, "Anti-Multiple Races")] <- "N"

HateCrimes$Anti_Hispanic <- NA
HateCrimes$Anti_Hispanic[str_detect(HateCrimes$bias_desc, "Anti-Hispanic or Latino")] <- "Y"
HateCrimes$Anti_Hispanic[!str_detect(HateCrimes$bias_desc, "Anti-Hispanic or Latino")] <- "N"

HateCrimes$Anti_Arab <- NA
HateCrimes$Anti_Arab[str_detect(HateCrimes$bias_desc, "Anti-Arab")] <- "Y"
HateCrimes$Anti_Arab[!str_detect(HateCrimes$bias_desc, "Anti-Arab")] <- "N"

HateCrimes$Anti_Jewish <- NA
HateCrimes$Anti_Jewish[str_detect(HateCrimes$bias_desc, "Anti-Jewish")] <- "Y"
HateCrimes$Anti_Jewish[!str_detect(HateCrimes$bias_desc, "Anti-Jewish")] <- "N"

HateCrimes$Anti_Other_Race <- NA
HateCrimes$Anti_Other_Race[str_detect(HateCrimes$bias_desc, "Anti-Other Race/Ethnicity/Ancestry")] <- "Y"
HateCrimes$Anti_Other_Race[!str_detect(HateCrimes$bias_desc, "Anti-Other Race/Ethnicity/Ancestry")] <- "N"

HateCrimes$Anti_Racial <- NA
HateCrimes$Anti_Racial[HateCrimes$Anti_Black == "Y" |
                    HateCrimes$Anti_White == "Y" |
                    HateCrimes$Anti_Asian == "Y" |
                    HateCrimes$Anti_Pac_Isl == "Y" |
                    HateCrimes$Anti_Native_Am == "Y" |
                    HateCrimes$Anti_Multiple_race == "Y" |
                    HateCrimes$Anti_Hispanic == "Y" |
                    HateCrimes$Anti_Arab == "Y" |
                    HateCrimes$Anti_Jewish == "Y" |
                    HateCrimes$Anti_Other_Race == "Y"] <- "Y"
HateCrimes$Anti_Racial[!(HateCrimes$Anti_Black == "Y" |
                    HateCrimes$Anti_White == "Y" |
                    HateCrimes$Anti_Asian == "Y" |
                    HateCrimes$Anti_Pac_Isl == "Y" |
                    HateCrimes$Anti_Native_Am == "Y" |
                    HateCrimes$Anti_Multiple_race == "Y" |
                    HateCrimes$Anti_Hispanic == "Y" |
                    HateCrimes$Anti_Arab == "Y" |
                    HateCrimes$Anti_Jewish == "Y" |
                    HateCrimes$Anti_Other_Race == "Y")] <- "N"
```

# Separate by Religion

``` r
HateCrimes$Anti_Catholic <- NA
HateCrimes$Anti_Catholic[str_detect(HateCrimes$bias_desc, "Anti-Catholic")] <- "Y"
HateCrimes$Anti_Catholic[!str_detect(HateCrimes$bias_desc, "Anti-Catholic")] <- "N"

HateCrimes$Anti_Protestant <- NA
HateCrimes$Anti_Protestant[str_detect(HateCrimes$bias_desc, "Anti-Protestant")] <- "Y"
HateCrimes$Anti_Protestant[!str_detect(HateCrimes$bias_desc, "Anti-Protestant")] <- "N"

HateCrimes$Anti_Jehovah <- NA
HateCrimes$Anti_Jehovah[str_detect(HateCrimes$bias_desc, "Anti-Jehovah's Witness")] <- "Y"
HateCrimes$Anti_Jehovah[!str_detect(HateCrimes$bias_desc, "Anti-Jehovah's Witness")] <- "N"

HateCrimes$Anti_Mormon <- NA
HateCrimes$Anti_Mormon[str_detect(HateCrimes$bias_desc, "Anti-Mormon")] <- "Y"
HateCrimes$Anti_Mormon[!str_detect(HateCrimes$bias_desc, "Anti-Mormon")] <- "N"

HateCrimes$Anti_Buddhist <- NA
HateCrimes$Anti_Buddhist[str_detect(HateCrimes$bias_desc, "Anti-Buddhist")] <- "Y"
HateCrimes$Anti_Buddhist[!str_detect(HateCrimes$bias_desc, "Anti-Buddhist")] <- "N"

HateCrimes$Anti_Sikh <- NA
HateCrimes$Anti_Sikh[str_detect(HateCrimes$bias_desc, "Anti-Sikh")] <- "Y"
HateCrimes$Anti_Sikh[!str_detect(HateCrimes$bias_desc, "Anti-Sikh")] <- "N"

HateCrimes$Anti_Other_Christian <- NA
HateCrimes$Anti_Other_Christian[str_detect(HateCrimes$bias_desc, "Anti-Other Christian")] <- "Y"
HateCrimes$Anti_Other_Christian[!str_detect(HateCrimes$bias_desc, "Anti-Other Christian")] <- "N"

HateCrimes$Anti_Hindu <- NA
HateCrimes$Anti_Hindu[str_detect(HateCrimes$bias_desc, "Anti-Hindu")] <- "Y"
HateCrimes$Anti_Hindu[!str_detect(HateCrimes$bias_desc, "Anti-Hindu")] <- "N"

HateCrimes$Anti_Atheist <- NA
HateCrimes$Anti_Atheist[str_detect(HateCrimes$bias_desc, "Anti-Atheism/Agnosticism")] <- "Y"
HateCrimes$Anti_Atheist[!str_detect(HateCrimes$bias_desc, "Anti-Atheism/Agnosticism")] <- "N"

HateCrimes$Anti_East_Orthodox <- NA
HateCrimes$Anti_East_Orthodox[str_detect(HateCrimes$bias_desc, "Anti-Eastern Orthodox \\(Russian, Greek, Other\\)")] <- "Y"
HateCrimes$Anti_East_Orthodox[!str_detect(HateCrimes$bias_desc, "Anti-Eastern Orthodox \\(Russian, Greek, Other\\)")] <- "N"

HateCrimes$Anti_Islamic <- NA
HateCrimes$Anti_Islamic[str_detect(HateCrimes$bias_desc, "Anti-Islamic \\(Muslim\\)")] <- "Y"
HateCrimes$Anti_Islamic[!str_detect(HateCrimes$bias_desc, "Anti-Islamic \\(Muslim\\)")] <- "N"

HateCrimes$Anti_Other_Religion <- NA
HateCrimes$Anti_Other_Religion[str_detect(HateCrimes$bias_desc, "Anti-Other Religion")] <- "Y"
HateCrimes$Anti_Other_Religion[!str_detect(HateCrimes$bias_desc, "Anti-Other Religion")] <- "N"

HateCrimes$Anti_Multiple_Religion <- NA
HateCrimes$Anti_Multiple_Religion[str_detect(HateCrimes$bias_desc, "Anti-Multiple Religions, Group")] <- "Y"
HateCrimes$Anti_Multiple_Religion[!str_detect(HateCrimes$bias_desc, "Anti-Multiple Religions, Group")] <- "N"

HateCrimes$Anti_Religious <- NA
HateCrimes$Anti_Religious[HateCrimes$Anti_Jewish == "Y" |
                            HateCrimes$Anti_Catholic == "Y" |
                            HateCrimes$Anti_Protestant == "Y" |
                            HateCrimes$Anti_Jehovah == "Y" |
                            HateCrimes$Anti_Mormon == "Y" |
                            HateCrimes$Anti_Buddhist == "Y" |
                            HateCrimes$Anti_Sikh == "Y" |
                            HateCrimes$Anti_Other_Christian == "Y" |
                            HateCrimes$Anti_Hindu == "Y" |
                            HateCrimes$Anti_East_Orthodox == "Y" |
                            HateCrimes$Anti_Islamic == "Y" |
                            HateCrimes$Anti_Other_Religion == "Y" |
                            HateCrimes$Anti_Multiple_Religion == "Y"] <-"Y"
HateCrimes$Anti_Religious[!(HateCrimes$Anti_Jewish == "Y" |
                            HateCrimes$Anti_Catholic == "Y" |
                            HateCrimes$Anti_Protestant == "Y" |
                            HateCrimes$Anti_Jehovah == "Y" |
                            HateCrimes$Anti_Mormon == "Y" |
                            HateCrimes$Anti_Buddhist == "Y" |
                            HateCrimes$Anti_Sikh == "Y" |
                            HateCrimes$Anti_Other_Christian == "Y" |
                            HateCrimes$Anti_Hindu == "Y" |
                            HateCrimes$Anti_East_Orthodox == "Y" |
                            HateCrimes$Anti_Islamic == "Y" |
                            HateCrimes$Anti_Other_Religion == "Y" |
                            HateCrimes$Anti_Multiple_Religion == "Y")] <-"N"
```

- Note that there isn’t a distinction between ethnicly Jewish and
  Religiously Jewish

## Separate by gender

``` r
HateCrimes$Anti_Female <- NA
HateCrimes$Anti_Female[str_detect(HateCrimes$bias_desc, "Anti-Female")] <- "Y"
HateCrimes$Anti_Female[!str_detect(HateCrimes$bias_desc, "Anti-Female")] <- "N"

HateCrimes$Anti_Male <- NA
HateCrimes$Anti_Male[str_detect(HateCrimes$bias_desc, "Anti-Male")] <- "Y"
HateCrimes$Anti_Male[!str_detect(HateCrimes$bias_desc, "Anti-Male")] <- "N"
```

## Separate by disability

``` r
HateCrimes$Anti_Phy_Disability <- NA
HateCrimes$Anti_Phy_Disability[str_detect(HateCrimes$bias_desc, "Anti-Physical Disability")] <- "Y"
HateCrimes$Anti_Phy_Disability[!str_detect(HateCrimes$bias_desc, "Anti-Physical Disability")] <- "N"

HateCrimes$Anti_Men_Disability <- NA
HateCrimes$Anti_Men_Disability[str_detect(HateCrimes$bias_desc, "Anti-Mental Disability")] <- "Y"
HateCrimes$Anti_Men_Disability[!str_detect(HateCrimes$bias_desc, "Anti-Mental Disability")] <- "N"

HateCrimes$Anti_Disability <- NA
HateCrimes$Anti_Disability[HateCrimes$Anti_Men_Disability == "Y" | 
                             HateCrimes$Anti_Phy_Disability == "Y"] <- "Y"
HateCrimes$Anti_Disability[HateCrimes$Anti_Men_Disability == "N" & 
                             HateCrimes$Anti_Phy_Disability == "N"] <- "N"
```

### Separating the date into day month and year.

``` r
HateCrimes[c("day", "month", "year")] <- str_split_fixed(HateCrimes$incident_date, "-", 3)
HateCrimes$year <- format(as.Date(HateCrimes$year, "%y"), "%Y")
HateCrimes$day <- format(as.Date(HateCrimes$day, "%d"), "%d")
```

# Compare Religious, Racial and LGBTQ hate crimes.

``` r
Total_Main <- list()
for(i in 1:29){
  Total_Main[[i]] <- c(nrow(filter(HateCrimes, Anti_LGBTQ == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_Racial == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_Religious == "Y" & year == 1990 + i)))
}
Total_Main <- t(data.frame(Total_Main))
rownames(Total_Main) <- c(1:29)
colnames(Total_Main) <- c("LGBTQ", "Racial", "Religious")
Total_Main <- data.frame(Total_Main)
Total_Main$year <- c(1991:2019)
# stacking the dataframe getting ready for plotting. 
Total_Main_stacked <- cbind(Total_Main[4], stack(Total_Main[1:3]))
```

    ## Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
colnames(Total_Main_stacked) <- c("year", "nHate_Crimes", "bias")

ggplot(Total_Main_stacked, aes(x = year, y = nHate_Crimes)) + 
  geom_line(aes(color = bias), size = 1) +
  scale_color_manual(name = "Victimized Group", 
                     values = c("LGBTQ" = "darkblue", "Racial" = "darkgreen", "Religious"="darkred")) + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") +
  ggtitle("Groups Victimized by Hate Crimes")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

- 1989 - The Hate Crimes Statistics Act is adopted. Justice Department
  is tasked with collecting and publishing data about crimes motivated
  by hatred based on Race, Religion, Ethnicity, and Sexual Orientation.

- 1993 - Legislation is passed allowing judges to impose harsher
  punishments for hate crimes. (numbers dip for a year in 1994)

- 1997 - Hate Crime Prevention Act is introduced; extending more
  “protection of the current federal hate crimes law to include those
  who are victimized because of their sexual orientation, gender or
  disability.”
  (<https://www.hrc.org/resources/hate-crimes-timeline#1990>)

- 1999 - Hate Crimes Prevention Act is passed.

- In terms of total hate crimes, racial hate crimes occur way more often
  than the other groups, but LGBTQ populations make up less of the
  population than the other two groups.

- we need to look at per capita hate crimes.

``` r
LGBTQ_Population <- read_csv("C:/Users/brett/Desktop/Employment Stuff/Portfolio/Kaggle Data Sets/Hate Crime (complete)/LGBTQ_Population.csv")
```

    ## Rows: 29 Columns: 4
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, LGBTQ_Percent, US_Population, LGBTQ_Population
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
LGBTQ_Capita <- data.frame(year = c(1991:2019))
LGBTQ_Capita$Racial <- round(100000*Total_Main$Racial/LGBTQ_Population$US_Population, digits = 2)
LGBTQ_Capita$LGBTQ <- round(100000*Total_Main$LGBTQ/LGBTQ_Population$LGBTQ_Population, digits = 2)
LGBTQ_Capita$Religious <- round(100000*Total_Main$Religious/LGBTQ_Population$US_Population, digits = 2)
LGBTQ_Capita_stacked <- cbind(LGBTQ_Capita[1], stack(LGBTQ_Capita[2:4]))
colnames(LGBTQ_Capita_stacked) <- c("Year", "nHate_Crimes", "bias")

ggplot(data = LGBTQ_Capita_stacked, mapping = aes(x = Year, y = nHate_Crimes)) + 
  geom_line(mapping = aes(color = bias), size = 1) + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Number of Hate Crimes Against 3 Main Groups Per 100000 People")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

- Religious hate crimes also includes hate crimes against Atheists and
  Agnostics.

- According to
  <https://www.hrc.org/resources/hate-crimes-timeline#1990>, the LGBTQ
  numbers are a little low.

- Note: I divided by population of US for Racial and Religious, since
  all types of racial and religious hate crimes were included. Everyone
  in the US has a “religion” (including Atheists/Agnostics) and a race.

- You are about 3 times as likely to be a victim of a hate crime for
  being a member of the LGBTQ population then for being a particular
  race.

- You are also about twice as likely to be a victim of hate crimes for
  racial reasons than religious.

# LGBTQ

``` r
ggplot(select(Total_Main,c("year", "LGBTQ")), aes(x = year, y = LGBTQ)) + 
  geom_line(size = 1, color = "darkblue") +
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Hate Crimes Against LGBTQ")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Separating the different groups under the LGBTQ umbrella

``` r
LGBTQ_Total <- list()
for(i in 1:29){
  LGBTQ_Total[[i]] <- c(nrow(filter(HateCrimes, Anti_Trans == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_Gay == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_Lesbian == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Bi == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Fluid == "Y" & year == 1990 + i)))
}

LGBTQ_Total <- t(data.frame(LGBTQ_Total))
rownames(LGBTQ_Total) <- c(1:29)
colnames(LGBTQ_Total) <- c("Transgender", "Gay", "Lesbian", "Bisexual", "Gender_Non_Conforming")
LGBTQ_Total <- data.frame(LGBTQ_Total)
LGBTQ_Total$year <- c(1991:2019)
LGBTQ_Total_stacked <- cbind(LGBTQ_Total[6], stack(LGBTQ_Total[1:5]))
```

    ## Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
colnames(LGBTQ_Total_stacked) <- c("year", "nHate_Crimes", "bias")

ggplot(LGBTQ_Total_stacked, mapping = aes(x  = year, y = nHate_Crimes)) + 
  geom_line(aes(color = bias), size = 1) + 
  scale_color_manual(name = "Victim Group", 
                     values = c("Gender_Non_Conforming" = "purple", 
                                "Transgender" = "red",
                                "Bisexual" = "#1D1af9", 
                                "Gay" = "orange",
                                "Lesbian" = "#55ca38")) + 
  ggtitle("Hate Crimes Against LGBTQ") + 
  xlab("Year") + 
  ylab("Number of Hate Crimes")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(data = subset(LGBTQ_Total, select = c(Transgender, year)), mapping = aes(x = year, y = Transgender)) + 
  geom_line(color = "red", size = 1) + 
  ggtitle("Hate Crimes Against Trans People") + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") 
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

- Immediately we see that hate crimes against gays are the majority of
  the hate crimes against LGBTQ.

- In recent years it looks like hate crimes against Transgender and
  Gender non-conforming has spiked. This is in part due to the lack of
  ability to classify hate crimes against Trans individuals before 2009.
  They have also been thrust into the spotlight post Obergefell (2015).

- We notice that hate crimes agains gays were lower in 2014 and we see
  an increase in and after 2015.

# Hate Crimes Against Relgious Groups

``` r
Religious_Total <- list()
for(i in 1:29){
  Religious_Total[[i]] <- c(nrow(filter(HateCrimes, Anti_Catholic == "Y" & year == 1990 + i)), 
                        nrow(filter(HateCrimes, Anti_Protestant == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Jewish == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Jehovah == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Mormon == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Buddhist == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Sikh == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_East_Orthodox == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Hindu == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Other_Christian == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Atheist == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Islamic == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Other_Religion == "Y" & year == 1990 + i)),
                        nrow(filter(HateCrimes, Anti_Multiple_Religion == "Y" & year == 1990 + i)))
}

Religious_Total <- t(data.frame(Religious_Total))
rownames(Religious_Total) <- c(1:29)
colnames(Religious_Total) <- c("Catholic", "Protestant", "Jewish", "Jahovah_Witness", "Mormon", "Buddhist",
                               "Sikh","East_Orthodox","Hindu","Other_Christian","Atheist","Islam",
                               "Other_Religion", "Multiple_Religions")
Religious_Total <- data.frame(Religious_Total)
Religious_Total$year <- c(1991:2019)
Religious_Total_stacked <- cbind(Religious_Total[15], stack(Religious_Total[1:14]))
```

    ## Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
colnames(Religious_Total_stacked) <- c("year", "nHate_Crimes", "bias")
Religious_Total_stacked$bias <- as.character(Religious_Total_stacked$bias) #makes column character column
Religious_Total_stacked$bias[Religious_Total_stacked$bias == "Jahovah_Witness"] <- "Jehovahs Witness"
Religious_Total_stacked$bias[Religious_Total_stacked$bias == "East_Orthodox"] <- "Eastern Orthodox"
Religious_Total_stacked$bias[Religious_Total_stacked$bias == "Other_Christian"] <- "Other Christian"
Religious_Total_stacked$bias[Religious_Total_stacked$bias == "Other_Religion"] <- "Other Religion"
Religious_Total_stacked$bias[Religious_Total_stacked$bias == "Multiple_Religions"] <- "Multiple Religions"
Religious_Total_stacked$bias <- as.factor(Religious_Total_stacked$bias) #changes column back to factor

ggplot(subset(Religious_Total_stacked, Religious_Total_stacked$bias %in% c("Jehovas Witness",
              "Mormon",
              "Buddhist",
              "Sikh",
              "Hindu",
              "Atheist",
              "Islam",
              "Other Religion",
              "Multiple Religions",
              "Jewish")), mapping = aes(x  = year, y = nHate_Crimes)) + 
  geom_line(aes(color = bias), size = 1) + 
  scale_color_manual(name = "Victimized Religious Group", 
                     values = c(#"Catholic" = "#751AF9", 
                                #"Protestant" = "#f91a1a",
                                "Jehovas Witness" = "#c40000", 
                                "Mormon" = "#c48900",
                                "Buddhist" = "#30c400",
                                "Jewish" = "#0d5600",
                                "Sikh" = "#00c1c4",
                                #"Eastern Orthodox" = "#881111",
                                "Hindu" = "#0065c4",
                                #"Other Christian" = "#f9af1a",
                                "Atheist" = "#7a00c4",
                                "Islam" = "#dc0ad5",
                                "Other Religion" = "#969696",
                                "Multiple Religions" = "#000000")) + 
  ggtitle("Hate Crimes Against Individual Religions") + 
  xlab("Year") + 
  ylab("Number of Hate Crimes")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(subset(Religious_Total_stacked, Religious_Total_stacked$bias=="Islam"), mapping = aes(x = year, y = nHate_Crimes)) +
  geom_line(color = "darkgreen", size = 1) + 
  ggtitle("Hate Crimes Against Muslims") + 
  xlab("Year") + 
  ylab("Number of Hate Crimes")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

- Note: Ethnic Jewish and Religious Jewish are combined.

- Hate Crimes against Jews is still a very pervasive problem.

- Obviously there is a spike of hate crimes against Muslims in 2001.
  9/11 happened, so this is expected.

- After 9/11 the number of hate crimes never went down to pre 9/11
  levels with another spike in 2015-2017 around the time where there was
  discussion of a Muslim ban.

## Isolating Abrahamic Religions

``` r
Abraham_Total <- subset(Religious_Total, select = c("Islam", "Jewish", "Catholic", "Protestant", "Other_Christian", "East_Orthodox", "year"))
Abraham_Total$Christian <- Abraham_Total$Catholic + Abraham_Total$Protestant + Abraham_Total$Other_Christian + Abraham_Total$East_Orthodox
Abraham_Total <- subset(Abraham_Total, select = c("Islam", "Jewish", "Christian", "year"))
Abraham_Total_stacked <- cbind(Abraham_Total[4], stack(Abraham_Total[1:3]))
```

    ## Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
colnames(Abraham_Total_stacked) <- c("year", "nHate_Crimes", "bias")

ggplot(data = Abraham_Total_stacked, mapping = aes(x = year, y = nHate_Crimes)) + 
  geom_line(aes(color = bias), size = 1) + 
  scale_color_manual(name = "Victimized Religion", values = c("Christian" = "darkgreen", 
                                                              "Jewish" = "darkblue", 
                                                              "Islam" = "darkred"))+
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Hate Crimes Against Abrahamic Religious Group")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

- We can see the total number of hate crimes against Christians matches
  those against Muslims by 2019.

- There are many more Christians in the country than Muslims so we look
  at per capita next.

``` r
Religious_Populations <- read_csv("C:/Users/brett/Desktop/Employment Stuff/Portfolio/Kaggle Data Sets/Hate Crime (complete)/Religions_US_Population.csv")
```

    ## Rows: 29 Columns: 6
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): Year, Jewish
    ## num (4): Catholic, Protestant, Islam, Atheist
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Per_Capita_Religion = data.frame(year = c(1991:2019))
Per_Capita_Religion$Islam = round(100000*Abraham_Total$Islam/Religious_Populations$Islam, digits = 2)
Per_Capita_Religion$Jewish = round(100000*Abraham_Total$Jewish/Religious_Populations$Jewish, digits = 2)
Per_Capita_Religion$Catholic = round(100000*Religious_Total$Catholic/Religious_Populations$Catholic, digits = 2)
Per_Capita_Religion$Protestant = round(100000*Religious_Total$Protestant/Religious_Populations$Protestant, digits = 2)
Per_Capita_Religion$Atheist = round(100000*Religious_Total$Atheist/Religious_Populations$Atheist, digits = 2)

Per_Capita_Religion_stacked <- cbind(Per_Capita_Religion[1], stack(Per_Capita_Religion[2:6]))
colnames(Per_Capita_Religion_stacked) = c("year", "nHate_Crimes", "bias")


ggplot(data = Per_Capita_Religion_stacked, mapping = aes(x = year, y = nHate_Crimes)) + 
  geom_line(mapping = aes(color = bias), size = 1) + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Hate Crimes Per 100,000 people")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

- Here we can see that hate crimes against Jews still excede that of
  Muslims (with exception of 2001).

- Sources: Source:
  <https://www.pewresearch.org/religion/wp-content/uploads/sites/7/2019/11/Detailed-tables-for-upload-11.11.19.pdf>

- Source:
  <https://commons.trincoll.edu/aris/files/2011/08/ARIS_Report_2008.pdf>

- Source: <https://news.gallup.com/poll/1690/Religion.aspx#1>

- Source:
  <https://www.macrotrends.net/countries/USA/united-states/population>

- All estimates, Jewish and Muslim are a little bit of modeling and
  estimating.

# Hate Crimes Against Racial Groups

``` r
Race_Total <- list()
for(i in 1:29){
  Race_Total[[i]] <- c(nrow(filter(HateCrimes, Anti_Black == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_White == "Y" & year == 1990 + i)), 
                       nrow(filter(HateCrimes, Anti_Asian == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Pac_Isl == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Native_Am == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Hispanic == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Arab == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Jewish == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Multiple_race == "Y" & year == 1990 + i)),
                       nrow(filter(HateCrimes, Anti_Other_Race == "Y" & year == 1990 + i)))
}
Race_Total <- t(data.frame(Race_Total))
rownames(Race_Total) <- c(1:29)
colnames(Race_Total) <- c("Black", "White", "Asian", "Pacific_Islander",
                          "Native_American", "Hispanic", "Arab", "Jewish", "Multiple", "Other")
Race_Total <- data.frame(Race_Total)
Race_Total$year <- c(1991:2019)
# stacking the dataframe getting ready for plotting. 
Race_Total_stacked <- cbind(Race_Total[11], stack(Race_Total[1:10]))
```

    ## Warning in data.frame(..., check.names = FALSE): row names were found from a short variable and have been discarded

``` r
colnames(Race_Total_stacked) <- c("year", "nHate_Crimes", "bias")

##  Plotting the totals for race
ggplot(data = Race_Total_stacked, mapping = aes(x = year, y = nHate_Crimes)) + 
  geom_line(mapping = aes(color = bias), size = 1) + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Hate Crimes Against Racial Groups")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

- There was a spike in hate crimes against Arab and “Other” groups in
  2001.

- This indicates that people were mistaking other races for Arab/Muslims
  or people were committing hate crimes against many other races after
  9/11.

- The 4 major groups are anti-black, anti-white, anti-hispanic and
  anti-jewish

## Per Capita Racial Hate Crimes

``` r
Race_Pop <- read_csv("C:/Users/brett/Desktop/Employment Stuff/Portfolio/Kaggle Data Sets/Hate Crime (complete)/Race_US_Population.csv")
```

    ## Rows: 29 Columns: 7
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (7): Year, Asian, Black, Hispanic, Native_American, Pacific_Islander, White
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Per_Capita_Race <- data.frame(year = c(1991:2019))
Per_Capita_Race$Asian <- round(100000*Race_Total$Asian/Race_Pop$Asian,digits = 2)
Per_Capita_Race$Black <- round(100000*Race_Total$Black/Race_Pop$Black, digits = 2)
Per_Capita_Race$Hispanic <- round(100000*Race_Total$Hispanic/Race_Pop$Hispanic, digits = 2)
Per_Capita_Race$Native_American <- round(100000*Race_Total$Native_American/Race_Pop$Native_American, digits = 2)
Per_Capita_Race$Pacific_Islander <- round(100000*Race_Total$Pacific_Islander/Race_Pop$Pacific_Islander, digits = 2)
Per_Capita_Race$White <- round(100000*Race_Total$White/Race_Pop$White, digits = 2)

Per_Capita_Race_stacked <- cbind(Per_Capita_Race[1],stack(Per_Capita_Race[2:7]))
colnames(Per_Capita_Race_stacked) <- c("year", "nHate_Crimes", "bias")

ggplot(data = Per_Capita_Race_stacked, mapping = aes(x = year, y = nHate_Crimes)) + 
  geom_line(mapping = aes(color = bias), size = 1) + 
  xlab("Year") + 
  ylab("Number of Hate Crimes") + 
  ggtitle("Hate Crimes Per 100000 People")
```

![](HateCrimeStats_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

- Note: racial populations are estimates using census data

- When we look at hate crimes per capita, we see the majority being
  against the black population.

- In the previous graph notice that there weren’t many hate crimes
  against Native Americans or Pacific Islanders. However, the low
  populations of these particular races yields a different result when
  we look at hate crimes per capita.

- Obviously, there are a lot of anti-white hate crimes, but since the
  white population is the majority in the US, hate crimes against White
  people are relatively small.

- 2018 is the last year the Cleveland baseball team used the name
  “Indians”, and Trump reversed veto in 2017 of the controversial
  Keystone XL pipeline which was to go through native land.

- 1992 LA race riots coming from the beating of Rodney King by the LA
  police department.
