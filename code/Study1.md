Study 1
================

  - [Create Scores](#create-scores)
  - [Reliabilities](#reliabilities)
  - [ANOVA](#anova)

``` r
s1raw <- read_csv("../data/study1_RMER.csv") %>% 
  dplyr::select(id,
         Age,
         Gender,
         Condition, 
         MusicInfo4_1,
         contains("RRQ"),
         contains("POMS"),
         contains("MARS")) %>% 
  dplyr::mutate(Condition = factor(Condition, 
                            levels = c(1, 2, 3),
                            labels = c("Control", "Experimenter Selected", "Self Selected")),
         Gender = as.factor(Gender),
         id = as.factor(id))
```

# Create Scores

``` r
s1dat <- s1raw %>% 
  mutate(Baseline = rowSums(dplyr::select(., POMSDSF1_1:POMSDSF8_1)),
         PostInduction = rowSums(dplyr::select(., POMSDSF1_2:POMSDSF8_2)),
         PostListening = rowSums(dplyr::select(., POMSDSF1_3:POMSDSF8_3)),
         MARS_Pos = rowMeans(dplyr::select(., MARS1_1,
                                    MARS2_1,
                                    MARS6_1,
                                    MARS7_1, 
                                    MARS8_1,
                                    MARS9_1,
                                    MARS11_1,
                                    MARS12_1,
                                    MARS14_1,
                                    MARS15_1)),
         MARS_Neg = rowMeans(dplyr::select(., MARS3_1,
                                    MARS4_1,
                                    MARS5_1,
                                    MARS10_1,
                                    MARS13_1)),
         RRQ6_1 = 6 - .$RRQ6_1,
         RRQ9_1 = 6 - .$RRQ9_1,
         RRQ10_1 = 6 - .$RRQ10_1) 

s1dat <- s1dat %>%  mutate(Rumination = rowMeans(dplyr::select(., RRQ1_1:RRQ12_1)))
s1dat <- s1dat %>% mutate(RumSplit = dicho(.$Rumination, val.labels = c("Low Rumiantion", "High Rumination")))

s1dat %>% 
  dplyr::select(Baseline, PostInduction, PostListening, Rumination, MARS_Pos, MARS_Neg) %>%
  skimr::skim() %>% 
  skimr::kable()
```

    ## Skim summary statistics  
    ##  n obs: 128    
    ##  n variables: 6    
    ## 
    ## Variable type: numeric
    ## 
    ##    variable       missing    complete     n     mean     sd      p0     p25     p50     p75     p100      hist   
    ## ---------------  ---------  ----------  -----  ------  ------  ------  ------  ------  ------  ------  ----------
    ##    Baseline          0         128       128    3.99    5.1      0       0       2       5       26     ▇▂▁▁▁▁▁▁ 
    ##    MARS_Neg          0         128       128    2.57    0.65    1.2     2.2     2.6      3      4.6     ▃▅▆▇▆▃▁▁ 
    ##    MARS_Pos          0         128       128    4.88    0.73    3.1     4.4     4.9     5.4      7      ▁▃▇▇▇▅▁▁ 
    ##  PostInduction       0         128       128    9.48    6.7      0       4       8       13      27     ▃▇▅▃▂▁▂▁ 
    ##  PostListening       0         128       128    5.4     6.12     0       1       4       8       30     ▇▃▂▁▁▁▁▁ 
    ##   Rumination         0         128       128    3.71    0.62    2.17    3.33    3.83    4.08     5      ▂▁▂▆▇▆▂▂

# Reliabilities

``` r
# Baseline ALpha 
psych::alpha(dplyr::select(s1dat, POMSDSF1_1:POMSDSF8_1 ))$total$raw_alpha
```

    ## [1] 0.9242407

``` r
# Post Induction Alpha
psych::alpha(dplyr::select(s1dat, POMSDSF1_2:POMSDSF8_2 ))$total$raw_alpha
```

    ## [1] 0.9288304

``` r
# Post Listening Alpha 
psych::alpha(dplyr::select(s1dat, POMSDSF1_3:POMSDSF8_3 ))$total$raw_alpha
```

    ## [1] 0.9435892

``` r
# Rumination Alpha 
psych::alpha(dplyr::select(s1dat, RRQ1_1:RRQ12_1 ))$total$raw_alpha
```

    ## [1] 0.8891114

``` r
# MARS Positive Alpha 
psych::alpha(dplyr::select(s1dat, MARS1_1,
                    MARS2_1,
                    MARS6_1,
                    MARS7_1, 
                    MARS8_1,
                    MARS9_1,
                    MARS11_1,
                    MARS12_1,
                    MARS14_1,
                    MARS15_1))$total$raw_alpha
```

    ## [1] 0.8405795

``` r
#MARS Negative Alpha 
psych::alpha(dplyr::select(s1dat, MARS3_1,
                    MARS4_1,
                    MARS5_1,
                    MARS10_1,
                    MARS13_1))$total$raw_alpha
```

    ## [1] 0.4447691

# ANOVA

``` r
s1long <-  gather(s1dat,
                  key = Timepoint, 
                  value = Sadness,
                  Baseline, PostInduction, PostListening)


ANOVAs1 <- afex::aov_ez(id = "id",
                        data = s1long,
                        dv = "Sadness",
                        between = c("Condition", "RumSplit"),
                        within = "Timepoint",
                        type = 2)
```

    ## Contrasts set to contr.sum for the following variables: Condition, RumSplit

``` r
knitr::kable(anova(ANOVAs1))
```

|                              |   num Df |   den Df |      MSE |          F |       ges |   Pr(\>F) |
| ---------------------------- | -------: | -------: | -------: | ---------: | --------: | --------: |
| Condition                    | 2.000000 | 122.0000 | 69.51726 |  0.8047651 | 0.0088720 | 0.4495524 |
| RumSplit                     | 1.000000 | 122.0000 | 69.51726 |  8.5566376 | 0.0454262 | 0.0041061 |
| Condition:RumSplit           | 2.000000 | 122.0000 | 69.51726 |  1.9444008 | 0.0211698 | 0.1474812 |
| Timepoint                    | 1.894707 | 231.1543 | 17.38482 | 63.2588852 | 0.1428814 | 0.0000000 |
| Condition:Timepoint          | 3.789414 | 231.1543 | 17.38482 |  2.7672314 | 0.0143748 | 0.0307199 |
| RumSplit:Timepoint           | 1.894707 | 231.1543 | 17.38482 |  0.0128512 | 0.0000339 | 0.9844103 |
| Condition:RumSplit:Timepoint | 3.789414 | 231.1543 | 17.38482 |  0.8790408 | 0.0046115 | 0.4725968 |

``` r
afex_plot(ANOVAs1,
          x = "Timepoint",
          trace = "Condition",
          error = "within",
          mapping = "color",
          data_plot = FALSE) +
  scale_x_discrete(labels = c("Baseline", "Post Induction", "Post Listening")) +
  scale_color_discrete(labels = c("No Music \nControl", "Experimenter Selected \nMusic", "Participant Selected \nMusic")) +
  ggplot2::ylim(0, 15) +
  ggplot2::theme_classic(base_size = 12, 
                         base_family = "Times New Roman") +
  ggplot2::theme(legend.title = element_blank()) 
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## Warning: Panel(s) show a mixed within-between-design.
    ## Error bars do not allow comparisons across all means.
    ## Suppress error bars with: error = "none"

![](Study1_files/figure-gfm/ANOVA-1.png)<!-- -->

``` r
summary(emmeans(ANOVAs1, pairwise ~  Timepoint))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $emmeans
    ##  Timepoint     emmean    SE  df lower.CL upper.CL
    ##  Baseline        3.96 0.519 242     2.94     4.98
    ##  PostInduction   9.53 0.519 242     8.50    10.55
    ##  PostListening   5.39 0.519 242     4.37     6.41
    ## 
    ## Results are averaged over the levels of: Condition, RumSplit 
    ## Warning: EMMs are biased unless design is perfectly balanced 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                      estimate    SE  df t.ratio p.value
    ##  Baseline - PostInduction         -5.57 0.516 244 -10.790 <.0001 
    ##  Baseline - PostListening         -1.43 0.516 244  -2.770 0.0165 
    ##  PostInduction - PostListening     4.14 0.516 244   8.020 <.0001 
    ## 
    ## Results are averaged over the levels of: Condition, RumSplit 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
s1time <- emmeans(ANOVAs1, pairwise ~ Timepoint | Condition)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
contrast(s1time,
         interaction = "pairwise")$emmeans
```

    ## Condition = Control:
    ##  Timepoint_pairwise            estimate    SE  df t.ratio p.value
    ##  Baseline - PostInduction        -4.847 0.905 244 -5.354  <.0001 
    ##  Baseline - PostListening        -0.308 0.905 244 -0.340  0.7341 
    ##  PostInduction - PostListening    4.539 0.905 244  5.014  <.0001 
    ## 
    ## Condition = Experimenter Selected:
    ##  Timepoint_pairwise            estimate    SE  df t.ratio p.value
    ##  Baseline - PostInduction        -6.149 0.887 244 -6.932  <.0001 
    ##  Baseline - PostListening        -3.559 0.887 244 -4.012  0.0001 
    ##  PostInduction - PostListening    2.590 0.887 244  2.920  0.0038 
    ## 
    ## Condition = Self Selected:
    ##  Timepoint_pairwise            estimate    SE  df t.ratio p.value
    ##  Baseline - PostInduction        -5.713 0.890 244 -6.422  <.0001 
    ##  Baseline - PostListening        -0.423 0.890 244 -0.476  0.6346 
    ##  PostInduction - PostListening    5.289 0.890 244  5.946  <.0001 
    ## 
    ## Results are averaged over the levels of: RumSplit

\`
