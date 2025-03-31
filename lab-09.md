Lab 09B: Algorithmic Bias
================
Benjamin Egan
3/26/25

Here is the link to the assignment page:
<https://datascience4psych.github.io/DataScience4Psych/lab09.html>. This
includes the relevant information for the assignment alongside required
questions I needed to answer.

    ## Warning: package 'purrr' was built under R version 4.3.3

    ## Warning: package 'janitor' was built under R version 4.3.3

### The data

Cleaning up some of the names

``` r
compas <- read_csv("data/compas-scores-2-years.csv") %>% 
  clean_names() %>% 
  rename(decile_score = decile_score_12,
                         priors_count = priors_count_15)
```

    ## New names:
    ## Rows: 7214 Columns: 53
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (19): name, first, last, sex, age_cat, race, c_case_number, c_charge_de... dbl
    ## (19): id, age, juv_fel_count, decile_score...12, juv_misd_count, juv_ot... lgl
    ## (1): violent_recid dttm (2): c_jail_in, c_jail_out date (12):
    ## compas_screening_date, dob, c_offense_date, c_arrest_date, r_offe...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `decile_score` -> `decile_score...12`
    ## • `priors_count` -> `priors_count...15`
    ## • `decile_score` -> `decile_score...40`
    ## • `priors_count` -> `priors_count...49`

``` r
view(compas)

nrow(compas)
```

    ## [1] 7214

``` r
ncol(compas)
```

    ## [1] 53

## Part 1 - Exploring the Data

Almost there! Keep building on your work and follow the same structure
for any remaining exercises. Each exercise builds on the last, so take
your time and make sure your code is working as expected.

1.  Each row of the dataset represents an individual who arrested in
    Broward County, Florida. There are 7,214 people in the dataset, and
    53 different variables

2.  There should be 7,214 unique people in the dataset. After a quick
    glance through the dataset using view(), I haven’t seen a repeated
    ID number or person’s name.

### Visualizing demographic data

#### The distribution of defendants by race

``` r
Race_graph <- compas %>%
  ggplot(aes(
    x = race
  ))+
  geom_histogram(stat = "count", fill = "orange", alpha = .7)+
  theme_bw()+
  geom_text(
    stat = "count",
    aes(x = race,
    y = ..count..,
    label = ..count..),
    size = 4, 
    color = "black",
    vjust = -0.2) +
  labs(
    x = "Race",
    y = NULL,
    title = "Distribution of Defendents by Race",
    subtitle = "People arrested in Broward County, Florida"
  )

Race_graph
```

![](lab-09_files/figure-gfm/graph%20race-1.png)<!-- -->

#### The distribution of defendants by sex

``` r
Sex_graph <- compas %>%
  ggplot(aes(
    x = sex
  ))+
  geom_histogram(stat = "count", fill = c("pink", "steelblue2"))+
  theme_bw()+
  geom_text(
    stat = "count",
    aes(x = sex,
    y = (..count../2),
    label = ..count..),
    size = 7, 
    color = "black") +
  labs(
    x = "Sex",
    y = NULL,
    title = "Distribution of Defendents by Sex",
    subtitle = "People arrested in Broward County, Florida"
  )

Sex_graph
```

![](lab-09_files/figure-gfm/graph%20sex-1.png)<!-- -->

#### The distribution of defendants by age

``` r
Age_graph <- compas %>%
  ggplot(aes(
    x = age_cat
  ))+
  geom_histogram(stat = "count", fill = "gray", alpha = .7)+
  theme_bw()+
  geom_text(
    stat = "count",
    aes(x = age_cat,
    y = ..count..,
    label = ..count..),
    size = 5, 
    color = "black",
    vjust = -0.2) +
  labs(
    x = "Age",
    y = NULL,
    title = "Distribution of Defendents by Age",
    subtitle = "People arrested in Broward County, Florida"
  )

Age_graph
```

![](lab-09_files/figure-gfm/graph%20age-1.png)<!-- -->

#### Optional - Graph all on the same page

Here I’m using plot_grid() from the cowplot package as one example of
how to do this. There are clear issues (such as size of text), but it
will get the job done.

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
plot_grid(Race_graph,Sex_graph, Age_graph)
```

![](lab-09_files/figure-gfm/all%20graphs%20together-1.png)<!-- -->

I turned to chatGPT to see if it had any suggestions on fixing this
issue. It initially recommended adding the arguments ncol = 1, align =
“v” to the plot_grid(). This ended up creating this… unique outcome.

<figure>
<img src="chat_suggestion.png"
alt="This reminds me of Stretch Armstrong" />
<figcaption aria-hidden="true">This reminds me of Stretch
Armstrong</figcaption>
</figure>

I told it I wanted the Axes to be fixed, and it gave me a great
suggestion.

I can take the original dataset and create a subset of the data that
just includes race, sex, and age. From there, I can use the wide-to-long
transformation, making three separate rows for each person (one for
race, one for sex, one for age). Category is the designation of the IV,
and Value is what they identify as.

``` r
compas_long <- compas %>%
  pivot_longer(cols = c(race, sex, age_cat), names_to = "Category", values_to = "Value")
view(compas_long)
```

Now, I can use facet_wrap() using category. This way, I can create three
separate graphs side by side. I copied its suggestion and then made
edits. These were for style and for aesthetic.

``` r
compas_long %>%
ggplot(aes(
  x = Value)) +
  geom_bar(aes(fill = Category), alpha = 0.7) +
  geom_text(
    stat = "count",
    aes(y = ..count.., 
        label = ..count..),
    vjust = -0.2,
    size = 3
  ) +
  theme_bw() +
  facet_wrap(~Category, labeller = as_labeller(c(`age_cat` = "Age", `race` = "Race", `sex` = "Sex")), scales = "free_x")+ # scales = "free_x" keeps different x-scales while aligning axes properly
  labs(
    x = NULL,
    y = "Count",
    title = "Distribution of Defendants",
    subtitle = "People arrested in Broward County, Florida"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=.5)
        )
```

![](lab-09_files/figure-gfm/optional%20demographic%202-1.png)<!-- -->

The downside is that I can’t change the colors of each of the graphs to
what I had them.

#### Visualization of the COMPAS risk scores

``` r
compas %>%
  ggplot(aes(
    x = decile_score
  ))+
  theme_bw()+
  geom_histogram(stat = "count", fill = "gray")+
    geom_text(
    stat = "count",
    aes(x = decile_score,
    y = ..count..,
    label = ..count..),
    size = 3.5, 
    color = "black",
    vjust = -0.2) +
  labs(
    x = "COMPAS risk scores",
    y = NULL,
    title = "Distribution of Defendents by COMPAS risk",
    subtitle = "People arrested in Broward County, Florida"
  )
```

![](lab-09_files/figure-gfm/COMPAS%20scores%20visual-1.png)<!-- -->

## Part 2 - Risk scores and recidivism

``` r
compas %>%
  ggplot(aes(
    x = two_year_recid,
    y = decile_score
  ))+
  geom_smooth(formula = y~x, se = FALSE, color = "black")+
   labs(
    x = "Whether the defendant recidivated within two years (0 = no, 1 = yes)",
    y = "COMPAS risk score",
    title = "The relationship between risk scores and actual recidivism"
  )
```

    ## `geom_smooth()` using method = 'gam'

![](lab-09_files/figure-gfm/risk%20score%20to%20recidivism%20visual-1.png)<!-- -->

Assuming I did this correctly, it appears that recidivism, on average,
increases COMPAS risk score.

#### Alternate plot

``` r
compas %>%
  ggplot(aes(
    x = decile_score
  ))+
  geom_histogram()+
  facet_wrap(~ two_year_recid, labeller = as_labeller(c(`0` = "Recidivated within two years", `1` = "Did not recidivate within two years")))+
  labs(
    x = "COMPAS risk score from 1-10 (higher = greater risk)",
    y = NULL,
    title = "The relationship between risk scores and actual recidivism"
  )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-09_files/figure-gfm/alt%20plot?-1.png)<!-- -->

This plot is more showing the breakdown of COMPAS scores based on
recidivism.

### COMPAS Accuracy

``` r
compas1 <- compas %>%
  mutate(compas_accurate = case_when(
    decile_score >= 7 & two_year_recid == 1 ~ 1,
    decile_score > 7 & two_year_recid == 0 ~ 0,
    decile_score <= 4 & two_year_recid == 0 ~ 1,
    decile_score > 4 & two_year_recid == 1 ~ 0
  ))



view(compas)
```
