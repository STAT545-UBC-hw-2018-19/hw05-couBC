hw04\_couBC
================
CouBC
2018-10-15

As always, need to load gapminder and tidyverse

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(forcats))
```

*Part 1 of the assignment - Factor management*

Elaboration for the gapminder data set: First, filter the Gapminder data to remove observations associated with the continent of Oceania. In order to get a comparison of the structure before tinkering around with gapminder, I will look at the structure of gapminder and the factorness of gapminder$continent

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
str(gapminder$continent)
```

    ##  Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nlevels(gapminder$continent)
```

    ## [1] 5

``` r
class(gapminder$continent)
```

    ## [1] "factor"

``` r
forcats::fct_count(gapminder$continent)
```

    ## # A tibble: 5 x 2
    ##   f            n
    ##   <fct>    <int>
    ## 1 Africa     624
    ## 2 Americas   300
    ## 3 Asia       396
    ## 4 Europe     360
    ## 5 Oceania     24

The other way to do this is using dplyr:

``` r
gapminder %>% 
  count(continent)
```

    ## # A tibble: 5 x 2
    ##   continent     n
    ##   <fct>     <int>
    ## 1 Africa      624
    ## 2 Americas    300
    ## 3 Asia        396
    ## 4 Europe      360
    ## 5 Oceania      24

``` r
no_oceania <- gapminder %>%
  filter(continent!="Oceania")

str(no_oceania)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
#after I filtered out Oceania, there are only 1680 rows compared to 1704 with Oceania not filtered, however the structure function tells me that continent is a factor with 5 levels as before.
```

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
#I still have Oceania as a level using the method above
```

Because I still have Oceania as a level using the filter method, I will now try using the forcats\_drop way.

``` r
no_oceania$continent %>% 
  fct_drop() %>% 
  levels()
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
no_oceania
```

    ## # A tibble: 1,680 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # ... with 1,670 more rows

``` r
#Oceania is now removed as a factor level. After dropping Oceania, I have 1,680 rows whereas before I had 1704, so that's a sanity check to see that it worked.
```

Now I will re-order the continents based on aggregate population of each continent from smallest to largest

``` r
fct_reorder(gapminder$continent, gapminder$pop, max) %>% 
  levels() %>% 
  head()
```

    ## [1] "Oceania"  "Europe"   "Africa"   "Americas" "Asia"

Backwards re-order, from largest population to smallest:

``` r
fct_reorder(gapminder$continent, gapminder$pop, max, .desc = TRUE) %>% 
  levels() %>% 
  head() 
```

    ## [1] "Asia"     "Americas" "Africa"   "Europe"   "Oceania"

*Part 2* Now to create a plot. I want to look at the population of the three most populated continents

``` r
gap_compare_2007 <- gapminder %>% 
  filter(year == "2007", continent %in% c("Asia", "Africa", "Americas")) 


ggplot(gap_compare_2007, aes(continent, pop)) + 
  geom_point()
```

![](hw05_couBC_files/figure-markdown_github/unnamed-chunk-10-1.png)
