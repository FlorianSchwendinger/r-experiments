GLM - Log-Binomial Start Values
================

``` r
library(lbreg)
data("Birth", "Caesarian", "Death", "Evans", "Heart", "PCS", package = "lbreg")
```

``` r
glm(lowbw ~ ., binomial("log"), data = Birth)
#> 
#> Call:  glm(formula = lowbw ~ ., family = binomial("log"), data = Birth)
#> 
#> Coefficients:
#> (Intercept)          alc          smo          soc  
#>     -3.7230       0.3278       0.4990       0.1533  
#> 
#> Degrees of Freedom: 899 Total (i.e. Null);  896 Residual
#> Null Deviance:       619.5 
#> Residual Deviance: 600.9     AIC: 608.9
```

``` r
glm(cbind(n1, n0) ~ ., binomial("log"), data = Caesarian)
#> 
#> Call:  glm(formula = cbind(n1, n0) ~ ., family = binomial("log"), data = Caesarian)
#> 
#> Coefficients:
#> (Intercept)         RISK        NPLAN        ANTIB  
#>      -1.977        1.295        0.545       -2.066  
#> 
#> Degrees of Freedom: 6 Total (i.e. Null);  3 Residual
#> Null Deviance:       83.49 
#> Residual Deviance: 6.308     AIC: 31.49
```

``` r
glm(death ~ ., binomial("log"), data = Death)
#> Error: no valid set of coefficients has been found: please supply starting values
```

``` r
glm(CDH ~ ., binomial("log"), data = Evans)
#> Error: no valid set of coefficients has been found: please supply starting values
```

``` r
glm(Heart ~ ., binomial("log"), data = Heart)
#> Error: no valid set of coefficients has been found: please supply starting values
```

Data preparation as shown in the **lbreg** package.

``` r
w <- PCS
w <- w[, -1]
w$race <- factor(w$race)
w$dpros <- factor(w$dpros)
w$dcaps <- factor(w$dcaps)
glm(tumor ~ ., binomial("log"), data = w)
#> Error: no valid set of coefficients has been found: please supply starting values
```

So for the examples from the **lbreg** package `glm` fails to find valid
start values in 4 out of 6 cases.
