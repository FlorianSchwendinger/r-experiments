GLM - Log-Binomial Start Values
================

## 0.1 Build `3 x 2` tables

``` r
ofi <- "data/crosstables_2x3.rds"
if (file.exists(ofi)) {
    df <- readRDS(ofi)
} else {
    number_of_distinct_tables <- function(n) factorial(n - 1) / (factorial(6 - 1) * factorial(n - 6))
    number_of_distinct_tables(50)

    ##
    ## write my own version
    ##
    nobs <- 50L
    seqi <- 1:45

    xnrow <- number_of_distinct_tables(50)
    xnrow / 1000
    df <- as.data.frame(matrix(0, nrow = xnrow, ncol = 6))
    iter <- 0L

    for (i1 in seqi) {
      for (i2 in seqi) {
        for (i3 in seqi) {
          for (i4 in seqi) {
            for (i5 in seqi) {
              for (i6 in seqi) {
                if ( (i1 + i2 + i3 + i4 + i5 + i6) == nobs ) {
                  iter <- iter + 1L
                  cat(100 * iter / xnrow, "%  ", iter, "\n")
                  df[iter, ] <- c(i1, i2, i3, i4, i5, i6)
                }
              }
            }
          }
        }
      }
    }
    saveRDS(df, ofi)
}

cat("Number of examples:", format(NROW(df), big.mark = ","), "\n")
#> Number of examples: 1,906,884
head(df)
#>   V1 V2 V3 V4 V5 V6
#> 1  1  1  1  1  1 45
#> 2  1  1  1  1  2 44
#> 3  1  1  1  1  3 43
#> 4  1  1  1  1  4 42
#> 5  1  1  1  1  5 41
#> 6  1  1  1  1  6 40
```

## 0.2 Functions

``` r
build_data <- function(v) {
    v <- unname(unlist(v))
    ## we assume the following order
    ##          -1   0   1
    ## y == 0:   1   2   3
    ## y == 1:   4   5   6
    x <- c(rep.int(-1, v[1]), rep.int(0, v[2]), rep.int(1, v[3]),
           rep.int(-1, v[4]), rep.int(0, v[5]), rep.int(1, v[6]))
    y <- c(rep.int(0, sum(v[1:3])), rep.int(1, sum(v[4:6])))
    cbind(data.frame(y = y), x)
}

build_data(df[1L, ])
#>    y  x
#> 1  0 -1
#> 2  0  0
#> 3  0  1
#> 4  1 -1
#> 5  1  0
#> 6  1  1
#> 7  1  1
#> 8  1  1
#> 9  1  1
#> 10 1  1
#> 11 1  1
#> 12 1  1
#> 13 1  1
#> 14 1  1
#> 15 1  1
#> 16 1  1
#> 17 1  1
#> 18 1  1
#> 19 1  1
#> 20 1  1
#> 21 1  1
#> 22 1  1
#> 23 1  1
#> 24 1  1
#> 25 1  1
#> 26 1  1
#> 27 1  1
#> 28 1  1
#> 29 1  1
#> 30 1  1
#> 31 1  1
#> 32 1  1
#> 33 1  1
#> 34 1  1
#> 35 1  1
#> 36 1  1
#> 37 1  1
#> 38 1  1
#> 39 1  1
#> 40 1  1
#> 41 1  1
#> 42 1  1
#> 43 1  1
#> 44 1  1
#> 45 1  1
#> 46 1  1
#> 47 1  1
#> 48 1  1
#> 49 1  1
#> 50 1  1
```

## 0.3 Run experiment

Take a random sample so it doesn’t take that long.

``` r
set.seed(732480991L)
size <- 300000L
i <- sample.int(NROW(df), size = size)
df <- df[i, ]
glm_start <- c(-1, 0)

cache_file <- "data/iterations.rds"
if (file.exists(cache_file)) {
    msg <- readRDS(cache_file)
} else {
    glm_cntrl <- list(maxit = 1000, trace = FALSE)
    results <- vector("list", NROW(df))
    for (i in seq_len(NROW(df))) {
        if ((i %% 100) == 0L) {
            print(100 * i / NROW(df))
        }
        dat <- build_data(df[i, ])

        result_default <- tryCatch(
            suppressWarnings(glm(y ~ ., family = binomial("log"), control = glm_cntrl, data = dat)),
            error = function(e) as.character(e)
        )

        result_start <- tryCatch(
            suppressWarnings(glm(y ~ ., family = binomial("log"), control = glm_cntrl, data = dat, start = glm_start)),
            error = function(e) as.character(e)
        )

        iter_default <- if (inherits(result_default, "glm")) result_default[["iter"]] else NA_integer_
        iter_start <- if (inherits(result_start, "glm")) result_start[["iter"]] else NA_integer_
        results[[i]] <- list(iter_default = iter_default, iter_start = iter_start)
    }
    saveRDS(results, cache_file)
}
```

``` r
df <- as.data.frame(apply(do.call(rbind, results), 2, as.numeric))
#> Error in do.call(rbind, results): object 'results' not found
str(df)
#> 'data.frame':    300000 obs. of  6 variables:
#>  $ V1: num  2 20 1 6 23 9 1 5 6 1 ...
#>  $ V2: num  19 1 5 18 5 15 7 14 6 4 ...
#>  $ V3: num  3 13 2 2 11 7 1 1 4 8 ...
#>  $ V4: num  6 3 23 2 3 17 7 3 11 22 ...
#>  $ V5: num  13 10 14 13 4 1 27 12 1 7 ...
#>  $ V6: num  7 3 5 9 4 1 7 15 22 8 ...
```

``` r
table(df[, 1L] == df[, 2L], useNA = "always")
#> 
#>  FALSE   TRUE   <NA> 
#> 284081  15919      0
```

``` r
d <- df[df[, 1L] != df[, 2L], ]
prop.table(table(d[, "iter_default"] > d[, "iter_start"]))
#> Error in `[.data.frame`(d, , "iter_default"): undefined columns selected
```

``` r
boxplot(d[, "iter_default"] - d[, "iter_start"], horizontal = TRUE, outline = FALSE)
#> Error in `[.data.frame`(d, , "iter_default"): undefined columns selected
```
