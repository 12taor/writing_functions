Iteration
================
Rachel Tao
11/9/20

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -1.8163 -0.4621  0.1693  0.1266  0.8739  1.8934

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

## ‘for’ loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 0.7718587 2.9762508 3.9177296 2.7323022 3.2821141 3.6100103 2.5981488
    ##  [8] 2.4166121 2.3914935 3.3966724 2.7042129 1.9988317 2.5019260 4.3587291
    ## [15] 4.4024742 1.0888081 3.7547553 2.9163253 2.0016405 3.3967266
    ## 
    ## $b
    ##  [1] -3.015251558  4.263208335  7.254870301 -0.494625984 -4.432118018
    ##  [6] -2.582674359  3.690085896 -4.971527358 -2.771485927  0.005861178
    ## [11] -0.648428320  1.843611636 -7.653807419 -2.784501792  5.223477517
    ## [16]  0.093616381 -3.931190028  9.380561744  3.416517771 -5.620385936
    ## [21]  1.628123443  1.540186582 -3.468353370 -4.486241300  4.197387673
    ## [26]  3.241123550  3.939513121  0.785306631  1.118801694  2.490864200
    ## 
    ## $c
    ##  [1]  9.888000  9.785599 10.054363 10.000546 10.055919 10.145772  9.953032
    ##  [8]  9.865711 10.290776  9.664470  9.984723 10.000405 10.383245  9.910657
    ## [15]  9.977481 10.339774 10.060792 10.082068  9.966944 10.112809 10.026333
    ## [22] 10.233120  9.704976 10.058145 10.351591 10.027247  9.631948 10.115563
    ## [29]  9.958828  9.919137 10.080749  9.916164 10.032739 10.373950  9.988363
    ## [36]  9.569751 10.119410 10.150198 10.249675  9.897699
    ## 
    ## $d
    ##  [1] -2.487786 -3.004016 -3.998656 -3.299547 -2.101327 -2.234994 -1.539748
    ##  [8] -3.932100 -3.788253 -2.204349 -2.661978 -2.853134 -2.677037 -3.561437
    ## [15] -5.270242 -2.502796 -2.293365 -2.960221 -2.213674 -4.241557

Pause and get my old function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numerric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )

}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86 0.960

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.242  4.08

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.193

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.99 0.907

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```
