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
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.91720 -0.71971 -0.08670 -0.03138  0.70242  1.97632

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
    ##  [1] 4.086444 1.528171 3.779038 2.177435 3.077578 2.074908 2.586317 3.058177
    ##  [9] 1.273287 1.706133 2.984296 3.416685 4.513013 2.634827 3.405736 2.911935
    ## [17] 4.734960 3.386714 1.012668 2.955350
    ## 
    ## $b
    ##  [1] -5.50017419 -6.69916807  3.95603498 -0.74899114 -0.14654352 -4.86900184
    ##  [7]  1.10873940 -4.77116133  2.95527528  5.53676853 -0.82057484 -2.16666477
    ## [13] -1.51622130 -9.42896524 -2.52047301 -1.49137296  1.56009146  0.73130515
    ## [19]  2.87692104 -3.90195787 -4.26379913 -0.08972009 -3.10701350 -3.95943554
    ## [25] -5.41099903  4.53321270 -7.28004690 -3.72223348 -2.51442266 10.73667560
    ## 
    ## $c
    ##  [1] 10.142546  9.850476  9.814081 10.074789 10.048431  9.969315  9.961399
    ##  [8]  9.876146 10.070420  9.774827  9.791384  9.669463  9.810279 10.094834
    ## [15] 10.050076  9.928832 10.065908  9.901789 10.115963 10.259671 10.309441
    ## [22] 10.105935 10.333394 10.084616 10.046372  9.602976 10.065048  9.839092
    ## [29]  9.985585  9.874342 10.470463  9.922324 10.148736  9.814762  9.961240
    ## [36]  9.850737 10.100469  9.996779 10.137032 10.234933
    ## 
    ## $d
    ##  [1] -2.869648 -1.916891 -2.626961 -3.494850 -3.700200 -3.007352 -3.352855
    ##  [8] -4.790621 -5.459273 -2.514562 -4.513850 -2.006256 -5.236880 -2.873597
    ## [15] -3.291183 -2.615648 -5.366282 -2.714546 -3.598621 -4.921249

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
    ## 1  2.87  1.02

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.36  4.30

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.181

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.54  1.12

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Let’s try a map\!

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.87  1.02
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.36  4.30
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.181
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.54  1.12

what if you want a different function…?

``` r
output = map(list_norm, median)
```
