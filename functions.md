Iteration
================
Rachel Tao
11/9/20

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.536028817  0.047611269 -1.150614359 -1.327800380 -0.244096361
    ##  [6] -0.974338889 -0.248669211  0.453399352  0.542832341 -0.530888375
    ## [11] -0.663280398 -1.921112603  1.454591016  1.740130925  0.608658156
    ## [16] -1.016247227  0.528309847 -0.118578496  0.904121172  2.713157511
    ## [21]  0.387544817 -0.810039379  0.842088716 -0.839429812  0.131355840
    ## [26]  1.048667935 -0.054529153 -0.005291176 -1.120570070  0.159045809

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numerric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.536028817  0.047611269 -1.150614359 -1.327800380 -0.244096361
    ##  [6] -0.974338889 -0.248669211  0.453399352  0.542832341 -0.530888375
    ## [11] -0.663280398 -1.921112603  1.454591016  1.740130925  0.608658156
    ## [16] -1.016247227  0.528309847 -0.118578496  0.904121172  2.713157511
    ## [21]  0.387544817 -0.810039379  0.842088716 -0.839429812  0.131355840
    ## [26]  1.048667935 -0.054529153 -0.005291176 -1.120570070  0.159045809

Try my function on some other things

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
z_scores("my name is rachel")
```

    ## Error in z_scores("my name is rachel"): Input must be numerric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numerric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numerric

## Multple outputs

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

Check that the function works.

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0551 0.982

## Mulitiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.59  3.10

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 3){
  
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
}

sim_mean_sd(100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.94  2.88
