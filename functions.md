Iteration
================
Rachel Tao
11/9/20

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.410205841 -0.000844396 -1.397690263 -0.693209842  1.601625161
    ##  [6]  0.882481380  1.627139062 -1.494797113  0.415098363 -0.480249486
    ## [11]  0.740070756  1.978953398 -1.020006262 -1.156269230 -0.285690935
    ## [16] -1.229516701  0.040300371  1.382937008 -1.014798762  1.279511546
    ## [21] -0.966329434 -0.146041939 -0.019808306  0.383648146 -0.337415091
    ## [26] -1.588647800  0.664564251  0.598091093  0.279487137  0.367613727

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

    ##  [1] -0.410205841 -0.000844396 -1.397690263 -0.693209842  1.601625161
    ##  [6]  0.882481380  1.627139062 -1.494797113  0.415098363 -0.480249486
    ## [11]  0.740070756  1.978953398 -1.020006262 -1.156269230 -0.285690935
    ## [16] -1.229516701  0.040300371  1.382937008 -1.014798762  1.279511546
    ## [21] -0.966329434 -0.146041939 -0.019808306  0.383648146 -0.337415091
    ## [26] -1.588647800  0.664564251  0.598091093  0.279487137  0.367613727

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
