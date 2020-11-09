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
    ## -1.9790 -0.8330 -0.1156 -0.0575  0.6043  2.8114

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
    ##  [1] 2.288525 2.454718 1.397338 2.720437 2.826921 3.908812 2.726753 3.725895
    ##  [9] 2.658455 2.496767 2.781593 4.108558 2.615187 3.185833 2.289541 2.607592
    ## [17] 4.346674 2.445425 3.307075 2.804193
    ## 
    ## $b
    ##  [1] -4.16836408 -1.35089592  3.89158028 -1.22481207 -6.68187193 -1.04526838
    ##  [7] -9.79230222 -8.44983679  4.71432838 -1.97205062 -7.53964200 -0.44819285
    ## [13] -5.96811530 -6.82729481 -2.02923303 -6.27627223 -1.18069227 -0.81990174
    ## [19]  0.01860002 -4.28991154  1.98153616  5.46602814 -0.13937413 -3.75659666
    ## [25]  5.90729636  8.48488178  1.56882463 -4.02333498  4.89461466 -1.68104291
    ## 
    ## $c
    ##  [1] 10.004968 10.187877  9.640623  9.753780  9.893978  9.795358  9.946832
    ##  [8]  9.780502 10.003212 10.336247  9.695099 10.130305 10.018165  9.914823
    ## [15] 10.430809  9.853228 10.027836  9.824017  9.871042  9.768973 10.134622
    ## [22]  9.826067  9.964319 10.014661 10.048006  9.632965  9.978052 10.158311
    ## [29] 10.250658 10.086152  9.816859 10.150579 10.130418  9.943903 10.008440
    ## [36] 10.030076 10.413786  9.812388  9.798669  9.987650
    ## 
    ## $d
    ##  [1] -5.085848 -3.574486 -3.148480 -3.752340 -1.717909 -3.151781 -1.988631
    ##  [8] -2.164733 -3.965120 -1.594255 -4.751425 -2.985095 -3.390924 -4.233490
    ## [15] -4.707361 -1.386064 -4.801567 -2.180420 -2.552695 -3.232594

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
    ## 1  2.88 0.703

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.42  4.61

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.193

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.15

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
    ## 1  2.88 0.703
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.42  4.61
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.193
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.15

what if you want a different function…?

``` r
output = map(list_norm, median)
```

``` r
output = map_dbl(list_norm, median, .id = "input")
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## list columns

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.288525 2.454718 1.397338 2.720437 2.826921 3.908812 2.726753 3.725895
    ##  [9] 2.658455 2.496767 2.781593 4.108558 2.615187 3.185833 2.289541 2.607592
    ## [17] 4.346674 2.445425 3.307075 2.804193
    ## 
    ## $b
    ##  [1] -4.16836408 -1.35089592  3.89158028 -1.22481207 -6.68187193 -1.04526838
    ##  [7] -9.79230222 -8.44983679  4.71432838 -1.97205062 -7.53964200 -0.44819285
    ## [13] -5.96811530 -6.82729481 -2.02923303 -6.27627223 -1.18069227 -0.81990174
    ## [19]  0.01860002 -4.28991154  1.98153616  5.46602814 -0.13937413 -3.75659666
    ## [25]  5.90729636  8.48488178  1.56882463 -4.02333498  4.89461466 -1.68104291
    ## 
    ## $c
    ##  [1] 10.004968 10.187877  9.640623  9.753780  9.893978  9.795358  9.946832
    ##  [8]  9.780502 10.003212 10.336247  9.695099 10.130305 10.018165  9.914823
    ## [15] 10.430809  9.853228 10.027836  9.824017  9.871042  9.768973 10.134622
    ## [22]  9.826067  9.964319 10.014661 10.048006  9.632965  9.978052 10.158311
    ## [29] 10.250658 10.086152  9.816859 10.150579 10.130418  9.943903 10.008440
    ## [36] 10.030076 10.413786  9.812388  9.798669  9.987650
    ## 
    ## $d
    ##  [1] -5.085848 -3.574486 -3.148480 -3.752340 -1.717909 -3.151781 -1.988631
    ##  [8] -2.164733 -3.965120 -1.594255 -4.751425 -2.985095 -3.390924 -4.233490
    ## [15] -4.707361 -1.386064 -4.801567 -2.180420 -2.552695 -3.232594

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.88 0.703

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.42  4.61

``` r
mean_and_sd(listcol_df$samp[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.193

``` r
mean_and_sd(listcol_df$samp[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.15

can i just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.88 0.703
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.42  4.61
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.193
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.15

So, can I add a list column??

``` r
listcol_df  =
  listcol_df%>% 
    mutate(
      summary = map(samp, mean_and_sd),
      medians = map_dbl(samp, median)
  )
```
