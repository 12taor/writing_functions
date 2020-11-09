Functions
================
Rachel Tao
11/9/20

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.070891286 -1.350922284 -1.973609695  1.296560902  0.433113118
    ##  [6]  0.664001117 -1.423624179 -1.225319598  0.082833797  0.005877268
    ## [11]  0.331583089 -0.003915601  0.220346741  0.971313772  0.767256751
    ## [16]  0.417594479  0.390676186  1.922345262 -1.676794476  0.877852179
    ## [21]  1.474481783  1.069724939  0.863160453 -0.292663426 -0.950212644
    ## [26] -0.808034446 -0.887798516  0.360342965 -0.506676005 -1.120385216

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

    ##  [1]  0.070891286 -1.350922284 -1.973609695  1.296560902  0.433113118
    ##  [6]  0.664001117 -1.423624179 -1.225319598  0.082833797  0.005877268
    ## [11]  0.331583089 -0.003915601  0.220346741  0.971313772  0.767256751
    ## [16]  0.417594479  0.390676186  1.922345262 -1.676794476  0.877852179
    ## [21]  1.474481783  1.069724939  0.863160453 -0.292663426 -0.950212644
    ## [26] -0.808034446 -0.887798516  0.360342965 -0.506676005 -1.120385216

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
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.107  1.01

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
    ## 1  4.31  2.97

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
    ## 1  2.42  2.96

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews…

Let’s turn that code into a function

``` r
dynamite_html = read_html(url)
  
read_page_reviews = function(url) {
  html = read_html(url)
  
  review_titles = 
     html %>%
     html_nodes(".a-text-bold span") %>%
     html_text()
   
  review_stars = 
     html %>%
     html_nodes("#cm_cr-review_list .review-rating") %>%
     html_text() %>%
     str_extract("^\\d") %>%
     as.numeric()
   
  review_text = 
     html %>%
     html_nodes(".review-text-content span") %>%
     html_text() %>% 
     str_replace_all("\n", "") %>% 
     str_trim()
   
  reviews = tibble(
     title = review_titles,
     stars = review_stars,
     text = review_text
  ) 
  
  reviews
} 
```

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Boo                                     1 "We rented this movie because our …
    ##  2 Movie is still silly fun....amazon…     1 "We are getting really frustrated …
    ##  3 Brilliant and awkwardly funny.          5 "I've watched this movie repeatedl…
    ##  4 Great purchase price for great mov…     5 "Great movie and real good digital…
    ##  5 Movie for memories                      5 "I've been looking for this movie …
    ##  6 Love!                                   5 "Love this movie. Great quality"   
    ##  7 Hilarious!                              5 "Such a funny movie, definitely br…
    ##  8 napoleon dynamite                       5 "cool movie"                       
    ##  9 Top 5                                   5 "Best MOVIE ever! Funny one liners…
    ## 10 👍                                      5 "Exactly as described and came on …

Let’s read a few pages of reviews.

``` r
dynamite_url_base ="https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews =
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
)
```

## Mean scoping example

``` r
f = function(x1) {
  z = x1 + x2
  z
}

x = 1
y = 2

f(x1 = 2)
```

    ## Error in f(x1 = 2): object 'x2' not found

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.579636

``` r
median(x_vec)
```

    ## [1] 3.218623

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.579636
