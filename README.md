
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcoincap <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcoincap)](https://CRAN.R-project.org/package=rcoincap)
<!-- badges: end -->

R Package for Querying Cryptocurrency Data from the [CoinCap
API](https://docs.coincap.io/)

## Installation

You can install the development version of rcoincap from
[GitHub](https://github.com/southernt/rcoincap) with:

``` r
# install.packages("devtools")
devtools::install_github("southernt/rcoincap")
```

## Examples

Basic usage examples:

``` r
library(rcoincap)
```

Available Assets

``` r
coincap_assets()
#> # A tibble: 100 × 12
#>    id         rank symbol name   supply max_supply market_cap_usd volume_usd24hr
#>    <chr>     <dbl> <chr>  <chr>   <dbl>      <dbl>          <dbl>          <dbl>
#>  1 bitcoin       1 btc    Bitc… 1.90e 7    2.1 e 7  551903227746.   12953743313.
#>  2 ethereum      2 eth    Ethe… 1.21e 8   NA        234780826035.    7074342235.
#>  3 tether        3 usdt   Teth… 7.42e10   NA         74157013213.   23299597940.
#>  4 usd-coin      4 usdc   USD … 5.28e10   NA         52031852532.    1799346724.
#>  5 binance-…     5 bnb    BNB   1.67e 8    1.67e 8   49387083735.     615924137.
#>  6 binance-…     6 busd   Bina… 1.86e10   NA         18577320481.    1518120486.
#>  7 xrp           7 xrp    XRP   4.54e10    1   e11   18512639848.     691882801.
#>  8 cardano       8 ada    Card… 3.38e10    4.5 e10   17117426852.     361181188.
#>  9 solana        9 sol    Sola… 3.39e 8   NA         16585499108.     408256998.
#> 10 dogecoin     10 doge   Doge… 1.33e11   NA         11018541945.     282989132.
#> # … with 90 more rows, and 4 more variables: price_usd <dbl>,
#> #   change_percent24hr <dbl>, vwap24hr <dbl>, explorer <chr>
```

Map between coin names and ticker symbols.

``` r
coincap_nm_to_symbol("bitcoin")
#> bitcoin 
#>   "btc"
coincap_symbol_to_nm("btc")
#>       btc 
#> "bitcoin"
```

Available granularities for historical data.

``` r
gran_vctr <- coincap_granularities()
names(gran_vctr)
#> [1] "five_min"    "fifteen_min" "one_hr"      "four_hr"     "one_day"    
#> [6] "one_wk"
```

Query available markets (exchanges).

``` r
coincap_markets()
#> # A tibble: 2,000 × 12
#>    exchange_id  rank base_symbol base_id       quote_symbol quote_id price_quote
#>    <chr>       <dbl> <chr>       <chr>         <chr>        <chr>          <dbl>
#>  1 aax             2 matic       polygon       usdt         tether       0.624  
#>  2 aax             3 sushi       sushiswap     usdt         tether       1.27   
#>  3 aax             4 kava        kava          usdt         tether       2.41   
#>  4 aax             5 alice       myneighboral… usdt         tether       2.79   
#>  5 aax             6 bake        bakerytoken   usdt         tether       0.258  
#>  6 aax             7 sun         sun-token     usdt         tether       0.00871
#>  7 alterdice       1 btc         bitcoin       usdt         tether   29000.     
#>  8 alterdice       2 eth         ethereum      usdt         tether    1940.     
#>  9 alterdice       3 eth         ethereum      btc          bitcoin      0.0669 
#> 10 alterdice       4 bnb         binance-coin  usdt         tether     296.     
#> # … with 1,990 more rows, and 5 more variables: price_usd <dbl>,
#> #   volume_usd24hr <dbl>, percent_exchange_volume <dbl>,
#> #   trades_count24hr <dbl>, updated <dttm>
```

Query OHLC data.

``` r
coincap_ohlc()
#> # A tibble: 723 × 6
#>    tm                   open  high   low close  volume
#>    <dttm>              <dbl> <dbl> <dbl> <dbl>   <dbl>
#>  1 2020-05-21 00:00:00 9511. 9578. 8815  9069. 108929.
#>  2 2020-05-22 00:00:00 9068. 9271  8934. 9170   58943.
#>  3 2020-05-23 00:00:00 9170  9308. 9070  9179.  43526.
#>  4 2020-05-24 00:00:00 9179. 9298  8700  8720.  70380.
#>  5 2020-05-25 00:00:00 8718. 8980. 8643. 8900.  62825.
#>  6 2020-05-26 00:00:00 8900. 9018. 8700  8841.  58300.
#>  7 2020-05-27 00:00:00 8841  9225  8812. 9204.  68910.
#>  8 2020-05-28 00:00:00 9204. 9625. 9110  9576.  74111.
#>  9 2020-05-29 00:00:00 9576. 9605. 9330  9427.  57374.
#> 10 2020-05-30 00:00:00 9427. 9740  9331. 9698.  55665.
#> # … with 713 more rows
```

<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->
