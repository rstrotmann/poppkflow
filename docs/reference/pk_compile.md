# Compile a specification into an nlmixr2 model

Generates the code of an `nlmixr2` model function (with `linCmt()`) from
a [`pk_spec()`](pk_spec.md). The code text is available as
`attr(model, "code")` and is what is stored with every run (`model.txt`)
and hashed for caching.

## Usage

``` r
pk_compile(spec)

pk_code(spec)
```

## Arguments

- spec:

  A `pk_spec`, `pk_candidate`, or run reference.

## Value

A model function with attribute `code`.

## Examples

``` r
spec <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), blocks = list(c("cl", "v2")))
cat(pk_code(spec))
#> function() {
#>   ini({
#>     lka <- log(1)
#>     lcl <- log(10)
#>     lv2 <- log(30)
#>     lq <- log(5)
#>     lv3 <- log(40)
#>     ltlag <- log(0.3)
#>     eta.cl + eta.v2 ~ c(0.10, 0.01, 0.10)
#>     eta.ka ~ 0.1
#>     prop.sd <- 0.2
#>   })
#>   model({
#>     ka <- exp(lka + eta.ka)
#>     cl <- exp(lcl + eta.cl)
#>     v2 <- exp(lv2 + eta.v2)
#>     q <- exp(lq)
#>     v3 <- exp(lv3)
#>     alag(depot) <- exp(ltlag)
#>     linCmt() ~ prop(prop.sd)
#>   })
#> }
```
