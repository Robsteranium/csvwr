# Render URI templates

Interpolate variable bindings into a URI template.

## Usage

``` r
render_uri_templates(templates, bindings = NULL, ...)
```

## Arguments

- templates:

  a character vector with URI templates

- bindings:

  a list of variable bindings to be interpolated into templates

- ...:

  further bindings specified as named function arguments

## Value

a character vector with the expanded URI

## Details

This doesn't yet implement the whole of RFC 6570, just enough to make
the tests pass

You can bind variables by passing a list to the explicit `bindings`
argument, or variadically with `...` by naming arguments according to
the variable name you wish to bind.

## Examples

``` r
render_uri_templates("{+url}/resource?query=value", list(url="http://example.net"))
#> [1] "http://example.net/resource?query=value"
render_uri_templates("{+url}", url="http://example.net")
#> [1] "http://example.net"
```
