# Transform date/time format string from Unicode TR35 to POSIX 1003.1

As per the [csvw specification for date and time
formats](https://www.w3.org/TR/2015/REC-tabular-data-model-20151217/#h-formats-for-dates-and-times)
we accept format strings using the [date field symbols defined in
unicode
TR35](https://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table).
These are converted to POSIX 1003.1 date format strings for use in
[`base::strptime()`](https://rdrr.io/r/base/strptime.html) or
[`readr::parse_date()`](https://readr.tidyverse.org/reference/parse_datetime.html)/[`readr::parse_datetime()`](https://readr.tidyverse.org/reference/parse_datetime.html).

## Usage

``` r
transform_datetime_format(format_string)
```

## Arguments

- format_string:

  a UAX35 date format string

## Value

a POSIX date format string

## Examples

``` r
if (FALSE) { # \dontrun{
fmt <- transform_datetime_format("dd.MM.yyyy")
strptime("01.01.2001", format=fmt)
} # }
```
