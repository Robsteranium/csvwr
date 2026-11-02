# CRAN Comments

## csvwr version 0.1.8

This update removes `csvwr::vec_depth` in favour of `purrr::pluck_depth` as
this no longer descends into errors.

## Test environments

The package BUILDs and CHECKs successfully on the following platforms:

- Microsoft Windows Server 2025 10.0.26100 / R 4.6.1
- macOS 26.6.2 / R 4.6.1
- Ubuntu 24.04.5 / R 4.6.1, R 4.5.3 and development build 2026-11-01 r90825

Continuous integration result:
  https://github.com/Robsteranium/csvwr/actions/runs/41447649530
Configuration:
  .github/workflows/r.yml

## R CMD Check results

No ERRORs, WARNINGs or NOTEs.

## Licensing

The tests/csvw-tests directory includes a copy of the W3C CSVW test suite. This
is distributed under the the W3C Test Suite License and the W3C 3-clause BSD
License.

https://www.w3.org/Consortium/Legal/2008/04-testsuite-license
http://www.w3.org/Consortium/Legal/2008/03-bsd-license

In compliance with this license the tests remain unchanged and the license
itself is linked from ./tests/csvw-tests/index.html.

The W3C (and it's copyright claim to these tests) is deliberately not mentioned
in the package's DESCRIPTION so as not to suggest the package is in any way
endorsed or authored by the W3C.
