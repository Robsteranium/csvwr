# CRAN Comments

## Update to address check failure 2021-11-08 21:51:20 CET

CRAN Package Check has picked up an update to the `glue` package which
led to failures in two `csvwr` tests. This update fixes those failures.

## Test environments

The package BUILDs and CHECKs successfully on the following platforms:

- Microsoft Windows Server 2019 10.0.17763 / R 4.1.2
- Mac OS X 10.15.7 / R 4.1.2
- Ubuntu 20.04.3 / R 4.0.5, R 4.1.2 and development build r81152

Continuous integration result:
  https://github.com/Robsteranium/csvwr/actions/runs/4144828415
Configuration:
  .github/workflows/r.yml

The package was also checked on R-hub and win-builder using the devtools
package.

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
