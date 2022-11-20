# CRAN Comments

## Update to address check failure 2022-11-01

> Please see the problems shown on
> <https://cran.r-project.org/web/checks/check_results_csvwr.html>.
>
> In current R-devel,
>
>       Experimentally, ‘balancePOSIXlt()’ and other functions returning
>       ‘POSIXlt’ objects now set a ‘logical’ attribute ‘"balanced_lt"’
>       with ‘NA’ meaning “filled-in”, i.e., not “ragged” and ‘TRUE’
>       means (fully) balanced.

Resolved in @efe40e1 by removing names before calling `strptime`.

## Test environments

The package BUILDs and CHECKs successfully on the following platforms:

- Microsoft Windows Server 2022 10.0.20348 / R 4.2.2
- Mac OS X 11 / R 4.2.2
- Ubuntu 20.04 / R 4.1.3, R 4.2.2 and development build r83308

Continuous integration result:
  https://github.com/Robsteranium/csvwr/actions/runs/3427610105
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
