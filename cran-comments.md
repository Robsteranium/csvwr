# CRAN Comments

## Test environments

The package BUILDs and CHECKs successfully on the following platforms:

- Windows x86_64-w64-mingw32, R 4.1.0
- Mac OS X 10.15.7, R 4.1.0
- Ubuntu 20.04, R 4.1.0 and development build r80448

Continuous integration result: https://github.com/Robsteranium/csvwr/actions/runs/902225136
Configuration: .github/workflows/r.yml

windows-latest

* Local Ubuntu 18.04, R 3.6.3
* Docker rocker/r-devel:latest (d4cf3d), R 4.1.0
* Github Workflow Ubuntu, R 3.6 
* win-builder x86_64-w64-mingw32, R 4.1.0 + R devel (r80444)
* R-Hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD Check results

No ERRORs, WARNINGs or NOTEs.

## Licensing

The tests/csvw-tests directory includes a copy of the W3C CSVW test suite. This
is distibuted under the the W3C Test Suite License and the W3C 3-clause BSD
License.

https://www.w3.org/Consortium/Legal/2008/04-testsuite-license
http://www.w3.org/Consortium/Legal/2008/03-bsd-license

In compliance with this license the tests remain unchanged and the license
itself is linked from ./tests/csvw-tests/index.html.

The W3C (and it's copyright claim to these tests) is deliberately not mentioned
in the package's DESCRIPTION so as not to suggest the package is in any way
endorsed or authored by the W3C.
