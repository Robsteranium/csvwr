# CRAN Comments

## Re-submission

This version addresses the following feedback:

### Missing `\value` in Rdocs

Now added to `render_uri_templates.Rd`. Thanks.

### Don't include examples in un-exported functions

I've exported some of the functions (`base_uri`, `datatype_to_type` and
`transform_datetime_format`) and removed the documentation for others
(`is_blank.Rd` and `property_type.Rd`) (so that the examples can remain
in the comments).

### Please ensure not to write to the home, package or working directory

I've not yet started on an e.g. `write_csvw` function, so there is no
functionality for writing to disk at this point. I've double-checked and can't
find any functions, tests, examples or vignettes that write anything.

The documentation illustrates how calls to `cat(file=...)` could be used to
write the csvw results but in both `README.md:55` and 
`vignettes/read-write-csvw.Rmd:124` these calls are not executed.

The `tests/csvw-tests` directory is a verbatim copy of the W3C source (unmodified
as per the license). There are two ruby scripts in this directory which would
write files, but these aren't executed (or even referenced) within the csvwr R
package.

I've also checked `lsof -c R` for any surprises but can't see anything
suspicious.

If something is picked-up during the review process then please let me know e.g.
the filename or context and I'll resolve it.

## Test environments

The package BUILDs and CHECKs successfully on the following platforms:

- Windows x86_64-w64-mingw32, R 4.1.0
- Mac OS X 10.15.7, R 4.1.0
- Ubuntu 20.04, R 4.1.0 and development build r80448

Continuous integration result:
  https://github.com/Robsteranium/csvwr/actions/runs/902225136
Configuration:
  .github/workflows/r.yml

The package was also checked against:
- win-builder x86_64-w64-mingw32, R 4.1.0 + R devel (r80444)
- R-Hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- Local Ubuntu 18.04, R 3.6.3 and R 4.1.0
- Docker rocker/r-devel:latest (d4cf3d), R 4.1.0

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
