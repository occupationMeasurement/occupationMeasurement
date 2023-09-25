# occupationMeasurement (development version)

# occupationMeasurement 0.3.1

* Gracefully handle unavailability of the KldB 2010 classification

# occupationMeasurement 0.3.0

* This release is accompanying an article in the Journal of Open Source Software
* The auxiliary classification (AuxCo) has been upgraded to v1.2.3
* A bug in the test cases has been fixed when the package was installed from CRAN (#7)
* The `Suggests` dependencies have been slimmed to only include packages that are actually referenced in code and would lead to issues with `R CMD CHECK`. This is to avoid additional reverse dependencies for other packages. (#5)
* There were minor additional changes updating wording in vignettes or improving the legibility of code which do not affect functioning of the package.

# occupationMeasurement 0.2.0

* Initial release of the package
* Added a `NEWS.md` file to track changes to the package.
