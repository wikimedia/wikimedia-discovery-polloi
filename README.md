Common Files and Functions
==========================

This repository contains files and functions used by all the [Discovery Dashboards](https://discovery.wmflabs.org/) projects.

## Installation

```R
# install.packages("remotes", repos = c(CRAN = "https://cran.rstudio.com/"))

remotes::install_git("https://gerrit.wikimedia.org/r/wikimedia/discovery/polloi")

# Alternatively, you can install from GitHub mirror:
remotes::install_github("wikimedia/wikimedia-discovery-polloi")
```

To update: `remotes::update_packages("polloi")`

### Maintainers

- [Mikhail Popov](https://meta.wikimedia.org/wiki/User:MPopov_(WMF))

## Additional Information

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
