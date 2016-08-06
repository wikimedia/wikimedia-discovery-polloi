polloi 0.1.4
============
- Updated Wikipedia prefix fetching.

polloi 0.1.3
============
- Now includes a de-duplication step after reading
  data to avoid spikes. Will issue warnings when
  it has detected duplicates.

polloi 0.1.2
============
- Downloads datasets over https now.
- Added spline smoothing with generalized additive
  models. See ?mgcv::gam for more info.

polloi 0.0.9
============
- Adds methods for converting wikiid to a language-
  project tuple.

polloi 0.0.8
============
- Now intelligently chooses which color scheme to use
  based on the number of variables in the dygraph.
- Sanitizes data before passing it through xts::xts().

polloi 0.0.7
============
- Added function for checking dataset for missing
  data and creating a notification if any detected.

polloi 0.0.6
============
- Added features from Search Metrics dashboard
  to be used across all dashboards.

polloi 0.0.5
=============
- Added subsetting by date range.

polloi 0.0.4
=============
- Added a change log.
- Added a contributor code of conduct.
- Added a readme.

polloi 0.0.3
=============
- Updated reading to support optional arguments.

polloi 0.0.2
=============
- Made make_dygraph support reactivity.

polloi 0.0.1
=============
- Initial release.
