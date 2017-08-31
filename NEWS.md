polloi 0.2.2
============
- Adds `reorder_columns`

polloi 0.2.1
============
- Adds `capitalize_first_letter`

polloi 0.2.0
============
- Adds unit tests and lint checking ([T145445](https://phabricator.wikimedia.org/T145445)).
- Adds an example dataset (`wdqs_usage`) that is used for running examples and tests.
- Fixes problem with spline smoothing ([T169125](https://phabricator.wikimedia.org/T169125)).
- Fixes a whole bunch of stylistic issues (removes lints).
- Fixes a bug with `compress()` wherein it would yield weird results if the input vector included a 0.

polloi 0.1.9
============
- Adds geography datasets and functions ([T167913](https://phabricator.wikimedia.org/T167913)).

polloi 0.1.8
============
- Updates dataset of prefixes.
- Changes path to download datasets from.
- Uses latest roxygen with markdown support.

polloi 0.1.7
============
- Fixes a bug wherein a "found duplicated data in" message was incorrectly displayed (specifically, when there was no duplicated data).

polloi 0.1.6
============
- Improved color palette.

polloi 0.1.5
============
- Added intelligent color palettes.
- Added [Chelsy Xie](https://meta.wikimedia.org/wiki/User:CXie_(WMF)) as a contact.
- Removed [Oliver Keyes](https://meta.wikimedia.org/wiki/User:Okeyes_(WMF)) as a contact.

polloi 0.1.4
============
- Updated Wikipedia prefix fetching.

polloi 0.1.3
============
- Now includes a de-duplication step after reading data to avoid spikes. Will issue warnings when it has detected duplicates.

polloi 0.1.2
============
- Downloads datasets over https now.
- Added spline smoothing with generalized additive models. See ?mgcv::gam for more info.

polloi 0.0.9
============
- Adds methods for converting wikiid to a language-project tuple.

polloi 0.0.8
============
- Now intelligently chooses which color scheme to use based on the number of variables in the dygraph.
- Sanitizes data before passing it through `xts::xts()`.

polloi 0.0.7
============
- Added function for checking dataset for missing data and creating a notification if any detected.

polloi 0.0.6
============
- Added features from Search Metrics dashboard to be used across all dashboards.

polloi 0.0.5
=============
- Added subsetting by date range.

polloi 0.0.4
=============
- Added a [Change log](NEWS.md).
- Added a [contributor code of conduct (CoC)](CONDUCT.md).
- Added a [readme](README.md).

polloi 0.0.3
=============
- Updated reading to support optional arguments.

polloi 0.0.2
=============
- Made `make_dygraph` support reactivity.

polloi 0.0.1
=============
- Initial release.
