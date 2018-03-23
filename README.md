
<!-- README.md is generated from README.Rmd. Please edit that file -->
transformr
==========

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/transformr.svg?branch=master)](https://travis-ci.org/thomasp85/transformr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/transformr?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/transformr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/transformr)](http://cran.r-project.org/package=transformr) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/transformr)](http://cran.r-project.org/package=transformr) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/transformr/master.svg)](https://codecov.io/github/thomasp85/transformr?branch=master)

If you've ever made animated data visualisations you'll know that arbitrary polygons and lines requires special considerations if the animation is to be smooth and believable. `transformr` is able to remove all of these worries by expanding [`tweenr`](https://github.com/thomasp85/tweenr) to understand spatial data, and thus lets you focus on defining your animation steps. `transformr` takes care of matching shapes between states, cutting some in bits if the number doesn't match between the states, and ensures that each pair of matched shapes contains the same number of anchor points and that these are paired up so as to avoid rotation and inversion during animation.

`transformr` supports both polygons (with holes), and paths either encoded as simple x/y data.frames or as simpel features using the [`sf`](https://github.com/r-spatial/sf) package.

Installation
------------

You can install transformr from github with:

``` r
# install.packages("devtools")
devtools::install_github("thomasp85/transformr")
```

More to come...
