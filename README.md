# debvctrs

<!-- badges: start -->
<!-- badges: end   -->

This tutorial package is meant to go along with my talk *vctrs: Creating custom vector classes with the vctrs package* at RStudio::conf 2020.

debvctrs is an example package to demonstrate the creation of new S3 vector classes using the [vctrs package](https://vctrs.r-lib.org). debvctrs is based on the more complete [debkeepr package](https://jessesadler.github.io/debkeepr) that integrates non-decimal currencies that use the tripartite system of pounds, shillings, and pence into R. This package is not meant for analytical use, though all aspects of the package work as expected and are fully tested using [testthat](https://testthat.r-lib.org).

## Installation

You can install debvctrs from GitHub with [remotes](https://remotes.r-lib.org):

``` r
# install.packages("remotes")
remotes::install_github("jessesadler/debvctrs")
```

## Usage

The debvctrs package demonstrates the process for creating two different types of S3-vector classes. The `deb_decimal`class represents non-decimal currencies in decimalized form and is based on a double vector. It has two attributes: a `unit` attribute to record whether the values represent pounds, shillings, or pence and a `bases` attribute to determine the bases of the shillings and pence units. The `deb_lsd` class uses the record style to maintain the tripartite structure of non-decimal currencies. A [record-style vector](https://vctrs.r-lib.org/articles/s3-vector.html#record-style-objects) uses a list of equal-length vectors to designate the components that make up each vector. Like the `deb_decimal` class, the class has a `bases` attribute. For a more thorough introduction to the structure of the two classes, see the description from the [debkeepr package](https://jessesadler.github.io/debkeepr/articles/debkeepr.html).

The R scripts for the package provide an order for constructing these two S3 vectors with vctrs. This tutorial is based on the [S3 vignette](https://laocr.org) from the vctrs package. It will be useful to work through this tutorial with the explanations from the vignette.

- 01: Construction of the vectors
    - 01.1-decimal-class.R: Construction of `deb_decimal` class based on a double vector.
    - 01.2-lsd-class.R: Construction of `deb_lsd` class, a record-style vector.
    - 01.3-checks.R: Checks used in the construction of the two classes to provide more user-friendly error messages.
- 02-coercion.R: Implicit transformation of the class of vectors
    - A) Coercion for `deb_decimal`
    - B) Coercion for `deb_lsd`
    - C) Coercion between `deb_decimal` and `deb_lsd`
- 03-casting.R: Explicit transformation of the class of vectors
    - A) Casting for `deb_decimal`
    - B) Casting for `deb_lsd`
    - C) Casting between `deb_decimal` and `deb_lsd`
- 04-comparison-lsd.R: Only necessary to implement for record-style vector
    - Equality: `==`, `!=`, `unique()`, `anyDuplicated()`, and `is.na()`
    - Comparison: `<`, `<=`, `>=`, `>`, `min()`, `max()`, and `xtfrm()`
- 05-mathematical-funcs.R: Only necessary to implement for record-style vector
    - Implemented Summary and Math group functions and other generics
    - Unimplemented functions
- 06-arithmetic-ops.R: Arithmetic operations
    - A) Arithmetic operations for `deb_decimal`
    - B) Arithmetic operations for `deb_lsd`
    - C) Arithmetic operations between `deb_decimal` and `deb_lsd`
- helper-convert-attr.R: Conversions of `bases` and `unit` attributes
    - Convert `bases` attribute
    - Convert `unit` attribute
- helper-normalize.R: Function to normalize non-decimal currency values
    - Normalization is central to integrating non-decimal currencies into R
- utils.R
    - ifelse function implemented with vctrs that is used in the package
- debvctrs-package.R
    - Package documentation and documentation for `vctrs` parameters

## Resources
- [vctrs site](https://vctrs.r-lib.org)
    - The [S3 vectors vignette](https://vctrs.r-lib.org/articles/s3-vector.html) is particularly important for building an S3-vector class.
- [Hadley Wickham, Advanced R - Chapter 13: S3](https://adv-r.hadley.nz/s3.html#s3-methods)
- [Hadley Wickham, vctrs: Tools for making size and type consistent functions at RStudio::conf2019](https://resources.rstudio.com/rstudio-conf-2019/vctrs-tools-for-making-size-and-type-consistent-functions)
