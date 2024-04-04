PimXMLtoDf
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

PIMsoft generated ‘Excel’ XML reports does not easily import in to R
envionments due to its inconsistency in data structure.

I made `PimXMLtoDf` in order to import these documents to R without too
much hassle.

## Installation

You can install `PimXMLtoDf` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Widaeus/PimXMLtoDf")
```

## Usage

The package consists of two functions; `xml_to_singlerow` and
`singlerow_df_compile`. `xml_to_singlerow` takes a XML file url and
PIMsoft report prerequisites and turns it into a data frame of one
row(one observation) and corresponding relevant columns(variables).
`singlerow_df_compile` takes all folder and applies `xml_to_singlerow`
to every XML file and puts in a data frame, binding the rows.
