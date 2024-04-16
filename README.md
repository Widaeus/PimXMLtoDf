pimxml
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

PIMsoft generated ‘Excel’ XML reports does not easily import in to R
envionments due to its inconsistency in data structure.

I made `pimxml` in order to import these documents to R without too much
hassle.

## Installation

You can install `pimxml` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Widaeus/pimxml")
```

## Usage

The package consists of two main functions; `xml_to_df` and
`folder_to_df`. `xml_to_df` takes a XML file url with certain PIMsoft
report prerequisites and turns it into a data frame of one row(one
observation) and corresponding relevant columns(variables).
`folder_to_df` takes all folder and applies `xml_to_df` to every XML
file and puts in a data frame, binding the rows.
