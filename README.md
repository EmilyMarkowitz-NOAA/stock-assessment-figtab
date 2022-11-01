<!-- README.md is generated from README.Rmd. Please edit that file -->

# [AFSC RACE GAP Figures for Stock Assessment Reports](%60r%20link_repo%60) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

**The creation of this repo was inspired by this
[issue](https://github.com/afsc-gap-products/data-requests/issues/41).
**

The scripts therein reproducibly produce annual figures and tables for
stock assessment reports, presentations, and other outreach documents;
from data to data product.

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/EmilyMarkowitz-NOAA/stock-assessment-figtab/releases).

## This code and the associated releases were used to develop the following reports, outreach documents, and presentations:

\[coming soon\]

<!-- <div id="refs"></div> -->
<!-- Use .bib file to cite reports in subsection titles -->

## Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/EmilyMarkowitz-NOAA/stock-assessment-figtab/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/EmilyMarkowitz-NOAA/stock-assessment-figtab/issues).

# R Version Metadata

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] rstudioapi_0.14  knitr_1.40       magrittr_2.0.3   munsell_0.5.0    tidyselect_1.2.0 cowplot_1.1.1    colorspace_2.0-3
    ##  [8] R6_2.5.1         rlang_1.0.6      fastmap_1.1.0    fansi_1.0.3      stringr_1.4.1    dplyr_1.0.10     tools_4.2.0     
    ## [15] grid_4.2.0       gtable_0.3.1     xfun_0.34        utf8_1.2.2       cli_3.4.1        DBI_1.1.3        htmltools_0.5.3 
    ## [22] yaml_2.3.6       digest_0.6.30    assertthat_0.2.1 tibble_3.1.8     lifecycle_1.0.3  ggplot2_3.3.6    RODBC_1.3-19    
    ## [29] vctrs_0.5.0      glue_1.6.2       evaluate_0.17    rmarkdown_2.17   stringi_1.7.8    compiler_4.2.0   pillar_1.8.1    
    ## [36] scales_1.2.1     generics_0.1.3   pkgconfig_2.0.3

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
