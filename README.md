
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `devel` set of functions

## Overview

This “package” is the development package. Do not attempt to load it as
you would a typical package. It contains a loose testing ground of
functionality that, if deemed sufficiently useful, will eventually be
migrated into an appropriate member of the `somaverse`.

-----

## Installation

You cannot currently install the `devel` library from the command line,
as it is not a package *per se*. The simplest way to optain the
functionality it contains is to load it along with the other
[`somaverse`](http://bitbucket.sladmin.com/projects/SV) packages using
the `RStudio` **addin** feature, which behaves similar to
`devtools::load_all()`.

    Addins Menu ->
      SOMAOBJECTS ->
        `Load Development Environment`

Alternatively, you may call the driver function directly from
`SomaObjects`:

``` r
# Help
?SomaObjects::sp

# Direct Call
# Note: all objects, even non-exported, are accessible
options(width = 110)
SomaObjects::sp()
#> ══ SomaR Dev Environment Loaded ══════════════════════════════════════════════════════════════════
ls("package:devel")
#>  [1] "addAssayVariance"                "calc.aic"                        "calc.dil.scale.factors"         
#>  [4] "calc.limits"                     "calc.mapped.scale.factors"       "calc.npl.ci"                    
#>  [7] "calcConnectedComponents"         "calcCVbands"                     "calcCVbyGroup"                  
#> [10] "calcICC"                         "calcPairwiseTables"              "calcPercentileTable"            
#> [13] "calcRsquared"                    "calcSS2L"                        "choose_init"                    
#> [16] "clusterCorHeatmaps"              "computeCVBands"                  "createConcordTable"             
#> [19] "createNormalizationSummaryTable" "createTestsList"                 "data_dims"                      
#> [22] "data.catch"                      "denormalizeAdat"                 "dilutionNormalize"              
#> [25] "duplicatedIndex"                 "em_1_step"                       "enrich_test"                    
#> [28] "equal.likelihood.pt"             "erf"                             "erf.inv"                        
#> [31] "erfc"                            "evaluateCalibrationQC"           "exploreNAnalytes"               
#> [34] "finder"                          "fit.npl"                         "fit.rlm"                        
#> [37] "get_pars"                        "get_tom_data"                    "get.4PLtheta.file"              
#> [40] "get.npl.stats"                   "getDupIdxList"                   "getInflectionPt"                
#> [43] "getInit"                         "getInput"                        "getRFUdata"                     
#> [46] "getSSmenu"                       "gg_color_hue"                    "gof"                            
#> [49] "ICA"                             "lsObjects"                       "lss"                            
#> [52] "modifyAptContent"                "my_x_log10"                      "my_y_log10"                     
#> [55] "my.hyb.plot"                     "my.qq"                           "normal_k2_mixture"              
#> [58] "order_somamers"                  "permute"                         "plot.heatmap.clusters"          
#> [61] "plot.mix_k2"                     "plot.npl"                        "plot.scramble.ks"               
#> [64] "plotHemo"                        "plotTwoWayInteraction"           "print.npl"                      
#> [67] "pvalueFDR"                       "quantileNormalize"               "rlm_calibration"                
#> [70] "rm_grep"                         "robustPCA"                       "robustPCAshrinkage"             
#> [73] "robustPCAsvdThresh"              "scrambleClasses"                 "scrambleKS"                     
#> [76] "searchBreadthFirst"              "seriateHeatmap"                  "sign_test"                      
#> [79] "sl"                              "Sys.sourceDir"                   "system.file"                    
#> [82] "test.triples.x"                  "triples_test"
```

-----

#### LICENSE

Please note that this SomaLogic, Inc. internal package is released with
a [LICENSE](LICENSE). By using in this package you agree to abide by its
terms.

-----

Created on 2019-03-01 by
[Rmarkdown](https://github.com/rstudio/rmarkdown) (v1.11) and R version
3.5.2 (2018-12-20).
