
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
SomaObjects::sp()
#> ══ SomaR Dev Environment Loaded ══════════════════════════════════════════════════════════════════════
ls("package:devel")
#>  [1] "addAssayVariance"                "calc.aic"                       
#>  [3] "calc.dil.scale.factors"          "calc.limits"                    
#>  [5] "calc.mapped.scale.factors"       "calc.npl.ci"                    
#>  [7] "calcConnectedComponents"         "calcCVbands"                    
#>  [9] "calcCVbyGroup"                   "calcICC"                        
#> [11] "calcPairwiseTables"              "calcPercentileTable"            
#> [13] "calcRsquared"                    "calcSS2L"                       
#> [15] "choose.init"                     "clusterCorHeatmaps"             
#> [17] "computeCVBands"                  "createConcordTable"             
#> [19] "createNormalizationSummaryTable" "createTestsList"                
#> [21] "data_dims"                       "data.catch"                     
#> [23] "denormalizeAdat"                 "dilutionNormalize"              
#> [25] "duplicatedIndex"                 "em.1.step"                      
#> [27] "enrich_test"                     "equal.likelihood.pt"            
#> [29] "erf"                             "erf.inv"                        
#> [31] "erfc"                            "evaluateCalibrationQC"          
#> [33] "finder"                          "fit.npl"                        
#> [35] "fit.rlm"                         "get_pars"                       
#> [37] "get_tom_data"                    "get.4PLtheta.file"              
#> [39] "get.npl.stats"                   "getDupIdxList"                  
#> [41] "getInflectionPt"                 "getInit"                        
#> [43] "getInput"                        "getRFUdata"                     
#> [45] "getSSmenu"                       "gg_color_hue"                   
#> [47] "gof"                             "ICA"                            
#> [49] "lsObjects"                       "lss"                            
#> [51] "modifyAptContent"                "my_x_log10"                     
#> [53] "my_y_log10"                      "my.hyb.plot"                    
#> [55] "my.qq"                           "normal.k2.mixture"              
#> [57] "permute"                         "plot.heatmap.clusters"          
#> [59] "plot.mix_k2"                     "plot.npl"                       
#> [61] "plot.scramble.ks"                "plotHemo"                       
#> [63] "plotTwoWayInteraction"           "print.npl"                      
#> [65] "pvalueFDR"                       "quantileNormalize"              
#> [67] "rlm.calibration"                 "rm.grep"                        
#> [69] "robustPCA"                       "robustPCAshrinkage"             
#> [71] "robustPCAsvdThresh"              "scrambleClasses"                
#> [73] "scrambleKS"                      "searchBreadthFirst"             
#> [75] "seriateHeatmap"                  "sign.test"                      
#> [77] "sl"                              "Sys.sourceDir"                  
#> [79] "system.file"                     "test.triples.x"                 
#> [81] "triples.test"
```

-----

#### LICENSE

Please note that this SomaLogic, Inc. internal package is released with
a [LICENSE](LICENSE). By using in this package you agree to abide by its
terms.

-----

Created on 2019-01-21 by
[Rmarkdown](https://github.com/rstudio/rmarkdown) (v1.11) and R version
3.5.1 (2018-07-02).
