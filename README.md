
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
      SOMALOADALL ->
        `Load Development Environment`

Alternatively, you may call the driver function directly from
`SomaLoadAll`:

``` r
# Help
?SomaLoadAll::sp

# Direct Call
# Note: all objects, even non-exported, are accessible
options(width = 110)
SomaLoadAll::sp()
#> ══ somaverse dev environment ══════════════════════════════════════════════════════════ Loaded ══
ls("package:devel")
#>  [1] "addAssayVariance"                "calc.aic"                        "calc.dil.scale.factors"         
#>  [4] "calc.limits"                     "calc.mapped.scale.factors"       "calc.npl.ci"                    
#>  [7] "calcConnectedComponents"         "calcCVbands"                     "calcCVbyGroup"                  
#> [10] "calcICC"                         "calcPairwiseTables"              "calcPercentileTable"            
#> [13] "calcRsquared"                    "calcSS2L"                        "choose_init"                    
#> [16] "computeCVBands"                  "computeRefParams"                "computeRefParams_kCentral"      
#> [19] "createConcordTable"              "createNormalizationSummaryTable" "createTestsList"                
#> [22] "data_dims"                       "data.catch"                      "decalibrate"                    
#> [25] "dehybNormalize"                  "demedianNormalize"               "denormalizationWrapper"         
#> [28] "denormalizeAdat"                 "deplateNormalize"                "dilutionNormalize"              
#> [31] "duplicatedIndex"                 "em_1_step"                       "enrich_test"                    
#> [34] "equal.likelihood.pt"             "erf"                             "erf.inv"                        
#> [37] "erfc"                            "evaluateCalibrationQC"           "exploreNAnalytes"               
#> [40] "finder"                          "fit.npl"                         "fit.rlm"                        
#> [43] "generateSingleSampleReference"   "get_pars"                        "get_tom_data"                   
#> [46] "get.4PLtheta.file"               "get.npl.stats"                   "getDupIdxList"                  
#> [49] "getInflectionPt"                 "getInit"                         "getInput"                       
#> [52] "getPlateScale_Scalar"            "getRFUdata"                      "getSSmenu"                      
#> [55] "gg_color_hue"                    "gof"                             "ICA"                            
#> [58] "kCentralMAD"                     "kCentralSD"                      "lsObjects"                      
#> [61] "lss"                             "mergeMetaData"                   "modifyAptContent"               
#> [64] "my_x_log10"                      "my_y_log10"                      "my.hyb.plot"                    
#> [67] "my.qq"                           "normal_k2_mixture"               "order_somamers"                 
#> [70] "permute"                         "plot.mix_k2"                     "plot.npl"                       
#> [73] "plot.scramble.ks"                "plotHemo"                        "plotTwoWayInteraction"          
#> [76] "print.npl"                       "pvalueFDR"                       "quantileNormalize"              
#> [79] "removePlateScale_Scalar"         "rlm_calibration"                 "rm_grep"                        
#> [82] "robustPCA"                       "robustPCAshrinkage"              "robustPCAsvdThresh"             
#> [85] "scrambleClasses"                 "scrambleKS"                      "searchBreadthFirst"             
#> [88] "seriateHeatmap"                  "sign_test"                       "sl"                             
#> [91] "Sys.sourceDir"                   "system.file"                     "test.triples.x"                 
#> [94] "triples_test"
```

-----

#### LICENSE

Please note that this SomaLogic, Inc. internal package is released with
a [LICENSE](LICENSE). By using in this package you agree to abide by its
terms.

-----

Created by [Rmarkdown](https://github.com/rstudio/rmarkdown) (v1.11) and
R version 3.5.2 (2018-12-20).
