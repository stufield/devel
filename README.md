
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `devel` set of functions

## Overview

This “package” is the development package. Do not attempt to load it as
you would a typical package. It contains a loose testing ground of
functionality that, if deemed sufficiently useful, will eventually be
migrated into an appropriate package.

------------------------------------------------------------------------

## Installation

You cannot currently install the `devel` library from the command line,
as it is not a package *per se*. The simplest way to obtain the
functionality it contains is to load it using the `RStudio`
`devtools::load_all()` shortcut.

``` r
options(width = 110)

devtools::load_all() # all objects, even non-exported, are accessible
#> ℹ Loading devel

ls("package:devel")
#>   [1] "%<>%"                                 "%>%"                                 
#>   [3] "abline"                               "addAssayVariance"                    
#>   [5] "apropos"                              "as_tibble"                           
#>   [7] "axis"                                 "binom.test"                          
#>   [9] "blue"                                 "boxplot"                             
#>  [11] "calc.aic"                             "calc.dil.scale.factors"              
#>  [13] "calc.limits"                          "calc.mapped.scale.factors"           
#>  [15] "calc.npl.ci"                          "calcConnectedComponents"             
#>  [17] "calcCVbands"                          "calcCVbyGroup"                       
#>  [19] "calcICC"                              "calcPairwiseTables"                  
#>  [21] "calcPercentileTable"                  "calcRsquared"                        
#>  [23] "calcSS2L"                             "choose_init"                         
#>  [25] "cleanNames"                           "combn"                               
#>  [27] "compact"                              "computeCVBands"                      
#>  [29] "computeRefParams"                     "computeRefParams_kCentral"           
#>  [31] "computeTestParamsNMetricsBatch"       "createConcordTable"                  
#>  [33] "createNormalizationSummaryTable"      "createTestsList"                     
#>  [35] "createUrineNormalizationSummaryTable" "curve"                               
#>  [37] "data_dims"                            "data.catch"                          
#>  [39] "decalibrate"                          "dehybNormalize"                      
#>  [41] "demedianNormalize"                    "denormalizationWrapper"              
#>  [43] "denormalizeAdat"                      "deplateNormalize"                    
#>  [45] "dilutionNormalize"                    "dir_ls"                              
#>  [47] "duplicatedIndex"                      "em_1_step"                           
#>  [49] "enrich_test"                          "equal.likelihood.pt"                 
#>  [51] "erf"                                  "erf.inv"                             
#>  [53] "erfc"                                 "evaluateCalibrationQC"               
#>  [55] "exploreNAnalytes"                     "file.edit"                           
#>  [57] "finder"                               "fisher.test"                         
#>  [59] "fit.npl"                              "fit.rlm"                             
#>  [61] "generateSingleSampleReference"        "get_pars"                            
#>  [63] "get_tom_data"                         "get.4PLtheta.file"                   
#>  [65] "get.npl.stats"                        "getCalSFs"                           
#>  [67] "getDupIdxList"                        "getInflectionPt"                     
#>  [69] "getInit"                              "getInput"                            
#>  [71] "getNormNames"                         "getPlateScale_Scalar"                
#>  [73] "getRFUdata"                           "getSSmenu"                           
#>  [75] "gg_color_hue"                         "gof"                                 
#>  [77] "grid"                                 "hist"                                
#>  [79] "ICA"                                  "kCentralMAD"                         
#>  [81] "kCentralSD"                           "legend"                              
#>  [83] "library.dynam.unload"                 "lines"                               
#>  [85] "list_modify"                          "lm"                                  
#>  [87] "lsObjects"                            "lss"                                 
#>  [89] "map"                                  "map_dbl"                             
#>  [91] "map_df"                               "map2"                                
#>  [93] "mclapply"                             "mergeMetaData"                       
#>  [95] "modifyAptContent"                     "my_x_log10"                          
#>  [97] "my_y_log10"                           "my.hyb.plot"                         
#>  [99] "my.qq"                                "normal_k2_mixture"                   
#> [101] "obj_size"                             "optim"                               
#> [103] "order_somamers"                       "par"                                 
#> [105] "path_real"                            "permute"                             
#> [107] "pf"                                   "plot"                                
#> [109] "plot.mix_k2"                          "plot.npl"                            
#> [111] "plot.scramble.ks"                     "plotHemo"                            
#> [113] "plotTwoWayInteraction"                "points"                              
#> [115] "print.npl"                            "pvalueFDR"                           
#> [117] "qt"                                   "quantile"                            
#> [119] "quantileNormalize"                    "read.csv"                            
#> [121] "removePlateScale_Scalar"              "rlm"                                 
#> [123] "rlm_calibration"                      "rm_grep"                             
#> [125] "rnorm"                                "robustPCA"                           
#> [127] "robustPCAshrinkage"                   "robustPCAsvdThresh"                  
#> [129] "safely"                               "scrambleClasses"                     
#> [131] "scrambleKS"                           "searchBreadthFirst"                  
#> [133] "seriate"                              "seriateHeatmap"                      
#> [135] "set_names"                            "set_rownames"                        
#> [137] "sign_test"                            "sl"                                  
#> [139] "str_glue"                             "str_remove_all"                      
#> [141] "str_replace_all"                      "str_squish"                          
#> [143] "summary.test_params"                  "Sys.sourceDir"                       
#> [145] "system.file"                          "test.triples.x"                      
#> [147] "triples_test"                         "UnormCompSubset"                     
#> [149] "UnormInGroups"                        "UnormOneGroup"                       
#> [151] "var"
```

------------------------------------------------------------------------

#### LICENSE

Please note that this SomaLogic, Inc. internal package is released with
a [LICENSE](LICENSE). By using in this package you agree to abide by its
terms.

------------------------------------------------------------------------

Created by [Rmarkdown](https://github.com/rstudio/rmarkdown) (v2.9) and
R version 4.1.0 (2021-05-18).
