# Reproducible Research: Closing the loop with QA and Products

Up to this point we have learned a lot and seen many different things that we can do with R.  But one of the topics we haven't really covered is how do we capture this work, weave it in with the products we create (e.g. presentations and papers), and share the analysis so that others can reproduce the results and check the quality of the work.  To do this, we will cover the basic concepts of reproducibility and discuss the types of files needed (e.g. scripts, R Markdown/Quarto documents, folders, etc.) to facilitate others using our analysis.   

## TL;DR

- Share all of the `.R` files used in your analysis
- Share the data needed by your `.R` files
- Make sure all the R packages you used are listed
- Provide basic documentation is something like a README file

An example of a simple case would be a folder lie:

- my_project_folder
    - my_project_analysis.R
    - my_project_data.csv
    - README.md

## What is reproducibility and what do you need to share?

Reproducibility in science is a very broad topic and even the definition of the assocaited terms (e.g., reproduce and replicate) are not universally agreed upon.  But the consensus that I have begun to see is that reproducibility relates to reproducing the same results with the same code and data.  This can also be referred to as "computational reproducibility".  Replicating a study relates to getting similar results but with a new study and new data.  These definitions are consitent with how the National Academies define reproducibility and replicability in a 2019 publication (https://nap.nationalacademies.org/catalog/25303/reproducibility-and-replicability-in-science).

In this lesson, we focus on computational reproducibility, how we can achieve that using R, and what we should plan on sharing to ensure proper QA and review.

## Computational reproducibility with scripts and documents

You may not have realized it, but throughout our class we have been working in a reproducible fashion.  The exercises that we are working on all have code that is saved as a `.R` script and those scripts include all of the bits and pieces we would need to successfully re-run the code.  The bare minimum that we need in order for our work to be reproduced (and thus reviewed) by someone else is:

- Details on the computational environment
- All of the code and the data the code uses
- Documentation

Let's look at each of these in a little more depth, but through some existing examples.

## Details on the computational environment

There are shades of grey on this one as the amount of detail ranges from simple to complex.  On the complex side of things a reproducible analysis would include access to the entire computational environment including operating system and version, all system dependencies, exact R version, and exact R package versions  This is often done with something like a [Docker](https://www.docker.com/) files/containers or [Binder](https://mybinder.org/) executable environments and exact package versions managed through something like [`renv`](https://cran.r-project.org/web/packages/renv/index.html).  For our first forays into reproducible work, we are not going to go this far.  Here are some examples to look at

- Packages loaded at beginning of script: <https://github.com/USEPA/lake_photic_zone/blob/master/R/model.R>.
- Packages kept in`packages.R` and loaded in other scripts using `source()`: 
    - <https://github.com/USEPA/fluoroproj/blob/main/R/packages.R>
    - <https://github.com/USEPA/fluoroproj/blob/main/R/figures.R>
    - NOTE: These are in a private repo, so will only work for Jeff.
    
In addition to making sure your code knows which packages to use, we can also provide more detailed information on the computing environment that is more geared to the humans looking at this.  The easiest way to do that is with `sessionInfo()` or for a more nicely formatted version you can use the version from the `sessioninfo` package


```r
sessionInfo()
```

```
## R version 4.2.2 (2022-10-31 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19042)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
## [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.utf8    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] stringr_1.5.0 dplyr_1.1.0   ggplot2_3.4.1 knitr_1.42   
## 
## loaded via a namespace (and not attached):
##  [1] pillar_1.8.1      compiler_4.2.2    tools_4.2.2       digest_0.6.31     jsonlite_1.8.4    evaluate_0.20    
##  [7] lifecycle_1.0.3   tibble_3.2.0      gtable_0.3.1      pkgconfig_2.0.3   png_0.1-8         rlang_1.0.6      
## [13] cli_3.6.0         rstudioapi_0.14   curl_5.0.0        yaml_2.3.7        xfun_0.37         fastmap_1.1.1    
## [19] withr_2.5.0       httr_1.4.5        generics_0.1.3    vctrs_0.5.2       rprojroot_2.0.3   grid_4.2.2       
## [25] tidyselect_1.2.0  glue_1.6.2        here_1.0.1        R6_2.5.1          dadjokeapi_1.0.2  fansi_1.0.4      
## [31] rmarkdown_2.20    sessioninfo_1.2.2 magrittr_2.0.3    scales_1.2.1      htmltools_0.5.4   colorspace_2.1-0 
## [37] utf8_1.2.3        stringi_1.7.12    munsell_0.5.0
```

```r
sessioninfo::session_info()
```

```
## ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.2.2 (2022-10-31 ucrt)
##  os       Windows 10 x64 (build 19042)
##  system   x86_64, mingw32
##  ui       RStudio
##  language (EN)
##  collate  English_United States.utf8
##  ctype    English_United States.utf8
##  tz       America/New_York
##  date     2023-03-24
##  rstudio  2022.12.0+353 Elsbeth Geranium (desktop)
##  pandoc   2.19.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
## 
## ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────
##  package     * version date (UTC) lib source
##  cli           3.6.0   2023-01-09 [1] CRAN (R 4.2.2)
##  colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.2.2)
##  curl          5.0.0   2023-01-12 [1] CRAN (R 4.2.2)
##  dadjokeapi    1.0.2   2021-03-01 [1] CRAN (R 4.2.2)
##  digest        0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
##  dplyr       * 1.1.0   2023-01-29 [1] CRAN (R 4.2.2)
##  evaluate      0.20    2023-01-17 [1] CRAN (R 4.2.2)
##  fansi         1.0.4   2023-01-22 [1] CRAN (R 4.2.2)
##  fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.2.2)
##  generics      0.1.3   2022-07-05 [1] CRAN (R 4.2.2)
##  ggplot2     * 3.4.1   2023-02-10 [1] CRAN (R 4.2.2)
##  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.2)
##  gtable        0.3.1   2022-09-01 [1] CRAN (R 4.2.2)
##  here          1.0.1   2020-12-13 [1] CRAN (R 4.2.2)
##  htmltools     0.5.4   2022-12-07 [1] CRAN (R 4.2.2)
##  httr          1.4.5   2023-02-24 [1] CRAN (R 4.2.2)
##  jsonlite      1.8.4   2022-12-06 [1] CRAN (R 4.2.2)
##  knitr       * 1.42    2023-01-25 [1] CRAN (R 4.2.2)
##  lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.2)
##  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.2)
##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.2)
##  pillar        1.8.1   2022-08-19 [1] CRAN (R 4.2.2)
##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.2)
##  png           0.1-8   2022-11-29 [1] CRAN (R 4.2.2)
##  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.2)
##  rlang         1.0.6   2022-09-24 [1] CRAN (R 4.2.2)
##  rmarkdown     2.20    2023-01-19 [1] CRAN (R 4.2.2)
##  rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.2)
##  rstudioapi    0.14    2022-08-22 [1] CRAN (R 4.2.2)
##  scales        1.2.1   2022-08-20 [1] CRAN (R 4.2.2)
##  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.2)
##  stringi       1.7.12  2023-01-11 [1] CRAN (R 4.2.2)
##  stringr     * 1.5.0   2022-12-02 [1] CRAN (R 4.2.2)
##  tibble        3.2.0   2023-03-08 [1] CRAN (R 4.2.2)
##  tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.2.2)
##  utf8          1.2.3   2023-01-31 [1] CRAN (R 4.2.2)
##  vctrs         0.5.2   2023-01-23 [1] CRAN (R 4.2.2)
##  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.2)
##  xfun          0.37    2023-01-31 [1] CRAN (R 4.2.2)
##  yaml          2.3.7   2023-01-23 [1] CRAN (R 4.2.2)
## 
##  [1] C:/Program Files/R/R-4.2.2/library
## 
## ───────────────────────────────────────────────────────────────────────────────────────────────────────────────
```

And the way to capture this with the `capture.output()` function is like this.


```r
capture.output(sessionInfo(),file="sessionInfo_2023-03-22.txt")
```

If you use the `sessioninfo::session_info()` version it provides a different (better, IMO) format.


```r
sessioninfo::session_info(to_file = "session_info_2023-03-22.txt")
```

## All of the code and the data the code uses

This part seems pretty simple.  Just make sure your provide access to your files that contain your R code (e.g., `.R` script(s), `.Rmd` files, or `.qmd` files).  For the most part that is going to be enough, you just need to make sure that all of the code is available. Also, you will need to make sure that the data your code uses is available.  Two common ways to do this are to have your data available via the web or include the data files (e.g. as CSV or excel files) along with your `.R` scripts.  

Here is an example:

- RMarkdown files (`.Rmd`) that reference `.R` files:
  - The code: <https://github.com/USEPA/ri_wq_trends/blob/aafaf6a43eebbf3e4093877e011d78e4f3f5de61/manuscript/ecosphere_resubmit_2/manuscript.Rmd#L24>
  - The code that initially accesses the data: <https://github.com/USEPA/ri_wq_trends/blob/master/R/data_clean.R>
  - The data in `ww_all.csv`: <https://github.com/USEPA/ri_wq_trends/tree/master/data> 

## Documentation

The last bit we would need is some level of documentation.  This could be provided by a simple README file or could be more complex like an `rmarkdown` or `quarto` document.

### Simple README

A README file could simply be written in text, but more commonly it would be written in Markdown (see <https://www.markdownguide.org/basic-syntax/>) to provide for some simple formatting options.  The type of information can vary, but you need to provide information about the code and data, details on use of the code and data (e.g. license/public domain dedication), links to other pertinent information (e.g. a published manuscript), and any other required pieces (e.g., a disclaimer).

Here are a few examples:

- A very simple case: <https://github.com/USEPA/Microcystinchla#readme>
- More detailed (a data only site): <https://github.com/USEPA/provisional_habs#readme>

## Quarto and R Markdown

In the materials above, we've covered the baseline case for what should be shared in order to make an analysis reproducible for other users, for reviewers, and for QA purposes.  Of course, we can get a lot fancier (which isn't necessarily better) with this.  The most likely solution would be with the use of reproducible research documents via Quarto or R Markdown.  Both of these provide the ability for us to combine code and text.  For both, the text is written using "Markdown" which is a mark-up language that provides relatively simple text mark-up to format text.  The code part is included inside of code chunks or inline code mixed directly in with the text in the same document.  These documents are "rendered" to a final output (e.g. html, pdf, docx) and in this rendering process all of the code inside of the document is run and the output is included in the final rendered document. This approach provides the foundation of reproducible documents that allow us to blend code with explanation.


We aren't going to have time to dig into the details of these today, but the materials provides by Posit (the company formerly known as RStudio) are probably the best around and will allow you to get pretty far.

Take a look at:

- Quarto:
  - Main site: <https://quarto.org/>
  - Hello, Quarto Tutorial: <https://quarto.org/docs/get-started/hello/rstudio.html>
  - Getting started: <https://quarto.org/docs/get-started/>
- R Markdown:
  - Main site: <https://rmarkdown.rstudio.com/>
  - Lessons: <https://rmarkdown.rstudio.com/lesson-1.html>

To see a bit of this in action, here are a few examples (these are R Markdown):

- Recent paper of mine with output to Word
  - Rmd: Download with code below then open in RStudio
  
  
  ```r
  download.file("https://raw.githubusercontent.com/USEPA/ri_wq_trends/master/manuscript/ecosphere_production/manuscript.Rmd", "manuscript.Rmd")
  ```
    
  - Word: Download with code below then open in Word
  
  
  ```r
  download.file("https://github.com/USEPA/ri_wq_trends/raw/master/manuscript/ecosphere_production/manuscript.docx", "manuscript_from_rmd.docx")
  ```

And, a gallery of all the things you can do! <https://quarto.org/docs/gallery/>



