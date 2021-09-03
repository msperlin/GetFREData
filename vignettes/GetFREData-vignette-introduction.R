## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  comment = "#>",
  out.width = "100%",
  results='hold'
)
library(knitr)

## ---- eval=FALSE--------------------------------------------------------------
#  # CRAN (stable)
#  install.packages('GetFREData')
#  
#  # github (development)
#  if (!require(devtools)) install.packages('devtools')
#  if (!require(GetFREData)) devtools::install_github('msperlin/GetFREData')

## -----------------------------------------------------------------------------
#  library(GetFREData)
#  library(tidyverse)
#  
#  search_company('grendene',
#                 cache_folder = tempdir())
#  
#  l_fre <- get_fre_data(companies_cvm_codes = 19615,
#                        fre_to_read = 'last',
#                        first_year = 2020,
#                        last_year = 2020,
#                        cache_folder = tempdir())
#  
#  glimpse(l_fre)

