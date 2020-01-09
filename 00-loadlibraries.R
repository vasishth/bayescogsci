## ----setup, include=FALSE-----------------------------------

knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)
# creates R code
knitr::knit_hooks$set(purl = knitr::hook_purl)

local({
  r = getOption('repos')
  if (!length(r) || identical(unname(r['CRAN']), '@CRAN@'))
    r['CRAN'] = 'https://cran.rstudio.com' 
  options(repos = r)
})

lapply(c('DT', 'citr', 'formatR', 'svglite'), function(pkg) {
  if (system.file(package = pkg) == '') install.packages(pkg)
})


## ----functions, include=FALSE, echo =FALSE------------------
# USEFUL FUNCTIONS:
# makes a separated by commas list from a vector
list_str <- function(vector, and = TRUE, s = "`"){
    vector <- paste0(s,vector,s)
    if(and) {
        paste0(vector,collapse = ", ")
    } else {
        paste0(paste0(vector[-length(vector)],collapse = ", "), ", and ", vector[length(vector)])
    }
}
list_code <- function(vector){
    toString(paste0('"', vector,'"'))
}


## ----eval=FALSE,fig.align='center', echo=FALSE, include=identical(knitr:::pandoc_to(), 'html')----
## fig.link='https://www.crcpress.com/product/isbn/9781138700109'
## knitr::include_graphics('images/cover.jpg', dpi = NA)


## provide comprehensive book recommendations


## to-do: add a Mackay type chapter ordering for different scenarios.


## to-do: provide solutions


## ----packages, echo = FALSE---------------------------------
library(dplyr)
index <- readLines("index.Rmd")
start <- which(stringr::str_detect(index, "```\\{r load")) + 1
endings <- which(stringr::str_detect(index, "```$")) -1
end <- min(endings[endings>start])

to_install <- index[start:end] %>%
    stringr::str_match("library\\((.*)\\)") %>%
    {.[,2]} %>%
    .[complete.cases(.)] %>%
    .[. != "rstan"]

installation <- paste0("install.packages(c(", list_code(to_install),")")



## ----load, cache = FALSE, message = FALSE-------------------
set.seed(42)
library(MASS)
##be careful to load dplyr after MASS
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(extraDistr)
library(ggplot2)
library(brms)
library(rstan)
## Save compiled models:
rstan_options(auto_write = TRUE)
## Parallelize the chains using all the cores:
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(tictoc)
library(gridExtra)


# To solve some conflicts between  packages
select <- dplyr::select
extract <- rstan::extract


## ---- include = FALSE, cache = FALSE------------------------
## Defauls values of some parameters
# I don't want windows opening:
formals(stan)$open_progress <- FALSE
# To be able to include full citations:
## library(RefManageR)
## bibl <- ReadBib("BayesCogSci.bib",check = "warn")
## citetitle <- function(key){
##     bibl[key= key]$title
## }
## fullcite <-  function(key){
##         capture.output(print(bibl[key= key]))
## }
library(bibtex)
bibl <- read.bib("BayesCogSci.bib")
fullcite <-  function(key){
    capture.output(print(bibl[key]))
}

## Look and feel:
# Plots
bayesplot_theme_set(theme_light())
theme_set(theme_light())
# format
options(
    htmltools.dir.version = FALSE,
    formatR.indent = 2,
    width = 55,
    digits = 2,
    signif =2,
    warnPartialMatchAttr = FALSE,
    warnPartialMatchDollar = FALSE,
    # Don't use scientific notation:
    scipen=10000,
    # tibbles:
    tibble.width = Inf,
    tibble.print_max = 5,
    tibble.print_min = 5
)


## Bruno Nicenboim,

## Daniel Schad,

## Shravan Vasishth,

## Potsdam, Germany

