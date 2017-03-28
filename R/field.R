#' Field experiment of 517 Arabidopsis thaliana accessions from the 1001Genomes
#'
#' A dataset containing different life history phenotypes and fitness estimates
#' of 517 accessions with 24 replicates each in different treatments, namely
#' site (Madrid or Tuebingen), watering (high and low), and density of plants
#' per pot (an experimental population or single individual).
#'
#'
#' @format A data frame with 12xxx rows and xx variables:
#' \describe{
#'   \item{id}{the 1001 Genomes IDs of the used accessions}
#'   \item{name}{common name of the accession}
#'   \item{country}{of origin}
#'   \item{latitude}{in degrees}
#'   \item{longitude}{in degrees}
#'   \item{kgroup}{genetic grups from ADMIXTURE analysis}
#' }
#' @name field
#' @source see \url{1001genomes.org/} for accession list, see \url{http://biorxiv.org/content/early/2017/03/19/118067} for genetic groups.
"field"
