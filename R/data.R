#' Sample of NLSY79 wage data from 2000
#'
#' @description Sample of National Longitudinal Survey (NLSY) 79
#'  containig wage data from the year 2000
#' of workers who were aged 35 to 43 in that year. The data is from O'Neill
#' and O'Neill (2006) and is used as an illustration of the Oxaca-Blinder mean
#' decomposition in Firpo, Fortin, and Lemieuex (2011). The data contains 2655
#' male and 2654 female observations, respectively.
#'
#' @format A data frame with 5,396 rows and 15 variables.
#' \describe{
#'   \item{female}{Female indicator}
#'   \item{wage}{Hourly wage in US dollars}
#'   \item{age}{Age in years}
#'   \item{central_city}{Central city indicator}
#'   \item{msa}{Metropolitan statistical area (MSA) indicator}
#'   \item{region}{Factor variable disinguishing 4 large regions}
#'   \item{black}{Black indicator}
#'   \item{hispanic}{Hispanic indicator}
#'   \item{education}{Factor variable indicating highest attained education}
#'   \item{afqt}{Percentile score of armed force qualification test (AFTQ) devided by 10}
#'   \item{family_responsibility}{Family responsibility indicator}
#'   \item{years_worked_civilian}{Years worked in cilivian labor force}
#'   \item{years_worked_military}{Years worked in military}
#'   \item{part_time}{Share of years worked in part-time}
#'   \item{industry}{Factor variable identifying 4 industries}
#' }
#'
#' @source
#' Fortin, Nicole M., Thomas Lemieux, and Firpo Segio. 2011.
#' "Decomposition Methods in Economics." In Orley Ashenfelter and David Card, eds.,
#' Handbook of Labor Economics, Volume 4a., Chapter 1, 1-102.
"nlys00"


#' Sample of male wage data from the CPS 1983-1985 and 2003-2005
#'
#' @description A sample of the the Merged Outgoing Rotation Group of the
#' Current Population Survey from 1983 to 1985 and 2003 to 2005, respectively,
#' used as example by Fortin, Lemieux & Firpo  (2011) in their handbook chapter.
#' The data set contains a selection of 8 variables and a sample of 40,347 observations
#' of male workers (i.e., a tenth of the origninal data set).
#'
#' @format A data frame with 40,347 rows and 8 variables.
#' \describe{
#'   \item{wage}{Hourly wage in US dollars at constant prices}
#'   \item{union}{Union status indicator}
#'   \item{education}{Factor variable with 6 education levels: high-school graduates (reference), elementary, high-school dropouts , some college, college graduates, post college graduates}
#'   \item{experience}{Factor variable with 9 potential experience levels, each of five years gap, 20 to 24 years as reference level)}
#'   \item{married}{Married indicator}
#'   \item{nonwhite}{Non-white indicator}
#'   \item{year}{Indicator distinguishing pooled observations from the 1983 to 1985 period and those from 2003 to 2005}
#'   \item{weights}{CPS sample weights}
#' }
#'
#' @source
#' Fortin, Nicole M., Thomas Lemieux, and Firpo Segio. 2011.
#' "Decomposition Methods in Economics." In Orley Ashenfelter and David Card, eds.,
#' Handbook of Labor Economics, Volume 4a., Chapter 1, 1-102.
"men8305"
