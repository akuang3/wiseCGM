#' ggplot2.
#'
#' @name wiseCGM
#' @docType package
#' @import ggplot2 dplyr
NULL

#' Continuous Glucose Monitoring Data from August
#'
#' A list of datasets containing the data from a DEXCOM CGM Device and the meta information that is returned from the format_dexcom() function:
#'
#' \itemize{
#'   \item full.cgm.data. A dataframe of 3264 rows and 20 columns
#'   \item cleaned.cgm.data. A dataframe of 1128 rows and 22 columns. This does not contain data for any days if the device needed calibration but was not performed. Missing data are imputed with mean of 2 measurements before and after the missing data.
#'   \item meta.data. A dataframe containing the meta information provided by DEXCOM devices
#'   \item informative.meta.data. A dataframe containing the High, Low, and Urgent Low threshold.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name grobman_Pt2_T1
#' @usage data(grobman_Pt2_T1)
#' @format A list of 4 data frames.
NULL


#' Continuous Glucose Monitoring Data from October
#'
#' A list of datasets containing the data from a DEXCOM CGM Device and the meta information that is returned from the format_dexcom() function:
#'
#' \itemize{
#'   \item full.cgm.data. A dataframe of 2947 rows and 20 columns
#'   \item cleaned.cgm.data. A dataframe of 2845 rows and 22 columns. This does not contain data for any days if the device needed calibration but was not performed. Missing data are imputed with mean of 2 measurements before and after the missing data.
#'   \item meta.data. A dataframe containing the meta information provided by DEXCOM devices
#'   \item informative.meta.data. A dataframe containing the High, Low, and Urgent Low threshold.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name grobman_Pt2_T2
#' @usage data(grobman_Pt2_T2)
#' @format A list of 4 data frames.
NULL

#' Continuous Glucose Monitoring Data from December
#'
#' A list of datasets containing the data from a DEXCOM CGM Device and the meta information that is returned from the format_dexcom() function:
#'
#' \itemize{
#'   \item full.cgm.data. A dataframe of 2279 rows and 20 columns
#'   \item cleaned.cgm.data. A dataframe of 2168 rows and 22 columns. This does not contain data for any days if the device needed calibration but was not performed. Missing data are imputed with mean of 2 measurements before and after the missing data.
#'   \item meta.data. A dataframe containing the meta information provided by DEXCOM devices
#'   \item informative.meta.data. A dataframe containing the High, Low, and Urgent Low threshold.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name grobman_Pt2_T3
#' @usage data(grobman_Pt2_T3)
#' @format A list of 4 data frames.
NULL
