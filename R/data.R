#' @title Olympic Marathon Data by Gender and Year
#' @docType data
#' @description
#' `womens_2020_data` contains data from the 2020 Tokyo Olympics Women's Marathon.
#' @usage womens_2020_data
#' @format `womens_2020_data` is an object of class tbl_df (inherits from tbl, data.frame) with 88 rows and 9 columns:
#' \describe{
#' \item{rank}{Finishing place based on race time}
#' \item{name}{Competitor's Name}
#' \item{nationality}{3 letter abbreviation of the country that the competitor is representing}
#' \item{result}{The competitor's finishing time (hr:min:sec) (DNF if did not finish)}
#' \item{gender}{Gender of the competitor (F = female, M = male)}
#' \item{event}{The Olympic event that the entry corresponds to}
#' \item{location}{The location of the Olympic Games}
#' \item{year}{The year of the Olympic Games}
#' \item{medal}{What medal the competitor earned, if any}
#' }
#' @examples womens_2020_data
"womens_2020_data"

#' @rdname womens_2020_data
#' @docType data
#' @description
#' `mens_2020_data` contains data from the 2020 Tokyo Olympics Men's Marathon.
#' @usage mens_2020_data
#'
#' @examples mens_2020_data
"mens_2020_data"

#' @rdname womens_2020_data
#' @docType data
#' @description
#' `mens_2016_data` contains data from the 2016 Rio de Janeiro Olympics Men's Marathon
#' @usage mens_2016_data
#'
#' @examples mens_2016_data
"mens_2016_data"
