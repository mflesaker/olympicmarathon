% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{olympic_marathon}
\alias{olympic_marathon}
\title{Olympic Marathon Data by Gender and Year}
\format{
\code{olympic_marathon} is an object of class tbl_df (inherits from tbl, data.frame) with 1956 rows and 9 columns
\describe{
\item{rank}{Finishing place based on race time}
\item{name}{Competitor's Name}
\item{nationality}{3 letter International Olympic Committee abbreviation of the country that the competitor is representing, see \href{https://en.wikipedia.org/wiki/List_of_IOC_country_codes}{Wikipedia}}
\item{result}{The competitor's finishing time (hr:min:sec) (DNF if did not finish, DNS if did not start, DQ if disqualified)}
\item{gender}{Gender of the competitor (F = female, M = male)}
\item{event}{The Olympic event that the entry corresponds to}
\item{location}{The location of the Olympic Games}
\item{year}{The year of the Olympic Games}
\item{medal}{What medal the competitor earned, if any (G = Gold, S = Silver, B = Bronze)}
}
}
\source{
\code{olympic_marathon} comes from the \href{https://www.worldathletics.org/competition/calendar-results?}{World Athletics website results page} and \href{https://www.olympiandatabase.com/}{Olympian Database}
}
\usage{
olympic_marathon
}
\description{
\code{olympic_marathon} contains data from Olympics men's and women's marathons
}
\examples{
olympic_marathon
}
\keyword{datasets}
