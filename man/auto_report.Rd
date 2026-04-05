\name{auto_report}
\alias{auto_report}
\title{AutoReportR: Generate Automatic Statistical Report}
\description{
  This function generates an automatic HTML statistical report with 
  interactive cards, univariate statistics, missing data analysis, 
  and data quality indicators.
}
\usage{
auto_report(data, vars = NULL, title = NULL, author = Sys.getenv("USERNAME"), 
            open_report = TRUE)
}
\arguments{
  \item{data}{A data.frame containing the data to analyze}
  \item{vars}{Optional character vector of variable names to select}
  \item{title}{Optional title for the report}
  \item{author}{Author name (defaults to system username)}
  \item{open_report}{Logical, whether to open the report after generation}
}
\value{
  Invisibly returns the path to the generated HTML file
}
\examples{
\dontrun{
# Basic usage
auto_report(mtcars)

# With custom title and variable selection
auto_report(mtcars, vars = c("mpg", "cyl", "hp"), 
            title = "Analyse des performances auto")
}
}