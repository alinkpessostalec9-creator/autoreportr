\name{auto_report}
\alias{auto_report}
\title{AutoReportR v2.0: Generate Automatic Statistical Report with AI Interpretations}
\description{
  AutoReportR v2.0 generates a complete HTML statistical report with:
  \itemize{
    \item Automatic detection of variable types
    \item Descriptive statistics (numeric and categorical variables)
    \item Missing values analysis
    \item AI-powered interpretations via Groq API (Llama 3.3-70B)
    \item Interactive and responsive HTML report
  }
}
\usage{
auto_report(data, vars = NULL, title = NULL, author = Sys.getenv("USERNAME"), 
            open_report = TRUE)
}
\arguments{
  \item{data}{A data.frame containing the data to analyze}
  \item{vars}{Optional character vector of variable names to select (all by default)}
  \item{title}{Optional custom title for the report}
  \item{author}{Author name (defaults to system username)}
  \item{open_report}{Logical, whether to open the report in browser after generation (default: TRUE)}
}
\value{
  Invisibly returns the full path to the generated HTML file
}
\examples{
\dontrun{
# Basic usage
auto_report(mtcars)

# With custom title
auto_report(iris, title = "Analyse des iris")

# Variable selection
auto_report(mtcars, vars = c("mpg", "cyl", "hp"), 
            title = "Analyse performances auto")
}
}
\note{
  An internet connection is required for AI interpretations.
  The Groq API key must be configured in the script.
}
\seealso{
  \code{\link{check_data}}, \code{\link{compute_descriptive_stats}}
}