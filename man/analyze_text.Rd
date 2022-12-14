% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{analyze_text}
\alias{analyze_text}
\title{A text analysis Function}
\usage{
analyze_text(
  data,
  text_column = "text",
  dict_name = "concrete",
  lemmatize = NULL
)
}
\arguments{
\item{data}{The data set with texts to analyze.}

\item{text_column}{The name of the column in data with the texts. Defaults to 'text'.}

\item{dict_name}{The name of the dictionary used for the analysis. Defaults to 'concrete'.}

\item{lemmatize}{Whether to lemmatize the words in the text (TRUE/FALSE). Defaults to NULL, in which case the dictionary's default is chosen (see \code{dict_info}).}
}
\description{
This function allows you to readily analyze texts using a dictionary of choice from the package.
}
\examples{
analyze_text(data.frame(text="Lord, what fools these mortals be!"))

analyze_text(data.frame(bard="Lord, what fools these mortals be!"),text_column='bard',dict='VAD')
}
\keyword{dictionary}
\keyword{lexicon,}
