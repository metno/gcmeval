\name{cleanstr}
\title{Remove punctuation from string}
\description{
Helper function to "clean" a character string by removing all punctuation and applying \code{tolower}. 
}
\usage{
cleanstr(x)
}
\arguments{
  \item{x}{a character string}
}
\value{a lowercase character string with no punctuation.
}
\examples{
cleanstr("Hello.World_THISISATEST!")
}