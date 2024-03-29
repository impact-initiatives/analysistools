% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_analysis_median.R
\name{create_analysis_median}
\alias{create_analysis_median}
\title{Calculate a median from a survey}
\usage{
create_analysis_median(design, group_var = NA, analysis_var, level = 0.95)
}
\arguments{
\item{design}{design survey}

\item{group_var}{dependent variable(s), variable to group by. If no dependent
variable, it should be NA or empty string. If more than one variable, it
should be one string with each variable separated by comma, e.g. "groupa, groupb"
to group for groupa and groupb.
NA is default for no grouping.}

\item{analysis_var}{the independent variable, variable to summarise}

\item{level}{the confidence level. 0.95 is default}
}
\value{
a data frame with the median for each group
}
\description{
Calculate a median from a survey
}
\note{
The results may differ with median(). There are lots of ways to calculate the median and
the default calculation between stats::median and survey::svyquantile/srvyr::survey_median are
different. Default from \emph{survey/srvyr} is "school" methodology and does not exist in \emph{stats}
package. The default for \emph{stats} is "hf7". \emph{survey/srvyr} methodology is prefered as these
packages are built for complex survey design.
}
\examples{
somedata <- data.frame(
  aa = 1:10,
  bb = rep(c("a", "b"), 5),
  weights = rep(c(.5, 1.5), 5)
)
me_design <- srvyr::as_survey(somedata)
create_analysis_median(me_design, analysis_var = "aa")
create_analysis_median(me_design, group_var = "bb", analysis_var = "aa")

me_design_w <- srvyr::as_survey(somedata, weights = weights)
create_analysis_median(me_design_w, analysis_var = "aa")
create_analysis_median(me_design_w, group_var = "bb", analysis_var = "aa")

}
