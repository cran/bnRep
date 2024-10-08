% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redmeat.R
\docType{data}
\name{redmeat}
\alias{redmeat}
\title{redmeat Bayesian Network}
\format{
A discrete Bayesian network to predict the potential effects of message delivery from the observation of the psychosocial antecedents. Probabilities were given within the referenced paper. The vertices are:
\describe{
\item{Baseline_Intention}{(high, medium, low);}
\item{Desensitization}{(high, medium, low);}
\item{Diffused_Responsibility}{(high, medium, low);}
\item{Food_Involvment}{(high, medium, low);}
\item{Future_Intention}{(high_positive, low_positive, neutral, low_negative, high_negative);}
\item{Message}{(gain, nonloss, nongain, loss);}
\item{Perceived_Control}{(high, medium, low);}
\item{Perceived_Severity}{(high, medium, low);}
\item{Prevention_Focus}{(high, medium, low);}
\item{Promotion_Focus}{(high, medium, low);}
\item{Systematic_Processing}{(high, medium, low);}
}
}
\value{
An object of class \code{bn.fit}. Refer to the documentation of \code{bnlearn} for details.
}
\description{
Framing and tailoring prefactual messages to reduce red meat consumption: Predicting effects through a psychology-based graphical causal model.
}
\references{
Catellani, P., Carfora, V., & Piastra, M. (2022). Framing and tailoring prefactual messages to reduce red meat consumption: Predicting effects through a psychology-based graphical causal model. Frontiers in Psychology, 13, 825602.
}
