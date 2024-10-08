% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seismic.R
\docType{data}
\name{seismic}
\alias{seismic}
\title{seismic Bayesian Network}
\format{
A discrete Bayesian network for the identification of the seismic risk associated with a particular building which can be utilised to guide stakeholders, policymakers and designers in the efficient planning of emergency response, rescue operations and recovery activities. The probabilities were given in the referenced paper. The vertices are:
\describe{
\item{ConstructionQuality}{(Low, Medium, High);}
\item{Distance}{(Short, Medium, Long);}
\item{Fragility}{(Low, Medium, High);}
\item{LiveLoad}{(Low, Medium, High);}
\item{Magnitude}{(Low, Medium, High);}
\item{SeismicHazard}{(Low, Medium, High);}
\item{SeismicRisk}{(Low, Medium, High);}
\item{ShakingIntensity}{(Low, Medium, High);}
\item{StrengthDegradation}{(Low, Medium, High);}
\item{Vulnerability}{(Low, Medium, High);}
}
}
\value{
An object of class \code{bn.fit}. Refer to the documentation of \code{bnlearn} for details.
}
\description{
Probabilistic seismic risk assessment of a reinforced concrete building considering hazard level and the resulting vulnerability using Bayesian Belief Network.
}
\references{
Roy, G., Sen, M. K., Singh, A., Dutta, S., & Choudhury, S. (2024). Probabilistic seismic risk assessment of a reinforced concrete building considering hazard level and the resulting vulnerability using Bayesian Belief Network. Asian Journal of Civil Engineering, 25(3), 2993-3009.
}
