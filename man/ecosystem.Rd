% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecosystem.R
\docType{data}
\name{ecosystem}
\alias{ecosystem}
\title{ecosystem Bayesian Network}
\format{
A discrete Bayesian network to infer the supply and demand match for cultural ecosystem services. Probabilities were given within the referenced paper. The vertices are:
\describe{
\item{Bus}{Density of bus and subway stations (Low, High);}
\item{Road}{Road density (Low, High);}
\item{Lot}{Density of public parking lots (Low, High);}
\item{Traffic}{Convenience for tourists to arrive (Low, Medium, High);}
\item{Park}{Convenience for visitors after arrival (Low, Medium, High);}
\item{Green}{Green space coverage rate (Low, Medium, High);}
\item{Water}{Whether there is a water body or not (No, Yes);}
\item{Opportunity}{Recreational convenience (Low, Medium, High);}
\item{Potential}{Aesthetic value of landscape (Low, Medium, High);}
\item{People}{Population density (Low, Medium, High);}
\item{Supply}{CES supply of communities (Low, Medium, High);}
\item{Demand}{CES demand of communities (Low, Medium, High);}
\item{Budget}{Balance of supply and demand (Deficit, Balance, Surplus).}
}
}
\value{
An object of class \code{bn.fit}. Refer to the documentation of \code{bnlearn} for details.
}
\description{
Evaluating the supply-demand balance of cultural ecosystem services with budget expectation in Shenzhen, China.
}
\references{
Wu, J., Jin, X., Wang, H., & Feng, Z. (2022). Evaluating the supply-demand balance of cultural ecosystem services with budget expectation in Shenzhen, China. Ecological Indicators, 142, 109165.
}
\keyword{ReverseTree}
