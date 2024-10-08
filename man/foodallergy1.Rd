% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foodallergy1.R
\docType{data}
\name{foodallergy1}
\alias{foodallergy1}
\title{foodallergy Bayesian Networks}
\format{
A discrete Bayesian network to to estimate conditional probabilities of each food allergy when other food allergies are present (full population). Probabilities were given within the referenced paper. The vertices are:
\describe{
\item{Cereals}{(T, F);}
\item{Eggs}{(T, F);}
\item{Fruits}{(T, F);}
\item{Milk}{(T, F);}
\item{Nuts}{(T, F);}
\item{Peanuts}{(T, F);}
\item{Seafood}{(T, F);}
\item{Veg_Leg}{(T, F);}
}
}
\value{
An object of class \code{bn.fit}. Refer to the documentation of \code{bnlearn} for details.
}
\description{
Prevalence of self-reported food allergy in Tunisia: General trends and probabilistic modeling.
}
\references{
Belmabrouk, S., Abdelhedi, R., Bougacha, F., Bouzid, F., Gargouri, H., Ayadi, I., ... & Rebai, A. (2023). Prevalence of self-reported food allergy in Tunisia: General trends and probabilistic modeling. World Allergy Organization Journal, 16(9), 100813.
}
