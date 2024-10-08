% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permaBN.R
\docType{data}
\name{permaBN}
\alias{permaBN}
\title{permaBN Bayesian Network}
\format{
A discrete Bayesian network to simulate permafrost thaw in the continuous permafrost region of the Arctic. The probabilities were given within the referenced paper. The vertices are:
\describe{
\item{ActiveLayerIceContent}{(Low, Medium, High);}
\item{AirTemperature}{(Low, Medium, High);}
\item{Aspect}{(North, East, South, West);}
\item{Insulation}{(Low, Medium, High);}
\item{Rain}{(Low, Medium, High);}
\item{Season}{(Snow free, Snow);}
\item{Snow}{(Low, Medium, High);}
\item{SnowDepth}{(None, Low, Medium, High);}
\item{SoilDensity}{(Low, Medium, High);}
\item{SoilMoisture}{(Low, Medium, High);}
\item{SoilTemperature}{(Low, Medium, High);}
\item{SoilWaterInput}{(Low, Medium, High);}
\item{ThawDepth}{(Low, Medium, High);}
\item{VegetationHeight}{(Low, Medium, High);}
}
}
\value{
An object of class \code{bn.fit}. Refer to the documentation of \code{bnlearn} for details.
}
\description{
PermaBN: A Bayesian Network framework to help predict permafrost thaw in the Arctic.
}
\references{
Beall, K., Loisel, J., & Medina-Cetina, Z. (2022). PermaBN: A Bayesian Network framework to help predict permafrost thaw in the Arctic. Ecological Informatics, 69, 101601.
}
