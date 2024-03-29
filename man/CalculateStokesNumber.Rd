% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculateStokesNumber.R
\name{CalculateStokesNumber}
\alias{CalculateStokesNumber}
\title{CalculateStokesNumber}
\usage{
CalculateStokesNumber(
  FrictionVelocity_ms,
  SettlingVelocity_ms,
  CharacteristicRadius_m,
  KinematicViscosityOfAir_m2s,
  SurfaceIsVegetated
)
}
\arguments{
\item{FrictionVelocity_ms}{Friction velocity in m/s}

\item{SettlingVelocity_ms}{Settling velocity in m/s}

\item{CharacteristicRadius_m}{Characteristic radius of receptor surface in m}

\item{KinematicViscosityOfAir_m2s}{Kinematic viscosity of air in m2/s}

\item{SurfaceIsVegetated}{Boolean value indicating whether the receptor
surface is a vegetation surface}
}
\value{
Stokes number
}
\description{
Calculates the stokes number according to Zhang et al. (2001)
}
\references{
Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
dry deposition scheme for an atmospheric aerosol module.
Atmospheric Environment 2001;35:549–560.
}
