module hem_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric
! Mercury Deposition on Human Multimedia Exposure: Projection from
! Observations in the Pearl River Delta Region, South China"
!
! This module is for Human Exposure Model
!
! by changming
! 2016-12-10

implicit none

real, parameter :: small_value = 1.e-36
real, parameter :: large_value = 1.e36

contains

subroutine disoiling_calculate(disoiling,cshg,irsoil,efsoil,ed,days,at,bw,hours)

  real :: disoiling ! Daily intake of Hg via non-dietary ingestion of soil (mg/kg-day)
  real :: cshg      ! Soil Hg concentration (mg/kg)
  real :: irsoil    ! Soil daily non-dietary intake (mg/day)


end subroutine disoiling_calculate

end module hem_calculate
