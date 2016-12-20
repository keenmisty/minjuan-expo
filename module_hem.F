module hem_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric
! Mercury Deposition on Human Multimedia Exposure: Projection from
! Observations in the Pearl River Delta Region, South China"
!
! This module is for Human Exposure Model
! 
! test for change
!
! by changming
! 2016-12-10

implicit none

real, parameter :: small_value = 1.e-36
real, parameter :: large_value = 1.e36
integer, parameter :: days = 365
integer, parameter :: hours = 24

contains

!-------------------------------------------

subroutine disoiling_calculate(disoiling,cshg,irsoil,efsoil,ed,days,at,bw,hours)

  real :: disoiling ! Daily intake of Hg via non-dietary ingestion of soil (mg/kg-day)
  real :: cshg      ! Soil Hg concentration (mg/kg)
  real :: irsoil    ! Soil daily non-dietary intake (mg/day)
  real :: efsoil    ! Exposure frequency to soil (h/day)
  real :: ed
  integer :: days
  real :: at
  real :: bw
  integer :: hours

  real :: underlines

  underlines= at*bw*hours

  disoiling = cshg*irsoil*efsoil*ed*days*1.0e-6/underlines

end subroutine disoiling_calculate

!------------------------------------------

subroutine disoilder_calculate

  real :: disoilder
  real :: cshg
  real :: esa
  real :: af
  real :: absf  ! Fraction of contaminant absorbed dermakky from soil
  real :: efsoil
  real :: ed
  real :: at
  real :: bw
  integer :: days,hours

  real :: underlines

  underlines= at*bw*hours

  disoilder = cshg*esa*af*absf*efsoil*ed*days*1.0e-2/underlines

end subroutine disoilder_calculate

!------------------------------------------

subroutine diswiming_calculate

  real :: diswiming
  real :: cw
  real :: irswiming
  real :: efswim
  real :: ed
  real :: at
  real :: bw
  integer :: days

end subroutine diswiming_calculate

!------------------------------------------

end module hem_calculate
