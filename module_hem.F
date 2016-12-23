module hem_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric
! Mercury Deposition on Human Multimedia Exposure: Projection from
! Observations in the Pearl River Delta Region, South China"
!
! This module is for Human Exposure Model
! 
! created by Dr. Minjuan Huang.
!
! modified by Dr. Ming Chang.
!
! 2016-12-23

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

subroutine disoilder_calculate(disoilder,cshg,esa,af,absf,efsoil,ed,at,bw,days,hours)

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

subroutine diswiming_calculate(diswiming,cw,irswiming,efswim,ed,at,bw)

  real :: diswiming
  real :: cw
  real :: irswiming
  real :: efswim
  real :: ed
  real :: at
  real :: bw

  real :: underline

  underline = at*bw

  diswiming = cw*irswiming*efswim*ed*1.0e-3/underline

end subroutine diswiming_calculate

!------------------------------------------

subroutine diswimder_calculate(diswimder,cw,tsa,kp,ev,efswim,ed,at,bw)

  real :: diswimder
  real :: cw
  real :: tsa
  real :: kp
  real :: ev
  real :: efswim
  real :: ed
  real :: at
  real :: bw

  real :: underln

  underln   = at*bw*6

  diswimder = cw*tsa*kp*ev*efswim*ed/underln

end subroutine diswiming_calculate

!------------------------------------------

subroutine diw_calculate(diw,cdw,irdrinkw,ed,years,at,bw)

  real :: diw
  real :: cdw
  real :: irdrinkw
  real :: ed
  real :: at
  real :: bw
  integer :: years

  real :: underline

  underline  = at*bw

  diw        = cdw*irdrinkw*ed*years*1.0e-3/underline

end subroutine diw_calculate

!------------------------------------------

subroutine difood_calculate(difood,cfood,irfood,ed,years,at,bw)

  real :: difood
  real :: cfood
  real :: irfood
  real :: ed
  real :: at
  real :: bw
  integer :: years

  real :: underline

  underline  = at*bw

  difood     = cfood*irfood*ed*years*1.0e-3/underline

end subroutine difood_calculate

!------------------------------------------

end module hem_calculate
