module module_hem
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
integer, parameter :: days = 365 ! days in a year
integer, parameter :: hours = 24 ! hours in a day

contains

!-------------------------------------------

subroutine disoiling_calculate(disoiling,cshg,irsoil,efsoil,ed,days,at,bw,hours)

  real :: disoiling ! Daily intake of Hg via non-dietary ingestion of soil (mg/kg-day)
  real :: cshg      ! Soil Hg concentration (mg/kg)
  real :: irsoil    ! Soil daily non-dietary intake (mg/day)
  real :: efsoil    ! Exposure frequency to soil (h/day)
  real :: ed        ! Exposure duration (year)
  integer :: days   
  real :: at        ! Total area of land (m2)
  real :: bw        ! Body weight (kg)
  integer :: hours

  real :: underlines

  underlines= at * bw * hours

  disoiling = cshg * irsoil * efsoil * ed * days * 1.0e-6 / underlines

end subroutine disoiling_calculate

!------------------------------------------

subroutine disoilder_calculate(disoilder,cshg,esa,af,absf,efsoil,ed,at,bw,days,hours)

  real :: disoilder ! Daily intake of Hg via dermal contact with soil (mg/kg-day)
  real :: cshg      ! 
  real :: esa       ! Extremities surface area for soil dermal contact scenario (m2)
  real :: af        ! Adherence factor of soil to skin (mg/cm2-day)
  real :: absf      ! Fraction of contaminant absorbed dermakky from soil
  real :: efsoil    ! Exposure frequency to soil(h/day)
  real :: ed        ! Exposure duration (year)
  real :: at        ! Total area of land (m2)
  real :: bw        ! Body weight (kg)
  integer :: days,hours

  real :: underlines

  underlines= at * bw * hours

  disoilder = cshg * esa * af * absf * efsoil * ed * days * 1.0e-2 / underlines

end subroutine disoilder_calculate

!------------------------------------------

subroutine diswiming_calculate(diswiming,cw,irswiming,efswim,ed,at,bw)

  real :: diswiming ! Daily intake of Hg via water ingestion during swimming (mg/kg-day)
  real :: cw        ! ---------------------What's this?
  real :: irswiming ! Ingestion rate during each swimming event in fresh water (ml/event)
  real :: efswim    ! Swimming frequency in fresh water per year (event/year)
  real :: ed        ! Exposure duration (year)
  real :: at        ! Total area of land (m2)
  real :: bw        ! Body weight (kg)

  real :: underline

  underline = at * bw

  diswiming = cw * irswiming * efswim * ed * 1.0e-3 / underline

end subroutine diswiming_calculate

!------------------------------------------

subroutine diswimder_calculate(diswimder,cw,tsa,kp,ev,efswim,ed,at,bw)

  real :: diswimder ! Daily intake of Hg via dermal contact with water during swimming (mg/kg-day)
  real :: cw        ! 
  real :: tsa       ! Total Surface area for swimming scenario (m2)
  real :: kp        ! Dermal permeability coefficient of compound in water (cm/h)
  real :: ev        ! Swimming event duration in fresh water (min/event)
  real :: efswim    ! Swimming frequency in fresh water per year (event/year)
  real :: ed        ! Exposure duration (year)
  real :: at        ! Total area of land (m2)
  real :: bw        ! Body weight (kg)

  real :: underln

  underln   = at * bw * 6

  diswimder = cw * tsa * kp * ev * efswim * ed / underln

end subroutine diswimder_calculate

!------------------------------------------

subroutine diw_calculate(diw,cdw,irdrinkw,ed,years,at,bw)

  real :: diw       ! Daily intake of Hg via drinking water consumption (mg/kg-day)
  real :: cdw       ! Dissolved phase water concentration (mg/L)
  real :: irdrinkw  ! Daily drinking water intake (ml/day)
  real :: ed        ! Exposure duration (year)
  real :: at        ! Total area of land (m2)
  real :: bw        ! Body weight (kg)
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

end module module_hem
