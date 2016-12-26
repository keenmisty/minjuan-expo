module apr_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric
! Mercury Deposition on Human Multimedia Exposure: Projection from
! Observations in the Pearl River Delta Region, South China"
!
! This module is for Alternative Projected Results related to EAME & HHR
!
! Created by Dr. Minjuan Huang.
!
! Modified by Dr. Ming Chang.
!
! 2016-12-23

implicit none

real, parameter :: small_value = 1.e-36
real, parameter :: large_value = 1.e36

contains

!-------------------------------------------------------------------------------------------

subroutine ks_calculate(ks,ksl,ksv,ksr,kse)

  real :: ks
  real :: ksl
  real :: ksv
  real :: ksr
  real :: kse

  ks = ksl + ksv + ksr + kse

end subroutine ks_calculate

!-------------------------------------------------------------------------------------------

subroutine ksl_calculate(p,i,ro,ev,thetasw,zs,bd,kds)

  real :: ksl
  real :: p
  real :: i
  real :: ro
  real :: ev
  real :: thetasw
  real :: zs
  real :: bd
  real :: kds

  real :: temp_a,temp_b

  temp_a = bd * kds / thetasw

  temp_b = thetasw * zs * (1 + temp_a)

  ksl    = (p + i - ro - ev) / temp_b

end subroutine ksl_calculate

!-------------------------------------------------------------------------------------------

end module apr_calculate
