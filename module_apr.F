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

subroutine ks_calculate(ks,kst,ksv,ksr,kse)

  real :: ks
  real :: kst
  real :: ksv
  real :: ksr
  real :: kse

  ks = kst + ksv + ksr + kse

end subroutine ks_calculate

!-------------------------------------------------------------------------------------------



end module apr_calculate
