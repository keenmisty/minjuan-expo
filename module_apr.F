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

subroutine ksv_calculate(ksv,h,zs,kds,r,ta,bd,da,rou_soil,thetasw)

  real, parameter :: temp_p = 3.1536e7
  real :: h
  real :: zs
  real :: kds
  real :: r
  real :: ta
  real :: bd
  real :: da
  real :: rou_soil
  real :: thetasw

  real :: temp_c,temp_d,temp_e

  temp_c = temp_p * h / (zs * kds * r * ta *bd)

  temp_d = da / zs

  temp_e = 1 - bd / rou_soil - theatsw

  ksv    = temp_c * temp_d * temp_e

end subroutine ksv_calculate

!-------------------------------------------------------------------------------------------

subroutine ksr_calculate(ksr,ro,thetasw,zs,kds,bd)

  real :: ksr
  real :: ro
  real :: thetasw
  real :: zs
  real :: kds
  real :: bd

  real :: temp_a,temp_b

  temp_a = bd * kds / thetasw

  temp_b = thetasw * zs * (1 + temp_a)

  ksr    = ro / temp_b

end subroutine ksr_calculate

!-------------------------------------------------------------------------------------------

end module apr_calculate
