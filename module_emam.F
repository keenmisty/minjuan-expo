module emam_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric 
! Mercury Deposition on Human Multimedia Exposure: Projection from 
! Observations in the Pearl River Delta Region, South China" 
!
! This module is for Environmental Media Accumulation Model 
!
! by changming
! 2016-12-12

implicit none

real, parameter :: small_value = 1.e-36
real, parameter :: large_value = 1.e36

contains

subroutine cshg_calculate(cshg,dtot,ks,t,zs,bd)

! implicit none
  real :: cshg      ! soil Hg concentration (mg/kg)
  real :: dtot      ! yearly dry deposition (ug/m2/year)
  real :: ks        ! soil Hg loss constant (year-1)
  real :: t         ! study period (year)
  real :: zs        ! soil mixing zone depth (cm)
  real :: bd        ! soil bulk density (g soil/cm3 soil)

  real :: online 
  real :: underline

  online    =100*dtot*(1.0-exp(-ks*t))
  underline =zs*bd*ks

  ! need to notice that underline shouldnot be zero.
  if(underline = 0) then
     underline = small_value
  end if

  cshg      =online/underline

end subroutine cshg_calculate

subroutine cwtot_calculate(cwtot,ldep,li,lr,le,vfx,fwc,kwt,aw,dz)

! implicit none
  real :: cwtot     ! Total water body Hg concentration (mg/L)
  real :: ldep      !  
  real :: li        !
  real :: lr        !
  real :: le        !
  real :: vfx       !
  real :: fwc       !
  real :: kwt       !
  real :: aw        !
  real :: dz        !

  real :: upline
  real :: unline

  upline = ldep+li+lr+le
  unline = vfx*fwc+kwt*aw*dz

  ! need to notice that underline shouldnot be zero.
  if(unline = 0) then
     unline = small_value
  end if

  cwtot  = upline/unline

end subroutine cwtot_calculate

end module emam_calculate
