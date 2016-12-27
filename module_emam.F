module module_emam
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric 
! Mercury Deposition on Human Multimedia Exposure: Projection from 
! Observations in the Pearl River Delta Region, South China" 
!
! This module is for Environmental Media Accumulation Model 
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
  if(underline == 0.0) then
     underline = small_value
  end if

  cshg      =online/underline

end subroutine cshg_calculate

!-------------------------------------------------------------------------------------------

subroutine cwtot_calculate(cwtot,ldep,li,lr,le,vfx,fwc,kwt,aw,dz)

! implicit none
  real :: cwtot     ! Total water body Hg concentration (mg/L)
  real :: ldep      ! Deposition load (g/year)
  real :: li        ! Runoff load from impervious surfaces (g/year)
  real :: lr        ! Runoff load from perious surfaces (g/year)
  real :: le        ! Soil erosion load (g/year)
  real :: vfx       ! Average volume metric flow rate through water body (m3/year)
  real :: fwc       ! Fraction of total water body Hg concentration in water column
  real :: kwt       ! Overall total water body Hg dissipation rate constant (year-1)
  real :: aw        ! Water body surface (m2)
  real :: dz        ! Total water body depth (m)

  real :: upline
  real :: unline

  upline = ldep+li+lr+le
  unline = vfx*fwc+kwt*aw*dz

  ! need to notice that underline shouldnot be zero.
  if(unline == 0.0) then
     unline = small_value
  end if

  cwtot  = upline/unline

end subroutine cwtot_calculate

!-------------------------------------------------------------------------------------------

subroutine cdw_calculate(cdw,fwc,cwtot,dz,dwc,kdsw,tss)

! implicit none
  real :: cdw       ! Dissolved phase water concentration (mg/L)
  real :: cwtot     ! Total water body Hg concentration (mg/L)
  real :: fwc       ! Fraction of total water body Hg concentration in water column
  real :: dz        ! Total water body depth (m)
  real :: dwc       ! Depth of water column (m)
  real :: kdsw      ! Suspended sediments/surface water partition coefficient
  real :: tss       ! Total suspended solids concentration (mg/L)

  real :: uline

  uline = 1+kdsw*tss*1.e-6

  if(uline == 0.0) then
     uline = small_value
  end if

  if(dwc == 0.0) then
     dwc = small_value
  end if

  cdw = fwc*cwtot*dz/(dwc*uline)

end subroutine cdw_calculate

!-------------------------------------------------------------------------------------------

subroutine cpdj_calculate(cpdj,dyd,dyw,rpj,kpj,tpj,ypj)

! implicit none
  real :: cpdj
  real :: dyd
  real :: dyw
  real :: rpj
  real :: kpj
  real :: tpj
  real :: ypj

  real :: undline

  undline = 1000*kpj*ypj

  if(undline == 0.0) then
     undline = small_value
  end if

  cpdj=(dyd+0.6*dyw)*rpj*(1-exp(-kpj*tpj))/undline

end subroutine cpdj_calculate

!-------------------------------------------------------------------------------------------

subroutine cprj_calculate(cprj,cshg,brj)

! implicit none
  real :: cprj
  real :: cshg
  real :: brj

  cprj = cshg*brj

end subroutine cprj_calculate

!-------------------------------------------------------------------------------------------

subroutine cai_calculate(cai,qpij,cpdj,cprj,qsi,cshg,bai)

! implicit none
  real :: cai
  real :: qpij
  real :: cpdj
  real :: cprj
  real :: qsi
  real :: cshg
  real :: bai

  real :: cai_temp
  real :: caiqp_temp = 0
  integer, parameter :: jline=num_of_plants

  cai_temp   = qsi*cs*bai
  
  do j=1, jline, 1
  caiqp_temp = caiqp_temp + qpij(j)*(cpdj(j)+cprj(j))
  end do

  cai        = cai_temp + caiqp_temp

end subroutine cai_calculate

!-------------------------------------------------------------------------------------------

subroutine cfish_calculate(cfish,cdw,baffish)

! implicit none
  real :: cfish
  real :: cdw
  real :: baffish

  cfish = cdw*baffish

end subroutine cfish_calculate

!-------------------------------------------------------------------------------------------

end module module_emam
