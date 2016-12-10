module emam_calculate
! The equations are from Minjuan Huang et al., "Impacts of Atmospheric 
! Mercury Deposition on Human Multimedia Exposure: Projection from 
! Observations in the Pearl River Delta Region, South China" 
!
! by changming
! 2016-12-09

contains

subroutine cshg_calculate(cshg,dtot,ks,t,zs,bd)

  implicit none
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

  cshg      =online/underline

end subroutine cshg_calculate

end module emam_calculate
