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

  real :: ks            ! Soil Hg loss constant (year-1)
  real :: ksl           ! Soil Hg loss constant (year-1) ?
  real :: ksv           ! Soil Hg loss constant (year-1) ?
  real :: ksr           ! Soil Hg loss constant (year-1) ?
  real :: kse           ! Soil Hg loss constant (year-1) ?

  ks = ksl + ksv + ksr + kse

end subroutine ks_calculate

!-------------------------------------------------------------------------------------------

subroutine ksl_calculate(p,i,ro,ev,thetasw,zs,bd,kds)

  real :: ksl           ! Soil Hg loss constant (year-1)
  real :: p             ! Average Annual Precipitation (cm/year) 
  real :: i             ! Average Annual Irrigation (cm/year)
  real :: ro            ! Average Annual Surface Runoff (cm/year)
  real :: ev            ! Average Annual Evapotranspiration (cm/year)
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil) 
  real :: zs            ! Soil Mixing Zone Depth (cm)
  real :: bd            ! Soil bulk density (g soil/cm3 soil)
  real :: kds           ! Suspended sediments/surface water partition coefficient 

  real :: temp_a,temp_b

  temp_a = bd * kds / thetasw

  temp_b = thetasw * zs * (1 + temp_a)

  ksl    = (p + i - ro - ev) / temp_b

end subroutine ksl_calculate

!-------------------------------------------------------------------------------------------

subroutine ksv_calculate(ksv,h,zs,kds,r,ta,bd,da,rou_soil,thetasw)

  real, parameter :: temp_z = 3.1536e7
  real :: ksv           ! Soil Hg loss constant (year-1)
  real :: h             ! Henry's law constant (atm-m3/mol)
  real :: zs            ! Soil mixing zone depth (cm)
  real :: kds           ! Suspended sediments/surface water partition coefficient
  real :: r             ! Universal gas temperture
  real :: ta            ! ?
  real :: bd            ! Soil bulk density (g soil/cm3 soil)
  real :: da            ! Diffuvity of COPC in air (cm2/s)
  real :: rou_soil      ! Soilds particle density (g/cm3)
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil)

  real :: temp_c,temp_d,temp_e

  temp_c = temp_z * h / (zs * kds * r * ta *bd)

  temp_d = da / zs

  temp_e = 1 - bd / rou_soil - thetasw

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

  real :: temp_f,temp_g

  temp_f = bd * kds / thetasw

  temp_g = thetasw * zs * (1 + temp_f)

  ksr    = ro / temp_g

end subroutine ksr_calculate

!-------------------------------------------------------------------------------------------

subroutine kse_calculate(kse,xe,sd,er,bd,zs,kds,thetasw)

  real :: kse
  real :: xe
  real :: sd
  real :: er
  real :: bd
  real :: zs
  real :: kds
  real :: thetasw

  real :: temp_h,temp_i

  temp_h = 0.1 * xe * sd * er / (bd * zs)

  temp_i = kds * bd / (kds * bd + thetasw)

  kse    = temp_h * temp_i

end subroutine kse_calculate

!-------------------------------------------------------------------------------------------

subroutine ldep_calculate(ldep,dtot,aw)

  real :: ldep
  real :: dtot
  real :: aw

  ldep = dtot * aw * 1e-6

end subroutine ldep_calculate

!-------------------------------------------------------------------------------------------

subroutine li_calculate(li,dtot,ai)

  real :: li
  real :: dtot
  real :: ai

  li   = dtot * ai * 1e-6

end subroutine li_calculate

!-------------------------------------------------------------------------------------------

subroutine lr_calculate(lr,ro,al,ai,cs,bd,thetasw,kds)
  
  real :: lr
  real :: ro
  real :: al
  real :: ai
  real :: cs
  real :: bd
  real :: thetasw
  real :: kds

  real :: temp_j,temp_k

  temp_j = cs * bd / (kds * bd + thetasw)

  temp_k = ro * (al - ai)

  lr     = temp_j * temp_k * 0.01

end subroutine lr_calculate

!-------------------------------------------------------------------------------------------

subroutine le_calculate(le,xe,al,ai,sd,er,cs,kds,bd,thetasw)

  real :: Le
  real :: Xe
  real :: AL
  real :: AI
  real :: SD
  real :: ER
  real :: Cs
  real :: Kds
  real :: BD
  real :: thetasw

  real :: temp_l,temp_m

  temp_l = Kds * CS * BD / (kds * bd + thetasw)
  
  temp_m = Xe * SD * ER * (AL-AI)

  Le     = temp_l * temp_m * 0.001

end subroutine le_calculate

!-------------------------------------------------------------------------------------------

subroutine fwc_calculate(Fwc,Kdsw,TSS,dwc,dz,thetabs,Kdbs,Cbs,dbs)

  real :: Fwc
  real :: Kdsw
  real :: TSS
  real :: dwc
  real :: dz
  real :: thetabs
  real :: Kdbs
  real :: Cbs
  real :: dbs

  real :: temp_n,temp_o

  temp_n = (1 + Kdsw * TSS * 1e-6) * dwc / dz

  temp_o = (thetabs + Kdbs * Cbs) * dbs / dz

  Fwc    = temp_n / (temp_n + temp_o)

end subroutine fwc_calculate

!-------------------------------------------------------------------------------------------

subroutine kwt_calculate(kwt,Fwc,Xe,AL,SD,Vfx,TSS,Aw,Cbs,dbs)

  real :: kwt
  real :: Fwc
  real :: Xe
  real :: AL
  real :: SD
  real :: Vfx
  real :: TSS
  real :: Aw
  real :: Cbs
  real :: dbs

  real :: temp_p,temp_q,temp_r

  temp_p = 1 - Fwc

  temp_q = (Xe * AL * SD * 1000 - Vfx * TSS) / (Aw * TSS)

  temp_r = TSS / (Cbs * dbs * 1e6)

  kwt    = temp_p * temp_q * temp_r

end subroutine kwt_calculate

!-------------------------------------------------------------------------------------------

subroutine vfx_calculate(Vfx,Aw,RO)

  real :: Vfx
  real :: Aw
  real :: RO

  Vfx = Aw * RO / 100

end subroutine vfx_calculate

!-------------------------------------------------------------------------------------------

subroutine rp_calculate(Rp,gamma_j,Yp)

  real :: Rp
  real :: gamma_j
  real :: Yp

  real :: temp_s

  temp_s = - gamma_j * Yp

  Rp = 1 - exp(temp_s)

end subroutine rp_calculate

end module apr_calculate
