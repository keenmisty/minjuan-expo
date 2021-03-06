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
  real :: ta            ! Surface ambient air temperature (K)
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

  real :: ksr           ! Soil Hg loss constant (year-1)
  real :: ro            ! Average annual surface runoff (cm/year)
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil)
  real :: zs            ! Soil mixing zone depth (cm)
  real :: kds           ! Suspended sediments/surface water partition coefficient
  real :: bd            ! Soil bulk density (g soil/cm3 soil)

  real :: temp_f,temp_g

  temp_f = bd * kds / thetasw

  temp_g = thetasw * zs * (1 + temp_f)

  ksr    = ro / temp_g

end subroutine ksr_calculate

!-------------------------------------------------------------------------------------------

subroutine kse_calculate(kse,Xe,SD,ER,bd,zs,kds,thetasw)

  real :: kse           ! Soil Hg loss constant (year-1)
  real :: Xe            ! Unit soil loss (kg/m 2 -year -1)
  real :: SD            ! Sediment delivery ratio (watershed)
  real :: ER            ! Soil enrichment ratio
  real :: bd            ! Soil bulk density (g soil/cm3 soil)
  real :: zs            ! Soil mixing zone depth (cm)
  real :: kds           ! Suspended sediments/surface water partition coefficient
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil)

  real :: temp_h,temp_i

  temp_h = 0.1 * Xe * SD * ER / (bd * zs)

  temp_i = kds * bd / (kds * bd + thetasw)

  kse    = temp_h * temp_i

end subroutine kse_calculate

!-------------------------------------------------------------------------------------------

subroutine ldep_calculate(ldep,dtot,aw)

  real :: ldep          ! Deposition Load (g/year)
  real :: dtot          ! Yearly total deposition(ug/m2/year)
  real :: aw            ! Water Body Surface (m2)

  ldep = dtot * aw * 1e-6

end subroutine ldep_calculate

!-------------------------------------------------------------------------------------------

subroutine li_calculate(li,dtot,ai)

  real :: li            ! Runoff load from impervious surface (g/year)
  real :: dtot          ! Yearly total deposition (ug/m2/year)
  real :: ai            ! Impervious watershed area (m2)

  li   = dtot * ai * 1e-6

end subroutine li_calculate

!-------------------------------------------------------------------------------------------

subroutine lr_calculate(lr,ro,al,ai,cs,bd,thetasw,kds)
  
  real :: lr            ! Runoff load from previous surface (g/year)
  real :: ro            ! Average annual surface runoff (cm/year) 
  real :: al            ! Total Watershed Area (m2)
  real :: ai            ! Impervious Watershed Area (m2)
  real :: cs            ! Soil Concentration due to Deposition (mg/kg)
  real :: bd            ! Soil Bulk Density (g soil/cm3 soil)
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil)
  real :: kds           ! Suspended sediments/surface water partition coefficient

  real :: temp_j,temp_k

  temp_j = cs * bd / (kds * bd + thetasw)

  temp_k = ro * (al - ai)

  lr     = temp_j * temp_k * 0.01

end subroutine lr_calculate

!-------------------------------------------------------------------------------------------

subroutine le_calculate(le,xe,al,ai,sd,er,cs,kds,bd,thetasw)

  real :: Le            ! Soil Erosion Load (g/year)
  real :: Xe            ! Unit soil loss (kg/m 2 -year -1)
  real :: AL            ! Total Watershed Area (m2)
  real :: AI            ! Impervious Watershed Area (m2)
  real :: SD            ! Sediment delivery ratio (watershed)  
  real :: ER            ! Soil enrichment ratio
  real :: Cs            ! Soil Concentration due to Deposition (mg/kg)
  real :: Kds           ! Suspended sediments/surface water partition coefficient
  real :: BD            ! Soil Bulk Density (g soil/cm3 soil)
  real :: thetasw       ! Soil volumetric content (ml water/cm3 soil)

  real :: temp_l,temp_m

  temp_l = Kds * CS * BD / (kds * bd + thetasw)
  
  temp_m = Xe * SD * ER * (AL-AI)

  Le     = temp_l * temp_m * 0.001

end subroutine le_calculate

!-------------------------------------------------------------------------------------------

subroutine fwc_calculate(Fwc,Kdsw,TSS,dwc,dz,thetabs,Kdbs,Cbs,dbs)

  real :: Fwc           ! Fraction of Total Water Body Hg Concentration in Water Column 
  real :: Kdsw          ! Suspended sediments/Surface Water Partition Coefficient
  real :: TSS           ! Total Suspended Solids Concentration (mg/L)
  real :: dwc           ! Depth of Water Column (m)
  real :: dz            ! Total Water Body Depth (m)
  real :: thetabs       ! Bed sediment porosity (l water/l sediment)
  real :: Kdbs   ! Bed sediments/sediment pore water partition coefficient (l water/kg bottom sediment)
  real :: Cbs           ! Bed sediment concentration (g/cm3)
  real :: dbs           ! Depth of upper benthic sediment layer (m)

  real :: temp_n,temp_o

  temp_n = (1 + Kdsw * TSS * 1e-6) * dwc / dz

  temp_o = (thetabs + Kdbs * Cbs) * dbs / dz

  Fwc    = temp_n / (temp_n + temp_o)

end subroutine fwc_calculate

!-------------------------------------------------------------------------------------------

subroutine kwt_calculate(kwt,Fwc,Xe,AL,SD,Vfx,TSS,Aw,Cbs,dbs)

  real :: kwt           ! Overall total water body Hg dissipation rate constant (year-1)
  real :: Fwc           ! Fraction of total water body Hg concentration in water column
  real :: Xe            ! Unit soil loss (kg/m2-year-1)
  real :: AL            ! Total Watershed Area (m2)
  real :: SD            ! Sediment delivery ratio (watershed)
  real :: Vfx           ! Average volume metric flow rate through water body (m3/year)
  real :: TSS           ! Total suspended solids concentration (mg/l)
  real :: Aw            ! Water body surface (m2)
  real :: Cbs           ! Bed sediment concentration (g/cm3)
  real :: dbs           ! Depth of upper benthic sediment layer (m)

  real :: temp_p,temp_q,temp_r

  temp_p = 1 - Fwc

  temp_q = (Xe * AL * SD * 1000 - Vfx * TSS) / (Aw * TSS)

  temp_r = TSS / (Cbs * dbs * 1e6)

  kwt    = temp_p * temp_q * temp_r

end subroutine kwt_calculate

!-------------------------------------------------------------------------------------------

subroutine vfx_calculate(Vfx,Aw,RO)

  real :: Vfx           ! Average volume metric flow rate through water body (m3/year)
  real :: Aw            ! Water body surface (m2)
  real :: RO            ! Average annual surface runoff (cm/year)

  Vfx = Aw * RO / 100

end subroutine vfx_calculate

!-------------------------------------------------------------------------------------------

subroutine rp_calculate(Rp,gamma_j,Yp)

  real :: Rp            ! Interception fraction 
  real :: gamma_j       ! Empirical constants
  real :: Yp            ! Standing biomass of plants (kg/m2)

  real :: temp_s

  temp_s = - gamma_j * Yp

  Rp = 1 - exp(temp_s)

end subroutine rp_calculate

end module apr_calculate
