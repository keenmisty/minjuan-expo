MODULE module_model_constants

!  2. Following are constants for use in defining real number bounds.

!  A really small number.

   REAL    , PARAMETER :: epsilon         = 1.E-15

!  4. Following is information related to the physical constants.

!  These are the physical constants used within the model.

   REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

#if 
      REAL    , PARAMETER :: r_d          = 287.04
      REAL    , PARAMETER :: cp           = 1004.6
#else
      REAL    , PARAMETER :: r_d          = 287.
      REAL    , PARAMETER :: cp           = 7.*r_d/2.
#endif

      REAL    , PARAMETER :: dyd          = 56.56 !yearly dry depostion(ug/m2/yr)
      REAL    , PARAMETER :: dyw          = 124.1 !yearly wet deposition(ug/m2/yr)
      REAL    , PARAMETER :: dtot         = 180.66!yearly total dep.(ug/m2/yr)
      REAL    , PARAMETER :: P            = 178.14!average annual precipitation
      REAL    , PARAMETER :: ev           = 162.65!average annual evap.(cm/yr)
      REAL    , PARAMETER :: ta           = 301.5 !surface ambient air T (K)
      REAL    , PARAMETER :: I            = 117.11!average annual irrigation
      REAL    , PARAMETER :: ro           = 98.3  !average annual surface runoff

      REAL    , PARAMETER :: thetabs      = 0.6
      REAL    , PARAMETER :: cbs          = 1
      REAL    , PARAMETER :: tss          = 10
	  
      REAL    , PARAMETER :: xe           = 100
      REAL    , PARAMETER :: sd           = 0.2
      REAL    , PARAMETER :: er           = 2
      REAL    , PARAMETER :: dbs          = 0.02
      REAL    , PARAMETER :: dwe          = 11.6
      REAL    , PARAMETER :: dz           = 11.62
	  
      REAL    , PARAMETER :: zs           = 17.91
	  REAL    , PARAMETER :: thetasw      = 0.2
	  REAL    , PARAMETER :: bd           = 1.37
	  REAL    , PARAMETER :: rousoil      = 2.7
	  


CONTAINS
SUBROUTINE init_module_model_constants
END SUBROUTINE init_module_model_constants
END MODULE module_model_constants

