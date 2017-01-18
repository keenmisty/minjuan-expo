MODULE module_model_constants

!  2. Following are constants for use in defining real number bounds.

!  A really small number.

   REAL    , PARAMETER :: epsilon         = 1.E-15

!  4. Following is information related to the physical constants.

!  These are the physical constants used within the model.

   REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

#if ( NMM_CORE == 1 )
      REAL    , PARAMETER :: r_d          = 287.04
      REAL    , PARAMETER :: cp           = 1004.6
#else
      REAL    , PARAMETER :: r_d          = 287.
      REAL    , PARAMETER :: cp           = 7.*r_d/2.
#endif



CONTAINS
SUBROUTINE init_module_model_constants
END SUBROUTINE init_module_model_constants
END MODULE module_model_constants

