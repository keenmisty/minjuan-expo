program driver

  ! 
  ! Through MODULE_IO, we access most of our top-level input/output routines.
  ! 

  use module_io

  !
  ! Use module_emam for Environmental Media Accumulation Model
  !
  
  use module_emam

  !
  ! Use module_hem for 
  !

  use module_hem

  !
  ! use module_apr for
  !

  use module_apr

  !-----------------------------------------------------------------------------------------
  
  implicit none

  !
  ! Command-Line:
  !

  character(len=256) :: executable_name   ! The name of the executable, as found by Fortran library 
  !                                       ! function GETARG

  character(len=256) :: forcing_filename  ! The name of the initial/forcing conditions file, as found
  !                                       ! by Fortran library function GETARG

  character(len=1024) :: output_dir       ! Output directory to which to write results.
  !

  !-----------------------------------------------------------------------------------------

  !
  ! Get the command-line arguments
  !

  call getarg(0, executable_name)
  call getarg(1, forcing_filename)

  if (forcing_filename == " ") then
     write(*,'(/," ***** Problem:  Program expects a command-line argument *****")')
     write(*,'(" ***** Please specify the forcing filename on the command-line.")')
     write(*,'(" ***** E.g.:  ''",A,1x,A,"''",/)') trim(executable_name), "bondville.dat"
     stop ":  ERROR EXIT"
  endif

end program driver
