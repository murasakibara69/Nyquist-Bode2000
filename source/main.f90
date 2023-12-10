program main
  
  use, intrinsic :: iso_fortran_env, only: stderror => error_unit
  use, non_intrinsic :: func, only: W
  use, non_intrinsic :: nyquist_and_bode, only: Nyquist, Bode
  
  implicit none
  
  real :: a, b, c, d, omega_max
  namelist /params/ a, b, c, d, omega_max
  
  integer :: ios, command_line_length
  character(len=:), allocatable :: command_line
  character(len=255) :: message
  
  a = 2.0; b = 12.0; c = 2.1; d = 3.0; omega_max = 1000.0
  
  call get_command(length = command_line_length)
  allocate(character(len=command_line_length) :: command_line)
  call get_command(command = command_line)
  command_line = adjustl(command_line) // ' '
  command_line = command_line(index(command_line, ' '):)
  command_line = "&params " // command_line // " /"
  read(command_line, nml = params, iostat = ios, iomsg = message)
  if (ios .ne. 0) then
    write(unit = stderror, fmt = "(I0)") ios
    write(unit = stderror, fmt = "(A)") trim(message)
    stop
  end if
  
  call Nyquist(a, b, c, d)
  call Bode(a, b, c, d, omega_max)
  
end program main
