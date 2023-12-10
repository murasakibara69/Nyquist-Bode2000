module nyquist_and_bode
  
  use, non_intrinsic :: func
  
  implicit none
  
contains
  
  subroutine Nyquist(a, b, c, d)
    
    implicit none
    
    real, intent(in) :: a, b, c, d 
    
    integer, parameter :: N = 100001
    real, parameter :: omega_min = -500.0, omega_max = 500.0
    integer :: i, io, uNyquist
    real :: h, omega
    complex :: Wp
    
    h = (omega_max - omega_min) / real(N - 1)
    omega = omega_min
    open(newunit = uNyquist, file = "Nyquist.dat", status = "new", action = "write")
    do i = 1, N
      omega = omega + h
      Wp = W(complex(0.0, omega), a, b, c, d)
      write(unit = uNyquist, fmt = *) real(Wp), aimag(Wp)
    end do
    open(newunit = io, file = "Nyquist.plt", status = "replace", action = "write")
    write(unit = io, fmt = "(A)") 'set xlabel "Re[W(0, i{/Symbol w})]"'
    write(unit = io, fmt = "(A)") 'set ylabel "Im[W(0, i{/Symbol w})]"'
    write(unit = io, fmt = "(A)") 'set grid'
    write(unit = io, fmt = "(A)") 'set nokey'
    write(unit = io, fmt = "(A)") 'set terminal pngcairo size 1200, 800'
    write(unit = io, fmt = "(A)") 'set output "Nyquist stability criterion.png"'
    write(unit = io, fmt = "(A)") 'plot "Nyquist.dat" u 1:2 w l lt 1 lw 2'
    close(unit = io, status = "keep")
    close(unit = uNyquist, status = "keep")
    call execute_command_line("gnuplot -p Nyquist.plt")
    call execute_command_line("rm Nyquist.plt Nyquist.dat")
    
  end subroutine Nyquist
  
  subroutine Bode(a, b, c, d, omega_max)
    
    implicit none
    
    real, intent(in) :: a, b, c, d, omega_max
    
    integer, parameter :: N = 100001
    real, parameter :: degrees_per_radian = 45.0 / atan(1.0)
    integer :: i, io, uBode
    real :: h, omega, argWp
    complex :: Wp
    
    h = omega_max / real(N - 1)
    open(newunit = uBode, file = "Bode.dat", status = "new", action = "write")
    do i = 1, N
      omega = h * real(i - 1)
      Wp = W(complex(0.0, omega), a, b, c, d)
      argWp = atan2(aimag(Wp), real(Wp))
      write(unit = uBode, fmt = *) log10(omega), 20.0 * log10(abs(Wp)), degrees_per_radian * argWp
    end do
    open(newunit = io, file = "Bode.plt", status = "new", action = "write")
    write(unit = io, fmt = "(A)") 'set grid'
    write(unit = io, fmt = "(A)") 'unset xlabel'
    write(unit = io, fmt = "(A)") 'set terminal pngcairo size 1200, 800'
    write(unit = io, fmt = "(A)") 'set output "Bode plot.png"'
    write(unit = io, fmt = "(A)") 'set multiplot layout 2,1 rowsfirst'
    write(unit = io, fmt = "(A)") 'set ylabel "Magnitude (dB)"'
    write(unit = io, fmt = "(A)") 'plot "Bode.dat" u 1:2 w l lt 1 lw 2 title "Bode magnitude plot"'
    write(unit = io, fmt = "(A)") 'set xlabel "Frequency"'
    write(unit = io, fmt = "(A)") 'set ylabel "Phase (Degrees)"'
    write(unit = io, fmt = "(A)") 'plot "Bode.dat" u 1:3 w l lc rgb ' // "'#0060ad'" // ' lt 1 lw 2 title "Bode phase plot"'
    close(unit = io, status = "keep") ! close Bode.plt
    close(unit = uBode, status = "keep") ! close Bode.dat
    call execute_command_line("gnuplot -p Bode.plt")
    call execute_command_line("rm Bode.plt Bode.dat")
    
  end subroutine Bode
  
end module nyquist_and_bode
