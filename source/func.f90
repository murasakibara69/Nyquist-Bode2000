module func
  
  implicit none
  
contains
  
  complex pure function W(p, a, b, c, d) result(Wp)
    
    implicit none
    
    complex, intent(in) :: p
    real, intent(in) :: a, b, c, d
    
    Wp = d / (a * p ** 2 + b * p + c) 
    
  end function
  
end module func
