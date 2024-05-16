       program swinnerton_dyer
         implicit none
       real*8 :: x, y
       real*8 :: a, b

       ! Coefficients of the elliptic curve y^2 = x^3 + ax + b
        a = -1.0d0
        b = +1.0d0

     ! Example point (x, y) to check if it lies on the elliptic curve
       x = 1.0d0
       y = 0.0d0

       if (is_on_curve(x, y, a, b)) then
       print *, "The point (", x, ",", y, ") lies on the"
       print *,"elliptic curve y^2 = x^3 + ", a, "x + ", b
       else
       print *, "The point (", x, ",", y, ") does not"
       print *,"lie on the elliptic curve y^2 = x^3 + ", a, "x + ", b
       end if

       contains

       logical function is_on_curve(x, y, a, b)
       real*8, intent(in) :: x, y, a, b
       real*8 :: lhs, rhs

        lhs = y**2
        rhs = x**3 + a * x + b

        if (abs(lhs - rhs) < 1.0d-10) then
        is_on_curve = .true.
        else
        is_on_curve = .false.
        end if
        end function is_on_curve

        end program swinnerton_dyer
 
