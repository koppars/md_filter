SUBROUTINE Lucy
! Calculates the Lucy weight function
    USE headerFile
    IMPLICIT NONE

    rOverH = r/h
    w = (5.0/(PI*(h**2)))*(1.0+(3.0*rOverH))*((1.0-rOverH)**3)
    wp=((15.0/(pi*h**3))*(1.0-rOverH)**3)-((15.0/(pi*h**3))*(1.0+(3.0*rOverH))*(1.0-rOverH)**2)
    wp2= (-60.0D0/(PI*h**4.0)) * (1.0D0 - 3.0D0*rOverH) * (1.0D0 - rOverH)

END SUBROUTINE	