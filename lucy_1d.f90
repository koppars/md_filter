SUBROUTINE LucyOneD
! Calculates the Lucy weight function

    USE headerFile
    IMPLICIT NONE

    rOverH = densityProfileR/h
    densityProfileW = (5.0/(4*h)*(1.0+(3.0*rOverH))*((1.0-rOverH)**3))

END SUBROUTINE