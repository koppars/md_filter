SUBROUTINE randomNumber
!Random number generator taken from Hoover

    USE headerFile
    IMPLICIT NONE
    INTEGER :: iRand, jRand

    iRand = 1029*intx + 1731
    jRand = iRand +1029*inty + 507*intx - 1731
    intx = mod(iRand,2048)
    jRand = jRand + (iRand-intx)/2048
    inty = mod(jRand,2048)
    randNum = (intx + 2048*inty)/4194304.0d00

END SUBROUTINE