SUBROUTINE writeResultsToFiles
!Writes the final coordinates, velocites and the profiles to files

    USE headerFile
    IMPLICIT NONE       

    !WRITE THE FINAL COORDINATES AND VELOCITIES OUT TO FILE
    OPEN(UNIT = 21, FILE = 'coordinates.txt',&
        &FORM = 'FORMATTED', STATUS = 'REPLACE',ACTION = 'WRITE')
    DO i=1,nop
        xFinal = x(1,i)
        yFinal = y(1,i)
        xvFinal = x(2,i)
        yvFinal = y(2,i)
        WRITE(21,*) xFinal,',',yFinal,',',xvFinal,',',yvFinal
    END DO

    !Write out the profiles to file too
    DO i=1,numPlanesX
        WRITE(4,*) profileHeightsX(i), ' ', densityProfilesX(i)  
        WRITE(5,*) profileHeightsX(i), ' ', massFluxX(i)
    END DO
    DO i=1,numPlanesY
        WRITE(12,*) profileHeightsY(i), ' ', densityProfilesY(i)  
        WRITE(13,*) profileHeightsY(i), ' ', massFluxY(i)  
    END DO


END SUBROUTINE