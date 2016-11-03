SUBROUTINE createProfilePlanes

    USE headerFile
    IMPLICIT NONE

        !If not filtering, then planes go on the top cell
        IF (filter == 0 ) THEN
            !SPAM AVERAGING
            IF (profileType == 0) THEN

                !!!!!X!!!!!
                !Set array to 0 and find the gap between each plane
                profileHeightsX = 0
                numberOfPlanesRealX = REAL(numPlanesX)
                planesGap = lHeight/numberOfPlanesRealX

                !WRITE(*,*) 'planesGap: ', planesGap 

                !Set the first and last plane
                profileHeightsX(1) = 0.0 + (lHeight/2.0) - planesGap/2.0
                profileHeightsX(numPlanesX) = 0.0 - (lHeight/2.0) + planesGap/2.0

                !Set the rest
                DO i=2,numPlanesX-1
                    profileHeightsX(i) = profileHeightsX(i-1) - planesGap
                END DO

                !!!!!Y!!!!!
                !Set array to 0 and find the gap between planes
                profileHeightsY = 0
                numberOfPlanesRealY = REAL(numPlanesY)
                planesGap = lWidth/numberOfPlanesRealY

                !WRITE(*,*) 'planesGap: ', planesGap 

                !Set the first and last plane
                profileHeightsY(1) = 0.0 + (lWidth/2.0) - planesGap/2.0
                profileHeightsY(numPlanesY) = 0.0 - (lWidth/2.0) + planesGap/2.0

                !Set the rest
                DO i=2,numPlanesY-1 
                    profileHeightsY(i) = profileHeightsY(i-1)  - planesGap
                END DO

            !BOXES
            ELSE
                !!!TO DO!!!!
            END IF      

        !If we are filtering, the x-profile planes need to be in the filter cell (below the equib cell)
        ELSE IF (filter == 1 ) THEN
            !SPAM AVERAGING
            IF (profileType == 0) THEN

                !!!!!X!!!!!
                !Set array to 0 and find the gap between each plane
                profileHeightsX = 0
                numberOfPlanesRealX = REAL(numPlanesX)
                planesGap = fHeight/numberOfPlanesRealX

                !WRITE(*,*) 'planesGap: ', planesGap 

                !Set the first and last plane
                profileHeightsX(1) = 0.0 - (lHeight/2.0) - planesGap/2.0
                profileHeightsX(numPlanesX) = 0.0 - (lHeight/2.0) - fHeight + planesGap/2.0

                !Set the rest
                DO i=2,numPlanesX-1
                    profileHeightsX(i) = profileHeightsX(i-1) - planesGap
                END DO

                !!!!!Y!!!!!
                !Set array to 0 and find the gap between planes
                profileHeightsY = 0
                numberOfPlanesRealY = REAL(numPlanesY)
                planesGap = lWidth/numberOfPlanesRealY

                !WRITE(*,*) 'planesGap: ', planesGap 

                !Set the first and last plane
                profileHeightsY(1) = 0.0 + (lWidth/2.0) - planesGap/2.0
                profileHeightsY(numPlanesY) = 0.0 - (lWidth/2.0) + planesGap/2.0

                !Set the rest
                DO i=2,numPlanesY-1 
                    profileHeightsY(i) = profileHeightsY(i-1)  - planesGap
                END DO

            !BOXES
            ELSE
                !!!TO DO!!!!
            END IF  
        END IF

       !WRITE(*,*) 'lHeight: ', lheight      
       !WRITE(*,*) 'xPlanes: ', profileHeightsX

       !WRITE(*,*) 'lWidth: ', lWidth
       !WRITE(*,*) 'yPlanes: ', profileHeightsY 

END SUBROUTINE