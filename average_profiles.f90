SUBROUTINE averageProfiles

    USE headerFile
    IMPLICIT NONE

    !!!!TO CHECK!!!
    !If we are using boxes then need to average out per box (density = n/area)
    IF (profileType == 1) THEN
        DO i=1,numPlanesX
            densityProfilesX(i) = densityProfilesX(i)/(lWidth*(lHeight/numPlanesX))
            densityProfilesX(i) = densityProfilesX(i)/nts
            massFluxX(i) = 0 - massFluxX(i)/lWidth
            massFluxX(i) = 0 - massFluxX(i)/nts
        END DO

        DO i=1,numPlanesY
            densityProfilesY(i) = densityProfilesY(i)/lHeight*(lWidth/numPlanesY)
            densityProfilesY(i) = densityProfilesY(i)/nts
            massFluxY(i) =  massFluxY(i)/lWidth
            massFluxY(i) =  massFluxY(i)/nts 
        END DO

    ELSE IF (profileType == 0) THEN

        !!!X!!!
        DO i=1,numPlanesX
            densityProfilesX(i) = densityProfilesX(i)/lHeight*lWidth
            densityProfilesX(i) = densityProfilesX(i)/nts
            massFluxX(i) = 0 - massFluxX(i)/lHeight*lWidth
            massFluxX(i) = 0 - massFluxX(i)/nts
        END DO

        !!!!Y!!!
        DO i=1,numPlanesY
            densityProfilesY(i) = densityProfilesY(i)/lWidth*lHeight
            densityProfilesY(i) = densityProfilesY(i)/nts
            massFluxY(i) = massFluxY(i)/lWidth*lHeight
            massFluxY(i) = massFluxY(i)/nts 
        END DO

    END IF


END SUBROUTINE