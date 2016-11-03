SUBROUTINE profiles
!Calculate properties for profiles

    USE headerFile
    IMPLICIT NONE

    REAL(kind = double) :: topOfBox, bottomOfBox, leftOfBox, rightOfBox !Box method profile

    !SPAM AVERAGING
    IF (profileType == 0) THEN

        !!!!Y!!!!!
        DO i=1,numPlanesY    

            !loop over all particles
            DO j=1,nop        

                densityProfileR = 0
                densityProfileW = 0

                !Calculate distance from plane and make +ve
                densityProfileR = x(1,j) - profileHeightsY(i)
                IF (densityProfileR < 0.0) THEN
                    densityProfileR = -densityProfileR
                END IF

                !Calculate the weight
                IF (densityProfileR < h) THEN
                    CALL LucyOneD
                    densityProfilesY(i) = densityProfilesY(i) + densityProfileW
                    massFluxY(i) = massFluxY(i) + (x(2,j)*densityProfileW)
                END IF      
            END DO     
            
        END DO

        !!!!X!!!!
        DO i=1,numPlanesX    

            !loop over all particles
            DO j=1,nop        

                densityProfileR = 0
                densityProfileW = 0

                !Calculate distance from point and make +ve
                densityProfileR = y(1,j) - profileHeightsX(i)
                IF (densityProfileR < 0.0) THEN
                    densityProfileR = -densityProfileR
                END IF

                !Calculate the weight
                IF (densityProfileR < h) THEN
                    CALL LucyOneD
                    densityProfilesX(i) = densityProfilesX(i) + densityProfileW
                    massFluxX(i) = massFluxX(i) + (y(2,j)*densityProfileW)
                END IF      
            END DO     
            
        END DO

    !BOX METHOD
    ELSE            

        !!!TO DO !!!

    END IF

END SUBROUTINE