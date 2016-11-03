SUBROUTINE calculateEnergies
!Calculate the energies at each time step

    USE headerFile
    IMPLICIT NONE

    embeddedAtomPotentialEnergy = 0.0
    potentialEnergy = 0.0
    kineticEnergy = 0.0
    totalEnergy = 0.0

    IF (fluidPotential == 2 .or. fluidPotential == 3) THEN
        CALL density(x,y)
    END IF
    
    !Core repulsive
    IF (fluidPotential == 1 .or. fluidPotential == 3) THEN
        DO i=1, nop
            !MD       
            DO j=1, nop
                IF (i /= j) THEN

                    ix = x(1,i)
                    iy = y(1,i)

                    jx = x(1,j)
                    jy = y(1,j)

                    xDiff = ix - jx
                    yDiff = iy - jy 

                    !Account for periodic boundary conditions
                    IF (topBoundary == 1) THEN                            
                        yDiff = yDiff - lHeight*NINT(yDiff/lHeight)
                    END IF
                    IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
                        xDiff = xDiff - lWidth*NINT(xDiff/lWidth)
                    END IF

                    r = DSQRT(xDiff**2 + yDiff**2)    

                    !MD
                    IF (r < 1) THEN
                        potentialEnergy = potentialEnergy + (1.0*((1.0-r**2)**4)) !Calculate potential energy     
                    END IF
                END IF                                 
            END DO        
        END DO
    END IF

    !Embedded atom
    IF (fluidPotential == 2 .or. fluidPotential == 3) THEN
        DO i=1, nop              
            embeddedAtomPotentialEnergy = embeddedAtomPotentialEnergy + (((densities(i)/p0)-1.0)**2)
        END DO            
    END IF

    !Sum the energies
    potentialEnergy = potentialEnergy/2.0

    !Potential energy with scatterers
    IF (filter == 1 .and. timeStep > equilibriumSteps) THEN
        DO i=1, nop
            DO j=1, nos
                
                ix = x(1,i)
                iy = y(1,i)

                jx = scatterersX(j)
                jy = scatterersY(j)

                xDiff = ix - jx
                yDiff = iy - jy

                IF (topBoundary == 1) THEN
                    yDiff = yDiff - lHeight*NINT(yDiff/lHeight)
                END IF

                IF (nonEquilibrium /= 1 .AND. sideBoundary == 1) THEN
                    xDiff = xDiff - lWidth*NINT(xDiff/lWidth)
                END IF

                r = DSQRT(xDiff**2 + yDiff**2)

                IF (r < 1) THEN

                    !Core repulsive
                    IF (filterPotential == 1) THEN
                        potentialEnergy = potentialEnergy + (1.0*((1.0-r**2)**4)) 
                    END IF

                END IF
            END DO
        END DO
    END IF

    !Add gravitational potential energy
    !MGH, M=1
    DO i=1, nop
        potentialEnergy = potentialEnergy + (((lHeight/2.0)+y(1,i))*(-1*g))
    END DO

    DO i=1, nop
        kineticEnergy = kineticEnergy + (0.5*((x(2,i)**2) + (y(2,i)**2)))
    END DO

    totalEnergy = potentialEnergy + kineticEnergy

END SUBROUTINE