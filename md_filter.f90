PROGRAM spamSimulation
!MRK 01/02/16 Program to simulate simple an MD filter.
!Main simulation program

    USE headerFile
    IMPLICIT NONE
        
    !Initialise distance stuff
    ix = 0.0
    iy = 0.0
    jx = 0.0
    jy = 0.0
    r = 0.0
    xDiff = 0.0
    yDiff = 0.0
    randNum = 0.0

    !First read the input file, initialise the velocities, and the locations
    WRITE(*,*) 'Reading input data'
    CALL readInputData
    WRITE(*,*) 'Done'

    !Allocate all of the arrays
    ALLOCATE(x(2,nop))
    ALLOCATE(y(2,nop))
    ALLOCATE(densities(nop))
    ALLOCATE(densityProfilesX(numPlanesX))    
    ALLOCATE(densityProfilesY(numPlanesY))
    ALLOCATE(massFluxY(numPlanesY))
    ALLOCATE(massFluxX(numPlanesX))
    ALLOCATE(profileHeightsX(numPlanesX))
    ALLOCATE(profileHeightsY(numPlanesY))
    ALLOCATE(scatterersX(nos))
    ALLOCATE(scatterersY(nos))

    !Initialise the unit cell
    WRITE(*,*) 'Initialising unit cell'
    CALL initialiseUnitCell
    WRITE(*,*) 'Done'

    !Load in coordinates or create them new
    IF (loadCoordinates /= 1) THEN
        WRITE(*,*) 'Initialising positions'
        CALL initialisePositions
        WRITE(*,*) 'Done'

        WRITE(*,*) 'Initialising velocities'
        CALL initialiseVelocities
        WRITE(*,*) 'Done'
    ELSE        
        WRITE(*,*)'Reading in coordiantes'
        OPEN(UNIT = 20, FILE = 'coordinates.txt')
        DO i=1,nop
            READ(20,*)x(1,i),y(1,i),x(2,i),y(2,i)
            WRITE(*,*) x(1,i),',',y(1,i),',',x(2,i),',',y(2,i)
        END DO
        CLOSE(20)
    END IF

    !Create scatterers
    IF (nos /= 0) THEN
        WRITE(*,*) 'Creating scatterers'
        CALL createScatterers
    END IF

    !Create the results files
    IF (loadCoordinates == 1) THEN
        OPEN(2, FILE = 'resultsAnimation.xyz',ACCESS = 'APPEND')
    ELSE
        OPEN(2, FILE = 'resultsAnimation.xyz')
        !If we are equilibriating first, then create a separate video file for that section
        IF (filter == 1) THEN
            OPEN(16, FILE = 'equilbrium.xyz')
        END IF
    END IF

    !Create the other files
    OPEN(3, FILE = 'resultsData.txt')

    !Create profiles files
   !IF (profileType == 0) THEN
   !    OPEN(4, FILE = 'denistyProfileSPAMAveragingX.txt')
   !    OPEN(5, FILE = 'massFluxSPAMAveragingX.txt')
   !    OPEN(12, FILE = 'densityProfileSPAMAveragingY.txt')
   !    OPEN(13, FILE = 'massFluxSPAMAveragingY.txt')
   !ELSE IF (profileType == 1) THEN
   !    OPEN(8, FILE = 'denistyProfileBoxesX.txt')
   !    OPEN(9, FILE = 'massFluxBoxesX.txt')
   !    OPEN(14, FILE = 'denistyProfileBoxesY.txt')
   !    OPEN(15, FILE = 'massFluxBoxesY.txt')
   !END IF
    OPEN(4, FILE = 'denistyProfileX.txt')
    OPEN(5, FILE = 'massFluxX.txt')
    OPEN(12, FILE = 'densityY.txt')
    OPEN(13, FILE = 'massFluxY.txt')

    !Set up animation file
    IF (loadCoordinates /= 1) THEN
        WRITE(2,*) nop
        WRITE(2,*) 'Test comment'

        IF (filter == 1) THEN
            WRITE(16,*) nop
            WRITE(16,*) 'Test comment'
        END IF
    END IF

    !Set up data file
    WRITE(3,*)'Total Energy,', ' Kinetic Energy,', ' Potential Energy'

    !Calculate the profile boxes/planes 
    WRITE(*,*)'Creating profile plane heights'

    CALL createProfilePlanes

    densityProfilesX = 0
    massFluxX = 0

    densityProfilesY = 0
    massFluxY = 0

    ! start the simulation
    WRITE(*,*) '*****Starting Simulation*****'

    timeStep = 1

    DO WHILE (timeStep <= nts)

        !Move particles
        CALL RK4

        !Check locations
        CALL boundaryConditions     

        !Record coordinates for animation file
        IF (MOD(timestep,iAnimate).EQ.0) THEN
            IF (filter == 1) THEN
                DO i=1, nop
                    IF (timestep > equilibriumSteps) THEN
                        WRITE(2,*) 'h ', x(1,i), ' ', y(1,i), ' 0'

                    ELSE
                        WRITE(16,*) 'h ', x(1,i), ' ', y(1,i), ' 0'
                    END IF
                END DO
            ELSE
                DO i=1, nop
                    WRITE(2,*) 'h ', x(1,i), ' ', y(1,i), ' 0'
                END DO
            END IF
        END IF

        !Calculate the energies
        CALL calculateEnergies

        !Write out to screen to check
        IF(MOD(timeStep,iScreen).EQ.0) THEN
            WRITE(*,*) 'Timestep: ', timeStep, 'Total Energy = ', totalEnergy
        END IF

        !Write them out to files
        IF(MOD(timestep,iResults).EQ.0) THEN
            WRITE(3,*)totalEnergy, ', ', kineticEnergy, ', ', potentialEnergy
        END IF

        !Calculate properties for profiles
        IF (filter == 1 .and. timeStep > equilibriumSteps) THEN
            CALL profiles
        END IF

        IF (filter == 0) THEN
            CALL profiles
        END IF

        !Increase the timestep
        timeStep = timeStep + 1      

    END DO

    CALL averageProfiles

    !Write the results to files
    Call writeResultsToFiles

    WRITE(*,*) '*****FINISHED*****'

END PROGRAM