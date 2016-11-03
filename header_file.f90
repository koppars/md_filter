MODULE headerFile
!Declare the shared variables

    IMPLICIT NONE
    PUBLIC
    SAVE

    !Parameters
    INTEGER, PARAMETER :: double = SELECTED_REAL_KIND(15,99)    
    REAL(kind = double),PARAMETER :: PI = 3.141592653589793d0

    !Integers
    INTEGER :: timeStep, i, j, err, intx, inty
    INTEGER :: nts,nop,nopPerRow,initaliseVels,nonEquilibrium,tau,topBoundary, sideBoundary,loadCoordinates,shape
    INTEGER :: nos,filter,equilibriumSteps,numPlanesX,numPlanesY,profileType,fluidPotential,filterPotential
    INTEGER :: iScreen, iAnimate, iResults

    !Reals
    REAL(kind = double) :: randNum, stepSize
    REAL(kind = double) :: dt, lWidth, lHeight, h, b0, p0, g, r, surften, w, wp, wp2, fWidth, fHeight,densityOfScatterers
    REAL(kind = double) :: kineticEnergy, potentialEnergy, totalEnergy, embeddedAtomPotentialEnergy !Energies
    REAL(kind = double) :: ix,iy,jx,jy,xDiff,yDiff, planesGap !Distance calculations
    REAL(kind = double) :: densityProfileH, densityProfileR, densityProfileW, rOverH, numberOfPlanesRealX, numberOfPlanesRealY !Density profile    
    REAL(kind = double) :: xFinal, yFinal, xvFinal, yvFinal !Final positions and velocities

    !Arrays
    REAL(kind = double), DIMENSION(4) :: unitCellXPositions, unitCellYPositions !Unit cell positions
    REAL(kind = double), DIMENSION(:,:), ALLOCATABLE :: x, y !Locations and velocities
    REAL(kind = double), DIMENSION(:), ALLOCATABLE :: scatterersX, scatterersY !Scatter locations
    REAL(kind = double), DIMENSION(:), ALLOCATABLE :: densityProfilesX,massFluxX,profileHeightsX !Profiles X
    REAL(kind = double), DIMENSION(:), ALLOCATABLE :: densityProfilesY,massFluxY,profileHeightsY !Profiles Y
    REAL(kind = double), DIMENSION(:), ALLOCATABLE :: densities !Embedded atom densities

    !Namelist
    NAMELIST / inputDeck/dt,nts,nop,fluidPotential,filterPotential,h,b0,p0,g,initaliseVels,nonEquilibrium,&
    &tau, topBoundary, sideBoundary,loadCoordinates,shape,surften,nos,densityOfScatterers,filter,equilibriumSteps,&
    &numPlanesX,numPlanesY,profileType,iScreen,iAnimate,iResults

END MODULE headerFile