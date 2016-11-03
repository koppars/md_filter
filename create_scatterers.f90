SUBROUTINE createScatterers
!Create the scatterers

    USE headerFile
    IMPLICIT NONE
    INTEGER :: nosCreated,numberOfRows,nosCreatedOnThisRow,numberOfRowsCreated,nosPerColumn,nosPerRow

    numberOfRowsCreated = 0
    nosCreated = 0
    nosPerRow = (nos**0.5)/2.0
    nosPerColumn = nos/nosPerRow    

    !Calculate the filter dimensions. The width is the same as the cell above, the height is determined by the density
    !Column
    fWidth = lWidth
    fHeight = nos/(densityOfScatterers*fWidth)

    WRITE(*,*) 'fWidth: ', fWidth
    WRITE(*,*) 'fHeight: ', fHeight

    DO WHILE (nosCreated < nos)

        nosCreatedOnThisRow = 0

        DO WHILE (nosCreatedOnThisRow < nosPerRow)

            scatterersX(nosCreated+1) = 0.0 - (fWidth/2.0) + (fWidth/(nosPerRow*2.0))&
            & + ((fWidth/nosPerRow)*nosCreatedOnThisRow)

            scatterersY(nosCreated+1) = 0.0 - (lHeight/2.0) - (fHeight/2.0) + (fHeight/2.0)&
            & - (fHeight/(nosPerColumn*2.0)) - ((fHeight/nosPerColumn)*numberOfRowsCreated)


            nosCreatedOnThisRow = nosCreatedOnThisRow + 1
            nosCreated = nosCreated + 1

        END DO

        numberOfRowsCreated = numberOfRowsCreated + 1

    END DO

    !Write them to file
    OPEN(UNIT = 25, FILE = 'scatterers.xyz')
    WRITE(25,*)nos
    WRITE(25,*)'comment'
    DO i=1,nos
        WRITE(25,*)'h',scatterersX(i),scatterersY(i), '0.0'
    END DO
    WRITE(*,*) 'Finished Creating scatterers'

END SUBROUTINE