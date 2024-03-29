!>
!> AUTHOR : GONZALO SAPRIZA
!> DATE CREATION : 2014-07-14
!> DATES MODIFICATIONS : -
!> DESCRIPTION : Save only the prognostic variables needed by CLASS as initial
!>               conditions.
!>
!> The variables saved are:
!> 1)   ALBS        - Snow albedo []
!> 2)   CMAI        - Aggregated mass of vegetation canopy [kg m-2]
!> 3)   GRO         - Vegetation growth index []
!> 4)   QAC         - Spec. Humidity of air within veget canopy space [kg kg-1]
!> 5)   RCAN        - Intercepted liquid water sotred on canopy [kg m-2]
!> 6)   RHOS        - Density of snow [kg m-3]
!> 7)   SCAN/SNCAN  - Intercepted frozen water stored on canopy [kg m-2]
!> 8)   SNO         - Mass of snow pack [kg m-2]
!> 9)   TAC         - Temp of air within veget canopy [K]
!> 10)  TBAR        - Temp of soil layers [k]
!> 11)  TBAS        - Temp of bedrock in third soil layer [K]
!> 12)  TCAN        - Temp veget canopy [K]
!> 13)  THIC        - Vol frozen water conetent of soil layers [m3 m-3]
!> 14)  THLQ        - Vol liquid water conetent of soil layers [m3 m-3]
!> 15)  TPND        - Temp of ponded water [k]
!> 16)  TSFS        - Ground surf temp over subarea [K]
!> 17)  TSNO        - Snowpack temp [K]
!> 18)  WSNO        - Liquid water content of snow pack [kg m-2]
!> 19)  ZPND        - Depth of ponded water on surface [m]
!>
    subroutine save_init_prog_variables_class(fls)

        use model_files_variables
        use sa_mesh_shared_variables

        implicit none

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        integer ierr, iun

        !> Open the resume state file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.runclass36', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

!>    type CLASS_prognostic_variables
!>        real, dimension(:), allocatable :: &
!>            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
!>            TCAN, TPND, TSNO, WSNO, ZPND
!>        real, dimension(:, :), allocatable :: &
!>            TBAR, THIC, THLQ, TSFS
!>    end type

        !> Write the current state of these variables to the file.
        write(iun) stas%sno%albs    !1 (NML)
        write(iun) stas%cnpy%cmai   !2 (NML)
        write(iun) stas%cnpy%gro    !3 (NML)
        write(iun) stas%cnpy%qac    !4 (NML)
        write(iun) stas%cnpy%rcan   !5 (NML)
        write(iun) stas%sno%rhos    !6 (NML)
        write(iun) stas%cnpy%sncan  !7 (NML)
        write(iun) stas%sno%sno     !8 (NML)
        write(iun) stas%cnpy%tac    !9 (NML)
        write(iun) stas%sl%tbar     !10 (NML, IGND)
        write(iun) stas%sl%tbas     !11 (NML)
        write(iun) stas%cnpy%tcan   !12 (NML)
        write(iun) stas%sl%thic     !13 (NML, IGND)
        write(iun) stas%sl%thlq     !14 (NML, IGND)
        write(iun) stas%sfc%tpnd    !15 (NML)
        write(iun) stas%sfc%tsfs    !16 (NML, 4)
        write(iun) stas%sno%tsno    !17 (NML)
        write(iun) stas%sno%wsno    !18 (NML)
        write(iun) stas%sfc%zpnd    !19 (NML)

        !> Close the file to free the unit.
        close(iun)

    end subroutine !save_init_prog_variables_class
