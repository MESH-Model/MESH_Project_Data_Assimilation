!> *********************************************************************
!> Author: Gonzalo Sapriza Azuri
!> Description: handled input and output files in mesh if needed
!>
!> Created on November 17, 2014, 4:12 PM
!> *********************************************************************
module model_files

    implicit none

    contains

    !> *****************************************************************
    !> Description: Initialize fl_ids type reading information from fld
    !> file that is readed when VARIABLEFILEFLAG is activated (=1)
    !> *****************************************************************
    subroutine Init_fls(flg, fld)

        use model_files_variabletypes
        use model_files_variables
        use SIMSTATS_config, only: mtsfl, mtsk, init_metricsout_files
        use WF_ROUTE_config
        use SA_RTE_module, only: SA_RTE_fls, SA_RTE_flkeys, SA_RTE_init_fls

        !> Input variables.
        !* fld: Name of the file that contains file information.
        character(len=*), intent(in), optional :: fld

        !> Input-Output variables.
        !* flg: Object stores file information.
        type(fl_ids), intent(inout) :: flg

        !> Local variables.
        integer ios, i
        logical exists
        character*500 str1, str2, phtfl

		! deallocate the input files 
		if (allocated (flg%fl)) deallocate (flg%fl)
		
        !> Allocate the array that stores file information.
        allocate(flg%fl(14))

        !> Default file names and attributes.
        flg%fl(mfk%f53)%fn = 'MESH_input_run_options.ini'
        flg%fl(mfk%f53)%iun = 53

        flg%fl(mfk%f20)%fn = 'MESH_drainage_database.r2c'
        flg%fl(mfk%f20)%iun = 20

        flg%fl(mfk%f50)%fn = 'MESH_parameters_CLASS.ini'
        flg%fl(mfk%f50)%iun = 50

        flg%fl(mfk%f23)%fn = 'MESH_parameters_hydrology.ini'
        flg%fl(mfk%f23)%iun = 23

        flg%fl(mfk%f52)%fn = 'MESH_input_soil_levels.txt'
        flg%fl(mfk%f52)%iun = 52

        flg%fl(mfk%f54)%fn = 'soil.ini'
        flg%fl(mfk%f54)%iun = 54

        flg%fl(mfk%f18)%fn = 'MESH_ggeo.ini'
        flg%fl(mfk%f18)%iun = 18

        flg%fl(mfk%f900)%fn = 'Basin_average_water_balance.csv'
        flg%fl(mfk%f900)%iun = 900

!todo: remove this
        flg%fl(mfk%f70)%fn = 'MESH_output_streamflow.csv'
        flg%fl(mfk%f70)%iun = 70

        flg%fl(mfk%out_response)%fn = ''
        flg%fl(mfk%out_response)%iun = 444

        flg%fl(mfk%f883)%fn = 'int_statVariables.seq'
        flg%fl(mfk%f883)%iun = 883

        !> For files used by SIMSTATS.
        !> Check if the array to keep file information for the metrics
        !> is allocated.
        if (.not. allocated(mtsfl%fl)) call init_metricsout_files()
        mtsfl%fl(mtsk%fo)%fn   = 'function_out.txt'
        mtsfl%fl(mtsk%MC)%fn   = 'MonteCarlo.txt'
        mtsfl%fl(mtsk%NSE)%fn  = 'NS.txt'
        mtsfl%fl(mtsk%NSW)%fn  = 'NSW.txt'
        mtsfl%fl(mtsk%RMSE)%fn = 'drms.txt'
        mtsfl%fl(mtsk%ABSE)%fn = 'abserr.txt'
        mtsfl%fl(mtsk%out)%fn  = 'Metrics_Out.txt'
        mtsfl%fl(mtsk%PE)%fn   = 'pre_emption_value.txt'

        !> For files used by WF_ROUTE.
        if (.not. allocated(WF_RTE_fls%fl)) call WF_ROUTE_init_fls()
        WF_RTE_fls%fl(WF_RTE_flks%stfl_in)%fn = 'MESH_input_streamflow.txt'
        WF_RTE_fls%fl(WF_RTE_flks%stfl_in)%iun = 22
        WF_RTE_fls%fl(WF_RTE_flks%resv_in)%fn = 'MESH_input_reservoir.txt'
        WF_RTE_fls%fl(WF_RTE_flks%resv_in)%iun = 21
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%fn = 'MESH_output_streamflow.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun = 70
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%fn = 'MESH_output_streamflow_ts.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun = 71
        WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%fn = 'MESH_output_reach_ts.csv'
        WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun = 708

        !> For files used by Standalone RTE.
        if (.not. allocated(SA_RTE_fls%fl)) call SA_RTE_init_fls()
        SA_RTE_fls%fl(SA_RTE_flkeys%RFF)%fn  = 'WR_runoff.r2c'
        SA_RTE_fls%fl(SA_RTE_flkeys%RFF)%iun = 31
        SA_RTE_fls%fl(SA_RTE_flkeys%RCH)%fn  = 'WR_recharge.r2c'
        SA_RTE_fls%fl(SA_RTE_flkeys%RCH)%iun = 32

        !> Replace default file information with values from file.
        if (present(fld)) then

            !> Open the file containing file information.
            open(unit = 271, file = trim(adjustl(fld)), status = 'old', action = 'read', iostat = IOS)
            print *, trim(adjustl(fld))

            !> Read the information from file.
            do while (ios == 0)

                !> Read line from file.
                read(271, *, iostat = IOS) str1, str2

                !> Assign the file attributes by key.

                !> Path to input files.
                if (trim(adjustl(str1)) == 'pthIn') then
                    flg%pthIn = trim(adjustl(str2))

                !> Path to output files.
                else if (trim(adjustl(str1)) == 'pthOut') then
                    flg%pthOut = trim(adjustl(str2))

                !> CLASS.ini file.
                else if (trim(adjustl(str1)) == 'class') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f50)%fn = phtfl
!                    flg%fl(mfk%f50)%isInit = .true.

                else if (trim(adjustl(str1)) == 'hydro') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f23)%fn = phtfl
!                    flg%fl(mfk%f23)%isInit = .true.

                else if (trim(adjustl(str1)) == 'run_option') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f53)%fn = phtfl
!                    flg%fl(mfk%f53)%isInit = .true.

                else if (trim(adjustl(str1)) == 'metrics_out') then
                    phtfl = trim(adjustl(flg%pthOut)) // trim(adjustl(str2))
                    mtsfl%fl(mtsk%out)%fn = phtfl
!                    mtsfl%fl(mtsk%out)%isInit = .true.

                else if (trim(adjustl(str1)) == 'basin_wb') then
                    phtfl = trim(adjustl(flg%pthOut)) // trim(adjustl(str2))
                    flg%fl(mfk%f900)%fn = phtfl
!                    flg%fl(mfk%f900)%isInit = .true.

                else if (trim(adjustl(str1)) == 'stream_flow') then
                    phtfl = trim(adjustl(flg%pthOut)) // trim(adjustl(str2))
!todo: update this
                    flg%fl(mfk%f70)%fn = phtfl
!                    WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%fn = phtfl
!                    WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%isInit = .true.

                else if (trim(adjustl(str1)) == 'ggeo_flux') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f18)%fn = phtfl
!                    flg%fl(mfk%f18)%isInit = .true.

                else if (trim(adjustl(str1)) == 'out_response') then
                    phtfl = trim(adjustl(flg%pthOut)) // trim(adjustl(str2))
                    flg%fl(mfk%out_response)%fn = str2
!                    flg%fl(mfk%out_response)%isInit = .true.

                else if (trim(adjustl(str1)) == 'int_statVariables') then
                    phtfl = trim(adjustl(flg%pthOut)) // trim(adjustl(str2))
                    flg%fl(mfk%f883)%fn = phtfl
!                    flg%fl(mfk%f883)%isInit = .true.

                else if (trim(adjustl(str1)) == 'soil_levels') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f52)%fn = phtfl
!                    flg%fl(mfk%f52)%isInit = .true.

                else if (trim(adjustl(str1)) == 'soil_ini') then
                    phtfl = trim(adjustl(flg%pthIn)) // trim(adjustl(str2))
                    flg%fl(mfk%f54)%fn = phtfl
!                    flg%fl(mfk%f54)%isInit = .true.

                end if
            end do
            close(271)
        end if

    end subroutine

end module
