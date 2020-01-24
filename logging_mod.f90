module logging_mod
    !! Logging for Fortran using a derived-type approach
    use, intrinsic :: iso_fortran_env, only : &
        output_unit, &
        error_unit
    implicit none
    private
    public :: &
        logging_init, &
        main_logging

    integer, parameter, public :: &
        logging_level_debug = 10, &
            !! debug level value
        logging_level_info = 20, &
            !! info level value
        logging_level_warn = 30, &
            !! warning level value
        logging_level_error = 40, &
            !! error level value
        logging_level_fatal = 50
            !! fatal level value

    integer, dimension(:) :: time_vals(8)

    type logging_type
        integer :: level
        integer, private :: &
            unit_output, &
            unit_error
        character(len=500), private :: &
            file_output, &
            file_error
    contains
        procedure debug
        procedure info
        procedure warn
        procedure :: warning => warn
        procedure error
        procedure fatal
        procedure, private :: logging_write
    end type logging_type

    interface logging_type
        procedure constructor
    end interface logging_type

    type(logging_type) :: &
        main_logging

    character(len=*), parameter :: &
        time_fmt = "(I0.2,A,I0.2,A,I0.2,A,I4,I0.2,I0.2)", &
        output_fmt = "(A,A,A)"

contains

    function constructor(level, unit_output, unit_error, file_output, file_error) result(this)
        !! Build an instance of the derived type
        ! Arguments
        ! ---------
        integer, intent(in) :: &
            level
                !! logging level
        integer, intent(in), optional :: &
            unit_output, &
                !! output unit for stdout
            unit_error
                !! unit for stderr
        character(len=*), intent(in), optional :: &
            file_output, &
            file_error
        ! Returns
        ! -------
        type(logging_type) :: &
            this
                !! instance of logging_type
        this%file_output = 'logging.out'
        this%file_error = 'logging.err'
        this%level = level
        this%unit_output = output_unit
        if (present(unit_output)) then
            if (unit_output .ne. output_unit) then
                this%unit_output = unit_output
                if (present(file_output)) then
                    this%file_output = trim(file_output)
                end if
                close(this%unit_output)
                open(unit=this%unit_output, file = trim(this%file_output), status = 'replace')
                close(this%unit_output)
            end if
        end if
        this%unit_error = error_unit
        if (present(unit_error)) then
            if (unit_error .ne. error_unit) then
                this%unit_error = unit_error
                if (present(file_error)) then
                    this%file_error = trim(file_error)
                end if
                close(this%unit_error)
                open(unit=this%unit_error, file=trim(this%file_error), status='replace')
                close(this%unit_error)
            end if
        end if
    end function constructor

    subroutine debug(this, message, log_module, log_proc)
        !! Log a debug message
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
                !! instance of logging_type
        character(len=*), intent(in) :: &
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        if (this%level .le. logging_level_debug) then
            call this%logging_write("DEBUG | ", message, this%unit_output, log_module, log_proc)
        end if
    end subroutine debug

    subroutine info(this, message, log_module, log_proc)
        !! Log an informational message
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
                !! instance of logging_type
        character(len=*), intent(in) :: &
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        if (this%level .le. logging_level_info) then
            call this%logging_write("INFO  | ", message, this%unit_output, log_module, log_proc)
        end if
    end subroutine info

    subroutine warn(this, message, log_module, log_proc)
        !! Log a warning; something the might impact behavior of the code
        !! but doesn't warrant stopping the program
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
                !! instance of logging_type
        character(len=*), intent(in) :: &
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        if (this%level .le. logging_level_warn) then
            call this%logging_write("WARN  | ", message, this%unit_output, log_module, log_proc)
        end if
    end subroutine warn

    subroutine error(this, message, log_module, log_proc)
        !! Error message that does not cause program to stop
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
                !! instance of logging_type
        character(len=*), intent(in) :: &
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        if (this%level .le. logging_level_error) then
            call this%logging_write("ERROR | ", message, this%unit_error, log_module, log_proc)
        end if
    end subroutine error

    subroutine fatal(this, message, log_module, log_proc)
        !! Log a fatal message
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
                !! instance of logging_type
        character(len=*), intent(in) :: &
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        if (this%level .le. logging_level_fatal) then
            call this%logging_write("FATAL | ", message, this%unit_error, log_module, log_proc)
            stop
        end if
    end subroutine fatal

    subroutine logging_write(this, message_type, message, unit_num, log_module, log_proc)
        !! Log the message
        ! Arguments
        ! ---------
        class(logging_type), intent(in) :: &
            this
        character(len=*), intent(in) :: &
            message_type, &
                !! message type decorator
            message
                !! Message to log
        integer, intent(in), optional :: &
            unit_num
                !! pipe unit number for output
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        ! Local variables
        ! ---------------
        integer :: &
            output_pipe
        character(len=17) :: &
            time_output
        logical :: &
            open_file
        if (present(unit_num)) then
            output_pipe = unit_num
        else
            output_pipe = output_unit
        end if
        call date_and_time(values=time_vals)
        write(time_output, time_fmt) time_vals(5), ":", time_vals(6), ":", time_vals(7), &
            " ", time_vals(1), time_vals(2), time_vals(3)
        open_file = .true.
        if (unit_num .eq. output_unit .or. unit_num .eq. error_unit) then
            open_file = .false.
        else if (unit_num .eq. this%unit_output) then
            open(unit=unit_num, file=trim(this%file_output), status='old', position='append', action='write')
        else if (unit_num .eq. this%unit_error) then
            open(unit=unit_num, file=trim(this%file_error),status='old', position='append', action='write')
        end if
        if (present(log_module) .and. present(log_proc)) then
            write(output_pipe, output_fmt) message_type, &
                time_output, " | "//log_module//" | "//log_proc//" | "//message
        else if (present(log_module)) then
            write(output_pipe, output_fmt) message_type, &
                time_output, " | "//log_module//" | "//message
        else if (present(log_proc)) then
            write(output_pipe, output_fmt) message_type, &
                time_output, " | "//log_proc//" | "//message
        else
            write(output_pipe, output_fmt) message_type, &
                time_output, " | "//message
        end if
        if (open_file) then
            close(output_pipe)
        else
            flush(output_pipe)
        end if
    end subroutine logging_write


    subroutine logging_init(level, unit_output, unit_error, file_output, file_error)
        !! Initialize the main logger
        integer, intent(in), optional :: &
            level
                !! Logging level, optional
        integer, intent(in), optional :: &
            unit_output, &
                !! output unit for stdout
            unit_error
                !! unit for stderr
        character(len=*), intent(in), optional :: &
            file_output, &
            file_error
        if (present(level)) then
            main_logging = logging_type(level, unit_output, unit_error,&
                file_output, file_error)
        else
            main_logging = logging_type(logging_level_info, &
                unit_output, unit_error, file_output, file_error)
        end if
    end subroutine logging_init

end module logging_mod
