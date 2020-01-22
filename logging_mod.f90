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
    contains
        procedure debug
        procedure info
        procedure warn
        procedure :: warning => warn
        procedure error
        procedure fatal
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

    function constructor(level) result(this)
        !! Build an instance of the derived type
        ! Arguments
        ! ---------
        integer, intent(in) :: &
            level
                !! logging level
        ! Returns
        ! -------
        type(logging_type) :: &
            this
                !! instance of logging_type
        this%level = level
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
            call logging_write("DEBUG | ", message, log_module, log_proc)
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
            call logging_write("INFO  | ", message, log_module, log_proc)
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
            call logging_write("WARN  | ", message, log_module, log_proc)
        end if
    end subroutine warn

    subroutine error(this, message, log_module, log_proc)
        !! Error message that causes the program to stop
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
            call logging_write("ERROR | ", message, log_module, log_proc, error_unit)
            stop
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
            call logging_write("FATAL | ", message, log_module, log_proc, error_unit)
            stop
        end if
    end subroutine fatal

    subroutine logging_write(message_type, message, log_module, log_proc, unit_num)
        !! Log the message
        ! Arguments
        ! ---------
        character(len=*), intent(in) :: &
            message_type, &
                !! message type decorator
            message
                !! Message to log
        character(len=*), intent(in), optional :: &
            log_module, &
                !! name of the module
            log_proc
                !! name of the procedure
        integer, intent(in), optional :: &
            unit_num
                !! pipe unit number for output
        ! Local variables
        ! ---------------
        integer :: &
            output_pipe
        character(len=17) :: &
            time_output
        call date_and_time(values=time_vals)
        write(time_output, time_fmt) time_vals(5), ":", time_vals(6), ":", time_vals(7), &
            " ", time_vals(1), time_vals(2), time_vals(3)
        if (present(unit_num)) then
            output_pipe = unit_num
        else
            output_pipe = output_unit
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
        flush(output_pipe)
    end subroutine logging_write


    subroutine logging_init(level)
        !! Initialize the main logger
        integer, intent(in), optional :: &
            level
                !! Logging level, optional
        if (present(level)) then
            main_logging = logging_type(level)
        else
            main_logging = logging_type(logging_level_info)
        end if
    end subroutine logging_init

end module logging_mod
