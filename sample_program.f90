program sample_program
    use logging_mod, only : &
        logging_init, &
        logging_level_debug, &
        logging_level_error, &
        logging => main_logging
    implicit none
    character(len=*), parameter :: log_mod = "sample_program"
    ! Initialize an instance of the logging_type derived type
    ! Here, we use the log level for debugging
    call logging_init(logging_level_debug)
    ! Here is an example of an info log message
    call logging%info("Sample program for logging")
    ! Here is a debug message where we've included the name of the
    ! module/program
    call logging%debug("Test debug message", log_mod)
    ! Normally, you won't do this. But, we'll override the original
    ! instance so we use the default level of info
    call logging_init()
    call logging%info("Showing each level")
    call logging%debug("This won't print to the terminal")
    call logging%info("This will be the first thing for this section")
    call logging%warn("We will still output warnings")
    call logging%error("This will be logged to stderr")
    call logging%fatal("stop the program")
    ! write to files instead
    call logging_init(unit_output=900, unit_error=800)
    call logging%info("Messages to log file")
    call logging%debug("Test debug to file")
    call logging%error("Error message to error file")
    call logging%fatal("stop the program")
end program sample_program
