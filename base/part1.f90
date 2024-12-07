program part1
    !use mergemod
    use stringmod

    implicit none

    integer :: fileid, io_err
    character (len=32) :: lines, filename
        
    filename = "input1"
    fileid = 8
    

    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit
        end do
    end if
    close(unit=fileid)
end program part1