program part1
    use stringmod

    implicit none

    integer :: fileid, count, io_err
    character (len=128) :: lines, filename
    integer, allocatable :: left(:)
    logical :: mysafe
      
    fileid = 8
    count = 0
    mysafe = .false.
   
    call get_command_argument(1, filename, STATUS=io_err)
    if (io_err .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if
   

    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit
            print *, lines
            left = str2intarray(lines,spacedelim)
            mysafe = issafe(left)

            if (mysafe .eqv. .true.) then
                count = count +1
            end if
        end do
    end if
    close(unit=fileid)

    print *, "Counted as safe: ", count

    contains

    function issafe(reports) result(result)
        integer, intent(in), allocatable :: reports (:)
        logical :: result
        logical :: up, down, safe
        integer :: i, n, diffreport

        safe = .false.
        up = .false.
        down = .false.
        n = size(reports)
        
        do i=1, n-1
            diffreport = reports(i) - reports(i+1)

            if ((diffreport .eq. 0) .or. (abs(diffreport) .gt. 3)) then
                safe = .false.
                exit
            end if

            if (reports(i) .gt. reports(i+1)) then
                up = .true.
            end if

            if (reports(i) .lt. reports(i+1)) then
                down = .true.
            end if

            if (up .eqv. down) then
                safe = .false.
                exit
            end if
            safe = .true.
        end do

        result = safe
    end function

end program part1