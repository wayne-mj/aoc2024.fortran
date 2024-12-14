program part1
    !use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, count, io_err, n, i, total, last
    character (len=128) :: lines, filename
    integer, allocatable :: left(:)
    logical :: safe, up,down
    !character (len=32) :: myArray(:), tempArray
    
    filename = "testinput1"
    fileid = 8
    count = 0
    n = 0
    last = 0
    safe = .false.
    up = .false.
    down = .false.
   

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
            n = size(left)
            
            do i=1,n-1
                !print *, left(i), left(i+1)

                if (left(i) .eq. left(i+1)) then
                    !print *, "Failed because of same value"
                    safe = .false.
                    exit
                end if

                if (left(i) .gt. left(i+1)) then
                    up = .true.
                end if

                if (left(i) .lt. left(i+1)) then
                    down = .true.
                end if

                if (up .eqv. down) then
                    !print *, "Failed because change of direction"
                    safe = .false.
                    exit
                end if

                total = left(i) - left(i+1)

                !print *, total, abs(total)
                if (abs(total) .gt. 3) then
                    !print *, "Failed because of change of step"
                    safe = .false.
                    exit
                end if
                
                safe = .true.
            end do
            
            print *, safe
            if (safe .eqv. .true.) then
                count = count + 1
            end if

            safe = .false.
            up = .false.
            down = .false.
        end do
    end if
    close(unit=fileid)

    print *, "Counted as safe: ", count

end program part1