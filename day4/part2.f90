program part1
    !use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, counter, length, io_err, n, i, total, r,c, xmascounter

    character (len=32) :: filename
    character (len=256) :: lines
    character(len=256), allocatable :: grid(:)
        
    filename = "input1.txt"
    fileid = 8
    counter = 0
    n = 0
    xmascounter = 0

    allocate(grid(counter))    
    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit
            counter = counter + 1
            length = len_trim(lines)
            call resizestringarray(grid,counter)
            ! print *, trim(lines)
            grid(counter) = trim(lines)
        end do
    end if
    close(unit=fileid)

    do r=1,counter
        do c=1, length
            if (grid(r)(c:c) .eq. "A") then
                ! print *, "Checking A at: ", r,c
                ! print *, grid(r-1)(c-1:c-1), grid(r)(c:c), grid(r+1)(c+1:c+1) 
                ! print *, grid(r-1)(c+1:c+1) , grid(r)(c:c) , grid(r+1)(c-1:c-1)                 

                if ((grid(r-1)(c-1:c-1) .eq. "M") .and. (grid(r+1)(c+1:c+1) .eq. "S") .and. &
                (grid(r-1)(c+1:c+1) .eq. "M") .and. (grid(r+1)(c-1:c-1) .eq. "S") )then
                    xmascounter = xmascounter + 1
                end if 

                if ((grid(r-1)(c-1:c-1) .eq. "S") .and. (grid(r+1)(c+1:c+1) .eq. "M") .and. &
                (grid(r-1)(c+1:c+1) .eq. "S") .and. (grid(r+1)(c-1:c-1) .eq. "M")  ) then
                    xmascounter = xmascounter +1
                end if

                if ((grid(r-1)(c-1:c-1) .eq. "M") .and. (grid(r+1)(c+1:c+1) .eq. "S") .and. &
                (grid(r-1)(c+1:c+1) .eq. "S") .and. (grid(r+1)(c-1:c-1) .eq. "M")  ) then
                    xmascounter = xmascounter +1
                end if

                if ((grid(r-1)(c-1:c-1) .eq. "S") .and. (grid(r+1)(c+1:c+1) .eq. "M") .and. &
                (grid(r-1)(c+1:c+1) .eq. "M") .and. (grid(r+1)(c-1:c-1) .eq. "S")  ) then
                    xmascounter = xmascounter +1
                end if
            end if
        end do
    end do

    print *, xmascounter
end program part1