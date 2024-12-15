program part1
    use stringmod

    implicit none

    integer :: fileid, counter, length, io_err, n, i, total, r,c, xmascounter

    character (len=32) :: filename
    character (len=256) :: lines
    character(len=256), allocatable :: grid(:)
        
    filename = ""
    fileid = 8
    counter = 0
    n = 0
    xmascounter = 0

    call get_command_argument(1, filename, STATUS=io_err)
    if (io_err .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if

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
            grid(counter) = trim(lines)
        end do
    end if
    close(unit=fileid)

    do r=1,counter
        do c=1, length
            if (grid(r)(c:c) .eq. "X") then
                if ((grid(r+1)(c:c) .eq. "M") .and. (grid(r+2)(c:c) .eq. "A") .and. (grid(r+3)(c:c) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if   
                if ((grid(r-1)(c:c) .eq. "M") .and. (grid(r-2)(c:c) .eq. "A") .and. (grid(r-3)(c:c) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if  
                if ((grid(r+1)(c+1:c+1) .eq. "M") .and. (grid(r+2)(c+2:c+2) .eq. "A") .and. (grid(r+3)(c+3:c+3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if   
                if ((grid(r-1)(c-1:c-1) .eq. "M") .and. (grid(r-2)(c-2:c-2) .eq. "A") .and. (grid(r-3)(c-3:c-3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if
                if ((grid(r+1)(c-1:c-1) .eq. "M") .and. (grid(r+2)(c-2:c-2) .eq. "A") .and. (grid(r+3)(c-3:c-3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if   
                if ((grid(r-1)(c+1:c+1) .eq. "M") .and. (grid(r-2)(c+2:c+2) .eq. "A") .and. (grid(r-3)(c+3:c+3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if
                if ((grid(r)(c+1:c+1) .eq. "M") .and. (grid(r)(c+2:c+2) .eq. "A") .and. (grid(r)(c+3:c+3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if   
                if ((grid(r)(c-1:c-1) .eq. "M") .and. (grid(r)(c-2:c-2) .eq. "A") .and. (grid(r)(c-3:c-3) .eq. "S")) then
                    xmascounter = xmascounter + 1
                end if              
            end if                   
        end do
    end do

    print *, xmascounter
end program part1