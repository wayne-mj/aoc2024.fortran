program part1
    use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, counter, io_err, n, i, total
    character (len=32) :: lines, filename
    integer, allocatable :: left(:),right(:)
    type(datatype), allocatable :: myArray(:)
    
    fileid = 8
    counter = 0
    n = 0
    total = 0

    call get_command_argument(1, filename, STATUS=io_err)
    if (io_err .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if

    allocate(myArray(counter))
    allocate(left(counter))
    allocate(right(counter))
    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit
            counter = counter + 1
            call resizestructarray(myArray,counter)
            myArray(counter) = string2values(lines)
            call resizearray(left,counter)
            call resizearray(right,counter)
            left(counter) = myArray(counter)%left
            right(counter) = myArray(counter)%right
        end do
        deallocate(myArray)
    end if
    close(unit=fileid)

    call mergesort(left, size(left))
    call mergesort(right, size(right))
    n = size(left)
    do i=1,n
        total = total + abs (left(i) - right(i))
    end do

    print *, total

end program part1