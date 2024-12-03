program part1
    use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, counter, io_err, n, i, total
    character (len=32) :: lines, filename
    integer, allocatable :: left(:),right(:)
    type(datatype), allocatable :: myArray(:), tempArray
    
    filename = "input1"
    fileid = 8
    counter = 0
    n = 0

    allocate(myArray(counter))
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
            print *, myArray(counter)%left, " - ",  myArray(counter)%right
        end do
    end if
    close(unit=fileid)

    n = size(myArray)
    allocate(left(n))
    allocate(right(n))
    do i =1, n
        left(i) = myArray(i)%left
        right(i) = myArray(i)%right
    end do
    call mergesort(left, size(left))
    call mergesort(right, size(right))
    do i=1,n
        total = total + abs (left(i) - right(i))
    end do

    print *, total

end program part1