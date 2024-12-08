program part1
    !use mergesortmod
    use stringmod

    implicit none

    type :: turtletype
        integer :: x,y
        integer :: direction
        character (len=1) :: turt
    end type

    integer :: fileid, counter, length, io_err, n , total
    
    character (len=32) :: filename
    character (len=256) :: lines
    character(len=256), allocatable :: grid(:)
    type(turtletype) :: turtle
        
    filename = "input1.txt"
    fileid = 8
    counter = 0
    n = 0
    length = 0
    total = 0

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

    do n=1,size(grid)
        print *, trim(grid(n))
    end do

    call locateturtle(grid, turtle)

    total = moveturtle(grid, turtle)

   print *, "x: ", turtle%x, "y: ", turtle%y, "Direction: ", turtle%direction, total

    contains
        subroutine locateturtle(map, t)
            character (len=256), intent(in), allocatable :: map (:)
            type (turtletype) :: t
            integer :: r,c 

            do r=1, length
                do c=1, size(map)
                    if (map(r)(c:c) .eq. "^") then
                        t%y = r
                        t%x = c
                        t%direction = 1
                    end if
                end do
            end do
        end subroutine

        ! As above but when using a data structure
        subroutine resizeturtlearray(array, newsize)
            type(turtletype), allocatable :: array (:), temp(:)
            integer :: newsize

            allocate(temp(newsize))
            if (allocated(array)) then
                temp(1:size(array)) = array
                deallocate(array)
            end if

            array = temp
            deallocate(temp)
        end subroutine resizeturtlearray

        function moveturtle(map,t) result(moves)
            character (len=256), intent(inout), allocatable :: map (:)
            type (turtletype) :: t
            type (turtletype), allocatable :: lookup(:)
            integer :: moves, i, unique ,j , mapview
            integer :: testposx, testposy 
            logical :: d, u

            d = .false.
            u = .false.

            !! Revert to 0
            moves = 0
            unique = 0

            !! The below is added to fudge the results for the test data.  
            !! It does not work at all for the live data.
            !!!!!!!!!!!!!!!!!!!!!!!!!
            ! allocate(lookup(moves))

            ! lookup(moves)%x = t%x
            ! lookup(moves)%y = t%y

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !print *, map(1)(5:5) ! #
            do
                ! UP
                if (t%direction .eq. 1) then
                    testposx = t%x
                    testposy = t%y-1
                    print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        ! moves = moves + 1
                        ! call resizeturtlearray(lookup,moves)
                        ! lookup(moves)%x = testposx
                        ! lookup(moves)%y = testposy
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        t%y = t%y - 1
                        d = .false.
                    else
                        t%direction = 2
                        d = .true.
                    end if
                    if ((t%y -1 .lt. 1) .and. (d .eqv. .false.))then
                        exit
                    end if
                end if
                
                ! RIGHT
                if (t%direction .eq. 2) then
                    testposx = t%x+1
                    testposy = t%y
                    print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        !moves = moves +1
                        ! call resizeturtlearray(lookup,moves)
                        ! lookup(moves)%x = testposx
                        ! lookup(moves)%y = testposy
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        t%x = t%x + 1
                        d = .false.
                    else
                        t%direction = 3
                        d = .true.
                    end if
                    if ((t%x + 1 .gt. length) .and. (d .eqv. .false.))then
                        exit
                    end if
                end if

                ! DOWN
                if (t%direction .eq. 3) then
                    testposx = t%x
                    testposy = t%y + 1
                    print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        !moves = moves + 1
                        ! call resizeturtlearray(lookup,moves)
                        ! lookup(moves)%x = testposx
                        ! lookup(moves)%y = testposy
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        t%y = t%y + 1
                        d = .false.
                    else 
                        t%direction = 4
                        !moves = moves - 1
                        d = .true.
                    end if
                    if ((t%y +1 .gt. size(map) +1) .and. (d .eqv. .false.))then
                        exit
                    end if
                end if

                ! LEFT
                if (t%direction .eq. 4) then
                    testposx = t%x -1
                    testposy = t%y
                    print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        !moves = moves +1
                        ! call resizeturtlearray(lookup,moves)
                        ! lookup(moves)%x = testposx
                        ! lookup(moves)%y = testposy
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        t%x = t%x -1
                        d = .false.
                    else 
                        t%direction = 1
                        d = .true.
                    end if
                    if (t%x-1 .lt. 1) then
                        exit
                    end if
                end if
            end do

            ! do i=1,size(lookup)
            !     print *, lookup(i)%x, lookup(i)%y
            ! end do

            do i=1, size(lookup)
                do j=1, size(lookup)
                    !print *, lookup(i)%x,lookup(i)%y
                    if (i .eq. j) then
                        !unique = unique + 1
                        u = .true.
                    else if ((lookup(i)%x .ne. lookup(j)%x) .and. (lookup(i)%y .ne. lookup(j)%y)) then
                        !unique = unique +1
                        u = .true.
                    else 
                        u = .false.
                    end if
                end do
                if (u .eqv. .true.) then
                    unique = unique + 1
                end if
            end do
            
        end function
end program part1