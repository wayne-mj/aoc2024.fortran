program part1
    use stringmod
    use timemod

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
        
    filename = "testinput1.txt"
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

        ! ! As above but when using a data structure
        ! subroutine resizeturtlearray(array, newsize)
        !     type(turtletype), allocatable :: array (:), temp(:)
        !     integer :: newsize

        !     allocate(temp(newsize))
        !     if (allocated(array)) then
        !         temp(1:size(array)) = array
        !         deallocate(array)
        !     end if

        !     array = temp
        !     deallocate(temp)
        ! end subroutine resizeturtlearray

        function moveturtle(map,t) result(moves)
            character (len=256), intent(inout), allocatable :: map (:)
            type (turtletype) :: t
            integer :: moves, mapview, mysleep
            integer :: testposx, testposy 
            logical :: d, u
            character (len=128) :: movestr

            d = .false.
            u = .false.

            mysleep = 10

            !! Revert to 0
            moves = 0

            do
                ! UP
                if (t%direction .eq. 1) then
                    testposx = t%x
                    testposy = t%y-1
                    !print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        print *, char(27)//'[2J'
                        print *, char(27)//'[H'
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        print *, " "
                        write(movestr, '(I10)') moves
                        print *, "Moves: ", movestr
                        call msdelay(mysleep)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
                    !print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        print *, char(27)//'[2J'
                        print *, char(27)//'[H'
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        print *, " "
                        write(movestr, '(I10)') moves
                        print *, "Moves: ", movestr
                        call msdelay(mysleep)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
                    !print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        print *, char(27)//'[2J'
                        print *, char(27)//'[H'
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        print *, " "
                        write(movestr, '(I10)') moves
                        print *, "Moves: ", movestr
                        call msdelay(mysleep)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        t%y = t%y + 1
                        d = .false.
                    else 
                        t%direction = 4
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
                    !print *, trim(map(testposy)(testposx:testposx)), " : ", testposx, testposy
                    if (trim(map(testposy)(testposx:testposx)) .ne. "#") then
                        if (map(t%y)(t%x:t%x) .ne. "X") then
                            map(t%y)(t%x:t%x) = "X"
                            moves = moves + 1
                        end if
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        print *, char(27)//'[2J'
                        print *, char(27)//'[H'
                        do mapview=1, length
                            print *, trim(map(mapview))
                        end do
                        print *, " "
                        write(movestr, '(I10)') moves
                        print *, "Moves: ", movestr
                        call msdelay(mysleep)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
        end function

        ! subroutine msdelay(delayms)
        !     integer :: delayms
        !     integer :: start, current, rate
        !     real :: ticks_per_ms

        !     call SYSTEM_CLOCK(COUNT_RATE=rate)
        !     ticks_per_ms = real(rate) / 1000.0

        !     call SYSTEM_CLOCK(COUNT=start)
        !     do 
        !         call SYSTEM_CLOCK(count=current)
        !         if ((current - start) >= delayms * ticks_per_ms) exit                
        !     end do
        ! end subroutine
end program part1