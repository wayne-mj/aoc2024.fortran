module stringmod
    implicit none
    character (len=1), parameter :: tabdelim = char(9)
    character (len=1), parameter :: newline = char(10)
    character (len=1), parameter :: cr = char(13)
    character (len=1) :: spacedelim = ' '
    character (len=1) :: commadelim = ','
    character (len=1) :: colondelim = ":"
    character (len=1) :: pipedelim = "|"

    type :: datatype
        integer :: left
        integer :: right
    end type datatype

   contains

    ! A small function that returns a data type tht is delimited by tabs
    ! Not my best function but I wrote it in a hurry and on the fly
    function string2values(str) result(value)
        character (len=*), intent(in) :: str
        type(datatype) :: value
        integer :: i, tabpos, length
        character (len=128) :: temp_str

        i = 0
        tabpos = 0
        length =0
        temp_str = ""
        length = len_trim(str)

        do i=1, length
            if (str(i:i) .eq. tabdelim) then
                tabpos = i
                exit
            end if
        end do

        temp_str = str(1:tabpos)
        value%left = str2int(temp_str)
        temp_str = str(tabpos+1:length)
        value%right = str2int(temp_str)
    end function string2values

    !function string2stringarray(str) result(array)

    ! This is a rewrite of the above and a more complex splitting that used multiple delimiters
    function str2intarray(str, delim) result(array)
        character (len=*), intent(in) :: str
        character (len=1), intent(in) :: delim
        character (len=128) :: temp_str
        integer, allocatable :: array (:)
        integer :: count, i, delimcount, length
        integer, allocatable :: delimpos(:)
        
        delimcount = 0
        count = 0

        length = len_trim(str)
        !print *, "stringmod: length: ", length, " str:", str

        do i = 1, length
            if (str(i:i) .eq. delim) then
                delimcount = delimcount + 1
            end if
        end do

        if (delimcount .gt. 0) then
            allocate(delimpos(delimcount))
            allocate(array(delimcount+1))
            count = 0
            do i=1,length
                if (str(i:i) .eq. delim) then
                    count = count + 1
                    delimpos(count) = i
                end if
            end do

            do i=1,count+1
                if (i .eq. 1) then
                    temp_str = str(1:delimpos(i)-1)
                    !print *, "stringmod: temp_str: ", temp_str, " i: ", i
                    array(i) = str2int(temp_str)
                else 
                    temp_str = str(delimpos(i-1)+1:delimpos(i)-1)
                    !print *, "stringmod: temp_str: ", temp_str, " i: ", i
                    array(i) = str2int(temp_str)
                end if
                
                if (i .eq. count +1) then
                    temp_str = str(delimpos(count)+1:length)
                    !print *, "stringmod: temp_str: ", temp_str, " i: ", i
                    array(i) = str2int(temp_str)
                end if               
            end do
        end if
    end function str2intarray

    ! Subroutine to resize the source array to a new size
    subroutine resizearray(array, newsize)
        integer, allocatable :: array(:)
        integer, intent(in) :: newsize
        integer, allocatable :: temp(:)

        allocate(temp(newsize))
        if (allocated(array)) then
            temp(1:size(array)) = array
            deallocate(array)
        end if
        array = temp

        deallocate(temp)
    end subroutine resizearray

    ! Subroutine to resize the source array to a new size
    subroutine resizestringarray(array, newsize)
        character(len=*), allocatable :: array(:)
        integer, intent(in) :: newsize
        character(len=256), allocatable :: temp(:)

        allocate(temp(newsize))
        if (allocated(array)) then
            temp(1:size(array)) = array
            deallocate(array)
        end if
        array = temp

        deallocate(temp)
    end subroutine resizestringarray

    ! As above but when using a data structure
    subroutine resizestructarray(array, newsize)
        type(datatype), allocatable :: array (:), temp(:)
        integer :: newsize

        allocate(temp(newsize))
        if (allocated(array)) then
            temp(1:size(array)) = array
            deallocate(array)
        end if

        array = temp
        deallocate(temp)

    end subroutine resizestructarray

    ! Convert Integer to string
    function int2str(i) result(str)
        integer, intent(in) :: i
        character(len=10) :: str
        write (str, '(I10)') i
        str = adjustl(str)
        !str = trim(str)
    end function int2str

    ! Converts a string to an integer otherwise return a wildly absurd negative number.
    ! Unless that is what they are looking for - oops!
    function str2int(str) result(i)
        character (len=32), intent(in) :: str
        integer :: i, ioerr

        read (str, '(I10)', iostat=ioerr) i

        if (ioerr .ne. 0) then
            i = -huge(i)
        end if
    end function str2int

    function str2int8(str) result(i)
        character (len=32), intent(in) :: str
        integer(8) :: i
        integer :: ioerr

        read (str, '(I19)', iostat=ioerr) i

        if (ioerr .ne. 0) then
            i = -huge(i)
        end if
    end function str2int8

    function str2int16(str) result(i)
        character (len=64), intent(in) :: str
        integer(8) :: i
        integer :: ioerr

        read (str, '(I39)', iostat=ioerr) i

        if (ioerr .ne. 0) then
            i = -huge(i)
        end if
    end function str2int16

    function containschar(str, ch) result(results)
        character(len=*), intent(in) :: str
        character(len=1), intent(in) :: ch
        logical :: results
        integer :: i, length

        i = 0
        length = 0

        length = len_trim(str)
        do i=1,length
            if (str(i:i) .eq. ch) then
                results = .true.
                return
            end if
        end do
        results = .false.
    end function containschar

end module stringmod