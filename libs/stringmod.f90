module stringmod
    implicit none
    character (len=1) :: tabdelim = '   '
    character (len=1) :: spacedelim = ' '
    character (len=1) :: commadelim = ','
    character (len=1) :: pipedelim = "|"

    type :: datatype
        integer :: left
        integer :: right
    end type datatype

    contains

    ! A small function that returns a data type tht is delimited by tabs
    ! Not my best function but I wrote it in a hurry and on the fly
    function string2values(str) result(value)
        character (len=32), intent(in) :: str
        type(datatype) :: value
        integer :: i, tabpos, length
        character (len=16) :: temp_str

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
        character (len=32), intent(in) :: str
        character (len=1), intent(in) :: delim
        character (len=32) :: temp_str
        integer, allocatable :: array (:)
        integer :: count, i, delimcount, length
        integer, allocatable :: delimpos(:)
        
        delimcount = 0
        count = 0

        length = len_trim(str)

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
                    temp_str = str(1:delimpos(i))
                    array(i) = str2int(temp_str)
                else 
                    temp_str = str(delimpos(i-1):delimpos(i))
                    array(i) = str2int(temp_str)
                end if
                
                if (i .eq. count +1) then
                    temp_str = str(delimpos(count):length)
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

end module stringmod