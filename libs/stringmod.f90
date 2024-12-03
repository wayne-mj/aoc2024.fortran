module stringmod
    implicit none
    character (len=1) :: tabdelim = '   '
    character (len=1) :: spacedelim = ' '

    type :: datatype
        integer :: left
        integer :: right
    end type datatype

    contains

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

    function str2int(str) result(i)
        character (len=32), intent(in) :: str
        integer :: i, ioerr

        read (str, '(I10)', iostat=ioerr) i

        if (ioerr .ne. 0) then
            i = -huge(i)
        end if
    end function str2int

end module stringmod