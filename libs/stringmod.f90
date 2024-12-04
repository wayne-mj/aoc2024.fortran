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

                !print *, temp_str


                ! if (i .eq. 2) then
                !     !temp_str = str(2:delimpos(i))
                !     temp_str = str(delimpos(i-1):delimpos(i))
                !     array(i) = str2int(temp_str)
                !     print *, temp_str, " ...(2)... "
                ! end if
                ! if (i .eq. 3) then
                !     !temp_str = str(4:delimpos(i))
                !     temp_str = str(delimpos(i-1):delimpos(i))
                !     array(i) = str2int(temp_str)
                !     print *, temp_str, " ...(3)... "
                ! end if
                ! if (i .eq. 4) then
                !     !temp_str = str(6:delimpos(i))
                !     temp_str = str(delimpos(i-1):delimpos(i))
                !     array(i) = str2int(temp_str)
                !     print *, temp_str, " ...(4)... "
                ! end if
                ! if (i .eq. 5) then
                !     temp_str = str(delimpos(i-1):length)
                !     array(i) = str2int(temp_str)
                !     print *, temp_str, " ...(5)... "
                ! end if                
            end do
        end if
    end function str2intarray

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