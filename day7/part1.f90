program part1
    use stringmod

    implicit none

    integer :: fileid, io_err
    character (len=128) :: lines, filename
    
    integer(8) :: answer, total, o, bal
    integer(8), allocatable :: elements (:)
        
    filename = ""
    fileid = 8
    bal =0
    total = 0

    call get_command_argument(1, filename, STATUS=io_err)
    if (io_err .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if
   
    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit

            answer = findanswer(lines)
            elements = findelements(lines)
            print *, answer, ": ", elements
            o = size(elements)  
            if (checkpath(answer, elements, o)) then
                bal = bal + answer
            end if
        end do
    end if
    close(unit=fileid)

    print *, "Balance: ", bal

    contains
    function findanswer(s) result(a)
        character (len=128), intent(in) :: s
        integer(8) :: a
        character (len=128) :: tempstr
        integer :: length, i, colonpos

        length = len_trim(s)
        a = 0
        colonpos = 0

        do i=1, length
            if (s(i:i) .eq. colondelim) then
                colonpos = i
            end if
        end do

        tempstr = s(1:colonpos-1)
        a = str2int8(tempstr)
    end function
    
    function findelements(s) result(e)
        character (len=128), intent(in) :: s
        integer(8), allocatable :: e(:)
        character (len=128) :: tempstr
        integer :: length, i, colonpos

        length = len_trim(s)
        colonpos = 0

        do i=1, length
            if (s(i:i) .eq. colondelim) then
                colonpos = i
            end if
        end do

        tempstr = trim(s(colonpos+2:length))
        e = str2intarray(tempstr,spacedelim)
    end function

    recursive function checkpath (left, right, num_elements) result(results)
        integer(8), intent(in) :: left, num_elements
        integer(8), intent(in) :: right (:)
        logical :: results
        logical :: r
        integer(8) :: i, rightside

        i = num_elements

        if (i .eq. 1) then
            results = (left .eq. right(1))
            return
        end if

        rightside = right(i)
        i = i - 1

        if (mod(left, rightside) .eq. 0) then
            r = checkpath(left / rightside , right, i)
            if (r) then
                results = .true.
                return
            end if
        end if

        if (left .ge. rightside) then
            r = checkpath (left - rightside, right, i)
            if (r) then
                results = .true.
                return
            end if
        end if

        results = .false.   
    end function
    
end program part1