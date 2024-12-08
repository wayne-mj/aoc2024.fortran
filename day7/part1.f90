program part1
    use stringmod

    implicit none

    integer :: fileid, io_err
    character (len=128) :: lines, filename

    !character (len=1) :: op(2) = (/"+","*"/)

    integer :: answer, total, e,o, bal
    integer, allocatable :: elements (:)
        
    filename = "testinput1.txt"
    fileid = 8
    bal =0
    total = 0
   
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
            
            total = 0
            do o=1,2                
                do e=1, size(elements)
                    if (o .eq. 1) then
                        total = total + elements(e)
                        print *, total
                    else if (o .eq. 2) then
                        total = total * elements(e)
                        print *, total
                    end if
                end do
                if (total .eq. answer) then
                    bal = bal + answer
                end if
            end do
        end do
    end if
    close(unit=fileid)

    print *, "Balance: ", bal

    contains
    function findanswer(s) result(a)
        character (len=128), intent(in) :: s
        integer :: a
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
        a = str2int(tempstr)
    end function
    
    function findelements(s) result(e)
        character (len=128), intent(in) :: s
        integer, allocatable :: e(:)
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
end program part1