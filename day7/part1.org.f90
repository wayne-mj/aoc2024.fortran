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
            
            
            ! do e=2,size(elements)                
            !     do o=1, 2
            !         total = elements(1)
            !         if (o .eq. 1) then
            !             total = total + elements(e)
            !             print *, total
            !         else if (o .eq. 2) then
            !             total = total * elements(e)
            !             print *, total
            !         end if
            !     end do
            !     if (total .eq. answer) then
            !         bal = bal + answer
            !     end if
            ! end do
            ! bal = bruteforce(elements, answer)
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
    
    function findelements(s) result(et)
        character (len=128), intent(in) :: s
        integer, allocatable :: et(:)
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
        et = str2intarray(tempstr,spacedelim)
    end function

    ! function bruteforce (ele, ans) result(bal)
    !     integer, intent(in), allocatable :: ele(:)
    !     integer, intent(in) :: ans
    !     integer :: bal
    !     integer :: num_elements, ltotal, lo,e, num_ops, op_set
    !     integer, allocatable :: ops (:)

    !     total = 0
    !     num_elements = size(ele)
    !     allocate(ops(num_elements))
    !     num_ops = size(ele) - 1
        
    !     do op_set = 0, 2**num_ops -1
    !         ops = 0
    !         do e = 1, num_ops
    !             print *, e
    !             ops(e) = mod(op_set / 2**(e-2),2) +1
    !             print *, mod(op_set / 2**(e-2),2) +1
    !         end do

    !         total = ele(1)
    !         do e=1,num_ops
    !             if (ops(e) ==1) then
    !                 total = total + ele(e+1)
    !             else if (ops(e) ==2) then
    !                 total = total * ele(e+1)
    !             end if
    !         end do
    !         print *, total
    !     end do
        
    ! end function
end program part1