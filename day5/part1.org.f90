program part1
    !use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, counter, length, n, i, total, in, out, io_err
    integer :: rs, bs, us
    character (len=32) :: filename
    character (len=256), allocatable :: lines(:)
    integer, allocatable :: rules(:), updateset(:), ruleset(:,:)
    
    filename = ""
    fileid = 8

    call get_command_argument(1, filename, STATUS=io_err)
    if (io_err .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if

    counter = 0
    rs = 0
    bs = 0
    us = 0
    n = 0
    i = 0
    length = 0
    total = 0
    allocate(lines(counter))
    
    call getsetsize()

    allocate(ruleset(rs,2))
    
    print *, rs, us, bs

    ! Rulesets
    do i=1,rs
        rules = str2intarray(lines(i), pipedelim)
        ruleset(i,1) = rules(1)
        ruleset(i,2) = rules(2) 
    end do

    print *, " *** "

    ! Updates
    do i=(rs+bs),size(lines)
        !print *, trim(lines(i))
        updateset = str2intarray(lines(i), commadelim)
        ! do out=1, size(ruleset)
        !     do in=1, size(ruleset)
                
        !     end do
        ! end do
    end do


    contains
        subroutine getsetsize()
            logical :: processrules
            integer :: io_err
            character (len=256) :: line

            processrules = .true.
            open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
            if (io_err .ne. 0) then
                print *, "File error occurred", io_err
                stop
            else 
                do 
                    read (unit = fileid, fmt='(A)', iostat=io_err) line
                    if (io_err .ne. 0) exit
                    counter = counter + 1
                    call resizestringarray(lines, counter)
                    lines(counter) = trim(line)
                    !print *, len_trim(lines)
                    if ((len_trim(line) .ne. 0) .and. (processrules .eqv. .true.))then
                        print *, "Processing rules..."
                        !rules = str2intarray(lines,pipedelim)
                        rs = rs + 1
                    else if ( len_trim(line) .eq. 0 ) then
                        print *, "Ignoring the blank line..."
                        bs = bs + 1
                        processrules = .false.
                    else 
                        print *, "Processing updates..."
                        !updates = str2intarray(lines, commadelim)
                        us = us + 1            
                    end if
                end do
            end if
            close(unit=fileid)
        end subroutine
    
end program part1