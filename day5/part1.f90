program part1
    !use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, ierr
    character (len=32) :: filename
    character(len=128), allocatable :: ruleset(:), updateset(:)
    integer :: rules, updates , i ,j , k , mid, t, count
    integer, allocatable :: setofrules(:,:)
    integer, allocatable :: setofupdates (:)
    logical :: match
    
    filename = ""
    fileid = 8
    
    mid =0
    count = 0

    call get_command_argument(1, filename, STATUS=ierr)
    if (ierr .ne. 0) then
        print *, "Usage: provide a filename."
        stop
    else 
        print *, "Using: ", trim(filename)
    end if

    call getsetsize(ruleset, updateset, rules, updates)
    setofrules = makerules(ruleset, rules)

    do k=1, updates
        match = .false.
        setofupdates = str2intarray(updateset(k),commadelim)
        do i=1, size(setofupdates) -1
            do j=1, (size(setofrules) / 2)
                if (all(setofrules(j,:) .eq. [setofupdates(i+1), setofupdates(i)]))then
                    match = .true.
                end if
                
                ! if ((setofrules(j,1) .eq. setofupdates(i+1)) .and. (setofrules(j,2) .eq. setofupdates(i))) then
                !     match = .true.
                !     exit
                ! end if
            end do
        end do
        if (.not. match) then
            t = (size(setofupdates) /2) +1
            mid = mid + setofupdates(t)
            count = count + 1
        end if
    end do

    print *, ""
    print *, mid
    print *, count

    contains
        subroutine getsetsize(rs, us, ri, ui)
            character(len=128), intent(inout), allocatable :: rs(:), us(:)
            integer, intent(inout) :: ri, ui
            integer :: io_err 
            character (len=128) :: line            

            ri = 0
            ui = 0
            allocate(rs(ri))
            allocate(us(ui))
            open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
            if (io_err .ne. 0) then
                print *, "File error occurred", io_err
                stop
            else 
                do 
                    read (unit = fileid, fmt='(A)', iostat=io_err) line
                    if (io_err .ne. 0) exit
                    if (containschar(line,pipedelim)) then
                        ri = ri + 1
                        call resizestringarray(rs,ri)
                        rs(ri) = line
                        !print *, ri, rs(ri)
                    end if
                    if (containschar(line,newline)) then
                        print *, "new line encountered"
                    end if
                    if (containschar(line,commadelim)) then
                        ui = ui + 1
                        call resizestringarray(us, ui)
                        us(updates) = line
                        !print *, ui, us(ui)
                    end if                 
                end do
            end if
            close(unit=fileid)
        end subroutine

        function makerules(rulesetstr, rulesize) result(rulesetint)
            character(len=128), intent(in), allocatable :: rulesetstr (:)
            integer, intent(in) :: rulesize
            integer, allocatable :: rulesetint (:,:)
            integer, allocatable :: tempint(:)
            integer :: ii

            allocate(rulesetint(rulesize,2))
            do ii=1,rulesize
                tempint = str2intarray(rulesetstr(ii),pipedelim)
                ! Reverse the order of the rule set
                rulesetint(ii,1) = tempint(1)
                rulesetint(ii,2) = tempint(2)
            end do
        end function
    
end program part1