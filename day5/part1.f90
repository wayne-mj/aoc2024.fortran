program part1
    !use mergesortmod
    use stringmod

    implicit none

    integer :: fileid, ierr
    character (len=32) :: filename
    character(len=128), allocatable :: ruleset(:), updateset(:)
    integer :: rules, updates , i ,j , k , mid, t
    integer, allocatable :: setofrules(:,:)
    integer, allocatable :: setofupdates (:)
    logical :: match
    
    filename = ""
    fileid = 8
    match = .false.
    mid =0

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
        setofupdates = str2intarray(updateset(k),commadelim)
        !print *, setofupdates
        do i=1, size(setofupdates) -1
            do j=1, (size(setofrules) / 2)
                !print *, "setofrules(j,1): ", setofrules(j,1), " setofupdates(i+1): ", setofupdates(i+1) , " setofrules(j,2): ", setofrules(j,2), " setofupdates(i): ", setofupdates(i)
                if (all(setofrules(:,j) .eq. [setofupdates(i+1), setofupdates(i)]))then
                    match = .true.
                end if
                !print *, "setofrules(j,1): ", setofrules(j,1), " setofupdates(i+1): ", setofupdates(i+1) , " setofrules(j,2): ", setofrules(j,2), " setofupdates(i): ", setofupdates(i)
                ! if ((setofrules(1,j) .eq. setofupdates(i+1)) .and. (setofrules(2,j) .eq. setofupdates(i))) then
                !     match = .true.
                ! end if
            end do
        end do
        ! print *, match
        if (.not. match) then
            t = (size(setofupdates) /2) +1
            ! print *, ""
            ! print *, "setofupdates: ", setofupdates
            ! print *, "setofupdates(t): ", setofupdates(t)
            mid = mid + setofupdates(t)
            ! print *,"mid: ", mid
            match = .false.
        end if
    end do

    print *, mid

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
            integer :: i

            allocate(rulesetint(rulesize,2))
            do i=1,rulesize
                tempint = str2intarray(rulesetstr(i),pipedelim)
                ! Reverse the order of the rule set
                rulesetint(i,2) = tempint(1)
                rulesetint(i,1) = tempint(2)
            end do
        end function
    
end program part1