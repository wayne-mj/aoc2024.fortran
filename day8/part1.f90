program part1
    !use mergemod
    use stringmod

    implicit none

    integer :: fileid, io_err
    character (len=32) :: lines, filename
    character (len=128) :: mydata, mydata2
        
    filename = "testinput1.txt"
    fileid = 8

    open (unit=fileid, file=filename, status='old', action='read', iostat=io_err)
    if (io_err .ne. 0) then
        print *, "File error occurred", io_err
        stop
    else 
        do 
            read (unit = fileid, fmt='(A)', iostat=io_err) lines
            if (io_err .ne. 0) exit

            print *, trim(lines)
            mydata = getdatagram(lines)

            mydata2 = compress(mydata)
            print *, mydata
        end do
    end if
    close(unit=fileid)

    contains
    ! 00...111...2...333.44.5555.6666.777.888899
    ! ##...###...#...###.##.####.####.###.######
    ! 00...111...2...333.44.5555.6666.777.888899

    function getdatagram(datamap) result(datagram)
        character (len=32), intent(in) :: datamap
        character(len=128) :: datagram
        character(len=32) :: tempstr
        integer :: i, length, instruct, count, j, blkcount

        datagram = ""

        blkcount = 0
        count =1
        length = len_trim(datamap)
        print *, length
        do i=1, length
            if (mod(i,2) .eq. 0) then
                tempstr = datamap(i:i)
                instruct = str2int(tempstr)
                do j=1, instruct
                    datagram(count:count) = "."
                    count = count +1
                end do
            else
                tempstr = datamap(i:i)
                instruct = str2int(tempstr)
                if (blkcount .gt. 9) then
                    blkcount = 0
                end if
                do j=1, instruct
                    datagram(count:count) = int2str(blkcount)
                    count = count + 1
                end do
                blkcount = blkcount + 1
            end if
        end do
    end function

    function compress(indatagram) result(outdatagram)
        character(len=128), intent(in) :: indatagram
        character(len=128) :: outdatagram, tempdatagram, cdatagram
        character(len=1) :: tempstr
        integer :: inlength, outlength, templength, i, j, count
        logical :: contig

        tempdatagram = ""
        cdatagram = ""
        outdatagram = ""
        count = 1
        contig = .false.

        inlength = len_trim(indatagram)
        tempdatagram = trim(indatagram)
        
        ! do i=1,inlength
        !     if (tempdatagram(i:i) .eq. ".") then
        !         tempdatagram(i:i) = " "
        !     end if
        ! end do

        cdatagram = tempdatagram

        print *, tempdatagram

        do i=inlength,1 ,-1
            if (tempdatagram(i:i) .ne.  ".") then
                tempstr = tempdatagram(i:i)
                do j=1,inlength
                    if (cdatagram(j:j) .eq. ".") then
                        cdatagram(j:j) = trim(tempstr)
                        cdatagram(i:i) = "."
                        print *, cdatagram, i, j
                        exit
                    end if
                end do
            end if
        end do
    end function
end program part1