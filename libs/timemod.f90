module timemod
    implicit none
    
    contains
    
    subroutine msdelay(delayms)
        integer :: delayms
        integer :: start, current, rate
        real :: ticks_per_ms

        call SYSTEM_CLOCK(COUNT_RATE=rate)
        ticks_per_ms = real(rate) / 1000.0

        call SYSTEM_CLOCK(COUNT=start)
        do 
            call SYSTEM_CLOCK(count=current)
            if ((current - start) >= delayms * ticks_per_ms) exit                
        end do
    end subroutine
end module timemod