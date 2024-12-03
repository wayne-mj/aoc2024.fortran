module mergesortmod
    implicit none
    contains
    
    recursive subroutine mergesort(arr, size)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: size
        
        real, allocatable :: temp(:)
        integer :: mid, i, j, k
        
        ! Base case: if the array has one or zero elements, it's already sorted
        if (size <= 1) return
        
        ! Calculate the midpoint of the array
        mid = size / 2
        
        ! Recursively sort the left and right halves of the array
        call mergesort(arr(1:mid), mid)
        call mergesort(arr(mid+1:size), size-mid)
        
        ! Merge the sorted halves back together
        allocate(temp(size))
        i = 1
        j = mid + 1
        k = 1
        do while (i <= mid .and. j <= size)
            if (arr(i) <= arr(j)) then
                temp(k) = arr(i)
                i = i + 1
            else
                temp(k) = arr(j)
                j = j + 1
            end if
            k = k + 1
        end do
        
        ! Copy any remaining elements from the left half
        do while (i <= mid)
            temp(k) = arr(i)
            k = k + 1
            i = i + 1
        end do
        
        ! Copy any remaining elements from the right half
        do while (j <= size)
            temp(k) = arr(j)
            k = k + 1
            j = j + 1
        end do
        
        ! Copy the sorted elements back to the original array
        arr = temp
        
        ! Free the temporary array
        deallocate(temp)
        
    end subroutine mergesort
end module mergesortmod