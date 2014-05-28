!! sorts.f90

module sorts
    use constants
    implicit none
    
contains
!Swaps a and b. 
subroutine Swap(a, b)
    real(dbl), intent(inout):: a, b 
	real(dbl) temp
	temp = a
	a = b
	b = temp
end subroutine Swap


!Sorts A using a bubblesort.
subroutine BubbleSort(A) 
    real(dbl) A(:)
    integer swapped, n, i
    n = size(A,1)
    
	swapped = 1
	do
	    if (swapped == 0) then
	        exit
	    end if    
	    swapped = 0
		do i = 1, n-1 
			if(A(i) > A(i+1)) then
				call Swap(A(i), A(i+1))
				swapped = 1
		    end if
		end do 
	end do
end subroutine BubbleSort				

	

!Private helper function which perdoms the insertion sort on
!the array "B" (of size "size") in-place.
subroutine Insertionsort(B)
    real(dbl) B(:), temp
    integer n, i, j
    n = size(B,1)
	do i = 2,n
		temp = B(i)	
		do j=i,2,-1
		    if (temp > B(j-1)) then
		        exit
		    end if	
			B(j) = B(j-1)
	    end do
		B(j) = temp
	end do
end subroutine Insertionsort



!Private helper function which perdoms the insertion sort on
!the array "B" (of size "size") in-place.
subroutine Insertionsort2(B,A)
    real(dbl) B(:), temp, A(:), temp2
    integer n, i, j
    n = size(B,1)
	do i = 2,n
		temp = B(i)
		temp2 = A(i)	
		do j=i,2,-1
		    if (temp > B(j-1)) then
		        exit
		    end if	
			B(j) = B(j-1)
			A(j) = A(j-1)
	    end do
		B(j) = temp
		A(j) = temp2
	end do
end subroutine Insertionsort2


!Private helper function which merges two sublists within "A"
!located at "left" to "right"-1, and "right" to "rightend".
!"tempA" is used do temporary storage.  It is passed in so it can
!be created only once and then re-used, which saves time.
recursive subroutine Merge2(A, tempA, left1, right1, rightend1)
    real(dbl), intent(inout):: tempA(:), A(:)
    integer, intent(in):: left1, right1, rightend1
    integer left, right, rightend
    integer leftend,  temp, n, i
    left = left1
    right = right1
    rightend = rightend1
	leftend = right-1
	temp = left
	n = rightend - left + 1
	
	do 
	    if (left > leftend .or. right > rightend) then
	        exit
	    end if
		if (A(left) <= A(right)) then
			tempA(temp) = A(left)
			temp = temp + 1
			left = left + 1
		else
			tempA(temp) = A(right)
			temp = temp + 1
			right = right + 1			
	    end if
	end do
	do
	    if (left > leftend) then  !copy the rest of the left half
	        exit
	    end if
		tempA(temp) = A(left)
		temp = temp + 1
		left = left + 1	
    end do	
	do
	    if (right > rightend) then   !copy the rest of the right half
	        exit
	    end if
		tempA(temp) = A(right)
		temp = temp + 1
		right = right + 1	
    end do
	!Copy tempA into A
	do i=1,n
		A(rightend) = tempA(rightend)
		rightend = rightend - 1
	end do	
end subroutine Merge2

!Private helper function which perdoms the merge sort algorithm
!on array "A" between the positions "left" and "right".
!"tempA" is used do temporary storage.  It is passed in so it can
!be created only once and then re-used, which saves time.
recursive subroutine MergeSort2(A, tempA, left1, right1)
    real(dbl), intent(inout):: tempA(:), A(:)
    integer, intent(in):: left1, right1
    integer left, right, middle
    left = left1
    right = right1
    
	if (left < right) then
		middle = (left+right)/2
		call MergeSort2(A, tempA, left, middle)
		call MergeSort2(A, tempA, middle+1, right)
		call Merge2(A, tempA, left, middle+1, right)
	end if
end subroutine MergeSort2	

subroutine MergeSort(A)
    real(dbl), intent(inout):: A(:)
    real(dbl) tempA(size(A,1))
    integer n
    n = size(A, 1)
    call MergeSort2(A, tempA, 1, n)

end subroutine MergeSort

!Private helper function which perdoms a quicksort on array "B"
!which is passed in.  "N" is the size of "B".
recursive subroutine Quicksort(B, N) 
   real(dbl) B(:)
   integer N
   integer pivotIndex
   
   if (N > 1) then
      pivotIndex = Partition(B, N)

      call Quicksort(B, pivotIndex)

      call Quicksort(B(pivotIndex+1:), N-pivotIndex)
   end if
end subroutine Quicksort


!Private helper function which perdoms a quicksort on array "B"
!which is passed in.  "N" is the size of "B".
recursive subroutine Quicksort2(B, N, A) 
   real(dbl) B(:), A(:)
   integer N
   integer pivotIndex
   
   if (N > 1) then
      pivotIndex = Partition2(B, N, A)

      call Quicksort2(B, pivotIndex, A)

      call Quicksort2(B(pivotIndex+1:), N-pivotIndex, A(pivotIndex+1:))
   end if
end subroutine Quicksort2


!Helper function do Quicksort which arranges the elements of "B"
!in relation to a pivot value.  The index of this pivot element is
!then returned.
function Partition(B, N)
    real(dbl) B(:)
    integer N
    integer i, j, incr, decr, swap2, Partition

    incr = 0
    decr = 1
    i = 1
    j = N

    do
        if (i == j) exit
 
        if (B(i) > B(j)) then
         call Swap(B(i),B(j))

         swap2 = incr
         incr = decr
         decr = swap2
        end if

        i = i + incr
        j = j - decr
    end do

    Partition = j
end function Partition

!Helper function do Quicksort which arranges the elements of "B"
!in relation to a pivot value.  The index of this pivot element is
!then returned.
function Partition2(B, N, A)
    real(dbl) B(:), A(:)
    integer N
    integer i, j, incr, decr, swap2, Partition2

    incr = 0
    decr = 1
    i = 1
    j = N

    do
        if (i == j) exit
 
        if (B(i) > B(j)) then
         call Swap(B(i),B(j))
         call Swap(A(i),A(j))
         swap2 = incr
         incr = decr
         decr = swap2
        end if

        i = i + incr
        j = j - decr
    end do

    Partition2 = j
end function Partition2


!Private helper function which perfoms an optimized quicksort on "B".
!If the size "N" is less than 14, an insertion sort is used instead,
!since a quicksort perfoms poorly on small arrays.
recursive subroutine Quicksortoptimized (B,N)
    real(dbl) B(:)
    integer N
    integer pivotIndex
	if(N <= 14) then
		call Insertionsort(B)
    
	else 
   	    if (N > 1) then
      	    pivotIndex = Partition(B, N)

      	    call Quicksortoptimized(B, pivotIndex)

      	    call Quicksortoptimized(B(pivotIndex+1:), (N-pivotIndex))
   	    end if
	end if
    
end subroutine Quicksortoptimized	

!Private helper function which perdoms an optimized quicksort on "B".
!If the size "N" is less than 14, an insertion sort is used instead,
!since a quicksort perfoms poorly on small arrays.
recursive subroutine Quicksortoptimized2 (B,N,A)
    real(dbl) B(:), A(:)
    integer N
    integer pivotIndex
	if(N <= 14) then
		call Insertionsort2(B,A)
    
	else 
   	    if (N > 1) then
      	    pivotIndex = Partition2(B, N,A)

      	    call Quicksortoptimized2(B, pivotIndex, A)

      	    call Quicksortoptimized2(B(pivotIndex+1:), (N-pivotIndex), A(pivotIndex+1:))
   	    end if
	end if
    
end subroutine Quicksortoptimized2		
			 
end module sorts