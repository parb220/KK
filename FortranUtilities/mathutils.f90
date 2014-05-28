
module mathutils
    use constants
    
    implicit none
    interface kron
        module procedure kronMat, kronVect
    end interface    
    
    contains  
    
    !kronecker product two matrices            
    function kronMat(Amat,Bmat)
        implicit none
        real(dbl), intent(in):: Amat(:,:), Bmat(:,:)
        real(dbl) kronMat(size(Amat,1)*size(Bmat,1),size(Amat,2)*size(Bmat,2))
        integer nA, mA, nB, mB, i, j

        nA = size(Amat,1)
        mA = size(Amat,2)
        nB = size(Bmat,1)
        mB = size(Bmat,2)
        do i=1,nA
            do j=1,mA
                kronMat(1+(i-1)*nB:nB+(i-1)*nB,1+(j-1)*mB:mB+(j-1)*mB) = Bmat*Amat(i,j)
            end do
        end do
    end function kronMat

    !kronecker product of two vectors
    function kronVect(Avect,Bvect)
        implicit none
        real(dbl), intent(in):: Avect(:), Bvect(:)
        real(dbl) kronVect(size(Avect,1)*size(Bvect,1))
        integer nA, nB, i

        nA = size(Avect,1)
        nB = size(Bvect,1)

        do i=1,nA
                kronVect(1+(i-1)*nB:nB+(i-1)*nB) = Bvect*Avect(i)
        end do
    end function kronVect
    
    !linspace
    function linspace(xmin,xmax,n)
        real(dbl) xmin, xmax, linspace(n)
        integer n, i
        
        linspace = (/ (xmin + (xmax-xmin)*i/(n-1),i=0,n-1) /)
    end function linspace
    
    SUBROUTINE qsortd(x,indices,n)
 
        ! Code converted using TO_F90 by Alan Miller
        ! Date: 2002-12-18  Time: 11:55:47

        IMPLICIT NONE
        

        REAL (dbl), INTENT(IN)  :: x(:)
        INTEGER   :: indices(:)
        INTEGER, INTENT(IN)    :: n

        !***************************************************************************

        !                                                         ROBERT RENKA
        !                                                 OAK RIDGE NATL. LAB.

        !   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO SORT A REAL (dp)
        ! ARRAY X INTO INCREASING ORDER.  THE ALGORITHM IS AS FOLLOWS.  IND IS
        ! INITIALIZED TO THE ORDERED SEQUENCE OF INDICES 1,...,N, AND ALL INTERCHANGES
        ! ARE APPLIED TO IND.  X IS DIVIDED INTO TWO PORTIONS BY PICKING A CENTRAL
        ! ELEMENT T.  THE FIRST AND LAST ELEMENTS ARE COMPARED WITH T, AND
        ! INTERCHANGES ARE APPLIED AS NECESSARY SO THAT THE THREE VALUES ARE IN
        ! ASCENDING ORDER.  INTERCHANGES ARE THEN APPLIED SO THAT ALL ELEMENTS
        ! GREATER THAN T ARE IN THE UPPER PORTION OF THE ARRAY AND ALL ELEMENTS
        ! LESS THAN T ARE IN THE LOWER PORTION.  THE UPPER AND LOWER INDICES OF ONE
        ! OF THE PORTIONS ARE SAVED IN LOCAL ARRAYS, AND THE PROCESS IS REPEATED
        ! ITERATIVELY ON THE OTHER PORTION.  WHEN A PORTION IS COMPLETELY SORTED,
        ! THE PROCESS BEGINS AGAIN BY RETRIEVING THE INDICES BOUNDING ANOTHER
        ! UNSORTED PORTION.

        ! INPUT PARAMETERS -   N - LENGTH OF THE ARRAY X.

        !                      X - VECTOR OF LENGTH N TO BE SORTED.

        !                    IND - VECTOR OF LENGTH >= N.

        ! N AND X ARE NOT ALTERED BY THIS ROUTINE.

        ! OUTPUT PARAMETER - IND - SEQUENCE OF INDICES 1,...,N PERMUTED IN THE SAME
        !                          FASHION AS X WOULD BE.  THUS, THE ORDERING ON
        !                          X IS DEFINED BY Y(I) = X(IND(I)).

        !*********************************************************************

        ! NOTE -- IU AND IL MUST BE DIMENSIONED >= LOG(N) WHERE LOG HAS BASE 2.

        !*********************************************************************

        INTEGER   :: iu(21), il(21)
        INTEGER   :: m, i, j, k, l, ij, it, itt, indx
        REAL      :: r
        REAL (dbl) :: t

        ! LOCAL PARAMETERS -

        ! IU,IL =  TEMPORARY STORAGE FOR THE UPPER AND LOWER
        !            INDICES OF PORTIONS OF THE ARRAY X
        ! M =      INDEX FOR IU AND IL
        ! I,J =    LOWER AND UPPER INDICES OF A PORTION OF X
        ! K,L =    INDICES IN THE RANGE I,...,J
        ! IJ =     RANDOMLY CHOSEN INDEX BETWEEN I AND J
        ! IT,ITT = TEMPORARY STORAGE FOR INTERCHANGES IN IND
        ! INDX =   TEMPORARY INDEX FOR X
        ! R =      PSEUDO RANDOM NUMBER FOR GENERATING IJ
        ! T =      CENTRAL ELEMENT OF X

        IF (n <= 0) RETURN

        ! INITIALIZE IND, M, I, J, AND R

        DO  i = 1, n
          indices(i) = i
        END DO
        m = 1
        i = 1
        j = n
        r = .375

        ! TOP OF LOOP

        20 IF (i >= j) GO TO 70
        IF (r <= .5898437) THEN
          r = r + .0390625
        ELSE
          r = r - .21875
        END IF

        ! INITIALIZE K

        30 k = i

        ! SELECT A CENTRAL ELEMENT OF X AND SAVE IT IN T

        ij = i + r*(j-i)
        it = indices(ij)
        t = x(it)

        ! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
        !   INTERCHANGE IT WITH T

        indx = indices(i)
        IF (x(indx) > t) THEN
          indices(ij) = indx
          indices(i) = it
          it = indx
          t = x(it)
        END IF

        ! INITIALIZE L

        l = j

        ! IF THE LAST ELEMENT OF THE ARRAY IS LESS THAN T,
        !   INTERCHANGE IT WITH T

        indx = indices(j)
        IF (x(indx) >= t) GO TO 50
        indices(ij) = indx
        indices(j) = it
        it = indx
        t = x(it)

        ! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
        !   INTERCHANGE IT WITH T

        indx = indices(i)
        IF (x(indx) <= t) GO TO 50
        indices(ij) = indx
        indices(i) = it
        it = indx
        t = x(it)
        GO TO 50

        ! INTERCHANGE ELEMENTS K AND L

        40 itt = indices(l)
        indices(l) = indices(k)
        indices(k) = itt

        ! FIND AN ELEMENT IN THE UPPER PART OF THE ARRAY WHICH IS
        !   NOT LARGER THAN T

        50 l = l - 1
        indx = indices(l)
        IF (x(indx) > t) GO TO 50

        ! FIND AN ELEMENT IN THE LOWER PART OF THE ARRAY WHCIH IS NOT SMALLER THAN T

        60 k = k + 1
        indx = indices(k)
        IF (x(indx) < t) GO TO 60

        ! IF K <= L, INTERCHANGE ELEMENTS K AND L

        IF (k <= l) GO TO 40

        ! SAVE THE UPPER AND LOWER SUBSCRIPTS OF THE PORTION OF THE
        !   ARRAY YET TO BE SORTED

        IF (l-i > j-k) THEN
          il(m) = i
          iu(m) = l
          i = k
          m = m + 1
          GO TO 80
        END IF

        il(m) = k
        iu(m) = j
        j = l
        m = m + 1
        GO TO 80

        ! BEGIN AGAIN ON ANOTHER UNSORTED PORTION OF THE ARRAY

        70 m = m - 1
        IF (m == 0) RETURN
        i = il(m)
        j = iu(m)

        80 IF (j-i >= 11) GO TO 30
        IF (i == 1) GO TO 20
        i = i - 1

        ! SORT ELEMENTS I+1,...,J.  NOTE THAT 1 <= I < J AND J-I < 11.

        90 i = i + 1
        IF (i == j) GO TO 70
        indx = indices(i+1)
        t = x(indx)
        it = indx
        indx = indices(i)
        IF (x(indx) <= t) GO TO 90
        k = i

        100 indices(k+1) = indices(k)
        k = k - 1
        indx = indices(k)
        IF (t < x(indx)) GO TO 100

        indices(k+1) = it
        GO TO 90
    END SUBROUTINE qsortd

end module mathutils