    module distributions
        use constants
        implicit none
        interface erfcc
            module procedure erfcc_s, erfcc_v   
        end interface
        INTERFACE poly
		    MODULE PROCEDURE poly_rr, poly_rrv
	    END INTERFACE     
        INTERFACE enordf
		    MODULE PROCEDURE enordf_s, enordf_v
	    END INTERFACE  	       
        contains
        
	    FUNCTION erfcc_s(x)
	        IMPLICIT NONE
	        REAL(dbl), INTENT(IN) :: x
	        REAL(dbl) :: erfcc_s
	        REAL(dbl) :: t,z
	        REAL(dbl), DIMENSION(10) :: coef = (/-1.26551223d0,1.00002368d0,&
		        0.37409196d0,0.09678418d0,-0.18628806d0,0.27886807d0,&
		        -1.13520398d0,1.48851587d0,-0.82215223d0,0.17087277d0/)
	        z=abs(x)
	        t=1.0d0/(1.0d0+0.5d0*z)
	        erfcc_s=t*exp(-z*z+poly(t,coef))
	        if (x < 0.0) erfcc_s=2.0d0-erfcc_s
	    END FUNCTION erfcc_s


	    FUNCTION erfcc_v(x)
	        IMPLICIT NONE
	        REAL(dbl), DIMENSION(:), INTENT(IN) :: x
	        REAL(dbl), DIMENSION(size(x)) :: erfcc_v,t,z
	        REAL(dbl), DIMENSION(10) :: coef = (/-1.26551223d0,1.00002368d0,&
		        0.37409196d0,0.09678418d0,-0.18628806d0,0.27886807d0,&
		        -1.13520398d0,1.48851587d0,-0.82215223d0,0.17087277d0/)
	        z=abs(x)
	        t=1.0d0/(1.0d0+0.5d0*z)
	        erfcc_v=t*exp(-z*z+poly(t,coef))
	        where (x < 0.0) erfcc_v=2.0d0-erfcc_v
	    END FUNCTION erfcc_v        
    
	    FUNCTION poly_rr(x,coeffs)
	        REAL(dbl), INTENT(IN) :: x
	        REAL(dbl), DIMENSION(:), INTENT(IN) :: coeffs
	        REAL(dbl) :: poly_rr
	        REAL(dbl) :: pow
	        REAL(dbl), DIMENSION(:), ALLOCATABLE :: vec
	        INTEGER :: i,n,nn
	        n=size(coeffs)
	        if (n <= 0) then
		        poly_rr=0.0d0
	        else if (n < 8) then
		        poly_rr=coeffs(n)
		        do i=n-1,1,-1
			        poly_rr=x*poly_rr+coeffs(i)
		        end do
	        else
		        allocate(vec(n+1))
		        pow=x
		        vec(1:n)=coeffs
		        do
			        vec(n+1)=0.0d0
			        nn=ishft(n+1,-1)
			        vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			        if (nn == 1) exit
			        pow=pow*pow
			        n=nn
		        end do
		        poly_rr=vec(1)
		        deallocate(vec)
	        end if
	    END FUNCTION poly_rr    
     
 	    FUNCTION poly_rrv(x,coeffs)
	        REAL(dbl), DIMENSION(:), INTENT(IN) :: coeffs,x
	        REAL(dbl), DIMENSION(size(x)) :: poly_rrv
	        INTEGER :: i,n,m
	        m=size(coeffs)
	        n=size(x)
	        if (m <= 0) then
		        poly_rrv=0.0d0
	        else if (m < n .or. m < 8) then
		        poly_rrv=coeffs(m)
		        do i=m-1,1,-1
			        poly_rrv=x*poly_rrv+coeffs(i)
		        end do
	        else
		        do i=1,n
			        poly_rrv(i)=poly_rr(x(i),coeffs)
		        end do
	        end if
	    END FUNCTION poly_rrv  
	 
        function enordf_s(x)
            real(dbl) x, enordf_s
            enordf_s = erfcc(-x/sqrt(2.0d0))/2.0d0
        end function enordf_s
    
        function enordf_v(x)
	        REAL(dbl), DIMENSION(:), INTENT(IN) :: x
	        REAL(dbl), DIMENSION(size(x)) :: enordf_v
            enordf_v = erfcc(-x/sqrt(2.0d0))/2.0d0
        end function enordf_v	
	    
     subroutine GetInitialDistribution(grid1, grid2, stdev, corr, dist)
        implicit none
        real(dbl), dimension(:), intent(in):: grid1
        real(dbl), dimension(:), intent(in):: grid2
        real(dbl), intent(in):: stdev
        real(dbl), intent(in):: corr
        real(dbl), dimension(size(grid1),size(grid2)), intent(out):: dist
        real(dbl) bivnor
        integer n1,n2,i, j
        real(dbl) w1, w2
       
        n1 = size(grid1)
        n2 = size(grid2)
        w1 = (grid1(2) - grid1(1))/2
        w2 = (grid2(2) - grid2(1))/2
        
        
        do i=2,n1-1
        do j=2,n2-1
            dist(i,j) = bivnor((grid1(i)-w1)/stdev,(grid2(j)-w2)/stdev,corr) &
                        - bivnor((grid1(i)+w1)/stdev,(grid2(j)-w2)/stdev,corr) &
                        - bivnor((grid1(i)-w1)/stdev,(grid2(j)+w2)/stdev,corr) &
                        + bivnor((grid1(i)+w1)/stdev,(grid2(j)+w2)/stdev,corr)
                        
        end do
        end do
               
        do i=1,n1
            dist(i,1) = (1-enordf( (grid1(i) - w1)/stdev )) - (1-enordf( (grid1(i) + w1)/stdev )) &
                         - bivnor((grid1(i)-w1)/stdev,(grid2(1)+w2)/stdev,corr) + &
                         bivnor((grid1(i)+w1)/stdev,(grid2(1)+w2)/stdev,corr)
                         
            dist(i,n2) = bivnor((grid1(i)-w1)/stdev,(grid2(n2)-w2)/stdev,corr) - &
                         bivnor((grid1(i)+w1)/stdev,(grid2(n2)-w2)/stdev,corr)                    
        end do
        do j=1,n2
            dist(1,j) = (1-enordf( (grid2(j) - w2)/stdev )) - (1-enordf( (grid2(j) + w2)/stdev )) &
                         - bivnor((grid1(1)+w1)/stdev,(grid2(j)-w2)/stdev,corr) &
                         + bivnor((grid1(1)+w1)/stdev,(grid2(j)+w2)/stdev,corr)
            dist(n1,j) = bivnor((grid1(n1)-w1)/stdev,(grid2(j)-w2)/stdev,corr) &
            - bivnor((grid1(n1)-w1)/stdev,(grid2(j)+w2)/stdev,corr)    
        end do
        dist(1,1) = 1 - (1-enordf( (grid1(1) + w1)/stdev )) - (1-enordf( (grid2(1) + w2)/stdev )) &
                    + bivnor((grid1(1)+w1)/stdev,(grid2(1)+w2)/stdev,corr)
        !dist(1,1) = 1 - bivnor((grid1(1)+w1)/stdev,-5*stdev,corr) - bivnor(-5*stdev,(grid2(1)+w2)/stdev,corr) &
        !            + bivnor((grid1(1)+w1)/stdev,(grid2(1)+w2)/stdev,corr)                    
        dist(1,n2) = (1-enordf( (grid2(n2) - w2)/stdev )) &
        - bivnor((grid1(1)+w1)/stdev,(grid2(n2)-w2)/stdev,corr)
        dist(n1,1) = (1-enordf( (grid1(n1) - w1)/stdev )) &
        - bivnor((grid1(n1)-w1)/stdev,(grid2(1)+w2)/stdev,corr)
        dist(n1,n2) = bivnor((grid1(n1)-w1)/stdev,(grid2(n2)-w2)/stdev,corr)
        
    end subroutine GetInitialDistribution
	    
    end module distributions