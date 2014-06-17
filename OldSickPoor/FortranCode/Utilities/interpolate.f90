module interpolate1
    use constants
    interface Interpolate
        module procedure InterpolateVect, InterpolateScalar
    end interface 
    interface Locate
        module procedure Locate_d, Locate_i    
    end interface Locate   
    contains
    
    !jl is the location of x1 in xvect 
    !xvect(jl) < x1 < xvect(jl+1)
	 subroutine Locate_d(xvect, x1, jl)
	    real(dbl), dimension(:), intent(in):: xvect
	    real(dbl) x1
	    integer jl
	    
	    integer N, jm, ju
	    logical ascnd
	    
	    N=size(xvect)
    	ascnd = (xvect(N) >= xvect(1))
        jl=0
        ju=N+1
        do
            if (ju-jl <= 1) exit
            jm = ( ju + jl ) / 2
            if (ascnd .eqv. (x1 >= xvect(jm))) then
	            jl = jm
            else
	            ju = jm
            end if
        end do
    end subroutine Locate_d
    
	 subroutine Locate_i(xvect, x1, jl)
	    integer, dimension(:), intent(in):: xvect
	    integer x1
	    integer jl
	    
	    integer N, jm, ju
	    logical ascnd
	    
	    N=size(xvect)
    	ascnd = (xvect(N) >= xvect(1))
        jl=0
        ju=N+1
        do
            if (ju-jl <= 1) exit
            jm = ( ju + jl ) / 2
            if (ascnd .eqv. (x1 >= xvect(jm))) then
	            jl = jm
            else
	            ju = jm
            end if
        end do
    end subroutine Locate_i    

    subroutine get_interps(x1vect,x1, loc, mx)  
        implicit none
            
        real(dbl), intent(in):: x1
        real(dbl), intent(in):: x1vect(:)
        integer, intent(out):: loc
        real(dbl), intent(out):: mx
        integer Nox1
             
        loc=1        
        Nox1 = size(x1vect,1)
        
        if (abs(x1-x1vect(loc)) < 1.d-10) then
            mx = 0.0d0        
            return               
        end if       
        if (x1vect(1) >= x1) then
            loc = 1
        else if (x1vect(Nox1) <= x1) then
            loc = Nox1 - 1
        else
            call Locate(x1vect,x1,loc)
        end if                    
	
	    mx = (x1-x1vect(loc))/(x1vect(loc+1)-x1vect(loc))	
	   
    end subroutine get_interps

	 subroutine InterpolateVect(xvect, yvect, x1vect, y1vect)
	    real(dbl), dimension(:), intent(in):: xvect
	    real(dbl), dimension(:), intent(in):: yvect
	    real(dbl), dimension(:), intent(in):: x1vect
	    real(dbl), dimension(:), intent(out):: y1vect
	    
	    real(dbl) slope, inter
	    integer N, N1
	    integer i,j
	    integer loc, jl,jm,ju
	    logical ascnd
	    
	    N = size(xvect)
	    N1 = size(x1vect)
	    
        do i=1,N1 
	    
		    if ( x1vect(i) < xvect(1) ) then
			    slope = ( yvect(2)-yvect(1) ) / ( xvect(2)-xvect(1) )
			    inter = yvect(2) - slope * xvect(2)
			    y1vect(i) = inter + slope * x1vect(i)
		    else if ( x1vect(i) > xvect(N)) then
			    slope = ( yvect(N)-yvect(N-1) ) / ( xvect(N)-xvect(N-1) )
			    inter = yvect(N) - slope * xvect(N)
			    y1vect(i) = inter + slope * x1vect(i)
		    else
		        call Locate(xvect, x1vect(i), loc)
			    slope = (yvect(loc+1)-yvect(loc))/(xvect(loc+1)-xvect(loc))
			    inter = yvect(loc) - slope * xvect(loc)
			    y1vect(i) = inter + slope * x1vect(i)
		    end if 
		    
        end do	    
	    
	 end subroutine InterpolateVect
	 
	 subroutine InterpolateScalar(xvect, yvect, x1, y1)
	    real(dbl), dimension(:), intent(in):: xvect
	    real(dbl), dimension(:), intent(in):: yvect
	    real(dbl), intent(in):: x1
	    real(dbl), intent(out):: y1
	    
	    real(dbl) slope, inter
	    integer N
	    integer i,j
	    integer loc, jl,jm,ju
	    logical ascnd
	    
	    N = size(xvect)	    
	    
	    if (x1 == xvect(1)) then
	        y1 = yvect(1)
	        return
	    end if
	    if (x1 >= xvect(N)) then
	        y1 = yvect(N)
	        return
	    end if
	    
	    if ( x1 < xvect(1) ) then
		    slope = ( yvect(2)-yvect(1) ) / ( xvect(2)-xvect(1) )
		    inter = yvect(2) - slope * xvect(2)
		    y1 = inter + slope * x1
	    !else if ( x1 > xvect(N)) then
		!    slope = ( yvect(N)-yvect(N-1) ) / ( xvect(N)-xvect(N-1) )
		!    inter = yvect(N) - slope * xvect(N)
		!    y1 = inter + slope * x1
		else if (N==2) then
		    slope = (yvect(2)-yvect(1))/(xvect(2)-xvect(1))
		    inter = yvect(1) - slope * xvect(1)
		    y1 = inter + slope * x1		
	    else	    
            call Locate(xvect, x1, loc)
            if (abs(xvect(loc)-x1)<1.d-10) then
                y1 = yvect(loc)
            else
		        slope = (yvect(loc+1)-yvect(loc))/(xvect(loc+1)-xvect(loc))
		        inter = yvect(loc) - slope * xvect(loc)
		        y1 = inter + slope * x1	            
            end if
	    end if  
	    
    end subroutine InterpolateScalar
    	    
    subroutine interpol2d(x1vect, x2vect, y, x1, x2, yi)
        
        implicit none
            
        real(dbl), intent(in):: y(:,:)
        real(dbl), intent(in):: x1, x2
        real(dbl), intent(in):: x1vect(:), x2vect(:)
        real(dbl), intent(out):: yi
        integer m,n, Nox1, Nox2
        real(dbl) yt(4), mx1, mx2
        
        m=1
        n=1
        
        Nox1 = size(y,1)
        Nox2 = size(y,2)       
        
        if (x1 == x1vect(1) .and. x2 == x2vect(1)) then
            yi = y(1,1)
            return
        end if
        if (x1 == x1vect(1)) then	        
	        call InterpolateScalar(x2vect, y(1,:), x2, yi)                         
            return             
        end if
        if (x2 == x2vect(1)) then	        
	        call InterpolateScalar(x1vect, y(:,1), x1, yi)                         
            return             
        end if        
        
        if (x1vect(1) >= x1) then
            m = 1
        else if (x1vect(Nox1) <= x1) then
        !    m = Nox1 - 1
            call InterpolateScalar(x2vect, y(Nox1,:), x2, yi)                         
            return 
        else
            call Locate(x1vect,x1,m)
        end if
        
        if (x2vect(1) >= x2) then
            n = 1
        else if (x2vect(Nox2) <= x2) then
        !    n = Nox2 - 1
	        call InterpolateScalar(x1vect, y(:,Nox2), x1, yi)                         
            return                
        else
            call Locate(x2vect,x2,n)
        end if        
        
	    yt(1) = y(m,n)
	    yt(2) = y(m+1,n)
	    yt(3) = y(m,n+1)
	    yt(4) = y(m+1,n+1)
        
        if (abs(x1-x1vect(m)) < 1.d-10) then
            mx1 = 0.0d0
        else
	        mx1 = (x1-x1vect(m))/(x1vect(m+1)-x1vect(m))	
	    end if
	    if (abs(x2-x2vect(n)) < 1.d-10) then
	        mx2 = 0.0d0
	    else
	        mx2 = (x2-x2vect(n))/(x2vect(n+1)-x2vect(n))
        end if
        	
	    yi =    (1-mx1)*(1-mx2)*yt(1) + &
                mx1*(1-mx2)*yt(2) + &
                (1-mx1)*mx2*yt(3) + &
                mx1*mx2*yt(4) 
    	    	
    	
    end subroutine interpol2d	     
	    
    subroutine interpol3d(x1vect, x2vect, x3vect, y, x1, x2, x3, yi)
        
        implicit none
            
        real(dbl), intent(in):: y(:,:,:)
        real(dbl), intent(in):: x1, x2, x3
        real(dbl), intent(in):: x1vect(:), x2vect(:), x3vect(:)
        real(dbl), intent(out):: yi
        integer m,n,i,j, Nox1, Nox2, Nox3
        real(dbl) yt(8), mx1, mx2, mx3
        
        m=1
        n=1
        j=1
        
        Nox1 = size(y,1)
        Nox2 = size(y,2)
        Nox3 = size(y,3)
        
        if (x1 == x1vect(1) .and. x2 == x2vect(1) .and. x3 == x3vect(1)) then
            yi = y(1,1,1)
            return
        else if (x1 == x1vect(1)) then	        
	        call interpol2d(x2vect,x3vect, y(1,:,:), x2,x3, yi)                         
            return             
        else if (x2 == x2vect(1)) then	        
	        call interpol2d(x1vect,x3vect, y(:,1,:), x1,x3, yi)                         
            return             
        else if (x3 == x3vect(1)) then
	        call interpol2d(x1vect,x2vect, y(:,:,1), x1,x2, yi)                         
            return        
        end if                
               
        if (x1vect(1) >= x1) then
            m = 1
        else if (x1vect(Nox1) <= x1) then
        !    m = Nox1 - 1
	        call interpol2d(x2vect,x3vect, y(Nox1,:,:), x2,x3, yi)                         
            return              
        else
            call Locate(x1vect,x1,m)
        end if
        
        if (x2vect(1) >= x2) then
            n = 1
        else if (x2vect(Nox2) <= x2) then
        !    n = Nox2 - 1
	        call interpol2d(x1vect,x3vect, y(:,Nox2,:), x1,x3, yi)                         
            return             
        else
            call Locate(x2vect,x2,n)
        end if        

        if (x3vect(1) >= x3) then
            j = 1
        else if (x3vect(Nox3) <= x3) then
        !    j = Nox3 - 1
	        call interpol2d(x1vect,x2vect, y(:,:,Nox3), x1,x2, yi)                         
            return            
        else
            call Locate(x3vect,x3,j)
        end if  
        
	    yt(1) = y(m,n,j)
	    yt(2) = y(m+1,n,j)
	    yt(3) = y(m,n+1,j)
	    yt(4) = y(m+1,n+1,j)
	    yt(5) = y(m,n,j+1)
	    yt(6) = y(m+1,n,j+1)
	    yt(7) = y(m,n+1,j+1)
	    yt(8) = y(m+1,n+1,j+1)

	    mx1 = (x1-x1vect(m))/(x1vect(m+1)-x1vect(m))	
	    mx2 = (x2-x2vect(n))/(x2vect(n+1)-x2vect(n))
	    mx3 = (x3-x3vect(j))/(x3vect(j+1)-x3vect(j))

        if (abs(x1-x1vect(m)) < 1.d-10) then
            mx1 = 0.0d0
        else
	        mx1 = (x1-x1vect(m))/(x1vect(m+1)-x1vect(m))	
	    end if
	    if (abs(x2-x2vect(n)) < 1.d-10) then
	        mx2 = 0.0d0
	    else
	        mx2 = (x2-x2vect(n))/(x2vect(n+1)-x2vect(n))
        end if  
	    if (abs(x3-x3vect(j)) < 1.d-10) then
	        mx3 = 0.0d0
	    else
	        mx3 = (x3-x3vect(j))/(x3vect(j+1)-x3vect(j))
        end if           	
    	
	    yi =    (1-mx1)*(1-mx2)*(1-mx3)*yt(1) + &
                mx1*(1-mx2)*(1-mx3)*yt(2) + &
                (1-mx1)*mx2*(1-mx3)*yt(3) + &
                mx1*mx2*(1-mx3)*yt(4) + &
                (1-mx1)*(1-mx2)*mx3*yt(5) + &
                mx1*(1-mx2)*mx3*yt(6) + &
                (1-mx1)*mx2*mx3*yt(7) + &
                mx1*mx2*mx3*yt(8)  
    	
    end subroutine interpol3d
    
    subroutine get_interp3d(x1vect, x2vect, x3vect, x1, x2, x3, locs, mxs)  
        implicit none
            
        real(dbl), intent(in):: x1, x2, x3
        real(dbl), intent(in):: x1vect(:), x2vect(:), x3vect(:)
        integer, intent(out):: locs(3)
        real(dbl), intent(out):: mxs(3)
        integer Nox1, Nox2, Nox3
             
        locs=1        
        Nox1 = size(x1vect,1)
        Nox2 = size(x2vect,1)
        Nox3 = size(x3vect,1)

        if (x1 == x1vect(1) .and. x2 == x2vect(1) .and. x3 == x3vect(1)) then            
            mxs = 0.0d0           
            return
        end if
        !else if (x1 == x1vect(1)) then	        
	    !    call interpol2d(x2vect,x3vect, y(1,:,:), x2,x3, yi)                         
        !    return             
        !else if (x2 == x2vect(1)) then	        
	    !    call interpol2d(x1vect,x3vect, y(:,1,:), x1,x3, yi)                         
        !    return             
        !else if (x3 == x3vect(1)) then
	    !    call interpol2d(x1vect,x2vect, y(:,:,1), x1,x2, yi)                         
        !    return        
        !end if                
               
        if (x1vect(1) >= x1) then
            locs(1) = 1
        else if (x1vect(Nox1) <= x1) then
            locs(1) = Nox1 - 1
	    !    call interpol2d(x2vect,x3vect, y(Nox1,:,:), x2,x3, yi)                         
        !    return            
        else
            call Locate(x1vect,x1,locs(1))
        end if
        
        if (x2vect(1) >= x2) then
            locs(2) = 1
        else if (x2vect(Nox2) <= x2) then
            locs(2) = Nox2 - 1
	    !    call interpol2d(x1vect,x3vect, y(:,Nox2,:), x1,x3, yi)                         
        !    return              
        else
            call Locate(x2vect,x2,locs(2))
        end if        

        if (x3vect(1) >= x3) then
            locs(3) = 1
        else if (x3vect(Nox3) <= x3) then
            locs(3) = Nox3 - 1
	    !    call interpol2d(x1vect,x2vect, y(:,:,Nox3), x1,x2, yi)                         
        !    return         
        else
            call Locate(x3vect,x3,locs(3))
        end if  
        
        mxs(1) = (x1-x1vect(locs(1)))/(x1vect(locs(1)+1)-x1vect(locs(1)))	
	    mxs(2) = (x2-x2vect(locs(2)))/(x2vect(locs(2)+1)-x2vect(locs(2)))
	    mxs(3) = (x3-x3vect(locs(3)))/(x3vect(locs(3)+1)-x3vect(locs(3)))

        if (abs(x1-x1vect(locs(1))) < 1.d-10) then
            mxs(1) = 0.0d0
        else
	        mxs(1) = (x1-x1vect(locs(1)))/(x1vect(locs(1)+1)-x1vect(locs(1)))	
	    end if
	    if (abs(x2-x2vect(locs(2))) < 1.d-10) then
	        mxs(2) = 0.0d0
	    else
	        mxs(2) = (x2-x2vect(locs(2)))/(x2vect(locs(2)+1)-x2vect(locs(2)))
        end if  
	    if (abs(x3-x3vect(locs(3))) < 1.d-10) then
	        mxs(3) = 0.0d0
	    else
	        mxs(3) = (x3-x3vect(locs(3)))/(x3vect(locs(3)+1)-x3vect(locs(3)))
        end if           	
 
    end subroutine get_interp3d
    
    subroutine interpol4d(x1vect, x2vect, x3vect, x4vect, y, x1, x2, x3, x4, yi)
        
        implicit none
            
        real(dbl), intent(in):: y(:,:,:,:)
        real(dbl), intent(in):: x1, x2, x3, x4
        real(dbl), intent(in):: x1vect(:), x2vect(:), x3vect(:), x4vect(:)
        real(dbl), intent(out):: yi
        integer m,n,i,j, Nox1, Nox2, Nox3, Nox4
        real(dbl) yt(16), mx1, mx2, mx3, mx4
        
        m=1
        n=1
        j=1
        i=1
        
        Nox1 = size(y,1)
        Nox2 = size(y,2)
        Nox3 = size(y,3)
        Nox4 = size(y,4)
        
        if(x1vect(m) > x1)then
            print *, "x1 outside of x1vect range (too low)!"
        end if
        
        do
            if(x1vect(m+1) > x1)then
                exit
            else
                m=m+1
            end if
            if (m==Nox1-1)then
                print *, "x1 outside of x1vect range (too high)!"
                exit
            end if
        end do
        
        if(x2vect(n) > x2)then
            print *, "x2 outside of x2vect range (too low)!"
        end if
        
        do
            if(x2vect(n+1) > x2)then
                exit
            else
                n=n+1
            end if
            if (n==Nox2-1)then
                print *, "x2 outside of x2vect range (too high)!"
                exit
            end if
        end do
        
        if(x3vect(j) > x3)then
            print *, "x3 outside of x3vect range (too low)!"
        end if
        
        do
            if(x3vect(j+1) > x3)then
                exit
            else
                j=j+1
            end if
            if (j==Nox3-1)then
                print *, "x3 outside of x3vect range (too high)!"
                exit
            end if
        end do
        
        if(x4vect(i) > x4)then
            print *, "x4 outside of x4vect range (too low)!"
        end if
        
        do
            if(x1vect(i+1) > x4)then
                exit
            else
                i=i+1
            end if
            if (i==Nox4-1)then
                print *, "x4 outside of x4vect range (too high)!"
                exit
            end if
        end do
       
	    yt(1) = y(m,n,j,i)
	    yt(2) = y(m+1,n,j,i)
	    yt(3) = y(m,n+1,j,i)
	    yt(4) = y(m+1,n+1,j,i)
	    yt(5) = y(m,n,j+1,i)
	    yt(6) = y(m+1,n,j+1,i)
	    yt(7) = y(m,n+1,j+1,i)
	    yt(8) = y(m+1,n+1,j+1,i)
	    yt(9) = y(m,n,j,i+1)
	    yt(10) = y(m+1,n,j,i+1)
	    yt(11) = y(m,n+1,j,i+1)
	    yt(12) = y(m+1,n+1,j,i+1)
	    yt(13) = y(m,n,j+1,i+1)
	    yt(14) = y(m+1,n,j+1,i+1)
        yt(15) = y(m,n+1,j+1,i+1)
	    yt(16) = y(m+1,n+1,j+1,i+1)	

	    mx1 = (x1-x1vect(m))/(x1vect(m+1)-x1vect(m))	
	    mx2 = (x2-x2vect(n))/(x2vect(n+1)-x2vect(n))
	    mx3 = (x3-x3vect(j))/(x3vect(j+1)-x3vect(j))
	    mx4 = (x4-x4vect(i))/(x4vect(i+1)-x4vect(i))

    	
	    yi =    (1-mx1)*(1-mx2)*(1-mx3)*(1-mx4)*yt(1) + &
                mx1*(1-mx2)*(1-mx3)*(1-mx4)*yt(2) + &
                (1-mx1)*mx2*(1-mx3)*(1-mx4)*yt(3) + &
                mx1*mx2*(1-mx3)*(1-mx4)*yt(4) + &
                (1-mx1)*(1-mx2)*mx3*(1-mx4)*yt(5) + &
                mx1*(1-mx2)*mx3*(1-mx4)*yt(6) + &
                (1-mx1)*mx2*mx3*(1-mx4)*yt(7) + &
                mx1*mx2*mx3*(1-mx4)*yt(8) + &
                (1-mx1)*(1-mx2)*(1-mx3)*mx4*yt(9) + &
                mx1*(1-mx2)*(1-mx3)*mx4*yt(10) + &
                (1-mx1)*mx2*(1-mx3)*mx4*yt(11) + &
                mx1*mx2*(1-mx3)*mx4*yt(12) + &
                (1-mx1)*(1-mx2)*mx3*mx4*yt(13) + &
                mx1*(1-mx2)*mx3*mx4*yt(14) + &
                (1-mx1)*mx2*mx3*mx4*yt(15) + &
                mx1*mx2*mx3*mx4*yt(16) 
    	
    end subroutine interpol4d
    
end module interpolate1
