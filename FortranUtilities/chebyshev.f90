    
module chebyshev
    use constants
    interface getPhi
        module procedure getPhiScalar, getPhiVect
    end interface  
    contains 
 
    !Evaluates the 1st S Cheybshev polynomials at each point in xvect
    function getPhiVect(xvect,S)
        implicit none
        
        real(dbl), intent(in):: xvect(:)
        integer, intent(in):: S
        real(dbl) getPhiVect(size(xvect),S)
        integer i
        
        getPhiVect(:,1) = 1
        if (S == 1) return
        getPhiVect(:,2) = xvect
        do i=3,S
            getPhiVect(:,i) = 2*xvect*getPhiVect(:,i-1) - getPhiVect(:,i-2)
        end do
        getPhiVect = merge(getPhiVect,1.0,getPhiVect<1.0)
        getPhiVect = merge(getPhiVect,-1.0,getPhiVect>-1.0)
    end function getPhiVect
    
    !Evaluates the 1st S Cheybshev polynomials at x
    function getPhiScalar(x,S)
        implicit none
        
        real(dbl), intent(in):: x
        integer, intent(in):: S
        real(dbl) getPhiScalar(S)
        integer i
        
        getPhiScalar(1) = 1
        if (S == 1) return
        getPhiScalar(2) = x
        do i=3,S
            getPhiScalar(i) = 2*x*getPhiScalar(i-1) - getPhiScalar(i-2)
        end do
        getPhiScalar = merge(getPhiScalar,1.0,getPhiScalar<1.0)
        getPhiScalar = merge(getPhiScalar,-1.0,getPhiScalar>-1.0)
    end function getPhiScalar
    
    !Evaluates the nth Cheybshev polynomials at each point in xvect
    function getCheb(xvect,n)
        implicit none
        
        real(dbl), intent(in):: xvect(:)
        integer, intent(in):: n
        real(dbl) getCheb(size(xvect))
             
        getCheb = cos((n-1)*acos(xvect))
        
    end function getCheb
    
    !Returns the s extrema of the of sth Chebyshev polynomial (also called the Gauss-Lobatto nodes)
    function getExtrema(s)
            integer s, j
            real(dbl) getExtrema(s)
            if (s == 1) then
                getExtrema(1) = 0.0d0
            else
                getExtrema =(/ ( -cos(pi*(j-1)/(s-1)), j=1,s) /)
            end if
    end function getExtrema
    
    !Returns the s roots of the of sth Chebyshev polynomial (also called the Gauss-Lobotto nodes)
    function getRoots(s)
            integer s, i
            real(dbl) getRoots(s)

            getRoots =(/( -cos((2*i-1)*pi/2/s) ,i=1,s)/)
 
    end function getRoots                
end module chebyshev    