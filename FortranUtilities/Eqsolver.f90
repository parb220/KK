module eqsolver
    contains
	FUNCTION zbrent(func,x1,x2,tol)
	use constants
	IMPLICIT NONE
	real(dbl), INTENT(IN) :: x1,x2,tol
	real(dbl) :: zbrent
	INTERFACE
		FUNCTION func(x)
	    use ComputeDistributions
	    use Policies
	    use Results    
		IMPLICIT NONE
		real(dbl), INTENT(IN) :: x
		real(dbl) :: func
		END FUNCTION func
	END INTERFACE
	integer, PARAMETER :: ITMAX=100
	real(dbl), PARAMETER :: EPS=epsilon(x1)
	integer :: iter
	real(dbl) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
	a=x1
	b=x2
	fa=func(a)
	fb=func(b)
	if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) then
        if ((fa > 0.0 .and. fb > 0.0)) then !tauhatYlow to high
            b = x1
            a = 0.5d0*x1
            fb=fa
            fa=func(a)	        
	        if ((fa > 0.0 .and. fb > 0.0)) then
	            a = 0.1d0*x1
	            fa=func(a)
	            if ((fa > 0.0 .and. fb > 0.0)) then
	                print *, "Still root is not bracketed."
	                print *, "a =", a, "b =", b
	                print *, "fa =", fa, "fb =", fb
	            end if
	        end if
	    else  !tauhatYup to low
	        a = x2
	        b = 1.5d0*x2   
            fa=fb
	        fb=func(b)	        
	        if ((fa < 0.0 .and. fb < 0.0)) then
	            b = 2.5d0*x2
	            fb = func(b)
	            if ((fa < 0.0 .and. fb < 0.0)) then
	                print *, "Still root is not bracketed."
	                print *, "a =", a, "b =", b
	                print *, "fa =", fa, "fb =", fb
	            end if	
	        end if
	    end if            
    end if
	c=b
	fc=fb
	do iter=1,ITMAX
		if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
			c=a
			fc=fa
			d=b-a
			e=d
		end if
		if (abs(fc) < abs(fb)) then
			a=b
			b=c
			c=a
			fa=fb
			fb=fc
			fc=fa
		end if
		tol1=2.0d0*EPS*abs(b)+0.5d0*tol
		xm=0.5d0*(c-b)
		if (abs(xm) <= tol1 .or. fb == 0.0) then
			zbrent=b		
			RETURN
		end if
		if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
			s=fb/fa
			if (a == c) then
				p=2.0d0*xm*s
				q=1.0d0-s
			else
				q=fa/fc
				r=fb/fc
				p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
				q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
			end if
			if (p > 0.0) q=-q
			p=abs(p)
			if (2.0d0*p  <  min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
				e=d
				d=p/q
			else
				d=xm
				e=d
			end if
		else
			d=xm
			e=d
		end if
		a=b
		fa=fb
		b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
		fb=func(b)
	end do
	print *, "zbrent: exceeded maximum iterations"
	zbrent=b
	END FUNCTION zbrent

end module eqsolver