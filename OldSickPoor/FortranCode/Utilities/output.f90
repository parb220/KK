
module output
    use constants
    interface Out
        module procedure OutInteger, OutReal, OutRealVector, OutRealMatrix, OutRealCube, &
        OutRealCube2, OutIntVector, OutIntMatrix, Out2RealMatrices, Out3RealMatrices, &
        OutString, OutRealLine, OutIntegerLine, OutTwoIntegers,  &
        OutTwoReals, OutThreeStrings, OutTwoRealVectors
    end interface    
    contains   
    subroutine OutInteger(fileNum, name, valueInt, form)
        integer fileNum
        character(*) name
        integer valueInt
        character(1) form
        
        if (fileNum == 0) then
            write(*, '(a,a,i10)') name, " = ", valueInt
        else
            write(fileNum, 101, advance ='no') name, form, valueInt, form
        end if
        101 format(a,a,i10,a)
    end subroutine OutInteger    
    
    subroutine OutTwoIntegers(fileNum, name, valueInt1, valueInt2, form)
        integer fileNum
        character(*) name
        integer valueInt1, valueInt2
        character(1) form
        
        if (fileNum == 0) then
            write(*, '(a,i5,i5)') name, valueInt1, valueInt2
        else
            write(fileNum, 101, advance ='no') name, form, valueInt1, form, valueInt2, ret
        end if
        101 format(a,a,i5,a,i5,a)
    end subroutine OutTwoIntegers      

    subroutine OutReal(fileNum, name, valueReal, form)
        integer fileNum
        character(*) name
        real(dbl) valueReal
        character(1) form
        
        if (fileNum == 0) then
            write(*, '(a,a,f14.8)') name, " = ", valueReal
        else
            write(fileNum, 101, advance ='no') name, form, valueReal, form
        end if
        101 format(a,a,f14.8,a)
    end subroutine OutReal 
    
    subroutine OutTwoReals(fileNum, name, valueReal1, valueReal2, form)
        integer fileNum
        character(*) name
        real(dbl) valueReal1, valueReal2
        character(1) form
        
        if (fileNum == 0) then
            write(*, '(a,f14.3,f14.3)') name, valueReal1, valueReal2
        else
            write(fileNum, 101, advance ='no') name, form, valueReal1, form, valueReal2, ret
        end if
        101 format(a,a,f14.3,a,f14.3,a)
    end subroutine OutTwoReals
        
    subroutine OutRealVector(fileNum, name, valueReal, form)
        integer fileNum
        character(*) name
        real(dbl) valueReal(:)
        character(1) form
        integer i
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do i=1,size(valueReal)
                write(*,'(f16.8)') valueReal(i)
            end do     
        else
            write(fileNum,101) name, form, (valueReal(i), form, i=1,size(valueReal))
        end if
        101 format(a, a, 100000(f16.12, a,:))
   end subroutine OutRealVector
   
    subroutine OutTwoRealVectors(fileNum, name1, valueReal1, name2, valueReal2)
        integer fileNum
        character(*) name1, name2
        real(dbl) valueReal1(:), valueReal2(:)     
        integer i
        
        if (fileNum == 0) then
            write(*,'(a,a)') name1, name2
            do i=1,size(valueReal1)
                write(*,'(f16.8,f16.8)') valueReal1(i), valueReal2(i)
            end do     
        else
            write(fileNum,101) name1, name2, ret, (valueReal1(i), tab, valueReal2(i), ret, i=1,size(valueReal1))
        end if
        101 format(a, a, a, 100000(f16.12, a, f16.12, a,:))
   end subroutine OutTwoRealVectors   
       
    subroutine OutRealMatrix(fileNum, name, valueReal)
        integer fileNum
        character(*) name
        real(dbl) valueReal(:,:)
        integer i,j
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do j=1,size(valueReal,1)
                write(*,101) (valueReal(j,i), tab, i=1,size(valueReal,2))       
            end do
        else
            write(fileNum,'(a)') name
            do j=1,size(valueReal,1)
                write(fileNum,101) (valueReal(j,i), tab, i=1,size(valueReal,2))       
            end do
        end if
        101 format(10000(f12.6, a,:)) 
   end subroutine OutRealMatrix  
   
    subroutine Out2RealMatrices(fileNum, name1, valueReal1, name2, valueReal2)
        integer fileNum
        character(*) name1, name2
        real(dbl) valueReal1(:,:), valueReal2(:,:)
        integer i,j
        integer L1, H1, L2, H2
        L1 = size(valueReal1,2)
        H1 = size(valueReal1,1)
        L2 = size(valueReal2,2)
        H2 = size(valueReal2,1)
        
        if (fileNum == 0) then
            write(*,'(a)',advance='no') name1
            do i=1,L1+1
                write(1,'(a)',advance='no') tab 
            end do
            write(*,'(a)') name2
            do j=1,H1
                write(*,101,advance='no') (valueReal1(j,i), tab, i=1,L1)
                write(*,101) (valueReal2(j,i), tab, i=1,L2)         
            end do  
        else
            write(fileNum,'(a)',advance='no') name1
            do i=1,L1+1
                write(1,'(a)',advance='no') tab 
            end do
            write(fileNum,'(a)') name2
            do j=1,H1
                write(fileNum,101,advance='no') (valueReal1(j,i), tab, i=1,L1)
                write(fileNum,101) (valueReal2(j,i), tab, i=1,L2)         
            end do          
        end if
        101 format(10000(f12.6, a,:)) 
   end subroutine Out2RealMatrices 
   
    subroutine Out3RealMatrices(fileNum, name1, valueReal1, name2, valueReal2, name3, valueReal3)
        integer fileNum
        character(*) name1, name2, name3
        real(dbl) valueReal1(:,:), valueReal2(:,:), valueReal3(:,:)
        integer i,j
        integer L1, H1, L2, H2, L3, H3
        L1 = size(valueReal1,2)
        H1 = size(valueReal1,1)
        L2 = size(valueReal2,2)
        H2 = size(valueReal2,1)
        L3 = size(valueReal3,2)
        H3 = size(valueReal3,1)
        
        write(fileNum,'(a)',advance='no') name1
        do i=1,L1+1
            write(1,'(a)',advance='no') tab 
        end do
        write(fileNum,'(a)',advance='no') name2
        do i=1,L2+1
            write(1,'(a)',advance='no') tab 
        end do   
        write(fileNum,'(a)') name3         
        do j=1,H1
            write(fileNum,101,advance='no') (valueReal1(j,i), tab, i=1,L1)
            write(fileNum,101,advance='no') (valueReal2(j,i), tab, i=1,L2) 
            write(fileNum,101) (valueReal3(j,i), tab, i=1,L3)         
        end do          

        101 format(10000(f12.6, a,:)) 
   end subroutine Out3RealMatrices        
   
    subroutine OutRealCube(fileNum, name, valueReal)
        integer fileNum
        character(*) name
        real(dbl) valueReal(:,:,:)
        integer i,j,k
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do k=1,size(valueReal,3)
                do j=1,size(valueReal,1)
                    write(*,101) (valueReal(j,i,k), tab, i=1,size(valueReal,2))       
                end do
                write(*,'(a)') ret
                write(*,'(a)') ret
            end do
        else
            write(fileNum,'(a)') name
            do k=1,size(valueReal,3)
                do j=1,size(valueReal,1)
                    write(fileNum,101) (valueReal(j,i,k), tab, i=1,size(valueReal,2))       
                end do
                write(*,'(a)') ret
                write(*,'(a)') ret
            end do
        end if
        101 format(100(f14.8, a,:)) 
   end subroutine OutRealCube    
   
    subroutine OutRealCube2(fileNum, name, valueReal)
        integer fileNum
        character(*) name
        real(dbl) valueReal(:,:,:,:)
        integer i,j,k,l
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do l=1,size(valueReal,3)
                do k=1,size(valueReal,4)
                    do j=1,size(valueReal,1)
                        write(*,101) (valueReal(j,i,l,k), tab, i=1,size(valueReal,2))    
                    end do
                    write(*,'(a)') ret
                    write(*,'(a)') ret
                end do
                write(*,'(a)') ret
                write(*,'(a)') ret
            end do
        else
            write(fileNum,'(a)') name
            do l=1,size(valueReal,4)
                do k=1,size(valueReal,3)
                    do j=1,size(valueReal,1)
                        write(fileNum,101) (valueReal(j,i,k,l), tab, i=1,size(valueReal,2))       
                    end do
                    write(*,'(a)') ret
                    write(*,'(a)') ret
                end do
                write(*,'(a)') ret
                write(*,'(a)') ret
            end do                
        end if
        101 format(100(f14.8, a,:)) 
   end subroutine OutRealCube2    
   
    subroutine OutIntVector(fileNum, name, valueInt, form)
        integer fileNum
        character(*) name
        integer valueInt(:)
        character(1) form
        integer i
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do i=1,size(valueInt)
                write(*,'(i8)') valueInt(i)
            end do     
        else
            write(fileNum,101) name, form, (valueInt(i), form, i=1,size(valueInt))
        end if
        101 format(a, a, 100(i8, a,:))
   end subroutine OutIntVector
       
    subroutine OutIntMatrix(fileNum, name, valueInt)
        integer fileNum
        character(*) name
        integer valueInt(:,:)
        integer i,j
        
        if (fileNum == 0) then
            write(*,'(a)') name
            do j=1,size(valueInt,1)
                write(*,101) (valueInt(j,i), tab, i=1,size(valueInt,2))       
            end do
        else
            write(fileNum,'(a)') name
            do j=1,size(valueInt,1)
                write(fileNum,101) (valueInt(j,i), tab, i=1,size(valueInt,2))       
            end do
        end if
        101 format(100(i8, a,:)) 
   end subroutine OutIntMatrix         
    
   subroutine OutString(fileNum, string)
        integer fileNum
        character(*) string
        if (fileNum == 0) then
            write(*,'(a)') string
        else
            write(fileNum,'(a)') string
        end if
   end subroutine OutString
   
   subroutine OutThreeStrings(fileNum, str1,str2,str3,form)
        integer fileNum
        character(*) str1, str2, str3
        character(1) form
        if (fileNum == 0) then
            write(*,'(a,a,a,a,a)') str1, form, str2, form, str3
        else
            write(fileNum,'(a,a,a,a,a)') str1, form, str2, form, str3
        end if
   end subroutine OutThreeStrings
      
    subroutine OutRealLine(fileNum, str1, real1, str2, real2, str3)
        integer fileNum
        character(*) str1, str2, str3
        real(dbl) real1, real2
        
        if (fileNum == 0) then
            write(*, '(a,f12.2,a,f12.2,a)') str1, real1, str2, real2, str3
        else
            write(fileNum, 101) str1, real1, str2, real2, str3
        end if
        101 format(a,f12.2,a,f12.2,a)
    end subroutine OutRealLine   
    
    subroutine OutIntegerLine(fileNum, str1, int1, str2, int2, str3)
        integer fileNum
        character(*) str1, str2, str3
        integer int1, int2
        
        if (fileNum == 0) then
            write(*, '(a,i8,a,i8,a)') str1, int1, str2, int2, str3
        else
            write(fileNum, 101) str1, int1, str2, int2, str3
        end if
        101 format(a,i8,a,i8,a)
    end subroutine OutIntegerLine    
    
    
  
end module output  

