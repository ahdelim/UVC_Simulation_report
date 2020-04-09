
!    subroutine Gauss_elimination_main(A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34, X_ans, Y_ans, Z_ans)
!
!! ===========================================================
!!  Solves a system of linear equations A*x=b
!!  Method: calls Gauss elimination (with scaled pivoting)
!!  AG: October 2009
!! ===========================================================
!    implicit none
!    integer, parameter :: n = 3
!    double precision a(n,n), b(n), x(n)
!    integer i,j
!    ! matrix A with size n x n
!      data (a(1,i), i=1,3) /  0.0,  0.0,  3.0 /
!      data (a(2,i), i=1,3) /  2.0,  0.0,  4.0 /
!      data (a(3,i), i=1,3) /  4.0,  2.0,  0.0 /
!    ! matrix b
!      data (b(i),   i=1,3) /  3.0,  6.0,  6.0 /
!
!    ! print a header and the original equations
!      write (*,200)
!      do i=1,n
!         write (*,201) (a(i,j),j=1,n), b(i)
!      end do
!
!      call gauss_2(a,b,x,n)
!
!    ! print matrix A and vector b after the elimination 
!      write (*,202)
!      do i = 1,n
!         write (*,201)  (a(i,j),j=1,n), b(i)
!      end do
!    ! print solutions
!      write (*,203)
!      write (*,201) (x(i),i=1,n)
!    200 format (' Gauss elimination with scaling and pivoting ' ,/,/,' Matrix A and vector b')
!    201 format (6f12.6)
!    202 format (/,' Matrix A and vector b after elimination')
!    203 format (/,' Solutions x(n)')
!end subroutine Gauss_elimination_main
    
subroutine Gauss_elimination(a,b,x,n)
    ! ===========================================================
    !   This version is with scaling and pivoting
    !   suitable for matrix with random zeros appear in the elements
    ! ===========================================================

    ! --------------------------
    ! For gauss with pivoting and scaling
    ! --------------------------
    implicit none 
    integer n
    double precision a(n,n), b(n), x(n)
    double precision s(n)
    double precision c, pivot, store
    integer i, j, k, l
    ! --------------------------
    
    ! ----------------------------------------
    ! The subroutine does scaling, pivoting, swapping and elimination after every loop
    ! ----------------------------------------
    ! step 1: begin forward elimination
    ! -------------- start from here is for checking -------------- !
    !do l=1,n
    !        write (6,201) (a(l,j),j=1,n), b(l)
    !end do
    !write(6,*)' '
    ! -------------- end here ------------------- !
    do k=1, n-1

    ! step 2: "scaling"
    ! s(i) will have the largest element from row i 
        do i=k,n                       ! loop over rows
            s(i) = 0.0
            do j=k,n                    ! loop over elements of row i
                s(i) = max(s(i),abs(a(i,j)))
            end do
        end do

    ! step 3: "pivoting 1" 
    ! find a row with the largest pivoting element
    ! calcualte first row pivot first, then compare it with other rows
        pivot = abs(a(k,k)/s(k))
        l = k       ! assign pivot row number to "l"
        do j=k+1,n
            if(abs(a(j,k)/s(j)) > pivot) then
              pivot = abs(a(j,k)/s(j))      ! assign new pivot row with scaled
              l = j                         ! assign number of new pivot row to "l"
            end if
        end do
    
    ! Check if the system has a singular matrix
    ! which means that one of the columns is all zero
        if(pivot == 0.0) then
            write(*,*) ' The matrix is singular '
            return
        end if

    ! step 4: "pivoting 2" interchange rows k and l (if needed)
    ! row "l" is the row having the largest pivot element
    ! First: check if the row "l" is the same as the current row "k"
        if (l .ne. k) then      ! not equal to
          do j=k,n
             store = a(k,j)
             a(k,j) = a(l,j)        ! swapping row k with l
             a(l,j) = store         ! assign original row k to row l
          end do
          store = b(k)              ! assign vector b
          b(k) = b(l)
          b(l) = store
        end if
        ! -------------------------------
        ! To find the repeated equation
        ! use the following command
        ! when pivot = NaN or when all zeros row appear
        ! k and l are the two rows that swaping
        ! -------------------------------
        !write(6,*) pivot, k, l
        !do l=1,n
        !    write (6,201) (a(l,j),j=1,n), b(l)
        !end do
        !   write(6,*)' '
        !pause
        ! step 5: the elimination (after scaling and pivoting)
        do i=k+1,n
            c=a(i,k)/a(k,k)
            a(i,k) = 0.0
            b(i)=b(i)- c*b(k)
            do j=k+1,n
                a(i,j) = a(i,j)-c*a(k,j)
            end do
        end do
        ! ----------- from this line onward is for checking (by showing on screen) ----------- !
        !do l=1,n
        !    write (6,201) (a(l,j),j=1,n), b(l)
        !end do
        !   write(6,*)' '
        !   pause
        ! ----------- checking end here -------------------- !
    end do
        
        ! step 6: back substiturion 
        x(n) = b(n)/a(n,n)
        do i=n-1,1,-1
           c=0.0
           do j=i+1,n
             c= c + a(i,j)*x(j)
           end do 
           x(i) = (b(i)- c)/a(i,i)
        end do
        
201 format (20ES12.2)
end subroutine Gauss_elimination
    
    
!! ===================================================================
!    ! This version of gauss_elimination is without pivoting
!    ! the matrix must be of upper triangle or cannot have random zero appear in the middle of the matrix elements
!    ! another version gauss_elim_pivot will be used in the simulation
!! ===================================================================
    !implicit none
    !integer i,j,k
    !integer, parameter :: m = 3, n = 4
    !double precision, dimension (m,n) :: array
    !double precision A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34
    !double precision xmult, X_ans, Y_ans, Z_ans
    !!data (array(1,j),j=1,n) /1.0,1.0,1.0,5.0/
    !!data (array(2,j),j=1,n) /2.0,3.0,5.0,8.0/
    !!data (array(3,j),j=1,n) /4.0,0.0,5.0,2.0/
    !!data (array(4,j),j=1,n) /-6.0,4.0,1.0,-18.0/
    !
    !array(1,1) = A11
    !array(1,2) = A12
    !array(1,3) = A13
    !array(1,4) = A14
    !array(2,1) = A21
    !array(2,2) = A22
    !array(2,3) = A23
    !array(2,4) = A24
    !array(3,1) = A31
    !array(3,2) = A32
    !array(3,3) = A33
    !array(3,4) = A34
    !
    !    !print *,' '
    !    !print *,' Basic gaussian elimination'
    !    !print *,' Section 4.3, Kincaid-Cheney'
    !    !print *,' '
    !
    !      do 4 k=1,m-1
    !         do 3 i=k+1,m      
    !            xmult = array(i,k)/array(k,k)
    !            !print *, i,k,xmult
    !            array(i,k) = 0.0
    !            do 2 j=k+1,n    
    !               array(i,j) = array(i,j) - xmult*array(k,j)      
    ! 2          continue
    ! 3       continue  
    ! 4    continue    
    !
    ! !     do 6 i=1,m
    ! !        do 5 j=1,n
    ! !           print 7,i,j,array(i,j)
    ! !5       continue
    ! !6    continue
    !      
    !      Z_ans = array(m,n)/array(3,3)
    !      Y_ans = array(2,4) - Z_ans*array(2,3)
    !      X_ans = array(1,4) - Y_ans*array(1,2) - Z_ans*array(1,3)
    !      
    !      !print *, ' '
    !      !print *, 'Solution ...'
    !      !print *, 'X = ', X_ans, 'Y = ', Y_ans, 'Z = ', Z_ans
    !      
    ! !7    format (1x,' a(',i2,',',i2,') =',f12.4)
    !     
    !       
    !end subroutine Gauss_elimination

