subroutine smoothing_data(array_size, array1)
    
    integer array_size, i
    double precision, dimension (1:array_size+1) :: array1
    
    do 401 i = 1, array_size+1
        if (i == 1) then
            array1(i) = (array1(i) + array1(i+1) + array1(i+2))/3
        elseif (i == 2) then
            array1(i) = (array1(i-1) + array1(i) + array1(i+1) + array1(i+2))/4
        elseif (i == array_size+1) then
            array1(i) = (array1(i) + array1(i-1) + array1(i-2))/3
        elseif (i == array_size) then
            array1(i) = (array1(i+1) + array1(i) + array1(i-1) + array1(i-2))/4
        else
            array1(i) = (array1(i+2) + array1(i+1) + array1(i) + array1(i-1) + array1(i-2))/5
        endif
401 continue        
endsubroutine smoothing_data