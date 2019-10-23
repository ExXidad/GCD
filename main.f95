program main
    use decomposition
    use search_procedures
    implicit none
    integer :: first, second
    integer :: len1, len2
    integer :: i, gcd = 1
    type(search_result) :: search_res
    integer, dimension(:), allocatable :: first_array, second_array

    write(*,"(A,$)") "Put numbers to find GCD: "

    read(*,*) first, second

    write(*,*) "Their decompositions are: "

    call decompose(first,first_array)
    call decompose(second,second_array)

    len1=size(first_array,1)
    len2=size(second_array,1)

    write(*,*) first,second
    write(*,*) "------------------------------------------------------------"

    do i = 1,max(len1,len2)
        if (len1 >= i) then
            write(*,"(I5,$)") first_array(i)
        else
            write(*,"(A5,$)") "     "
        end if

        if (len2 >= i) then
            write(*,"(I5,$)") second_array(i)
        else
            write(*,"(A5,$)") "     "
        end if
        write(*,*)
    end do

    write(*,*) "------------------------------------------------------------"

    do i = 1, len1
        search_res = linsearch(second_array,first_array(i))
        if (search_res%foundQ) then
            gcd = gcd * first_array(i)
            second_array(search_res%elem_pos) = -1
        end if
    end do

    write(*,"(A,$)") "GCD is: "
    write(*,*) gcd
end
