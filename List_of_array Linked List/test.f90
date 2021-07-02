program temp
  use linked_list

  type(list) :: L
  integer    :: i
  integer    :: length
  real :: T1,T2
  class(*), pointer :: data

  read(*,*) length
  call cpu_time(T1)

  do i = 1,length
    call L%append(i)
  end do
  call cpu_time(T2)
  write(*,*) T2-T1

  call cpu_time(T1)
  call srand(123456789)
  do i = 1,100
    j = rand()*(length-1) +1
    data=>L%get(j);
    select type (data)
    type is (integer)
    end select 
  end do
  call cpu_time(T2)
  write(*,*) T2-T1

  call cpu_time(T1)
  call L%destroy()
  call cpu_time(T2)
  write(*,*) T2-T1


end program temp