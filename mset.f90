!A CGI Mandelbrot Set explorer
!David Faden, February 24, 2004
!dfaden@gmail.com

program mset

interface
subroutine getparam(name, value)
  character(len=*), intent(in) :: name
  character(len=*), intent(out) :: value
end subroutine getparam

subroutine writeReal(prefix, x)
  character(len=*), intent(in) :: prefix
  real, intent(in) :: x
end subroutine writeReal

subroutine writeInteger(prefix, i)
  character(len=*), intent(in) :: prefix
  integer, intent(in) :: i
end subroutine writeInteger

function iterationsSurvivedBy(zr0, zi0)
  real, intent(in) :: zr0, zi0
  integer :: iterationsSurvivedBy
end function iterationsSurvivedBy

end interface

integer, parameter :: width = 50
integer, parameter :: height = 50

character*2 :: crlf = char(13) // char(10)

integer :: ReadStatus
character*20 :: zr_string
character*20 :: zi_string
character*20 :: delta_string
real :: zr, minZr, zi, delta

integer :: iteration
real :: newdelta
character*50 :: letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVW@"

character*100 :: numberBuffer

integer :: x
integer :: y

call getparam('r', zr_string)
call getparam('i', zi_string)
call getparam('d', delta_string)

if ((zr_string .eq. '') .or. (zi_string .eq. '') .or. (delta_string .eq. '')) then
   minZr = 0.0
   zi = 0.0
   delta = 4.0 / width
else
   read (zr_string, *, iostat=ReadStatus) minZr
   if (ReadStatus .ne. 0) then
      minZr = 0.0
   end if
   read (zi_string, *, iostat=ReadStatus) zi
   if (ReadStatus .ne. 0) then
      zi = 0.0
   end if
   read (delta_string, *, iostat=ReadStatus) delta
   if (ReadStatus .ne. 0) then
      delta = 4.0 / width
   end if
endif

newdelta = delta / 2.0

write (*, '(A$)') 'Content-Type: text/html',  crlf, crlf
write (*, '(A)') '<html><head><title>Fortran M Set explorer</title>'
write (*, '(A)') '<meta name="robots" content="nofollow">'
write (*, '(A)') '<link rel="stylesheet" type="text/css" href="' // &
     'mset_colors.css">'
write (*, '(A$)') '</head>'
write (*, '(A$)') '<body><pre>'

zi = zi + height / 2 * delta
do y = 1, height
   zr = minZr - width / 2 * delta
   do x = 1, width
      iteration = iterationsSurvivedBy(zr, zi)
      write (*, '(A$)') '<a href="fortranmset.cgi?'
      call writeReal('r=', zr)
      call writeReal('&i=', zi)
      call writeReal('&d=', newdelta)
      call writeInteger('" class="i', iteration)
      write (*, '(A2$)') '">'
      write (*, '(A5$)') letters(iteration:iteration) // &
           '</a>'
      zr = zr + delta
      end do
      write (*, '(A2$)') crlf
      zi = zi - delta
end do

write (*, '(A$)') '</pre>'
write (*, '(A$)') '<a href="fortranmset.f90">Source</a>'
write (*, '(A$)') '</body></html>'


stop
end program mset

subroutine writeReal(prefix, x)
  character(len=*), intent(in) :: prefix
  real, intent(in) :: x
  character * 100 :: buffer
  write (buffer, '(F18.15$)') x
  write (*, '(A$)') prefix
  write (*, '(A$)') trim(adjustl(buffer))
end subroutine writeReal

subroutine writeInteger(prefix, i)
  character(len=*), intent(in) :: prefix
  integer, intent(in) :: i
  character * 100 :: buffer
  write (buffer, '(I3$)') i
  write (*, '(A$)') prefix
  write (*, '(A$)') trim(adjustl(buffer))
end subroutine writeInteger


function iterationsSurvivedBy(zr0, zi0)
  real, intent(in) :: zr0, zi0
  integer :: iterationsSurvivedBy
  
  integer, parameter :: maxiterations = 50
  real :: zr, zi
  real :: zrSquared, ziSquared, oldZr

  zr = zr0
  zi = zi0

  do iteration = 1, maxiterations
     zrSquared = zr * zr
     ziSquared = zi * zi
     oldZr = zr
     if (zrSquared + ziSquared > 4) then
        iterationsSurvivedBy = iteration
        return
        endif
        zr = zrSquared - ziSquared + zr0
        zi = 2 * zi * oldZr + zi0
        end do
        iterationsSurvivedBy = maxiterations
        return
end function iterationsSurvivedBy

subroutine getparam(name, value)
  character(len=*), intent(in) :: name
  character(len=*), intent(out) :: value

  integer nameindex
  integer namelength
  integer endindex

  character(len=2048) :: input
  call getenv('QUERY_STRING', input)

  nameindex = index(input, name // '=')
  if (nameindex.eq.0) then
     value = ''
     return
     endif
     
     namelength = len(name)
     input = input(nameindex + namelength + 1 : )
     
     endindex = index(input, '&')
     if (endindex.eq.0) endindex = len(input)

     endindex = endindex - 1

     value = input( : endindex)
     return
end subroutine getparam

