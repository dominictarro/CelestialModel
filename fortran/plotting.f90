SUBROUTINE plot_bodies(n,names,outfile)
IMPLICIT NONE
        INTEGER(KIND=4), INTENT(IN)  :: n 
        CHARACTER(LEN=*), INTENT(IN) :: names(n)
        CHARACTER(LEN=*), INTENT(IN) :: outfile
        INTEGER(KIND=4)              :: i
        open(unit=99,file=TRIM(outfile)//'.gnu',status='replace')
        write(99,*) 'set terminal pngcairo size 1024,768 enhanced font "Verdana,12"'
        write(99,*) 'set output "'//TRIM(outfile)//'.png"'
        write(99,*) 'set key right top'
        write(99,*) 'set size ratio 0.66'
        write(99,*) 'set autoscale'
        write(99,*) 'set grid x'
        write(99,*) 'set grid y'
        write(99,*) 'set xlabel "y position" font ",16"'
        write(99,*) 'set ylabel "x position" font ",16"'
        write(99,*) 'set label "Evolution of Selected Planets" font ",12," at graph 0.5, graph 1.08'
        write(99,*) 'plot "output_'//TRIM(names(1))//'.dat" using 1:2 with lines title "'//TRIM(names(1))//'", \'
        DO i = 2,n
            write(99,*) '     "output_'//TRIM(names(i))//'.dat" using 1:2 with lines title "'//TRIM(names(i))//'", \'
        ENDDO


        call system("tail -n 1 output_Sun.dat | cut -f 1 > Sun")
        DO i = 1,n
            call system("tail -n 1 output_"//TRIM(names(i))//".dat | cut -f 2 >> bodies")
        ENDDO
        write(99,*) "    'bodies' with points lc rgb 'black' pt 7, \"
        write(99,*) "    'Sun' with points lc rgb 'red' pt 16"
        

        close(unit=99)
        call system('gnuplot '//TRIM(outfile)//'.gnu')
        call system('rm Sun')
        call system('rm bodies')
END SUBROUTINE plot_bodies

SUBROUTINE plot_earth_moon(n,names,outfile)
IMPLICIT NONE
        INTEGER(KIND=4), INTENT(IN)  :: n 
        CHARACTER(LEN=*), INTENT(IN) :: names(n)
        CHARACTER(LEN=*), INTENT(IN) :: outfile
        INTEGER(KIND=4)              :: i
        REAL(KIND=8)                 :: x,y
        CHARACTER(LEN=99)            :: xmax,xmin,ymax,ymin

        DO i = 1,n
            call system("tail -n 1 output_"//TRIM(names(i))//".dat | cut -f 2 >> bodies")
        ENDDO
        open(unit=98,file='bodies',status='old')
        read(98,*) x, y
        write(xmin,*) x-x*1d-2
        write(xmax,*) x+x*1d-2

        write(ymin,*) y-y*1d-2
        write(ymax,*) y+y*1d-2
        close(98)

        open(unit=99,file=TRIM(outfile)//'.gnu',status='replace')
        write(99,*) 'set terminal pngcairo size 1024,768 enhanced font "Verdana,12"'
        write(99,*) 'set output "'//TRIM(outfile)//'.png"'
        write(99,*) 'set key right top'
        write(99,*) 'set size ratio 0.66'
        write(99,*) 'set autoscale'
        write(99,*) 'set grid x'
        write(99,*) 'set grid y'
        write(99,*) 'set xlabel "y position" font ",16"'
        write(99,*) 'set ylabel "x position" font ",16"'
        write(99,*) "set xrange ["//TRIM(xmin)//":"//TRIM(xmax)//"]"
        write(99,*) "set yrange ["//TRIM(ymin)//":"//TRIM(ymax)//"]"
        write(99,*) 'set label "Evolution of Earth-Moon System" font ",12," at graph 0.5, graph 1.08'
        write(99,*) 'plot "output_'//TRIM(names(1))//'.dat" using 1:2 with lines title "'//TRIM(names(1))//'", \'
        DO i = 2,n
            write(99,*) '     "output_'//TRIM(names(i))//'.dat" using 1:2 with lines title "'//TRIM(names(i))//'", \'
        ENDDO

        write(99,*) "    'bodies' with points lc rgb 'black' pt 7"
        


        close(unit=99)
        call system('gnuplot '//TRIM(outfile)//'.gnu')
        call system('rm bodies')
END SUBROUTINE plot_earth_moon