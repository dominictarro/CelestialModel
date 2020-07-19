PROGRAM solar_system
USE global_data  ! import all other variables from this module
IMPLICIT NONE

	INTEGER(KIND=4) :: i,j,k    ! some counting variables

	!--------------------------------------------
	!--------------------------------------------
	! For anything needing text name
	CHARACTER(LEN=100) :: filename   ! will be used to open files later on
	CHARACTER(LEN=20)  :: names(n)   ! will hold the names of each planet
	!--------------------------------------------
	!--------------------------------------------


	!--------------------------------------------
	! Integration variables
	REAL(KIND=8) :: y_init(neqn) = 0.0d0 ! stores the starting conditions for all bodies
	REAL(KIND=8) :: y(neqn)      = 0.0d0 ! stores positions/velocities/masses of each body (solution array)
	REAL(KIND=8) :: yp(neqn)     = 0.0d0 ! stores derivatives: dx/dt,dy/dt,dx/dt,dvx/dt,dvy/dt,dvz/dt,dm/dt
	!--------------------------------------------
	!--------------------------------------------

	! This section for execution timing
	REAL(KIND=8) :: start, finish

	CALL cpu_time(start)


	! use y_init to store the initial conditions
	CALL get_initial_data(names, y_init)

	!*************************
	! set y equal to y_init
	y = y_init


	! we're opening a data file for each Body, giving the file a header and recording the initial conditions
	DO i=1,n
		WRITE(filename,'(A)') 'output_'//TRIM(names(i))//'.dat'
		OPEN(unit=30+i, FILE=filename, STATUS='REPLACE')
		WRITE(30+i,*) 'posx   posy   posz   velx   vely   velz   mass'
		WRITE(30+i,*) (y(j),j = 1 + (i-1)*7, i*7)
	ENDDO



	!******************************************************************
	! Write this DO WHILE loop for running the ODE
	! integration. We take a small timestep, starting at some start
	! time, each iteration, until we reach some defined final time.
	!
	! The DO WHILE should have conditions, check to see if "t_new" is less
	! or equal to "t_final" and check to see if "step" is less than or equal
	! to "step_max"
	DO WHILE ( t_new .lt. t_final .and. step .lt. step_max)

		! count new iteration
		step = step + 1



		!**********************************
		! Write code that stores the new simulation time from PREVIOUS iteration
		! in the old simulation time variable for the CURRENT iteration
		t_old = t_new

		!**********************************
		! Find what the NEW time will be after integrating one timestep
		! Will either be t_old + dt or t_final, whichever is smaller
		! use the MIN( var 1, var 2) function
		t_new = t_old + dt


		! Call the Runge-Kutta-Fehlberg 4th/5th order integrator
		! I'll provide this line or you, DON'T change the value of "flag"
		CALL r8_rkf45(f, neqn, y, yp, t_old, t_new, relerr, abserr, flag)
		flag = -2




		!**********************************
		! Here, write a simple do loop to record each Body's new positions, velocities, and mass
		! You might want to refer to the lines where we opened the files, some of the lines
		! might be helpful here.0

		DO i=1,n
			WRITE(30+i, *) (y(j), j=1 + (i-1)*7, i*7)
		ENDDO



		!**********************************
		! write a line of code here that will print out the step number,"step", and new time, "t_new"
		!WRITE(*,*) t_old, t_new, dt



	!******************************
	! End the DO WHILE LOOP HERE
ENDDO


	!******************************
	! close all of the files we
	! opened, a DO LOOP can make
	! this efficient

DO i=1,n
	CLOSE(UNIT=30+i)
ENDDO


call cpu_time(finish)
WRITE(*,*) "Time = ",finish-start," seconds."

	! call plotting subroutines
	CALL plot_bodies(4, (/'Mercury','Venus  ','Earth  ','Mars   '/), 'inner_planets')
	CALL plot_bodies(4, (/'Jupiter','Saturn ','Uranus ','Neptune'/), 'outer_planets')
	CALL plot_earth_moon(2, (/'Earth', 'Moon '/), 'earth_moon')



END PROGRAM solar_system
