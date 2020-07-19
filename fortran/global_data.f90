

MODULE global_data
! This module defines a number of parameters that are needed to
! run the simulation properly
!
! The subroutine at the end needs to be completed to run the code
!


	!--------------------------------------------
	!--------------------------------------------
	! Define how many bodies and ODEs we have
	INTEGER(KIND=4), PARAMETER :: n    = 11  ! Number of bodies in our gravitating system
	INTEGER(KIND=4), PARAMETER :: neqn = 7*n ! Number of ODE equations that need to be solved
	                                         ! POSITION: dx/dt, dy/dt, dz/dt
	                                         ! VELOCITY: dvx/dt, dvy/dt, dvz/dt
	                                         ! MASS    : dm/dt (can account for changes in mass,
	                                         !                  but not implemented in this code)
	!--------------------------------------------
	!--------------------------------------------



	!--------------------------------------------
	!--------------------------------------------
	! Some variables to control integration
	INTEGER(KIND=4), PARAMETER :: step_max = 9999999 ! max number of integration steps before we give up
	INTEGER(KIND=4)            :: flag     = 1       ! an integer flag for the integrator, don't change
	INTEGER(KIND=4)            :: step     = 0       ! counts how many steps integrator takes
	REAL(KIND=8), PARAMETER    :: relerr   = 1.0d-11 ! relative error
	REAL(KIND=8), PARAMETER    :: abserr   = 1.0d-11 ! absolute error
	!--------------------------------------------
	!--------------------------------------------


	!--------------------------------------------
	!--------------------------------------------
	! Time variables
	REAL(KIND=8), PARAMETER :: t_start = 0.0d0  ! starting simulation time
	REAL(KIND=8), PARAMETER :: t_final = 100.0d0 ! final simulation time
	REAL(KIND=8), PARAMETER :: dt      = 1.0d-2 ! timestep we give to the integrator method
	REAL(KIND=8)            :: t_old   = 0.0d0  ! old/current simuation time
	REAL(KIND=8)            :: t_new   = 0.0d0  ! new simulation time after integrating
	!--------------------------------------------
	!--------------------------------------------


	!--------------------------------------------
	!--------------------------------------------
	! Some physical constants
	REAL(KIND=8), PARAMETER :: soft = 0.02d0   ! softening term that helps reduce error in
	                                           ! bodies that move too close together during
	                                           ! the simulation

	REAL(KIND=8), PARAMETER :: G = 2.959d-4    ! Parameterizes all time measure variables into
	                                           ! simple units of seconds. All other values
	                                           ! become simple unitless numbers.

	REAL(KIND=8), PARAMETER :: nbm = 1.989d30  ! use to normalize masses of bodies
	!--------------------------------------------
	!--------------------------------------------


CONTAINS

	SUBROUTINE f(t, y, yp)
	  !        THIS SUBROUTINE REQUIRES NO EDITING                           *
	  !                                                                      *
	  !        it should evaluate yp, given n, t and y                       *
	  !                                                                      *
	  !     t  independent variable - initial value supplied by user         *
	  !                                                                      *
	  !     y  dependent variable - initial values of components y(1), y(2), *
	  !
	IMPLICIT NONE
		REAL(KIND=8), INTENT(IN)    :: t, y(neqn)
		REAL(KIND=8), INTENT(INOUT) :: yp(neqn)

		INTEGER(KIND=4) :: k, kj, i, ij

		! These variables can be used to make things more readable
		! x,y,z positions of body "k" and body "i" defined
		! Mass of body "i" is "mi"
		! "r" is the Euclidean distance between Bodies "k" and "i"
		REAL(KIND=8)    :: xk, yk, zk, r
		REAL(KIND=8)    :: xi, yi, zi, mi

		yp = 0.e0


		!********************************************************************
		!********************************************************************
		! For this subroutine, we'll be defining the process of evaluating our ODEs at a given point
		! in time.
		!
		! Things you will need to use here, the Lecture 27 slides for sure
		!
		! There will be a DO LOOP with another DO LOOP inside of it.
		!
		! That interior DO LOOP will have an IF statement inside of it.
		!
		! You will need to assign values to array "yp" in some iterative, generic manner.
		! Kind of like how we do Forward Euler in radioactive decay equations or the Newton iterations
		! for rootfinding. Basically, use DO LOOP indexing
		!
		! You can use the variables defined above (xk,yi,mi, etc) in some way, but its your choice to do so
		! and to do so correctly.
		!
		! "G" and "soft" are defined in the MODULE and are directly acccessible here.
		!
		! This is probably the hardest part of the coding portion of the project, don't hesitate to ask
		! questions (and attend future lectures where some hints may be given).
		!
		! Some specific instructions have been indented to show where they fall within LOOPs and IF
		! statements.
		!********************************************************************
		!********************************************************************



		!**********************************
		!  Use a DO LOOP to go through Bodies 1 to N
		!  to select equations for body 1, then body 2,
		! through to the last body
		DO k = 1, n


			!**********************************
			! Compute your x,y,z position derivatives for yp(i), body "k"
			! Consider referencing the last few slides of Lecture 27
			kj = (k-1)*7
			xk = y(kj + 1)
			yk = y(kj + 2)
			zk = y(kj + 3)

			yp(kj + 1) = y(kj + 4) ! dx(k)/dt = vx(k)
			yp(kj + 2) = y(kj + 5) ! dy(k)/dt = vy(k)
			yp(kj + 3) = y(kj + 6) ! dz(k)/dt = vz(k)

			!**********************************
			!  Using another DO LOOP setup your derivatives of velocity with gravitational force equations as
			!  guidance...
			! loop through equations for all bodies again
			DO i = 1, n


				!**********************************
				! check to see if i does not equal k; if TRUE, then compute
				! the velocity derivatives of body "k" with respect to body "i"
				IF (k .ne. i) THEN



					!**********************************
					! Using the indexing scheme from the slides, write code defining "ij"
					ij = (i-1)*7



					!**********************************
					! Euclidean distance bewteen "k" and "i"
					! Finish this line of code
					! Set up xi, yi, zi

					xi = y(ij + 1)
					yi = y(ij + 2)
					zi = y(ij + 3)
					mi = y(ij + 7)
					r = SQRT((xk - xi)**2 + (yk - yi)**2 + (zk - zi)**2)




					!**********************************
					! Finish setting up the ODEs, should be three lines
					! computing the dvx/dt, dvy/dt, dvz/dt
					! Use xk,yk,zk,xi,yi,zi,mi,r,G,soft variables here
					yp(kj + 4) = yp(kj + 4) - G * mi * (xk - xi) / (r**2.d0 + soft**2.0d0)**1.5d0
					yp(kj + 5) = yp(kj + 5) - G * mi * (yk - yi) / (r**2.d0 + soft**2.0d0)**1.5d0
					yp(kj + 6) = yp(kj + 6) - G * mi * (zk - zi) / (r**2.d0 + soft**2.0d0)**1.5d0



				!******************************
				! End the IF block HERE
				END IF

			!******************************
			! End the inner DO WHILE LOOP HERE
			ENDDO

		!******************************
		! End the outer DO WHILE LOOP HERE
		ENDDO


	END SUBROUTINE f

END MODULE global_data
