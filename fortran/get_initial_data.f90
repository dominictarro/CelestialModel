
SUBROUTINE get_initial_data (names_init, init_array)
!
! This subroutine initializes the data
!
! Retrieves the name of all bodies for simulation
! for this problem, Sun, 8 planets, moon, and Pluto
!
! Sets the initial positions, velocities, and masses
! of each body in the system (11 bodies, 77 data values
! with 7 values per body)
!
! Data obtained from NASA HORIZONS webpage
!
! These data values correpsond to the planets on the date
! January 1, 2020
!
USE global_data, ONLY : n, neqn ,nbm
IMPLICIT NONE


	CHARACTER(LEN=*), INTENT(OUT) :: names_init(n)
	REAL(KIND=8), INTENT(OUT)     :: init_array(neqn)



	! Body names
	names_init(1)  = "Sun"
	names_init(2)  = "Mercury"
	names_init(3)  = "Venus"
	names_init(4)  = "Earth"
	names_init(5)  = "Moon"
	names_init(6)  = "Mars"
	names_init(7)  = "Jupiter"
	names_init(8)  = "Saturn"
	names_init(9)  = "Uranus"
	names_init(10) = "Neptune"
	names_init(11) = "Pluto"

	! Sun
	init_array(1)  = -3.798619468647882d-03   ! x position
	init_array(2)  =  7.439926519813712d-03   ! y position
	init_array(3)  =  2.303011840607431d-05   ! z position
	init_array(4)  = -8.352236181200173d-06   ! vx velocity
	init_array(5)  = -1.991101976755789d-06   ! vy velocity
	init_array(6)  =  2.303616187715271d-07   ! vz velocity
	init_array(7)  =  1.988544d30 / nbm       ! mass

	! Mercury
	init_array(8)  = -6.713349519259718d-02
	init_array(9)  = -4.534054004610566d-01
	init_array(10) = -3.182458153238026d-02
	init_array(11) =  2.221981555538470d-02
	init_array(12) = -2.401844191885121d-03
	init_array(13) = -2.234975522083475d-03
	init_array(14) =  3.302d23 / nbm  

	! Venus
	init_array(15) =  7.194016804983604d-01
	init_array(16) =  5.998830582310215d-02
	init_array(17) = -4.098973222838171d-02
	init_array(18) = -1.555617805193969d-03
	init_array(19) =  2.007933659165609d-02
	init_array(20) =  3.650632309800465d-04
	init_array(21) =  48.685d23 / nbm

	! Earth
	init_array(22) = -1.701443808233178d-01
	init_array(23) =  9.765603186945023d-01
	init_array(24) = -1.822571331401604d-05
	init_array(25) = -1.724754632489101d-02
	init_array(26) = -2.983511998041463d-03
	init_array(27) =  6.558216388187520d-07
	init_array(28) =  5.97219d24 / nbm

	! Moon
	init_array(29) = -1.675361509284642d-01
	init_array(30) =  9.759029512978545d-01
	init_array(31) = -2.485074774953481d-04
	init_array(32) = -1.710389404398059d-02
	init_array(33) = -2.443078545193917d-03
	init_array(34) = -1.958221847802022d-05
	init_array(35) =  734.9d20 / nbm

	! Mars
	init_array(36) = -1.323906224669323d+00
	init_array(37) = -8.783175374839439d-01
	init_array(38) =  1.385210621844926d-02
	init_array(39) =  8.312502505202782d-03
	init_array(40) = -1.042477083991175d-02
	init_array(41) = -4.223314046399220d-04
	init_array(42) =  6.4185d23 / nbm

	! Jupiter
	init_array(43) =  5.223484389354299d-01
	init_array(44) = -5.193582583066160d+00
	init_array(45) =  9.853537301853853d-03
	init_array(46) =  7.415322218022803d-03
	init_array(47) =  1.114874487996517d-03
	init_array(48) = -1.705268990722869d-04
	init_array(49) =  1898.13d24 / nbm

	! Saturn
	init_array(50) =  3.793446245537600d+00
	init_array(51) = -9.280656579684447d+00
	init_array(52) =  1.035620210507365d-02
	init_array(53) =  4.855156277940873d-03
	init_array(54) =  2.095215290585633d-03
	init_array(55) = -2.300751169256309d-04
	init_array(56) =  5.68319d26 / nbm

	! Uranus
	init_array(57)  =  1.622169834985700d+01
	init_array(58)  =  1.138647559662688d+01
	init_array(59)  = -1.678643601291325d-01
	init_array(60)  = -2.288449529894169d-03
	init_array(61)  =  3.035896309705393d-03
	init_array(62)  =  4.097802109019479d-05
	init_array(63)  =  86.8103d24 / nbm

	! Neptune
	init_array(64)  =  2.923901404431117d+01
	init_array(65)  = -6.359750396990894d+00
	init_array(66)  = -5.428756529427533d-01
	init_array(67)  =  6.461740441876787d-04
	init_array(68)  =  3.085635751878358d-03
	init_array(69)  = -7.869691796050940d-05
	init_array(70)  =  102.41d24 / nbm

	! Pluto
	init_array(71)  =  1.297291157616514d+01
	init_array(72)  = -3.136273187454788d+01
	init_array(73)  = -3.965374359206294d-01
	init_array(74)  =  2.968249824832238d-03
	init_array(75)  =  5.356788760061291d-04
	init_array(76)  = -9.036709403060780d-04
	init_array(77)  =  1.307d22 / nbm


END SUBROUTINE get_initial_data