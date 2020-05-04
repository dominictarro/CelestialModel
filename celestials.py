'''
This module contains an ODE algorithm that calculates the movement of defined celestial bodies in our solar system including
The sun, Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, and Pluto.

'''

class Body:
	FILENAME = "solarmodel.csv"
	G = 0.0002959
	SOFT = 0.02
	DT = 0.1
	iterations = 0
	history = []


	'''
	Initial formats must be lists/tuples with format:
	(x, y, z)
	'''
	def __init__(self, obj_id, initial_pos, initial_vel, mass, initial_acc=[0,0,0]):
		self.id = obj_id
		self.pos = initial_pos
		self.pnew = initial_pos
		self.vel = initial_vel
		self.vnew = initial_vel
		self.acc = initial_acc
		self.anew = initial_acc
		self.mass = mass
		# Initialize position and characteristics
		self.store()

	def euclidean(self, objk):
		return ( (self.pos[0] - objk.pos[0])**2 + (self.pos[1] - objk.pos[1])**2 + (self.pos[2] - objk.pos[2])**2 ) ** 0.5

	def reposition(self, obodies):
		'''
		For each reposition, we use the values calculated in the prior iteration
		pnew, vnew, and anew act as copies so we don't calculate the current moment's values using the next values
		'''
		self.pos = self.pnew
		self.vel = self.vnew
		self.acc = self.anew
		'''
		Calculate the new position for each dimension 'dm' {0:x, 1:y, 2:z}

		r is a dictionary of each object id (key) and its current distance from this body (value)
		Prevents redundant calculation of distance for each dimension
		'''
		r = {obj.id: self.euclidean(obj) for obj in obodies if (obj.id != self.id)}
		for dm, p_dm in enumerate(self.pos):
			'''
			Object i's dm-acceleration due to one object k is equal to
			acc_i_dm = -G * mass_k * (dm_k - dm_i) / distance ** 3

			Object i's x-acceleration due to n objects is equal to
			acc_i_dm = sum(acc_i_k) k to n for k != i
			'''
			self.anew[dm] = sum([self.dv(objk=obj, dm=dm, dist=r[obj.id]) for obj in obodies if (obj.id != self.id)])
			'''
			Using forward difference, the new velocity is proved via
			(v_dm_1 - v_dm_0) / dt = acc_dm
			v_dm_1 = v_dm_0 + acc_dm * dt
			or
			v_dm += acc_dm * dt
			'''
			self.vnew[dm] += self.acc[dm] * self.DT
			'''
			Same proof applies to new position:
			(p_dm_1 - p_dm_0) / dt = vel_dm
			p_dm_1 = p_dm_0 + vel_dm * dt
			or
			p_dm += vel_dm * dt
			'''
			self.pnew[dm] += self.vel[dm] * self.DT

		# For recording data and keeping track of time
		self.iterations += 1

		line = f"""{self.iterations*self.DT}, {self.id}, {self.mass}, {self.pnew[0]}, {self.pnew[1]}, {self.pnew[2]}, {self.vnew[0]}, {self.vnew[1]}, {self.vnew[2]}, {self.anew[0]}, {self.anew[1]}, {self.anew[2]}\n"""
		self.history.append(line)
		# Only save every 100 moments
		if self.iterations%100 == 0:
			self.bulkstore()

	# Change in velocity from another body on dimension dm
	def dv(self, objk, dm, dist):
		'''
		The change in velocity, or acceleration, due to one object is calculated via an aforementioned proof
		acc_i_dm = -G * mass_k * (dm_k - dm_i) / distance ** 3
		'''
		numerator = self.G * objk.mass * (objk.pos[dm] - self.pos[dm])
		#denominator = self.euclidean(objk) + self.SOFT**2
		return numerator / dist ** 3

	def store(self):
		with open(self.FILENAME, "a") as outfile:
			# Format: time, id, mass, x_position, y_position, z_position, x_velocity, y_velocity, z_velocity, x_acceleration, y_acceleration, z_acceleration
			line = f"""{self.iterations*self.DT}, {self.id}, {self.mass}, {self.pnew[0]}, {self.pnew[1]}, {self.pnew[2]}, {self.vnew[0]}, {self.vnew[1]}, {self.vnew[2]}, {self.anew[0]}, {self.anew[1]}, {self.anew[2]}\n"""
			outfile.write(line)

	def bulkstore(self):
		with open(self.FILENAME, "a") as outfile:
			outfile.writelines(self.history)

		self.history.clear()


if __name__ == '__main__':
	# Initialize celestial objects
	nbm = 1.989 * 10**29
	a = Body(
			obj_id="Sun",
			initial_pos=[-0.003798619468647882, 0.007439926519813712, 0.00002303011840607431],
			initial_vel=[-0.000008352236181200173, -0.000001991101976755789, 0.0000002303616187715271],
			mass = 1
			)
	b = Body(
			obj_id="Mercury",
			initial_pos=[-0.06713349519259718, -0.4534054004610566, -0.03182458153238026],
			initial_vel=[0.02221981555538470, -0.002401844191885121, -0.002234975522083475],
			mass=3.302 * 10**22 / nbm
			)
	c = Body(
			obj_id="Venus",
			initial_pos=[0.7194016804983604, 0.05998830582310215, -0.04098973222838171],
			initial_vel=[-0.001555617805193969, 0.02007933659165609, 0.0003650632309800465],
			mass= 48.685 * 10**22 / nbm
			)
	d = Body(
			obj_id="Earth",
			initial_pos=[-0.1701443808233178, 0.9765603186945023, -0.00001822571331401604],
			initial_vel=[-0.01724754632489101, -0.002983511998041463, 0.0000006558216388187520],
			mass=5.97219 * 10**23 / nbm
			)
	e = Body(
			obj_id="Moon",
			initial_pos=[-0.1675361509284642, 0.9759029512978545, -0.0002485074774953481],
			initial_vel=[-0.01710389404398059, -0.002443078545193917, -0.00001958221847802022],
			mass=734.9 * 10**19 / nbm
			)
	f = Body(
			obj_id="Mars",
			initial_pos=[-1.323906224669323, -0.8783175374839439, 0.01385210621844926],
			initial_vel=[0.008312502505202782, -0.01042477083991175, -0.0004223314046399220],
			mass=6.4185*10**22 / nbm
			)
	g = Body(
			obj_id="Jupiter",
			initial_pos=[0.5223484389354299, -5.193582583066160, 0.009853537301853853],
			initial_vel=[0.007415322218022803, 0.001114874487996517, -0.0001705268990722869],
			mass=1898.13 * 10**23 / nbm
			)
	h = Body(
			obj_id="Saturn",
			initial_pos=[3.793446245537600, -9.280656579684447, 0.01035620210507365],
			initial_vel=[0.004855156277940873, 0.002095215290585633, -0.0002300751169256309],
			mass=5.68319 * 10**25 / nbm
			)
	i = Body(
			obj_id="Uranus",
			initial_pos=[16.22169834985700, 11.38647559662688, -0.1678643601291325],
			initial_vel=[-0.002288449529894169, 0.003035896309705393, 0.00004097802109019479],
			mass=86.8103 * 10**23 / nbm
			)
	j = Body(
			obj_id="Neptune",
			initial_pos=[29.23901404431117, -6.359750396990894, -0.5428756529427533],
			initial_vel=[0.0006461740441876787, 0.003085635751878358, -0.00007869691796050940],
			mass=102.41 * 10**23 / nbm
			)
	k = Body(
			obj_id="Pluto",
			initial_pos=[12.97291157616514, -31.36273187454788, -0.3965374359206294],
			initial_vel=[0.002968249824832238, 0.0005356788760061291, -0.0009036709403060780],
			mass=1.307 * 10**21 / nbm
			)

	celestials = (a, b, c, d, e, f, g, h, i, j, k)

	'''Initialize file and headers'''
	with open("solarmodel.csv", "w") as outfile:
		# Format: time, id, mass, x_position, y_position, z_position, x_velocity, y_velocity, z_velocity, x_acceleration, y_acceleration, z_acceleration
		headers = "time, id, mass, x, y, z, dx/dt, dy/dt, dz/dt, dx/dt2, dy/dt2, dz/dt2\n"
		outfile.write(headers)

	itmax = 30000
	for it in range(itmax):
		for clstl in celestials:
			clstl.reposition(celestials)

		if (it == itmax - 1) & (len(clstl.history) > 0):
			clstl.bulkstore()

