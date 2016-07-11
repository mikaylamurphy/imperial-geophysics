#use three classes to group objects

class curve:
	family = 'undefined'
	n_points = 0
	n_edges = 0

	edges = []
	points = []  # Contains curve points from define_curve section.

class surface:
	n_surfaces = 0
	name = "undefined"
	points = []
	bspline = []

	def __init__(self):
		family = 'undefined'
		n_points = 0
		n_edges = 0
		edges = []
		n_surfaces = 0

class Tins:
	curves = []
	surfaces = []
	points = []

# now define a function for extracting the prescribed points. These all have the same format in a tin file
# need to extract the 2nd, 3rd and 4th objects of each line
#extracting the name of the point too, might be useful


	def extract_prescribed_point(self, line):

		strings = line.split()
		thispoint_x = strings[1]
		thispoint_y = strings[2]
		thispoint_z = strings[3]

		return (thispoint_x, thispoint_y, thispoint_z)


#now define a function for extracting curves
#these have 2 different formats in the tin file so much trickier!


	def extract_define_curve(self, lines):

		thiscurve = curve
		points_added_so_far = 0
		edges_added_so_far = 0

		for line in range(len(lines)):
			# Choosing whether to split line based on whitespace or commas.
			if ',' in lines[line]:
				splitline = lines[line].split(',')
			else:
 				splitline = lines[line].split()

			if line == 0:
				thiscurve.family = splitline[2]
			elif line == 1:
				if splitline[0] == "unstruct_curve":
					thiscurve.n_points = int(splitline[2])
					thiscurve.n_edges = int(splitline[4])
			else:
				if points_added_so_far < thiscurve.n_points:
					try:
						x = float(splitline[0])
						y = float(splitline[1])
						if len(splitline) > 2:	
							z = int(splitline[2])
							thiscurve.points.append([x, y, z])
						else:
							thiscurve.points.append([x, y])
					except:
						print splitline
						print "This is not a float :("
	# this shows that the code isn't reading the line i want it to.
	# it might not be looping round each define_curve and just continuing

				# if edges_added_so_far < thiscurve.n_edges:
				# 	print 'splitline 0'
				# 	print splitline[0]
				# 	start = float(splitline[0])
				# 	end = float(splitline[1])
				# 	print [start, end]
				# 	thiscurve.edges.append([start, end])
		return thiscurve

# now define a function to extract the surfaces
# again, there is 2 different formats in the tin file

	def extract_define_surface(self, lines):

		thissurface = surface
		polyline_points_added_so_far = 0 # not sure about the use of this line

# take the name of the surface from the first line
# ignore the second line- no useful information

		for line in range(len(lines)):
			# Splits based on whitespace or commas depending on line.
			if ',' in lines[line]:
				splitline = lines[line].split(',')
			else:
				splitline = lines[line].split()

			print line
			print lines[line]

			if line == 0:
				thissurface.name = splitline[2]
				print thissurface.name
			elif line == 1:
				pass
			elif "coedge" in lines[line]:
				thissurface.n_surfaces = str(splitline[2])
			elif "bspline" in lines[line]:
				for index in (line+1, len(lines)):
					thissurface.bspline.append(index)
				break
			else:
				print 'else'
				print polyline_points_added_so_far < thissurface.n_surfaces
				if polyline_points_added_so_far < thissurface.n_surfaces and 'polyline' not in splitline and 'loop' not in splitline and 'affix' not in splitline:
					print 'splitline 0'
					print splitline[0]
					x_point = float(splitline[0])
					y_point = float(splitline[1])

					if len(splitline) > 2:
						z_point = float(splitline[2])
						thissurface.points.append([x_point, y_point, z_point])
					else:
						thissurface.points.append([x_point, y_point])

		return thissurface


# now define a function to parse the tin files
# this uses the functions defined previously

	def parsing_TIN_files(self, file_name_as_string):
		tin_file = open(file_name_as_string)

		lines = tin_file.readlines()
		TIN_points = []
		TIN_curves = []
		TIN_surfaces = []

# firstly, ignore irrelevant lines using 'pass' function

		for line in range(len(lines)):
			if "set_triangulation_tolerance" in lines[line]:
				pass
			if "define_family" in lines[line]:
				pass
			if "bspline" in lines[line]:
				pass
			if "return" in lines[line]:
				pass
			if len(lines[line]) == 0:
				pass

# for the prescribed points, use the function defined above

			if "prescribed_point" in lines[line]:
				self.points.append(self.extract_prescribed_point(lines[line]))


			if "define_curve" in lines[line]:
				newlines = lines[line+1:]
				counter = 1
				for i in newlines:
					split = i.split()
					if split[0] == "define_curve" or split[0] == "define_surface" or split[0] == "prescribed_point" or split[0] == "define_family" or len(split[0]) == 0:
						break
					else:
						counter += 1

				print "define_curve line found, got this many additional lines: " , counter
				print lines[line:line+counter]
				self.curves.append(self.extract_define_curve(lines[line:line+counter]))

			if "define_surface" in lines[line]:
				newlines = lines[line+1:]
				counter = 1
				for j in newlines:
					print j
					split = j.split()
					if split[0] == "define_curve" or split[0] == "define_surface" or split[0] == "prescribed_point" or split[0] == "define_family" or len(split[0]) == 0 or split[0] == 'define_model':
						break
					else:
						counter+= 1
				print "define_surface link found, got this many lines: ", counter
				print lines[line:line+counter]
				self.surfaces.append(self.extract_define_surface(lines[line:line+counter]))

		return TIN_points, TIN_curves, TIN_surfaces


##############################################################
# Test Case 1

mytin = Tins()
print mytin.parsing_TIN_files('tetin_test0.tin')


