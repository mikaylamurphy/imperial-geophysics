# need numpy to create an array later on
import numpy as np

# first of all, extract necessary information

class Tins:
	points = []

class point:
	def __init__(self):
		x = []
		y = []
		z = []

# define a function for extracting the prescribed points. These all have the same format in a tin file
# need to extract the 2nd, 3rd and 4th objects of each line
# extracting the name of the point too, might be useful

	def extract_prescribed_point(self, lines):

		thispoint = point
		pres_point = lines
		for k in pres_point:
			strings = k.split()
			thispoint.x = strings[1]
			thispoint.y = strings[2]
			thispoint.z = strings[3]

		return thispoint

	def writing_TIN_files(self, file_name_as_string):
			tin_file = open(file_name_as_string)

			lines = tin_file.readlines()
			TIN_points = []

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
			if "//" in lines[line]:
				pass

# for the prescribed points, use the function defined above

			if 'prescribed_point' in lines[line]:
				self.points.append(self.extract_prescribed_point(lines))
		print TIN_points
		return TIN_points

TIN_points_array = np.array(TIN_points)

mytin = Tins()
mytin.writing_TIN_files('tetin_test0.tin')

# writing the extracted information into a new file.
#
outfile = open("tin2vtk", "w")
for x, y, z in TIN_points_array:
	outfile.write((x, y, z))
	outfile.write("\n")
oufile.close()