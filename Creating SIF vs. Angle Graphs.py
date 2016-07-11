# Imports needed modules.
import matplotlib.pyplot as plt
import numpy as np
import math

class SIFAngleGraphs:

	# This class contains all the necessary functions required to plot SIF vs Angle graphs. The only input to the class is a SIF .txt file provided as a string.

	def __init__(self, file_name_as_string):
		self.file_name_as_string = file_name_as_string


	# This function parses the SIF file and produces four lists: one containing the coordinates of each point ([in x, y, z] form), and three containing each Type I, Type II, and Type III SIF values respectively.

	def parse_SIF_file(self):
		sif_file = open(self.file_name_as_string)

		# Initializing all lists.
		line_counter = -1
		list_of_points = []
		list_of_SIF_values_Type_I = []
		list_of_SIF_values_Type_II = []
		list_of_SIF_values_Type_III = []

		# Iterating through every line in the SIF file.
		for line in sif_file:
			line = line.rstrip()
			line_counter += 1

			# We skip the first 6 lines of extraneous information in each file.
			if line_counter < 6:
				pass

			# For the remaining lines, data is appended to the appropriate list: coordinates (in [x, y, z] form) and the Type I, II, and III SIF values.
			else:
				line = line.split()
				list_of_points.append([line[0:3]])
				list_of_SIF_values_Type_I.append([line[3]])
				list_of_SIF_values_Type_II.append([line[4]])
				list_of_SIF_values_Type_III.append([line[5]])

		sif_file.close()

		
		# Four lists are returned: one containing the coordinates of each point ([in x, y, z] form), and three containing each Type I, Type II, and Type III SIF values respectively.
		return [list_of_points, list_of_SIF_values_Type_I, list_of_SIF_values_Type_II, list_of_SIF_values_Type_III]

	# This function calculates the coordinates of the center of the circle and returns them as [x, y, z]. Code was adapted from Jaime on Stack Overflow.
	def find_center_of_circle(self):

		# Three coordinates are chosen from the edge of the circle.
		list_of_coordinates = self.parse_SIF_file()[0]
		first_coordinate = list_of_coordinates[0][0]
		second_coordinate = list_of_coordinates[1][0]
		third_coordinate = list_of_coordinates[2][0]

		x1 = float(first_coordinate[0])
		y1 = float(first_coordinate[1])
		z1 = float(first_coordinate[2])

		x2 = float(second_coordinate[0])
		y2 = float(second_coordinate[1])
		z2 = float(second_coordinate[2])

		x3 = float(third_coordinate[0])
		y3 = float(third_coordinate[1])
		z3 = float(third_coordinate[2])

		# Three vectors are calculated using the three coordinates selected above.
		pointA = np.array([x1, y1, z1])
		pointB = np.array([x2, y2, z2])
		pointC = np.array([x3, y3, z3])
		vector_a = np.linalg.norm(pointC - pointB)
		vector_b = np.linalg.norm(pointC - pointA)
		vector_c = np.linalg.norm(pointB - pointA)

		b1 = vector_a * vector_a * (vector_b * vector_b + vector_c * vector_c - vector_a * vector_a)
		b2 = vector_b * vector_b * (vector_a * vector_a + vector_c * vector_c - vector_b * vector_b)
		b3 = vector_c * vector_c * (vector_a * vector_a + vector_b * vector_b - vector_c * vector_c)

		# The coordinates of the center of the circle are calculated.
		center_coordinates = np.column_stack((pointA, pointB, pointC)).dot(np.hstack((b1, b2, b3)))
		center_coordinates /= b1 + b2 + b3
		
		# Returns the center coordinates of the circle [x, y, z].
		return [center_coordinates[0], center_coordinates[1], center_coordinates[2]]


	# This function calculates the reference vector, from which the angles are calculated.

	def calculate_reference_vector(self):

		# The list of coordinates is retrieved using the previous parse_SIF_file function. The coordinates of the centre of the circle calculated using the find_center_of_circle function.
		points_for_angle_ref = self.parse_SIF_file()[0][0]
		points_circle_centre = self.find_center_of_circle()

		# The first coordinate is chosen as a reference point from which all the angles are calculated.
		angle_ref_x = float(points_for_angle_ref[0][0])
		angle_ref_y = float(points_for_angle_ref[0][1])
		angle_ref_z = float(points_for_angle_ref[0][2])

		centre_x = float(points_circle_centre[0])
		centre_y = float(points_circle_centre[1])
		centre_z = float(points_circle_centre[2])

		# Calculates reference vector by subtracting center coordinates from reference point.
		vector_x = angle_ref_x - centre_x
		vector_y = angle_ref_y - centre_y
		vector_z = angle_ref_z - centre_z

		# Returns reference vector [x, y, z].
		return [vector_x, vector_y, vector_z]


	# This function calculates the normal vector to the fracture circle.

	def calculate_normal_vector(self):

		# Three points from the center of the circle are retrieved: the first two points in the initial file and the circle centrepoint.
		point1 = self.parse_SIF_file()[0][0][0]
		point2 = self.parse_SIF_file()[0][1][0]
		point3 = self.find_center_of_circle()

		# Two vectors are calculated between the two points on the edge of the circle and the centre point.
		vector1_x = float(point2[0]) - float(point1[0])
		vector1_y = float(point2[1]) - float(point1[1])
		vector1_z = float(point2[2]) - float(point1[2])

		vector2_x = float(point3[0]) - float(point1[0])
		vector2_y = float(point3[1]) - float(point1[1])
		vector2_z = float(point3[2]) - float(point1[2])

		# Calculates the cross product (aka the normal vector) between the two vectors calculated above.
		cross_product = [(vector1_y * vector2_z - vector1_z * vector2_y), (-1 * vector1_x * vector2_z + vector1_z * vector2_x), (vector1_x * vector2_y - vector1_y * vector2_x)]

		# Returns the normal vector [x, y, z].
		return cross_product


	# This function calculates the vector from a point on the edge of the circle to the centrepoint of the circle. It takes three inputs: the x, y, and z coordinates of the point on the edge of the circle.

	def calculate_new_vector(self, x_coord, y_coord, z_coord):

		# Retrieves coordinates of circle centre using find_center_of_circle function.
		points_circle_centre = self.find_center_of_circle()

		centre_x = float(points_circle_centre[0])
		centre_y = float(points_circle_centre[1])
		centre_z = float(points_circle_centre[2])

		# Calculates the new vector by subtracting the circle centrepoint from the given coordinates.
		vector_x = x_coord - centre_x
		vector_y = y_coord - centre_y
		vector_z = z_coord - centre_z

		# Returns new vector as [x, y, z].
		return [vector_x, vector_y, vector_z]


	# This function calculates the angle between the reference vector and the given point on the circle's edge. This function takes three inputs: the x, y, and z coordinate of the given point.

	def calculate_angle(self, x_coord_1, y_coord_1, z_coord_1):

		# Calculates the reference vector and new vector using the calculate_reference_vector and calculate_new_vector functions.
		reference_vector = self.calculate_reference_vector()
		new_vector = self.calculate_new_vector(x_coord_1, y_coord_1, z_coord_1)

		angle_ref_x = reference_vector[0]
		angle_ref_y = reference_vector[1]
		angle_ref_z = reference_vector[2]

		x_coord = new_vector[0]
		y_coord = new_vector[1]
		z_coord = new_vector[2]

		# Calculates length of each vector.
		lenVectorRef = math.sqrt((angle_ref_x ** 2) + (angle_ref_y ** 2) + (angle_ref_z ** 2))
		lenVectorGiven = math.sqrt((x_coord ** 2) + (y_coord** 2) + (z_coord ** 2))

		# Calculates angle using the dot product and length of each vector.
		equation = (angle_ref_x * x_coord + angle_ref_y * y_coord + angle_ref_z * z_coord)/(lenVectorGiven * lenVectorRef)

		# Acos function has range [-1, 1], due to Python rounding errors sometimes data can be outside range, so this ensures all data is in correct range.
		if equation > 1:
			equation = 1.0
		elif equation < -1:
			equation = -1.0

		# Calculates angle.
		angle = math.acos(equation)

		# Calculates the cross product between the reference vector and new vector.
		cross_product = [(y_coord * angle_ref_z - z_coord * angle_ref_y), (x_coord * angle_ref_z - z_coord * angle_ref_x), (x_coord * angle_ref_y - y_coord * angle_ref_x)]
		
		# Retrieves normal vector from normal_vector function.
		normal_vector = self.calculate_normal_vector()

		# If the dot product between the normal vector and cross product is negative, the angle is made negative as acos function only returns positive angles but angle is actually negative.
		dot_product = normal_vector[0] * cross_product[0] + normal_vector[1] * cross_product[1] + normal_vector[2] * cross_product[2]

		if dot_product < 0:
			angle = angle * -1

		# Converts angle from radians to degrees.
		angle_degrees = angle * (180/math.pi)

		# Returns angle in degrees.
		return angle_degrees


	#This function plots the SIF vs. Angle graph.

	def plot_SIF_vs_angle_graph(self):

		# We create a list of points using the above parse_SIF_file function and initalize a list of angles.
		list_of_points = self.parse_SIF_file()[0]
		list_of_all_angles = []

		# We iterate through every coordinate in the file, calculating the angle for each set of coordinates.
		for list_index in range(len(list_of_points)):

			coordinates = list_of_points[list_index]

			x_coord = float(coordinates[0][0])
			y_coord = float(coordinates[0][1])
			z_coord = float(coordinates[0][2])

			angle = self.calculate_angle(x_coord, y_coord, z_coord)

			list_of_all_angles.append(angle)

		# We create arrays for each list of data: the central angles and the Type I, II, and III SIF values.
		angle_array = np.asarray(list_of_all_angles)
		Type_I_array =np.asarray(self.parse_SIF_file()[1])
		Type_II_array =np.asarray(self.parse_SIF_file()[2])
		Type_III_array =np.asarray(self.parse_SIF_file()[3])

		# Initializing the 2D plot.
		fig = plt.figure()
		ax = fig.add_subplot(111)

		# Plotting each of the Type I, Type II, and Type III SIF values versus the angles.
		for y_values, shape, label1 in [(Type_I_array, 'o', 'Type I'), (Type_II_array, '^', 'Type II'), (Type_III_array, '-', 'Type III')]:
			ax.plot(angle_array, y_values, shape, label=label1)

		# Setting various plot parameters (y-label, x-label, and legend location).
		ax.set_ylabel('Stress intensity factor (Pa/m^2)')
		ax.set_xlabel('Angle (degrees)')
		ax.legend(loc='center right')
		ax.set_xlim([-180, 180])

		# Returns plot for viewing.
		plt.show()

###############################################################
# # Test Case 1

# test = SIFAngleGraphs('SIF_FRACTURE0_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')
# test.plot_SIF_vs_angle_graph()


# # Test Case 2

# test = SIFAngleGraphs('SIF_FRACTURE1_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')
# test.plot_SIF_vs_angle_graph()


# # Test Case 3

# test = SIFAngleGraphs('SIF_FRACTURE2_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')
# test.plot_SIF_vs_angle_graph()


# # Test Case 4

# test = SIFAngleGraphs('SIF_FRACTURE3_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')
# test.plot_SIF_vs_angle_graph()
