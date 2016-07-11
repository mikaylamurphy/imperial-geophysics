# Imports needed modules.
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt


# This function parses an SIF .txt file and plots the given coordinates in 3D. The only input is an SIF .txt file name given as a string.

def plot_coordinates_from_SIF_file(file_name_as_string):
		sif_file = open(file_name_as_string)

		# Initializing all lists.
		line_counter = -1
		list_of_points = []
		list_of_x_values_Type_I = []
		list_of_y_values_Type_II = []
		list_of_z_values_Type_III = []

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
				list_of_x_values_Type_I.append(float(line[0]))
				list_of_y_values_Type_II.append(float(line[1]))
				list_of_z_values_Type_III.append(float(line[2]))

		sif_file.close()

		# Lists of coordinates are converted to arrays.
		x_array = np.asarray(list_of_x_values_Type_I)
		y_array	= np.asarray(list_of_y_values_Type_II)
		z_array = np.asarray(list_of_z_values_Type_III)

		# Initializing the matplotlib 3D plot.
		fig = plt.figure()
		ax = fig.add_subplot(111, projection='3d')

		# Plotting the coordinates as a scatter plot.
		ax.scatter(x_array, y_array, z_array)

		# Labelling the plot axes
		ax.set_xlabel('X Label')
		ax.set_ylabel('Y Label')
		ax.set_zlabel('Z Label')

		# Returning the plot
		plt.show()

###############################################################
# # Test Case 1
plot_coordinates_from_SIF_file('SIF_FRACTURE3_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')

# # Test Case 2
plot_coordinates_from_SIF_file('SIF_FRACTURE0_S_1_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')

# # Test Case 3
# parse_SIF_file('SIF_FRACTURE0_S_0_METHOD__QP1_NTIPS_20_DR_TIP_HALF.txt')