import csv
import os.path
import numpy as np
import matplotlib.pyplot as plt

# collect stats from csv
num_pods = []
tpp_total_mean = []
tpp_build_mean = []
tpp_minimize_mean = []
tpp_order_mean = []
tpp_gen_mean = []
tpp_compress_mean = []

# read values from csv file
direct = os.path.dirname(os.path.realpath(__file__))
with open(direct + os.path.sep + 'stats.csv') as f:
  r = csv.reader(f)
  for row in r:
    num_pods.append(row[0])
    tpp_total_mean.append(row[10])
    tpp_build_mean.append(row[13])
    tpp_minimize_mean.append(row[16])
    tpp_order_mean.append(row[19])
    tpp_gen_mean.append(row[22])
    tpp_compress_mean.append(row[25])

# remove first row, and convert type
num_pods = map(int, num_pods[2:])
tpp_total_mean = map(float, tpp_total_mean[2:])
tpp_build_mean = map(float, tpp_build_mean[2:])
tpp_minimize_mean = map(float, tpp_minimize_mean[2:])
tpp_order_mean = map(float, tpp_order_mean[2:])
tpp_gen_mean = map(float, tpp_gen_mean[2:])
tpp_compress_mean = map(float, tpp_compress_mean[2:])

# stack data lowest -> highest (build, minimize, order, gen, compress)
data = (tpp_build_mean, tpp_minimize_mean, tpp_order_mean, tpp_gen_mean, tpp_compress_mean)
foo = np.row_stack( data )
y_stack = np.cumsum(foo, axis=0)

# plot colors
color1 = "#FFC09F"
color2 = "#FFEE93"
color3 = "#FCF5C7"
color4 = "#A0CED9"
color5 = "#ADF7B6"

# stacked plot showing different running times
fig = plt.figure()
plt.grid()
ax1 = fig.add_subplot(111)
ax1.fill_between(num_pods, 0, y_stack[0,:], facecolor=color1, alpha=.7)
ax1.fill_between(num_pods, y_stack[0,:], y_stack[1,:], facecolor=color2, alpha=.7)
ax1.fill_between(num_pods, y_stack[1,:], y_stack[2,:], facecolor=color3)
ax1.fill_between(num_pods, y_stack[2,:], y_stack[3,:], facecolor=color4)
ax1.fill_between(num_pods, y_stack[3,:], y_stack[4,:], facecolor=color5)
ax1.set_xlabel('Fattree pod size')
ax1.set_ylabel('Time (sec)')
# custom legend for stack color
p1 = plt.Rectangle((0, 0), 1, 1, fc=color1, alpha=.7)
p2 = plt.Rectangle((0, 0), 1, 1, fc=color2, alpha=.7)
p3 = plt.Rectangle((0, 0), 1, 1, fc=color3, alpha=.7)
p4 = plt.Rectangle((0, 0), 1, 1, fc=color4, alpha=.7)
p5 = plt.Rectangle((0, 0), 1, 1, fc=color5, alpha=.7)
leg_boxes = [p1, p2, p3, p4, p5]
descrs = ["Construct PG", "Minimize PG", "Find Preferences", "Generate ABGP", "Minimize ABGP"]
ax1.legend(leg_boxes, descrs, loc=2)
fig.savefig('compilation-time-stacked.png')

# plot figures 
fig = plt.figure()
plt.plot(num_pods, tpp_total_mean, label='Total')
plt.xlabel('Fattree pod size')
plt.ylabel('Time (sec)')
plt.legend(loc=2)
fig.savefig('compilation-time.png')