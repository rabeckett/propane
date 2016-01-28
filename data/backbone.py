import csv
import os.path
import numpy as np
import matplotlib.pyplot as plt

# collect stats from csv
num_pods = []
num_nodes = []
sizes_raw = []
sizes_compressed = []
tpp_total_mean = []
tpp_build_mean = []
tpp_minimize_mean = []
tpp_order_mean = []
tpp_gen_mean = []
tpp_compress_mean = []

# read values from csv file
direct = os.path.dirname(os.path.realpath(__file__))
with open(direct + os.path.sep + 'stats-backbone.csv') as f:
  r = csv.reader(f)
  for row in r:
    num_pods.append(row[0])
    num_nodes.append(row[1])
    sizes_raw.append(row[4])
    sizes_compressed.append(row[5])
    tpp_total_mean.append(row[10])
    tpp_build_mean.append(row[13])
    tpp_minimize_mean.append(row[16])
    tpp_order_mean.append(row[19])
    tpp_gen_mean.append(row[22])
    tpp_compress_mean.append(row[25])

# remove header info, and convert type
num_pods = map(int, num_pods[1:])
num_nodes = map(int, num_nodes[1:])
sizes_raw = map(int, sizes_raw[1:])
sizes_compressed = map(int, sizes_compressed[1:])
tpp_total_mean = map(float, tpp_total_mean[1:])
tpp_build_mean = map(float, tpp_build_mean[1:])
tpp_minimize_mean = map(float, tpp_minimize_mean[1:])
tpp_order_mean = map(float, tpp_order_mean[1:])
tpp_gen_mean = map(float, tpp_gen_mean[1:])
tpp_compress_mean = map(float, tpp_compress_mean[1:])

#====================================================
# 
# Stack plot of compilation times broken down by task
#
#====================================================

# stack data lowest -> highest (build, minimize, order, gen, compress)
data = (tpp_build_mean, tpp_minimize_mean, tpp_order_mean, tpp_gen_mean)
foo = np.row_stack( data )
y_stack = np.cumsum(foo, axis=0)

# plot colors

#color1 = "#FFC09F"
#color2 = "#FFEE93"
#color3 = "#FCF5C7"
#color4 = "#A0CED9"
#color5 = "#ADF7B6"

color1 = "#828A95"
color2 = "#CEEAF7"
color3 = "#CCD7E4"
color4 = "#D5C9DF"
# color5 = "#DCB8CB"


# stacked plot showing different running times
fig = plt.figure()
plt.grid()
ax1 = fig.add_subplot(111)
ax1.fill_between(num_nodes, 0, y_stack[0,:], facecolor=color1, alpha=.7)
ax1.fill_between(num_nodes, y_stack[0,:], y_stack[1,:], facecolor=color2, alpha=.7)
ax1.fill_between(num_nodes, y_stack[1,:], y_stack[2,:], facecolor=color3)
ax1.fill_between(num_nodes, y_stack[2,:], y_stack[3,:], facecolor=color4)
ax1.set_xlabel('Routers', fontsize=35)
ax1.set_ylabel('Avg. Time / Prefix (s)', fontsize=35)
ax1.tick_params(axis='both', which='major', labelsize=35)
ax1.tick_params(axis='both', which='minor', labelsize=35)
ax1.xaxis.set_ticks([0,40,80,120,160,200])
ax1.yaxis.set_ticks([5,15,25,35,45])
#ax1.set_xlim([0,1400])
#ax1.set_ylim([0,20])
# custom legend for stack color
p1 = plt.Rectangle((0, 0), 1, 1, fc=color1, alpha=.7)
p2 = plt.Rectangle((0, 0), 1, 1, fc=color2, alpha=.7)
p3 = plt.Rectangle((0, 0), 1, 1, fc=color3, alpha=.7)
p4 = plt.Rectangle((0, 0), 1, 1, fc=color4, alpha=.7)
leg_boxes = [p4, p3, p2, p1]
descrs = ["Gen/Min ABGP", "Find Preferences", "Minimize PG", "Construct PG"]
ax1.legend(leg_boxes, descrs, loc=2, fontsize=24)
fig.savefig('compilation-times-backbone.png', bbox_inches='tight')


#====================================================
# 
# Size of generated vs compressed ABGP (bar)
#
#====================================================

num_nodes1 = num_nodes 
num_nodes2 = map(lambda x: x, num_nodes)
sizes_raw_per = map(lambda (size,n): size/n, zip(sizes_raw, num_nodes))
sizes_compressed_per = map(lambda (size,n): size/n, zip(sizes_compressed, num_nodes))

fig = plt.figure()
ax1 = fig.add_subplot(111)
ax1.bar(num_nodes1, sizes_raw_per, width=3.2, color=color1, alpha=1, align='center', log=True)
ax1.bar(num_nodes2, sizes_compressed_per, width=3.2, color=color3, alpha=1, align='center',log=True)
ax1.set_xlabel('Routers', fontsize=35)
ax1.set_ylabel('ABGP Lines/Router', fontsize=35)
ax1.tick_params(axis='both', which='major', labelsize=35)
ax1.tick_params(axis='both', which='minor', labelsize=35)
ax1.set_xlim([0,220])
ax1.set_ylim([0,10*10*10*10*10*10*10])
leg_boxes = [p1, p3]
descrs = ["Raw Config", "Minimized Config"]
ax1.legend(leg_boxes, descrs, loc=2, fontsize=24)
fig.savefig('config-compression-backbone.png', bbox_inches='tight')