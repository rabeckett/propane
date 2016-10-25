import csv
import sys
import os.path
import numpy as np
import matplotlib.pyplot as plt

direct = os.path.dirname(os.path.realpath(__file__))

def numRuns(data):
  i = 0
  start = data[0]
  while data[i] == start: 
    i = i+1
  return i

def average(data,i):
  d = []
  acc = 0
  mean = 0
  for x in data:
    acc = acc + 1    
    mean = mean + x
    if acc == i:
      d.append( mean / i )
      acc = 0
      mean = 0
  return d

def sortAll(size, total, agg, sub, core):
  indicesTotal = {}
  indicesAgg = {}
  indicesSub = {}
  indicesCore = {}
  for i in range(0, len(size)):
    indicesTotal[size[i]] = total[i]
    indicesAgg[size[i]] = agg[i]
    indicesSub[size[i]] = sub[i]
    indicesCore[size[i]] = core[i]
  s = sorted(size)
  tot = []
  agg = []
  sub = []
  core = []
  for v in s:
     tot.append(indicesTotal[v])
     agg.append(indicesAgg[v])
     sub.append(indicesSub[v])
     core.append(indicesCore[v])
  return s,tot,agg,sub,core

def adjust(size, total, agg, sub, core):
  i = numRuns(size)
  a = average(size,i)
  b = average(total,i)
  c = average(agg,i)
  d = average(sub,i)
  e = average(core,i)
  return sortAll(a,b,c,d,e)

def plot(file):
  with open(direct + os.path.sep + file) as f:
    size = []
    total = []
    totalParse = []
    totalToAbgp = []
    totalToConfig = []
    pgConstruct = []
    pgMinimize = []
    aggAnalysis = []
    findOrdering = []
    genAbgp = []
    minAbgp = []
    genCore = []
    genLowLevel = []
    substitution = []
    genVendor = []
    r = csv.reader(f)
    parity = False
    for row in r:
      parity = not parity
      if parity: 
        size.append(int(row[0]))
      else:
        total.append(float(row[0]))
        totalParse.append(float(row[1]))
        totalToAbgp.append(float(row[2]))
        totalToConfig.append(float(row[3]))
        pgConstruct.append(float(row[4]))
        pgMinimize.append(float(row[5]))
        aggAnalysis.append(float(row[6]))
        findOrdering.append(float(row[7]))
        genAbgp.append(float(row[8]))
        minAbgp.append(float(row[9]))
        genCore.append(float(row[10]))
        genLowLevel.append(float(row[11]))
        substitution.append(float(row[12]))
        genVendor.append(float(row[13]))
    return adjust(size, total, aggAnalysis, substitution, genCore)

if len(sys.argv) != 3: 
  print "Invalid Use:"
  print "   plot.py concrete.csv abstract.csv"
  exit()

sizeCon, totalCon, _, _, _ = plot(sys.argv[1])
sizeAbs, totalAbs, aggAbs, subAbs, genCore = plot(sys.argv[2])

# Plot colors
color1 = "#FFC09F"
color2 = "#ADF7B6"    # "#FFEE93"
color3 = "#FCF5C7"
color4 = "#A0CED9"

# Compilation times
fig = plt.figure()
plt.plot(sizeCon, totalCon, label="Concrete", linewidth=3.0, color="cornflowerblue")
plt.plot(sizeAbs, totalAbs, label="Abstract", linewidth=3.0, linestyle='--', color="lightsalmon")
plt.xlabel('Fattree #Pods', fontsize=25)
plt.ylabel('Compilation Time (sec)', fontsize=25)
plt.tick_params(axis='both', which='major', labelsize=25)
plt.tick_params(axis='both', which='minor', labelsize=25)
#plt.xticks([0,25,50,75,100,125,150])
#plt.ylim([.01, 10*10*10])
plt.xlim([4, 26])
plt.legend(loc=2, fontsize=22)
plt.yscale('log')
fig.savefig(direct + '/graphs/fat-time.png', bbox_inches='tight')

#====================================================
# 
# Stack plot of compilation times broken down by task
#
#====================================================

# stack data lowest -> highest (build, minimize, order, gen, compress)
other = map(lambda (x,y,z): x-y-z, zip(totalAbs,aggAbs,subAbs))
data = (aggAbs, subAbs, genCore, other)
y_stack = np.cumsum(np.row_stack( data ), axis=0)

# stacked plot showing different running times
fig = plt.figure()
plt.grid()
ax1 = fig.add_subplot(111)
ax1.fill_between(sizeAbs, 0, y_stack[0,:], facecolor=color1, alpha=.7)
ax1.fill_between(sizeAbs, y_stack[0,:], y_stack[1,:], facecolor=color2, alpha=.7)
ax1.fill_between(sizeAbs, y_stack[1,:], y_stack[2,:], facecolor=color3)
ax1.fill_between(sizeAbs, y_stack[2,:], y_stack[3,:], facecolor=color4)
ax1.set_xlabel('Fattree #Pods', fontsize=25)
ax1.set_ylabel('Compilation Time (sec)', fontsize=25)
ax1.tick_params(axis='both', which='major', labelsize=25)
ax1.tick_params(axis='both', which='minor', labelsize=25)
#ax1.xaxis.set_ticks([0,40,80,120,160,200])
#ax1.yaxis.set_ticks([5,15,25,35,45])
ax1.set_xlim([4,26])
ax1.set_ylim([0,10])

# custom legend for stack color
p1 = plt.Rectangle((0, 0), 1, 1, fc=color1, alpha=.7)
p2 = plt.Rectangle((0, 0), 1, 1, fc=color2, alpha=.7)
p3 = plt.Rectangle((0, 0), 1, 1, fc=color3, alpha=.7)
p4 = plt.Rectangle((0, 0), 1, 1, fc=color4, alpha=.7)
leg_boxes = [p4, p3, p2, p1]
descrs = ["Policy to ABGP", "ABGP to Quagga", "Substitution", "Failure Analysis"]
ax1.legend(leg_boxes, descrs, loc=2, fontsize=22)
fig.savefig(direct + '/graphs/fat-analysis-time.png', bbox_inches='tight')