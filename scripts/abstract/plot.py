import csv
import sys
import os.path
import numpy as np
import matplotlib.pyplot as plt

direct = os.path.dirname(os.path.realpath(__file__))

def numRuns(data):
  i = 0
  start = data[0]
  l = len(data)
  while i < l and data[i] == start: 
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
    inboundAnalysis = []
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
        inboundAnalysis.append(float(row[8]))
        genAbgp.append(float(row[9]))
        minAbgp.append(float(row[10]))
        genCore.append(float(row[11]))
        genLowLevel.append(float(row[12]))
        substitution.append(float(row[13]))
        genVendor.append(float(row[14]))
    return adjust(size, total, aggAnalysis, substitution, genCore)

def makeGraph(con, abs, name, descr):
  sizeCon, totalCon, _, _, _ = plot(con)
  sizeAbs, totalAbs, aggAbs, subAbs, genCore = plot(abs)

  # Plot colors
  color1 = "#FFC09F"
  color2 = "#ADF7B6"    # "#FFEE93"
  color3 = "#FCF5C7"
  color4 = "#A0CED9"

  # Compilation times
  fig = plt.figure()
  plt.plot(sizeCon, totalCon, label="Concrete", linewidth=5.0, color="cornflowerblue")
  plt.plot(sizeAbs, totalAbs, label="Abstract", linewidth=5.0, linestyle='--', color="lightsalmon")
  plt.xlabel(descr, fontsize=28)
  plt.ylabel('Compilation Time (sec)', fontsize=28)
  plt.tick_params(axis='both', which='major', labelsize=28)
  plt.tick_params(axis='both', which='minor', labelsize=28)
  if name == "Fattree":
    plt.xlim([4, 24])
  else:
    plt.xlim([10,240])
  plt.legend(loc=2, fontsize=22)
  plt.yscale('log')
  fig.savefig(direct + '/graphs/' + name + '-time.png', bbox_inches='tight')

  #====================================================

  # stack data lowest -> highest (build, minimize, order, gen, compress)
  other = map(lambda (w,x,y,z): w-x-y-z, zip(totalAbs,aggAbs,subAbs,genCore))
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
  ax1.set_xlabel(descr, fontsize=28)
  ax1.set_ylabel('Compilation Time (sec)', fontsize=28)
  ax1.tick_params(axis='both', which='major', labelsize=28)
  ax1.tick_params(axis='both', which='minor', labelsize=28)
  if name == "Fattree":
    ax1.set_xlim([4,24])
    ax1.set_ylim([0,14])
  else: 
    ax1.set_xlim([10,240])
    ax1.set_ylim([0,5])
  # custom legend for stack color
  p1 = plt.Rectangle((0, 0), 1, 1, fc=color1, alpha=.7)
  p2 = plt.Rectangle((0, 0), 1, 1, fc=color2, alpha=.7)
  p3 = plt.Rectangle((0, 0), 1, 1, fc=color3, alpha=.7)
  p4 = plt.Rectangle((0, 0), 1, 1, fc=color4, alpha=.7)
  leg_boxes = [p4, p3, p2, p1]
  descrs = ["Policy to ABGP", "ABGP to Quagga", "Substitution", "Failure Analysis"]
  ax1.legend(leg_boxes, descrs, loc=2, fontsize=22)
  fig.savefig(direct + '/graphs/' + name + '-analysis-time.png', bbox_inches='tight')


fattree_con = "data_fat_con.csv"
fattree_abs = "data_fat_abs.csv"
backbone_con = "data_core_con.csv"
backbone_abs = "data_core_abs.csv"

makeGraph(fattree_con, fattree_abs, "Fattree", "Fattree #Pods")
makeGraph(backbone_con, backbone_abs, "Backbone", "Backbone #Routers")