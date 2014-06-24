import numpy as np
import matplotlib.pyplot as plt
import math

f = open('1.24.dat')
lines = f.readlines()
f.close()
count = len(lines)
beginCount = math.log(float(lines[0].split()[0]), 10)
endCount = math.log(float(lines[-1].split()[0]), 10)
x = np.logspace(beginCount, endCount, num=(endCount - beginCount + 2))
y = [float(line.split()[1]) for line in lines]
print len(x)
print len(y)
print x
print y
plt.plot(x, y)
plt.xscale('log')
plt.show()