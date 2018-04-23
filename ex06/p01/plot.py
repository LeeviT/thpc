from matplotlib import pyplot
import pylab
import numpy as np

def f1(n):
    return 0.1e-10*n**3.01

def f2(n):
    return 0.1e-10*n**3.0

t = np.arange(0.0, 20000, 10)
data = pylab.genfromtxt("plotdata")

pyplot.plot(data[:,0], data[:,1], 'bo', f1(t), '-g', f2(t), '-r')
pyplot.xlabel('order of matrix (n)')
pyplot.ylabel('CPU time (seconds)')
pyplot.legend()
pyplot.show()
