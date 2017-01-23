
import numpy as np
import random
def generate_centers(k, var, dim):
    """Generate k centers from 15-dimensional spherical Gaussian distribution with the given variance"""
    random.seed(22)
    centers = np.random.multivariate_normal(np.zeros(dim),  np.eye(dim)*var, k)
    return centers

def generate_data(k, var, dim, npoints):
    """Generate data points around each center such that there are 10,000 data points total including the centers?
    This could also be 10000 data points total plus the centers if this is better? Just chance the -k in 
    the sampData line"""
    random.seed(22)
    # generate centers
    centers = generate_centers(k, var, dim)
    # array to store points #
    points = np.empty([1,dim])
    # generate data around each center
    for i in range(k):
        points = np.concatenate((points, np.random.multivariate_normal(centers[i],np.eye(dim),npoints)), axis=0)
        points = np.delete(points, 0, axis=0)
    # sample points from array and combine these with centers
    sampData = np.concatenate((centers, points[np.random.choice(len(points),npoints-k)]), axis = 0)
    return(sampData)