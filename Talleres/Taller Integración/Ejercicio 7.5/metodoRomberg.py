from __future__ import print_function
from numpy import add, isscalar, asarray, arange
from scipy.integrate.quadrature import tupleset

def RombergMethod(y, dx, show=False):

    axis=-1
    y = asarray(y)
    nd = len(y.shape)
    Nsamps = y.shape[axis]
    Ninterv = Nsamps-1
    n = 1
    k = 0

    while n < Ninterv:
        n <<= 1
        k += 1

    R = {}
    all = (slice(None),) * nd
    slice0 = tupleset(all, axis, 0)
    slicem1 = tupleset(all, axis, -1)
    h = Ninterv*asarray(dx)*1.0
    R[(1,1)] = (y[slice0] + y[slicem1])/2.0*h
    slice_R = all
    start = stop = step = Ninterv
    for i in range(2,k+1):
        start >>= 1
        slice_R = tupleset(slice_R, axis, slice(start,stop,step))
        step >>= 1
        R[(i,1)] = 0.5*(R[(i-1,1)] + h*add.reduce(y[slice_R],axis))
        for j in range(2,i+1):
            R[(i,j)] = R[(i,j-1)] + \
                       (R[(i,j-1)]-R[(i-1,j-1)]) / ((1 << (2*(j-1)))-1)
        h = h / 2.0

    if show:
        precis = 5
        width = 8
        formstr = "%" + str(width) + '.' + str(precis)+'f'

        print('\nMétodo de Romberg')
        for i in range(1,k+1):
            for j in range(1,i+1):
                print(formstr % R[(i,j)], end=' ')
            print()
        print('')

    return R[(k,k)]


def main():
    data_sample = [3.592, 3.110, 3.017, 2.865, 2.685]
    res = RombergMethod(data_sample, 0.2, show=True)

    print('Resultado: {}u²'.format(res))

if __name__ == "__main__":
    main()