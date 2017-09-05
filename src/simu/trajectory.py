"""
Get the info about trajectories from plns.txt, sol.txt that are produced by
genetic algorithm.
"""

import os, sys, glob
import matplotlib.pyplot as plt
from math import pi

new_acft = lambda : {'original':[], 'deviate':[]}

def collect_acft(path='AG_TD/', f1_name='plns.txt', f2_name='sol.txt'):
    """
    Get the info in files and create acft according to new_acft representation.
    """
    acftList = [new_acft() for i in range(2)]
    with open(path+f1_name, 'r') as f1:
        f1.readline()
        for acft in acftList:
            lgn = f1.readline()
            txyList = lgn[:-1].split(' ')[1:]
            for txy in txyList:
                t, xy = txy.split(':')
                x, y = xy.split(',')
                acft['original'].append((float(t),(float(x), float(y))))
    with open(path+f2_name, 'r') as f2:
        f2.readline()
        for acft in acftList:
            lgn = f2.readline()
            txyList = lgn[:-1].split(' ')[1:]
            for txy in txyList:
                t, xy = txy.split(':')
                x, y = xy.split(',')
                acft['deviate'].append((float(t),(float(x), float(y))))
    return acftList

def time_ratio(acft):
    t1, _ = acft['original'][-1]
    t2, _ = acft['deviate'][-1]
    return t2/t1

def time_ratio_sum(acfts):
    sm = 0
    for acft in acfts:
        sm += time_ratio(acft)
    return sm

def angle(pathf):
    return float(pathf.split('/')[-1].split('_')[3]) * 180./pi

def incert(pathf):
    return float(pathf.split('/')[-1].split('_')[2])

def list_to_hist(l):
    hist = {}
    for elt in l:
        if elt in hist.keys():
            hist[elt] += 1
        else:
            hist[elt] = 0
    return (hist.keys(), [hist[key] for key in hist.keys()])

def time_of_angle(path='images/out/'):
    lorg = sorted(glob.glob(path+'*_org*.txt'))
    ldev = sorted(glob.glob(path+'*_dev*.txt'))
    x = [[] for i in range(3)]
    y = [[] for i in range(3)]
    for pathf1, pathf2 in zip(lorg, ldev):
        acfts = collect_acft('', pathf1, pathf2)
        classe = int(incert(pathf1)*10)
        y[classe].append(time_ratio_sum(acfts))
        if (classe == 0):
            mv = -1
        if (classe == 1):
            mv = 0
        if (classe == 2):
            mv = 1
        x[int(incert(pathf1)*10)].append(angle(pathf1)+mv)
    fig, ax = plt.subplots()
    ax.plot(x[0], y[0], 'ro', label='Incert = 0.0')
    ax.plot(x[1], y[1], 'go', label='Incert = 0.1')
    ax.plot(x[2], y[2], 'bo', label='Incert = 0.2')
    ax.set_title('Temps en fonction de l\'angle.')
    ax.set_xlabel('Angle (°)')
    ax.set_ylabel('Ratio Temps')
    plt.legend()

    fig, ax = plt.subplots()
    width = 2       # the width of the bars
    xh0, yh0 = list_to_hist(x[0])
    xh1, yh1 = list_to_hist(x[1])
    xh2, yh2 = list_to_hist(x[2])

    ax.bar(xh0, yh0, width, color='r', label='Incert = 0.0')
    ax.bar(xh1, yh1, width, color='g', label='Incert = 0.1')
    ax.bar(xh2, yh2, width, color='b', label='Incert = 0.2')
    ax.set_xlabel('Angle (°)')
    ax.set_ylabel('Nombres d\'exemples')
    plt.legend()
    plt.show()



if __name__ == '__main__':
    time_of_angle()
