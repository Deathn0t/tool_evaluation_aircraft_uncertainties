"""
Ce script à pour but de transformer l'ensemble des *.png récupérés
en un fichier unique les rassemblants. Ce fichier servira alors à
l'entrainement du réseau de neurones.
"""
from PIL import Image
import numpy as np
import pickle
from glob import glob
from random import shuffle
import sys
import mxnet as mx

PATH = 'simu/images/224x224/'

def filterName(name):
    """
    Retourne un dictionnaire des informations contenues dans le nom
    de l'image et les converties en valeures.
    """
    listOfString = name.split('_')
    dic = {'num':int(listOfString[0]),
           'insVitesse':float(listOfString[1])}
    return dic

def thOutput(insVitesse):
    """
    Retourne l'entier correspondant à la classe CNN.
    """
    return int(insVitesse*10)


def gatherImages(path='', mode='trainO'):
    """
    Rassemble les images et les informations sur celles-ci.
    path: Path where are the PNG to gather.
    mode: *trainO, all data are train data
          *valO, all data are validation data
          *both, 1/7 are validation data & 6/7 are train data
    """
    l = glob(path+'*.png')
    shuffle(l) #Mélange aléatoire en place
    train_limit = int((6/7)*len(l)) if mode == 'both' else len(l)
    partName = 'train' if (mode == 'both') or (mode == 'trainO') else 'val'
    images2gather = {'train_data':[],
                     'train_label':[],
                     'val_data':[],
                     'val_label':[],}
    for i in range(0,len(l)):
        nameImage = l[i]
        infoImage = filterName(nameImage.split('/')[-1])
        im = Image.open(nameImage)
        if (i >= train_limit):
            partName = 'val'
        images2gather[partName+'_data'].append(
                                np.reshape(np.array(im), (4, 224, 224)))
        images2gather[partName+'_label'].append(
                                thOutput(infoImage['insVitesse']))

    for key in images2gather:
        ex = mx.nd.array(np.array(images2gather[key]))
        images2gather[key] = ex
    return images2gather

def gatherImages2folders(pathf1='', pathf2=''):
    """
    Rassemble les images et les informations sur celles-ci.
        * pathf1: images pour l'entrainement.
        * pathf2: images pour la validation.
    """
    lf1 = glob(pathf1+'*.png')
    lf2 = glob(pathf2+'*.png')
    images2gather = {'train_data':[],
                     'train_label':[],
                     'val_data':[],
                     'val_label':[],}

    partName = 'train'
    l = lf1
    for i in range(0,len(l)):
        nameImage = l[i]
        infoImage = filterName(nameImage.split('/')[-1])
        im = Image.open(nameImage)
        images2gather[partName+'_data'].append(
                                np.reshape(np.array(im), (4, 224, 224)))
        images2gather[partName+'_label'].append(
                                thOutput(infoImage['insVitesse']))

    partName = 'val'
    l = lf2
    for i in range(0,len(l)):
        nameImage = l[i]
        infoImage = filterName(nameImage.split('/')[-1])
        im = Image.open(nameImage)
        images2gather[partName+'_data'].append(
                                np.reshape(np.array(im), (4, 224, 224)))
        images2gather[partName+'_label'].append(
                                thOutput(infoImage['insVitesse']))

    for key in images2gather:
        ex = mx.nd.array(np.array(images2gather[key]))
        images2gather[key] = ex
    return images2gather


if __name__ == '__main__':
    """
    Exemple de commande : python3 png2gather.py both data_file path_where_data
    """
    # mode = sys.argv[1]
    # out_name = sys.argv[2]
    # path_where_data = sys.argv[3]
    # images2gather = gatherImages(path_where_data, mode)
    out_name = sys.argv[1]
    path_f1 = sys.argv[2]
    path_f2 = sys.argv[3]
    images2gather = gatherImages2folders(path_f1, path_f2)
    with open('data/'+out_name, 'wb') as f:
        pickle.dump(images2gather, f)
