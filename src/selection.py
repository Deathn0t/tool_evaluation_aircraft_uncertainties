"""
Module permettant de sélectionnant des images se trouvants dans le dossier
indiqué par PATH, suivant certains critères.
"""
import glob
import sys
import os
from math import pi
from PIL import Image
import numpy as np

PATH = 'simu/images/224x224_training/'

def search_image_with(numIm='*', incVi='*', angle='*', viAv1='*', viAv2='*', depA1='*', depA2='*', path=''):
    """
    Retourne une liste de noms d'images suivant plusieurs critères.
        * numIm : numéro de l'image générée [0]
        * incVi : incertitude sur la vitesse [1]
        * angle : angle de la trajectoire du second avion [2]
        * viAv1 : vitesse du premier avion [3]
        * viAv2 : vitesse du second avion [4]
        * depA1 : temps de dépard du premier avion [5]
        * depA2 : temps de dépard du premier avion [6]
    """
    images_found = []
    filter1 = lambda x,y,z : classic_filter(x, y, z, err=0.01)
    filtres_list = [lambda x: filter1(angle, 2, x),
                    lambda x: filter1(viAv1, 3, x),
                    lambda x: filter1(viAv2, 4, x),
                    lambda x: filter1(depA1, 5, x),
                    lambda x: filter1(depA2, 6, x),]
    for pathIm in glob.glob(path+'*.png'):
        nameIm = pathIm.split('/')[-1]
        infoList = nameIm.split('_')
        should_add = True
        for filtre in filtres_list:
            should_add = should_add and filtre(infoList)
        print(angle,'=', infoList[2], '::', should_add)
        if should_add:
            images_found.append(pathIm)
    return images_found

def classic_filter(param, iInfoList, infoList, err):
    return (param == '*') or (abs(float(infoList[iInfoList]) - param) <= err)

def move_selection():
    images_found = search_image_with(angle=1.25, difVi=0.92, decTp=0.)
    for imPath in images_found:
        os.system('cp {0} simu/images/selection/'.format(imPath))

def selection_according_image(image_path='', looking_folder=''):
    imageName = image_path.split('/')[-1]
    infoList  = imageName.split('_')
    images_found = search_image_with(path=looking_folder,
                                     angle=float(infoList[2]))
    for im in images_found:
        os.system('cp {0} simu/images/selection'.format(im))
        print(im)

def distance_min():
    images_found = search_image_with(angle=pi/2)
    for pathIm in images_found:
        im = Image.open(pathIm)
        imArray = np.array(im)

if __name__ == '__main__':
    """
    Nomenclature des images :
        * numIm : numéro de l'image générée [0]
        * incVi : incertitude sur la vitesse [1]
        * angle : angle de la trajectoire du second avion [2]
        * viAv1 : vitesse du premier avion [3]
        * viAv2 : vitesse du second avion [4]
        * depA1 : temps de dépard du premier avion [5]
        * depA2 : temps de dépard du premier avion [6]
    Exemple de commande : python3 selection.py simu/images/out/image_name.png
    """
    image_path = sys.argv[1]
    selection_according_image(image_path, looking_folder='simu/images/out/')
