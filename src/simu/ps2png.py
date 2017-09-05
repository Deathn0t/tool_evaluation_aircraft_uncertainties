import os
import glob
from PIL import Image, ImageFilter

PATH = 'images/'
for f in glob.glob(PATH+'*.ps'):
    os.system('convert '+f+' '+f[:-3]+'.png')

f_name = glob.glob(PATH+'apres*.png')[0]

im_ap = Image.open(f_name).convert(mode='RGBA')

out = im_ap.filter(ImageFilter.DETAIL)

l = glob.glob(PATH+'out/'+'*.png')
long = len(l)
partName = f_name[len(PATH)+5:]
out.save(PATH+'out/'+str(long)+partName, 'PNG')
os.system('cp AG_TD/plns.txt images/out/{0}_org{1}.txt'.format(long, partName[:-4]))
os.system('cp AG_TD/sol.txt images/out/{0}_dev{1}.txt'.format(long, partName[:-4]))
