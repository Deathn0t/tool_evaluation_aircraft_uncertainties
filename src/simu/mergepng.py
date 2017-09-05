from PIL import Image, ImageFilter
import glob
import os
cwd = os.getcwd()


PATH = 'images/'
print('TRYYYYYYY : ', glob.glob(PATH+'avant*.png'))
print(cwd)
f1_name = glob.glob(PATH+'avant*.png')[0]
f2_name = glob.glob(PATH+'apres*.png')[0]
print('NAMES : ', f1_name, f2_name)

# f1_name = 'image_avant'
# f2_name = 'image_apres'

im_av = Image.open(f1_name).convert(mode='RGBA')
im_ap = Image.open(f2_name).convert(mode='RGBA')

im_nw = Image.blend(im_av, im_ap, 0.5)
out = im_nw.filter(ImageFilter.DETAIL)

l = glob.glob(PATH+'out/'+'*.png')
if len(l):
    out.save(PATH+'out/'+str(len(l))+f1_name[len(PATH)+5:], 'PNG')
else:
    out.save(PATH+'out/'+'0'+f1_name[len(PATH)+5:], 'PNG')
