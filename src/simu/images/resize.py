import glob
from PIL import Image
import sys

def resize(path='out/'):
    """
    Redimensionne les images au format size.
    """
    for f in glob.glob('{0}*.png'.format(path)):
      im = Image.open(f).convert(mode='RGBA')
      size = 224,244
      out = im.resize((224,224), Image.ANTIALIAS)
      out.save('224x224/'+f.split('/')[-1], "PNG")

if __name__ == '__main__':
    path = sys.argv[1]
    resize(path)
