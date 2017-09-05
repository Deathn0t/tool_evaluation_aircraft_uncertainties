"""
Ask prediction to CNNs
"""
import os, sys, glob
import pickle as pk
import mxnet as mx
from PIL import Image
import numpy as np
from collections import namedtuple
import selection

Batch = namedtuple('Batch', ['data'])

#Config
cnn_name = 'cnn_1st'
epoch = 8
is_gpu = False
context = mx.gpu() if (is_gpu) else mx.cpu()
data_shape = (1, 4, 224, 224)

def thPrediction_from_name(name):
    return float(name.split('_')[1])

def predict(path, mode='single'):
    """
    Ask prediction to CNN.
    mode: *single, to ask prediction about a single PNG
          *folder, to ask prediction about a folder of PNG
          *data, to ask prediction from a data_file
    """
    dic = {'single':predict_single_png,
           'folder':predict_folder_png,
           'data':predict_data_file,}
    dic[mode](path)

def predict_single_png(path):
    """
    Ask prediction to CNN where input is the PNG image referenced
    with PATH.
    path : absolute or relative image's path
    """
    sym, arg_params, aux_params = mx.model.load_checkpoint(cnn_name, epoch)
    mod = mx.mod.Module(symbol=sym, context=context, data_names=['data'],
            label_names=['softmax_label'])
    mod.bind(for_training=False, data_shapes=[('data', data_shape)],
    	label_shapes=mod._label_shapes)
    mod.set_params(arg_params, aux_params, allow_missing=True)

    print('Ask prediction for PNG : {0}'.format(path.split('/')[-1]))
    im = Image.open(path)
    imArray = np.reshape(np.array(im), (4, 224, 224))
    mod.forward(Batch(mx.nd.array([[imArray]])))
    prob = mod.get_outputs()[0].asnumpy()
    prob = np.squeeze(prob)
    a = np.argsort(prob)[::-1]
    for i in a:
        print('probability={0}, class={1}'.format(prob[i], i/10))
    print()
    return prob

def predict_folder_png(path):
    """
    Ask CNN to predict a folder of png at PATH.
    path: absolute or relative folder's path
    """
    sym, arg_params, aux_params = mx.model.load_checkpoint(cnn_name, epoch)
    mod = mx.mod.Module(symbol=sym, context=context, data_names=['data'],
            label_names=['softmax_label'])
    mod.bind(for_training=False, data_shapes=[('data', data_shape)],
    	label_shapes=mod._label_shapes)
    mod.set_params(arg_params, aux_params, allow_missing=True)

    print('Ask prediction for : {0} \n'.format(file_path.split('/')[-1]))
    if path[-1] != '/':
        path += '/'
    for png_path in glob.glob(path+'*.png'):
        for f in glob.glob('simu/images/selection/*.png'):
            os.system('rm {0}'.format(f))
        prob = predict_single_png(png_path)
        a = np.argsort(prob)[::-1]
        name_im = png_path.split('/')[-1]
        print('Prediction is : {0}'.format(thPrediction_from_name(name_im) == a[0]/10))
        if (thPrediction_from_name(name_im) != a[0]/10):
            selection.selection_according_image(png_path)
        input('Next ?')

def predict_data_file(path):
    """
    Ask CNN to predict a whole data_file, that contain the following dic
    structure :
    dic = {'train_data':..., 'train_label':...,
           'val_data':..., 'val_label':...}
    """
    print('Ask accuracy for : {0}'.format(path.split('/')[-1]))
    with open(path, 'rb') as f:
        dataset = pk.load(f)
    is_train_data = len(dataset['train_data']) > 0
    try:
        is_val_data = len(dataset['val_data']) > 0
    except:
        is_val_data = False
    acc = mx.metric.Accuracy()
    perp = mx.metric.Perplexity(ignore_label=None)

    if is_train_data:
        train_iter = mx.io.NDArrayIter(dataset['train_data'],
                                       dataset['train_label'],
                                       10)
    else:
        print('No training data...')
    if is_val_data:
        val_iter = mx.io.NDArrayIter(dataset['val_data'],
                                     dataset['val_label'],
                                     10)
    else:
        print('No validation data...')

    print('Loading trained CNN...')
    sym, arg_params, aux_params = mx.model.load_checkpoint(cnn_name, epoch)
    print('Trained CNN successfuly loaded !')

    print('Loading module...')
    mod = mx.mod.Module(symbol=sym, context=context, data_names=['data'],
            label_names=['softmax_label'])
    mod.bind(for_training=False,
             data_shapes=train_iter.provide_data,
    	     label_shapes=train_iter.provide_label)
    mod.set_params(arg_params, aux_params, allow_missing=True)
    print('Module successfuly loaded !')

    if is_train_data:
        mod.score(train_iter, [acc, perp])
        print('Accuracy for training data : {0}'.format(acc))
        print('Perplexity for training data : {0}'.format(perp))
    if is_val_data:
        mod.score(val_iter, [acc, perp])
        print('Accuracy for validation data : {0}'.format(acc))
        print('Perplexity for validation data : {0}'.format(perp))

#This part is not run if the script is imported.
if __name__ == '__main__':
    """
    Command example : python3 prediction.py data ./data_path
    """
    mode = sys.argv[1]
    file_path = sys.argv[2]
    predict(file_path, mode)
