"""
Module pour entrainer le CNN.
"""

import logging
logging.getLogger().setLevel(logging.INFO)
import mxnet as mx
import generation as gen
import numpy as np
import pickle as pk
from collections import namedtuple
import sys

Batch = namedtuple('Batch', ['data'])

def train(dic, epoch=10, learning_rate=0.01, numAff=1):
    """
    For training an already created modele.
    """
    model_prefix = 'cnn_1st'
    checkpoint = mx.callback.do_checkpoint(model_prefix)
    dic['module'].fit(dic['train_iter'],
                  eval_data=dic['val_iter'],
                  optimizer='sgd',
                  optimizer_params={'learning_rate': learning_rate},
                  eval_metric='acc',
                  batch_end_callback=mx.callback.Speedometer(dic['batch_size'], numAff),
                  epoch_end_callback=checkpoint,
                  num_epoch=epoch,)
    return dic

def go(train=True, data_file_path='data/data_0.1only_training', pretrained_name='', pretrained_epoch=0):
    #Loading CNN
    if len(pretrained_name) == 0:
        print('Creating new net...')
        data = mx.sym.Variable('data')
        cnn = gen.genNetwork(data, '1st')
        print('Net successfuly created.')
    else:
        print('Loading trained CNN : {0} \n...'.format(pretrained_name))
        cnn, arg_params, aux_params = mx.model.load_checkpoint(pretrained_name, pretrained_epoch)
        print('Trained CNN successfuly loaded !')

    print('Arguments : {0}'.format(cnn.list_arguments()))
    print('Outputs : {0} \n'.format(cnn.list_outputs()))

    #Loading data set
    with open(data_file_path, 'rb') as f:
        dataset = pk.load(f)

    print('Data was loaded !')
    print('Shape is : {0}'.format(list(dataset.keys())))
    print('FOR TRAINING :')
    print('Shape train_data is : {0}'.format(np.shape(dataset['train_data'])))
    print('Shape train_label is : {0}'.format(np.shape(dataset['train_label'])))
    print('FOR VALIDATION :')
    print('Shape val_data is : {0}'.format(np.shape(dataset['val_data'])))
    print('Shape val_label is : {0}'.format(np.shape(dataset['val_label'])))

    # ex = mx.nd.array(dataset['train_data'])
    # dataset['train_data'] = ex
    # ex = mx.nd.array(dataset['train_label'])
    # dataset['train_label'] = ex
    # print('Exemple shape TRAIN DATA : {0}'.format(np.shape(dataset['train_data'])))
    # print('Exemple shape TRAIN LABEL : {0}'.format(np.shape(dataset['train_label'])))

    #Setting iterator
    batch_size = 30
    isShuffle = False
    train_iter = mx.io.NDArrayIter(dataset['train_data'],
                                   dataset['train_label'],
                                   batch_size, shuffle=isShuffle)
    val_iter = mx.io.NDArrayIter(dataset['val_data'],
                                 dataset['val_label'],
                                 batch_size)
    print('Train iterator is ready with : batch_size = {0}, shuffle = {1}'.format(batch_size, isShuffle))
    print('Validation iterator is ready.')

    #Creating module
    cnn_model = mx.mod.Module(symbol=cnn,
                              context=[mx.gpu(0), mx.cpu(0), mx.cpu(1), mx.cpu(2), mx.cpu(3)],
			                  #context=mx.cpu(),
                              data_names=['data'],
                              label_names=['softmax_label'],)
    # cnn_model.set_params(arg_params, aux_params)
    print('Module creation successful.')

    model_prefix = 'cnn_1st'
    checkpoint = mx.callback.do_checkpoint(model_prefix)

    if train:
    	cnn_model.fit(train_iter,
                  eval_data=val_iter,
                  optimizer='sgd',
                  optimizer_params={'learning_rate': 0.1},
                  eval_metric='acc',
                  batch_end_callback=mx.callback.Speedometer(batch_size, 30),
                  epoch_end_callback=checkpoint,
                  num_epoch=300,
                  arg_params=arg_params,
                  aux_params=aux_params)
    	print('Fit successful.')

    dic = { 'net': cnn,
            'dataset': dataset,
            'module': cnn_model,
            'train_iter': train_iter,
            'val_iter': val_iter,
	        'batch_size':batch_size,
            }
    return dic

if __name__ == '__main__':
    """
    Command examples :
        * python3 training.py data_file pretrained_name pretrained_epoch
        * python3 training.py data_training cnn_1st 140
    """
    data_file_path = sys.argv[1]
    pretrained_name = sys.argv[2]
    pretrained_epoch = sys.argv[3]
    dic = go(data_file_path=data_file_path, pretrained_name=pretrained_name, pretrained_epoch=int(pretrained_epoch))
