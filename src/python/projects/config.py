import os

class Config(object):
    PATH_PROJECT = os.path.dirname(os.path.abspath(__file__))
    PATH_PYTHON = os.path.dirname(PATH_PROJECT)
    PATH_SRC = os.path.dirname(PATH_PYTHON)
    PATH_BASE = os.path.dirname(PATH_SRC)
    PATH_DATA_BASE = os.path.join(PATH_BASE, 'data')
