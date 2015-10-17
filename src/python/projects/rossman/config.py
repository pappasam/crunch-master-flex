import os
from ..config import Config

class ConfigRossman(Config):
    PROJECT_NAME = "rossman"
    PATH_DATA = os.path.join(Config.PATH_DATA_BASE, PROJECT_NAME)
    PATH_DATA_TEST = os.path.join(PATH_DATA, "test.csv")
    PATH_DATA_TRAIN = os.path.join(PATH_DATA, "train.csv")
