import pandas as pd
import numpy as np
from projects.rossman.config import ConfigRossman as config

df = pd.read_csv(config.PATH_DATA_TRAIN, low_memory=False)
