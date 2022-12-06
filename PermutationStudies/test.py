import os
import sys

sys.path.insert(0, ".")
from src.functions import *

df = read_blab(base_path="..")

print(df["Any_Result"][0:20])
