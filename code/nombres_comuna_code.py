

import pandas as pd

path = "/Users/mdm9757/Desktop/nombres_data"

f = open(path + "/nombres_comuna.txt", "rb")
lines = f.readlines()
lines.pop(0) # remove header

data = {
  "ano-mes": [],
  "comuna": [],
  "nombre": [],
  "N": []
}


for l in lines:
  one_line = l.decode('latin-1').split(";")
  data["ano-mes"].append(one_line[0][:4] + "-" + one_line[0][4:])
  data["comuna"].append(one_line[1])
  data["nombre"].append(one_line[2].strip())
  data["N"].append(one_line[3].strip())

# export to CSV
df = pd.DataFrame.from_dict(data)
df.to_csv(path + '/nombres_comuna_clean.csv', index=False, header=True)
