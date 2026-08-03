import re

with open('R/plot.R', 'r') as f:
    code = f.read()

# Fix T/F
for _ in range(3):
    code = re.sub(r'([=(,\[\s])F([),\]\s])', r'\g<1>FALSE\g<2>', code)
    code = re.sub(r'([=(,\[\s])T([),\]\s])', r'\g<1>TRUE\g<2>', code)

with open('R/plot.R', 'w') as f:
    f.write(code)

print("plot.R T/F fixed")
