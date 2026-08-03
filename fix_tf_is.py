import re

files = ['R/dmcFit.R', 'R/dmcSim.R', 'R/plot.R']
for fname in files:
    with open(fname, 'r') as f:
        code = f.read()

    # Apply multiple times to catch overlapping e.g. "F, F"
    for _ in range(3):
        code = re.sub(r'([=(,\[\s])F([),\]\s])', r'\g<1>FALSE\g<2>', code)
        code = re.sub(r'([=(,\[\s])T([),\]\s])', r'\g<1>TRUE\g<2>', code)

    code = re.sub(r'is\(([^,]+),\s*"([^"]+)"\)', r'inherits(\1, "\2")', code)

    with open(fname, 'w') as f:
        f.write(code)

with open('DESCRIPTION', 'r') as f:
    desc = f.read()

desc = desc.replace('  methods,\n', '')

with open('DESCRIPTION', 'w') as f:
    f.write(desc)

print("Modifications done.")
