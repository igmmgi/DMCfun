import re

with open('R/plot.R', 'r') as f:
    code = f.read()

# Fix T/F
for _ in range(3):
    code = re.sub(r'([=(,\[\s])F([),\]\s])', r'\g<1>FALSE\g<2>', code)
    code = re.sub(r'([=(,\[\s])T([),\]\s])', r'\g<1>TRUE\g<2>', code)

# Fix sapply -> vapply
# Look for sapply(x, function(x) 
# We know the closing parenthesis is at the end of the line for all these instances in plot.R.
# Let's verify by matching sapply(.*) on a single line.
# Actually, we can just replace 'sapply(' with 'vapply(' and then append ', numeric(1))' before the last closing bracket on that line.
lines = code.split('\n')
for i, line in enumerate(lines):
    if 'sapply(x, function(x)' in line:
        # replace sapply with vapply
        line = line.replace('sapply(x, function(x)', 'vapply(x, function(x)')
        # find the last closing parenthesis and insert ', FUN.VALUE = numeric(1)'
        last_paren_idx = line.rfind(')')
        if last_paren_idx != -1:
            line = line[:last_paren_idx] + ', FUN.VALUE = numeric(1)' + line[last_paren_idx:]
        lines[i] = line

code = '\n'.join(lines)

with open('R/plot.R', 'w') as f:
    f.write(code)

print("plot.R fixed")
