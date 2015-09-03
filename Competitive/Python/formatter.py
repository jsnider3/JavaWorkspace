lines = open('possible_tunnels_tests.txt') .readlines()
lines = [line.split()[1] for line in lines]
lines = ['"' + line + '"' for line in lines]
print('[' + ',\n '.join(lines) + ']')

