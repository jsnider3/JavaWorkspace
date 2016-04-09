'''
  Solve the pancake revenge problem
  from Google Code Jam 2016.

  @author: Josh Snider

'''

''' Flip the top num pancakes.'''
def flip(pancakes, num):
  top = pancakes[:num]
  bottom = pancakes[num:]
  topflipd = []
  for n in reversed(top):
    if n == '+':
      topflipd.append('-')
    else:
      topflipd.append('+')
  return ''.join(topflipd) + bottom

''' Count the minimal number of flips
    needed to make a stack of pancakes
    all smiley.

    Current strategy:
    if there's a stack of happies on the top:
      flip them over.
    else:
      flip over everything down to the last sad face.  '''
def make_smiley(pancakes):
  if pancakes.count('-') == 0:
    return 0
  else:
    hap = -1
    for c in pancakes:
      if c == '+':
        hap += 1
      else:
        break
    if hap != -1:
      nxt = flip(pancakes, hap + 1)
    else:
      sad = pancakes.rfind('-')
      nxt = flip(pancakes, sad + 1)
    return 1 + make_smiley(nxt)

if __name__ == '__main__':
  numtests = int(input(''))
  for n in range(numtests):
    flips = make_smiley(raw_input(''))
    print ('Case #' + str(n+1) + ": " + str(flips))
