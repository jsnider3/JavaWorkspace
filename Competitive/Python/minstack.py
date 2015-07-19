''' Cracking the Coding Interview 3.2.
    @author: Josh Snider'''

class MinStack(object):
  ''' A stack which also tracks the minimum in O(1).'''

  def __len__(self):
    ''' Length is same as underlying list.'''
    return len(self.lst)

  def __init__(self):
    ''' Make an empty stack.'''
    self.lst = []
    self.mins = []

  def empty(self):
    ''' Check if we're empty.'''
    return not len(self)

  def min(self):
    ''' Return the minimum in the stack.'''
    if self.empty():
      raise ValueError("Reading from empty MinStack")
    return self.mins[-1]

  def peek(self):
    ''' Return the top of the stack.'''
    if self.empty():
      raise ValueError("Reading from empty MinStack")
    return self.lst[-1]

  def pop(self):
    ''' Return and remove the top of the stack.'''
    if self.empty():
      raise ValueError("Popping from empty MinStack")
    elif self.mins[-1] == self.lst[-1]:
      self.mins.pop()
    self.lst.pop()

  def push(self, elm):
    ''' Add to stack.'''
    if self.empty() or elm <= self.mins[-1]:
      self.mins.append(elm)
    self.lst.append(elm)

