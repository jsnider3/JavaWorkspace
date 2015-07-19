''' Wrapper for BinarySearchTree
  @author: Josh Snider
'''

class BinarySearchTree(object):
  ''' Binary Search Trees can store unique sorted
      data, so that most uses are O(log n).'''

  def __contains__(self, dat):
    ''' Search ourselves recursively for dat.'''
    return (dat == self.data or
      (self.left and dat in self.left) or
      (self.right and dat in self.right))

  def __init__(self, dat):
    ''' Create a leaf node.'''
    self.data = dat
    self.left = None
    self.right = None

  def __iter__(self):
    ''' In-order traversal.'''
    if self.left:
      for node in self.left:
        yield node  
    yield self.data
    if self.right:
      for node in self.right:
        yield node  

  def __len__(self):
    ''' Number of nodes in this tree.'''
    size = 1
    if self.left:
      size += len(self.left)
    if self.right:
      size += len(self.right)
    return size

  def __str__(self):
    ''' Print debug info.'''
    return (str(self.data) + ' has ' + str(len(self.children())) +
            ' children.') 

  def add(self, dat):
    ''' Add dat to this tree.'''
    if self.data < dat:
      if self.right:
        self.right.add(dat)
      else:
        self.right = BinarySearchTree(dat)
    elif self.data > dat:
      if self.left:
        self.left.add(dat)
      else:
        self.left = BinarySearchTree(dat)

  def children(self):
    ''' Our immediate children.'''
    nodes = []
    if self.left:
      nodes.append(self.left)
    if self.right:
      nodes.append(self.right)
    return nodes

  @staticmethod
  def from_array(arr):
    ''' Take a uniquified sorted array and make
        a minimal-depth BST of it.'''
    if not len(arr):
      return None
    elif len(arr) == 1:
      return BinarySearchTree(arr[0])
    else:
      mid = (len(arr) - 1)/2 
      tree = BinarySearchTree(arr[mid])
      tree.left = BinarySearchTree.from_array(arr[:mid])
      tree.right = BinarySearchTree.from_array(arr[mid + 1:])
      return tree

  def height(self):
    ''' Height of this tree.'''
    heights = [0] + [tree.height() for tree in self.children()]
    return max(heights) + 1

