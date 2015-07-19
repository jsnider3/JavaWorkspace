''' Wrapper for BinarySearchTree
  @author: Josh Snider
'''
import random

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

  def is_balanced(self):
    ''' Check that our subtrees are similar in height.'''
    child_heights = [tree.height() for tree in self.children()]
    if len(child_heights) == 1:
      child_heights.append(0)
    return self.is_leaf() or (max(child_heights) - min(child_heights) < 2)

  def is_leaf(self):
    ''' Convenience method.'''
    return not (self.left or self.right)

  def random_node(self):
    ''' Return a random element of the tree.'''
    idx = random.randrange(len(self))
    itr = iter(self)
    for x in range(idx):
      itr.next()
    return itr.next()

  def remove(self, val):
    ''' Delete the node n, where self.data == val, if present
        and return the updated tree.'''
    if val == self.data:
      if self.is_leaf():
        self = None
      elif len(self.children()) == 1:
        self = self.children()[0]
      else:
        kids = self.children()
        self = kids[0]
        self.add(kids[1])
    elif val < self.data and self.left:
      self.left = self.left.remove(val)
    elif val > self.data and self.right:
      self.right = self.right.remove(val)
    return self

  def sequences(self):
    ''' Cracking the Coding Interview 6th Edition 4.9,
        return a set of arrays which when added to a bst
        in left-to-right order, produces this. I believe
        the solution is to call sequences() on our children,
        get every possible interleaving of them, and then stick
        self.data on the beginning of them all.'''
    if self.is_leaf():
      return self.data
    elif len(self.children()) == 1:
      seqs = self.children()[0].sequences()
      seqs = {[self.data] + seq for seq in seqs}
      return seqs
    else:
      seqs_one = self.children()[0].sequences()
      seqs_two = self.children()[1].sequences()
      # TODO
      #seqs = every possible interleaving of a thing from seqs_one and
      # a thing from seqs_two.
      seqs = {[self.data] + seq for seq in seqs}
      return seqs
