''' Represents a directed weighted graph and
    provides common operations on them.'''

class Graph(object):
  ''' Class for graphs, containing vertices linked by
      weighted edges. '''

  def __init__(self, directed=True):
    ''' Graph with no edges or vertices.'''
    self.edge_map = {}
    self.vertices = set([])
    self.directed = directed

  def connected(self, source, dest):
    ''' True if there's a path between source and dest.'''
    if source not in self.edge_map or dest not in self.edge_map:
      raise KeyError("Source and dest must be in graph.")
    visits = set()
    queue = [source]
    while queue:
      visit = queue.pop()
      for neigh in self.edge_map[visit]:
        if neigh not in visits:
          queue.append(neigh)
      visits.add(visit)
    return dest in visits

  def dijkstra(self, source):
    ''' Perform dijkstra's algorithm
        starting at source. '''
    dist = {}
    prev = {}
    unvisited = []
    #TODO Fixme
    for vert in self.vertices:
      dist[vert] = float("inf")
      prev[vert] = float("inf")
      unvisited.append(vert)
    dist[source] = 0
    while unvisited:
      visit = min(unvisited, key=lambda x: dist[x])
      unvisited.remove(visit)
      edges = self.edge_map[visit]
      for (neighbor, leng) in edges.items():
        weight = dist[visit] + leng
        if weight < dist[neighbor]:
          prev[neighbor] = visit
          dist[neighbor] = weight
    return (dist, prev)

  def dump_edges(self):
    ''' Print the edges in the graph. '''
    print(self.edge_map)

  @staticmethod
  def from_edgelist(edgelist):
    ''' Make a graph out of a list of edges.
        Edges have the form of (from, to, weight).'''
    graph = Graph()
    for (source, dest, weight) in edgelist:
      graph.set_edge(source, dest, weight)
    return graph

  @staticmethod
  def from_matrix(matrix):
    ''' Make a graph out of a matrix. '''
    graph = Graph()
    rows = len(matrix)
    num_vertices = rows * len(matrix[0])
    func = lambda x, y: rows * y + x
    graph = Graph()
    for col in range(rows):
      for row in range(rows):
        vertex = func(col, row)
        left = func(col - 1, row)
        above = func(col, row - 1)
        right = func(col + 1, row)
        down = func(col, row + 1)
        weight = matrix[row][col]
        if col > 0 and left >= 0:
          graph.set_edge(left, vertex, weight)
        if row > 0 and above >= 0:
          graph.set_edge(above, vertex, weight)
        if col < rows - 1 and right < num_vertices:
          graph.set_edge(right, vertex, weight)
        if row < rows - 1 and down < num_vertices:
          graph.set_edge(down, vertex, weight)
    return graph

  @staticmethod
  def from_triangle(triangle):
    ''' Make a graph out of a triangle. '''
    count = 0
    rows = len(triangle)
    graph = Graph()
    for row in range(rows):
      columns = len(triangle[row])
      if row != rows - 1:
        next_row = triangle[row + 1]
        for col in range(columns):
          vertex = count + col
          left_child = vertex + columns
          graph.set_edge(vertex, left_child, next_row[col])
          graph.set_edge(vertex, left_child + 1, next_row[col + 1])
      else:
        for col in range(columns):
          vertex = count + col
          graph.set_edge(vertex, -1, 0)
      count += columns
    return graph

  def is_connected(self):
    pass

  def is_dag(self):
    pass

  def longest_dag_path(self, source, dest):
    ''' Given that self is a dag, find the longest path
        between source and dest. '''
    dist = {}
    prev = {}
    unvisited = []
    for key in self.vertices:
      dist[key] = - float("inf")
      unvisited.append(key)
    dist[source] = 0
    while unvisited:
      visit = max(unvisited, key=lambda x: dist[x])
      unvisited.remove(visit)
      edges = self.edge_map[visit]
      for entry in edges.items():
        (neighbor, leng) = entry
        weight = -(dist[visit] + leng)
        if weight > dist[neighbor]:
          prev[neighbor] = visit
          dist[neighbor] = weight
    tree = prev
    path = []
    prev = dest
    while prev != source:
      path.append(prev)
      prev = tree[prev]
    path.append(prev)
    path.reverse()
    return path

  def set_edge(self, source, dest, weight=1):
    ''' Set the weight of source -> dest to weight. '''
    self.vertices.add(source)
    self.vertices.add(dest)
    if source not in self.edge_map:
      self.edge_map[source] = {}
    self.edge_map[source][dest] = int(weight)
    if not dest in self.edge_map:
      self.edge_map[dest] = {}
    if not self.directed:
      self.edge_map[dest][source] = int(weight)

  def shortest_path(self, source, dest):
    ''' Return the nodes along the shortest path from
        source to dest. Includes source and dest.'''
    (_, tree) = self.dijkstra(source)
    path = []
    prev = dest
    while prev != source:
      path.append(prev)
      prev = tree[prev]
    path.append(prev)
    path.reverse()
    return path

  def topo_sort(self):
    #assert self.is_dag()
    visited = set([])
    queue = set(self.vertices)
    order = []
    while len(queue):
      for vert in list(queue):
        if all(dest in visited for dest in self.edge_map[vert].keys()):
          order.append(vert)
          visited.add(vert)
          queue.remove(vert)
    return list(reversed(order))
