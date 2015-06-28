

class Graph(object):
  ''' Class for graphs, containing vertices linked by
      weighted edges. '''
  def __init__(self):
    self._EdgeMap = {}
    self._Vertices = set([])

  def dijkstra(self, source):
    ''' Perform dijkstra's algorithm
        starting at source. '''
    tDist = {}
    tPrev = {}
    tUnvisited = []
    #TODO Fixme
    for tKey in self._Vertices:
      tDist[tKey] = float("inf")
      tPrev[tKey] = float("inf")
      tUnvisited.append(tKey)
    tDist[source] = 0
    tDist[-2] = float("inf")
    while tUnvisited:
      tVisit = min(tUnvisited, key=lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for entry in tEdges.items():
        (tNeighbor, tLeng) = entry
        tWeight = tDist[tVisit] + tLeng
        if tWeight < tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = tWeight
    return (tDist, tPrev)

  def dump_edges(self):
    ''' Print the edges in the graph. '''
    print(self._EdgeMap)

  @staticmethod
  def from_matrix(matrix):
    ''' Make a graph out of matrix. '''
    graph = Graph()
    rows = len(matrix)
    num_vertices = rows * len(matrix[0])
    func = lambda x, y: rows * y + x
    graph = Graph()
    for tX in range(rows):
      for tY in range(rows):
        vertex = func(tX, tY)
        left = func(tX - 1, tY)
        up = func(tX, tY - 1)
        right = func(tX + 1, tY)
        down = func(tX, tY + 1)
        weight = matrix[tY][tX]
        if tX > 0 and left >= 0:
          graph.set_edge(left, vertex, weight)
        if tY > 0 and up >= 0:
          graph.set_edge(up, vertex, weight)
        if tX < rows - 1 and right < num_vertices:
          graph.set_edge(right, vertex, weight)
        if tY < rows - 1 and down < num_vertices:
          graph.set_edge(down, vertex, weight)
    return graph

  @staticmethod
  def from_triangle(aTriangle):
    ''' Make a graph out of aTriangle. '''
    count = 0
    rows = len(aTriangle)
    graph = Graph()
    for row in range(rows):
      tColumns = len(aTriangle[row])
      if row != rows - 1:
        next_row = aTriangle[row + 1]
        for tX in range(tColumns):
          vertex = count + tX
          left_child = vertex + tColumns
          graph.set_edge(vertex, left_child, next_row[tX])
          graph.set_edge(vertex, left_child + 1, next_row[tX + 1])
      else:
        for tX in range(tColumns):
          vertex = count + tX
          graph.set_edge(vertex, -1, 0)
      count += tColumns
    return graph

  def longest_dag_path(self, source, dest):
    ''' Given that self is a dag, find the longest path
        between source and dest. '''
    tDist = {}
    tPrev = {}
    tUnvisited = []
    for tKey in self._Vertices:
      tDist[tKey] = - float("inf")
      #tPrev[tKey] = - float("inf")
      tUnvisited.append(tKey)
    tDist[source] = 0
    while tUnvisited:
      tVisit = max(tUnvisited, key=lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for entry in tEdges.items():
        (tNeighbor, tLeng) = entry
        weight = -(tDist[tVisit] + tLeng)
        if weight > tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = weight
    tTree = tPrev
    tPath = []
    tPrev = dest
    while tPrev != source:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

  def set_edge(self, aX, aY, weight):
    ''' Set the weight of aX -> aY to weight. '''
    self._Vertices.add(aX)
    self._Vertices.add(aY)
    if aX in self._EdgeMap:
      self._EdgeMap[aX][aY] = int(weight)
    else:
      self._EdgeMap[aX] = {aY : int(weight)}
    if not aY in self._EdgeMap:
      self._EdgeMap[aY] = {}

  def shortest_path(self, source, dest):
    ''' Return the nodes along the shortest path from
        source to dest. '''
    (tWeight, tTree) = self.dijkstra(source)
    tPath = []
    tPrev = dest
    tDist = tWeight[tPrev]
    while tPrev != source:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
      tDist = tWeight[tTemp]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath
