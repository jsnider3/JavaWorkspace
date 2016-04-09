#!/usr/bin/env python2

import sheep
import unittest

class Tests(unittest.TestCase):

  def test_sheep(self):
    assert sheep.count_sheep(0) == None
    assert sheep.count_sheep(1) == 10
    assert sheep.count_sheep(2) == 90
    assert sheep.count_sheep(11) == 110
    assert sheep.count_sheep(1692) == 5076

if __name__ == '__main__':
  unittest.main()

