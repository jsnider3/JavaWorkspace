#!/usr/bin/env python3

import timeit

print(timeit.timeit('request.urlopen("http://www.joshuasnider.com").read()',
                    setup='from urllib import request', number=100))
print(timeit.timeit('request.urlopen("http://jsnider3.github.io").read()',
                    setup='from urllib import request', number=100))
