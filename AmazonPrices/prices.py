#!/usr/bin/env python3

from bs4 import BeautifulSoup
from urllib import request

def get_product_url():
  ''' Get the url for a random Amazon product. '''
  req = request.Request('http://www.randomamazonproduct.com/',
      headers = { 'User-Agent':
                  'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
    })
  soup = BeautifulSoup(request.urlopen(req).read())
  product = soup('a')[0]['href']
  return product

def main():
  print(get_product_url())

if __name__ == '__main__':
  main()
