from bs4 import BeautifulSoup
from HTMLParser import HTMLParser
import urllib2

root = "https://www.fanfiction.net/s/12533655/{0}/Get-Me-Out-of-This-Hell-hole"

def save_page(page_num):
  test = urllib2.urlopen(root.format(page_num))
  webpage = test.read()
  soup = BeautifulSoup(webpage, 'html.parser')
  lst = soup.find(id='storytext')
  print((lst.renderContents()))
  with open('hell_hole_{0}.rtf'.format(page_num), "w") as text_file:
    text_file.write(lst.renderContents())

def main():
  for n in range(1, 15):
    save_page(n)

if __name__ == '__main__':
  main()

