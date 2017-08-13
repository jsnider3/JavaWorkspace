'''
  Download all of Be All My Sins
  from the Space Battles forum.

  @author: Joshua Snider
'''

import chardet
from HTMLParser import HTMLParser
import os
import urllib2

from bs4 import BeautifulSoup

root = "https://forums.spacebattles.com/threads/"

class Post:

  def __init__(self, html):
    self.html = html
    self.threadmark = self.html.find(class_='threadmarker')
    self.body = self.html.find(class_='messageContent')
    self.post_num = self.html.get('id')
    if self.threadmark:
      self.threadmark = self.threadmark.find(class_='label').text.strip()
      self.threadmark = self.threadmark[self.threadmark.index(':') + 1:].strip()
    else:
      self.threadmark = ''

  def __len__(self):
    return len(str(self))

  def __str__(self):
    text = self.threadmark.encode('utf-8') + str(self.body)
    return text

  def is_threadmarked(self):
    return self.threadmark != None and self.threadmark != ''

  def save_post(self, folder):
    if not os.path.exists(folder):
        os.makedirs(folder)
    print(self.threadmark, self.post_num, len(self))
    filename = os.path.abspath(folder + '/' + self.post_num + '_' +
                self.threadmark.replace(' ', '_') + '.html')
    with open(filename,'w') as f:
      f.write(str(self))
      cmd = 'google-chrome --headless --disable-gpu --print-to-pdf={0} file://{1}'.format(
        filename.replace('html','pdf'), filename)
      os.system(cmd)

class Thread:

  def __init__(self, thread_name):
    self.first_page = root + thread_name
    self.title = self.get_title()

  def get_all_pages(self):
    num_pages = self.get_num_pages()
    pages = {}
    for ind in range(1, num_pages + 1):
      print(ind)
      cur_str = self.get_page(ind)
      pages[ind] = cur_str
    return pages

  def get_num_pages(self):
    page_text = self.get_page(1)
    soup = BeautifulSoup(page_text, 'html.parser')
    lst = soup.find(class_="pageNavHeader")
    num_pages = lst.string
    num_pages = num_pages[num_pages.index('of') + 2:]
    return int(num_pages.strip(' '))

  def get_page(self, ind):
    cur_page = self.first_page + 'page-{0}'.format(ind)

    connect = urllib2.urlopen(cur_page)
    cur_str = connect.read()
    return cur_str

  def get_posts(self, page_text):
    soup = BeautifulSoup(page_text, 'html.parser')

    lst = soup.find(id="messageList")
    posts = [Post(p) for p in lst.find_all(class_='message')]
    return posts

  def get_title(self):
    test = urllib2.urlopen(self.first_page)
    webpage = test.read()
    soup = BeautifulSoup(webpage, 'html.parser')
    title = soup.title.string
    if '(' in title:
      title = title[:title.index('(')]
    if '[' in title:
      title = title[:title.index('[')]
    title = title.replace(' ', '')
    print(title)
    return title

  def get_story(self):
    pages = self.get_all_pages()
    posts = []
    for page_num in pages:
      posts.extend(self.get_posts(pages[page_num]))
    posts = [p for p in posts if p.is_threadmarked()]
    for post in posts:
      post.save_post(self.title)

def main():
  #Thread("be-all-my-sins-40k-si.388340/").get_story()
  Thread("the-games-we-play-rwby-the-gamer-ryuugi-complete.351105/").get_story()

if __name__ == '__main__':
  main()
