'''
  Download all of Be All My Sins
  from the Space Battles forum.

  @author: Joshua Snider
'''

from HTMLParser import HTMLParser
import urllib2

from bs4 import BeautifulSoup

root = "https://forums.spacebattles.com/threads/"
first_page = root + "be-all-my-sins-40k-si.388340/"

class Post:

  def __init__(self, html):
    self.html = html
    self.threadmark = self.html.find(class_='threadmarker')
    self.body = self.html.find(class_='messageContent')
    self.post_num = self.html.get('id')
    if self.threadmark:
      self.threadmark = self.threadmark.find(class_='label').text.strip()
      self.threadmark = self.threadmark[self.threadmark.index(':') + 1:].strip()

  def __str__(self):
    text = ''
    if self.threadmark:
      text = str(self.threadmark)
    #print(self.body.encode('utf-8')[330:340])
    text = text + str(self.body)#.encode('utf-8')
    return text#str(self.html)

def filter_threadmarked(posts):
  return [p for p in posts if p.threadmark != None]

def get_all_pages():
  num_pages = 89
  pages = {}
  for ind in range(1, num_pages + 1):
    print(ind)
    cur_str = get_page(ind)
    pages[ind] = cur_str
  return pages

def get_main_page():
  test = urllib2.urlopen(first_page)
  test_str = test.read()

  posts = filter_threadmarked(get_posts(test_str))

  for p in posts:
    print(p)

def get_page(ind):
  cur_page = first_page + 'page-{0}'.format(ind)

  connect = urllib2.urlopen(cur_page)
  cur_str = connect.read()
  return cur_str

def get_posts(page_text):
  soup = BeautifulSoup(page_text, 'html.parser')

  lst = soup.find(id="messageList")
  posts = [Post(p) for p in lst.find_all(class_='message')]
  return posts

def get_threadmarks_list(page):
  #TODO Broken
  threadmarks_page = first_page + "threadmarks"
  print(threadmarks_page)
  test = urllib2.urlopen(first_page)
  test_str = test.read()
  #test_str = test_str[test_str.index('<ol class="threadmarkList ThreadmarkCategory_1">'):]
  print(test_str)

def get_story():
  pages = get_all_pages()
  posts = []
  for page_num in pages:
    posts.extend(get_posts(pages[page_num]))
  posts = filter_threadmarked(posts)
  for post in posts:
    save_post(post)

def save_post(post):
  print(post.threadmark, post.post_num, len(str(post)))
  filename = post.post_num + '_' + post.threadmark.replace(' ', '_') + '.html'
  print(filename)
  with open(filename,'w') as f:
    f.write(str(post))

def main():
  get_story()

if __name__ == '__main__':
  main()
