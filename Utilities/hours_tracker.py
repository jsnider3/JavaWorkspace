'''
	Tool to track how much time you've spent working on a project.
	@author: Josh Snider
	@copyright: 2018
'''

import argparse
from datetime import datetime
from datetime import timedelta
import sys

def main():
	assert sys.version_info > (3,0)
	parser = argparse.ArgumentParser(description='Tool to track hours spent on a project.')
	parser.add_argument('project', help="The name of the project you're working on.")
	args = parser.parse_args()
	fname = args.project + '.txt'
	with open(fname, 'a') as f:
		com = ''
		while com != 'EXIT':
			com = input('')
			time_s = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
			f.write('{0} {1}\n'.format(com, time_s))
			f.flush()

def sum_hours(fname):
  start = None
  dur = timedelta()
  with open(fname) as f:
    lines = f.readlines()
    for line in lines:
      line = line.strip(' \n')
      print(line)
      if "START" in line:
        line = line[6:]
        start = datetime.strptime(line, '%Y-%m-%d %H:%M:%S')
      elif "STOP" in line:
        line = line[5:]
        stop = datetime.strptime(line, '%Y-%m-%d %H:%M:%S')
        dur += stop - start
  print(dur)

if __name__ == '__main__':
  sum_hours(sys.argv[1])#	main()
