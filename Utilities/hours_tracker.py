'''
	Tool to track how much time you've spent working on a project.
	@author: Josh Snider
	@copyright: 2018
'''

import argparse
from datetime import datetime

def main():
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

if __name__ == '__main__':
	main()
