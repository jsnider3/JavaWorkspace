import eyed3
import os
import youtube_dl

def cmd(sh_cmd):
  print(sh_cmd)
  os.system(sh_cmd)

def set_tags(playlist):
  #TODO
  videos = os.listdir('Lyric Videos')
  for video in videos:
    audiofile = {}
    audiofile['tag'] = {}
    audiofile.tag.album = 'playlist_title'
    audiofile.tag.track_num = 2
	#audiofile.tag.save()
    print(os.path.join(os.getcwd(), 'Lyric Videos', video))

def convert_all_to_mp3():
  if not os.path.exists('Output'):
    os.mkdir('Output')
  videos = os.listdir('Lyric Videos')
  for video in videos:
    os.system('ffmpeg -i "{0}" -f mp3 "Output/{1}.mp3"'.format(
      os.path.join('Lyric Videos', video), os.path.splitext(video)[0]))

def get_machinima(playlist):
  print("Get playlist of {0} videos.".format(playlist[0]))
  cmd(
    'youtube-dl --embed-thumbnail --add-metadata --audio-format mp3 -x -o {0} {1}'.format(
    '"%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s"',
	playlist[1]))
	#"https://www.youtube.com/watch?v=bwksqBOeYlo&list=UU40gs0opj389ohjLnJIAJzA"))
    #"https://www.youtube.com/playlist?list=PLjO04ZlrzPlh90yQok1nZSn4VX0pCCc64"))
  #videos = []
  #for video in videos:
  #  download_youtube(video)

if __name__ == "__main__":
  playlist = ["a capella science", "https://www.youtube.com/playlist?list=PLboc_P9zcJhgZoRGbypiY29q-RCmIcN2D"]
  get_machinima(playlist)
  #set_tags(playlist)
  #convert_all_to_mp3()
