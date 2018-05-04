import os
import youtube_dl

def cmd(sh_cmd):
  print(sh_cmd)
  os.system(sh_cmd)

def set_artist():
  #TODO
  videos = os.listdir('Lyric Videos')
  for video in videos:
    print(os.path.join(os.getcwd(), 'Lyric Videos', video))

def convert_all_to_mp3():
  if not os.path.exists('Output'):
    os.mkdir('Output')
  videos = os.listdir('Lyric Videos')
  for video in videos:
    os.system('ffmpeg -i "{0}" -f mp3 "Output/{1}.mp3"'.format(
      os.path.join('Lyric Videos', video), os.path.splitext(video)[0]))

def get_machinima():
  print("Get list of JT Machinima videos.")
  cmd(
    'youtube-dl --audio-format mp3 -x -o {0} {1}'.format(
    '"%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s"',
    "https://www.youtube.com/playlist?list=PLjO04ZlrzPlh90yQok1nZSn4VX0pCCc64"))
  #videos = []
  #for video in videos:
  #  download_youtube(video)

if __name__ == "__main__":
  get_machinima()
  #convert_all_to_mp3()
