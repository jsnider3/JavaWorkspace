import os
import youtube_dl

def set_artist():
  #TODO
  videos = os.listdir('Lyric Videos')
  for video in videos:
    print(os.path.join(os.getcwd(), 'Lyric Videos', video))

def convert_all_to_mp3():
  os.mkdir('Output')
  videos = os.listdir('Lyric Videos')
  for video in videos:
    os.system('ffmpeg -i "{0}" -f mp3 "Output/{1}.mp3"'.format(
      os.path.join('Lyric Videos', video), os.path.splitext(video)[0]))

def get_machinima():
  print("Get list of JT Machinima videos.")
  os.system(
    "youtube-dl --extract-audio -o '%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s' {0}".format(
    "https://www.youtube.com/playlist?list=PLjO04ZlrzPlh90yQok1nZSn4VX0pCCc64"))
  #videos = []
  #for video in videos:
  #  download_youtube(video)

if __name__ == "__main__":
  convert_all_to_mp3()
