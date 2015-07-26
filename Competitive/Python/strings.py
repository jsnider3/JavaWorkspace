''' A library for solving coding challenges
    involving strings.
    @author: Josh Snider'''
import string

def british_number_string(num):
  ''' Convert a number to a string
      in the british way. '''
  strn = str(num)
  words = []
  tens = {"1" : "ten", "2" : "twenty", "3" : "thirty", "4" : "forty",
          "5" : "fifty", "6" : "sixty", "7" : "seventy", "8" : "eighty",
          "9" : "ninety", "11" : "eleven", "12" : "twelve", "13" : "thirteen",
          "14" : "fourteen", "15" : "fifteen", "16" : "sixteen",
          "17" : "seventeen", "18" : "eighteen", "19" : "nineteen"}
  ones = {"1" : "one", "2" : "two", "3" : "three", "4" : "four",
          "5" : "five", "6" : "six", "7" : "seven", "8" : "eight",
          "9" : "nine"}
  if len(strn) == 4:
    words.append(ones[strn[0]])
    words.append("thousand")
    strn = strn[1:]
  if len(strn) == 3:
    if strn[0] in ones:
      words.append(ones[strn[0]])
      words.append("hundred")
    if strn[1:] != "00":
      words.append("and")
    strn = strn[1:]
  if len(strn) == 2:
    if strn in tens:
      words.append(tens[strn])
      strn = strn[2:]
    else:
      if strn[0] in tens:
        words.append(tens[strn[0]])
      strn = strn[1:]
  if len(strn) == 1:
    if strn[0] in ones:
      words.append(ones[strn[0]])
    strn = strn[1:]
  strn = " ".join(words)
  print(strn)
  return strn

def chars_used(word):
  ''' Get the number of unique characters in a string. '''
  return len(set(word))

def is_anagram(first, second):
  ''' return if first and second are anagrams
      of each other. '''
  first = str(first)
  second = str(second)
  for char in first + second:
    if first.count(char) != second.count(char):
      return False
  return True

def is_pangram(phrase):
  ''' Determine if phrase contains each of [a-z] once.'''
  phrase = set(phrase.lower())
  for c in string.ascii_lowercase:
    if c not in phrase:
      return False
  return True

def unique_str(words):
  ''' Determine if a string is composed entirely
      of unique characters. '''
  return len(words) == chars_used(words)

