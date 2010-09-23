import os
import string
import re

hostname = "localhost"
port = 70
cache_filename = ".cache"

files_type = {".txt": 6, ".exe" : 9, ".rtf" : 6, ".jpg" : 9}

class Entry:
  def __init__(self, hostname, port, selector_name, selector, file_type):
    self.hostname = hostname
    self.port = str(port)
    self.selector_name = selector_name
    self.selector = selector
    self.file_type = str(file_type)

  def __str__(self):
    return (string.join ((self.file_type + self.selector_name, self.selector,
      		   self.hostname, self.port), "\t") + "\n")

#Transform the list of objects Entry in a .cache file
#entries : list of Entry
def write_entries(entries, cache_filename = ".cache"):
  o_file = open(cache_filename, 'w')
  for entry in entries:
    o_file.write(str(entry))
  o_file.close()

#TODO :
# - look the file extension
# - look the mime type
def get_file_type(filename):
  for extension, file_type  in files_type.iteritems():
    if (re.match(filename, ".*\." + extension)):
      return (file_type)
  return (9)

def create_entry(filename):
  if (os.path.isdir (filename)):
    new_entry = Entry (hostname, port, filename, filename, 0)
    create_cache(filename)
  else :
    new_entry = Entry (hostname, port, filename, filename,
		       get_file_type (filename))
  return (new_entry)

#Create all object Entry from a pathname,
#and call recursively the function create_cache on all subdirectory
def create_entries(pathname = "./"):
  entries = []
  list_filenames = os.listdir(pathname)
  for filename in list_filenames:
    if not (os.path.islink(filename)):
      if (filename != cache_filename):
	entries.append(create_entry(filename))
    else :
      print (filename + " is a link : not yet supported")
  return (entries)

#Create the .cache of the directory 'pathname'
def create_cache(pathname = "./"):
  entries = create_entries(pathname)
  write_entries(entries, pathname + "/" + ".cache")

if __name__ == "__main__":
  create_cache()
