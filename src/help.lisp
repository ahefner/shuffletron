(in-package :shuffletron)

(defun print-usage-message ()
  (format t "Usage: shuffletron [options]
   --profile name   Use alternate library/profile
   --list           List available profiles
   --version        Display program version
   --help           Display this message
"))

(defun print-help ()
  (format t "
Shuffletron  is a text-mode  music player  oriented around  search and
tagging. Its principle of operation  is simple: search for songs, then
play them. Searches are performed by typing a / followed by the search
string:

library> /chromeo
fa      0 Chromeo, She's In Control, 10: Ah Oui Comme Ca
fa      1 \"........................\"  1: My And My Man
fa      2 \"........................\"  2: Needy Girl
fa      3 \"........................\"  3: You're So Gangsta
fa      4 \"........................\"  4: Woman Friend
fa      5 \"........................\"  7: Since You Were Gone
fa      6 \"........................\"  8: Way Too Much
fa      7 \"........................\"  9: Mercury Tears
f b     8 DJ Mehdi, I Am Somebody featuring Chromeo,  2: I Am Somebody (Montreal Version)
9 matches>

If ID3 tags are present, songs are presented in the following form:

   Artist, [Album,] [Track:] Title

Although  not shown  above, artist  names are  color coded  red, album
names yellow,  and song  titles white.  In  successive lines  with the
same artist or  album and artist, the redundant  fields are elided. If
ID3 information on the artist and title is not available, the filename
is printed instead.

In the  leftmost column is some  subset of the letters  'f', 'a', 'b',
and 't'.   These indicate  which fields matched  the query  string, as
follows:

   f: Filename
   a: Artist
   b: Album
   t: Title

Following this is a column of numbers, starting from zero. These allow
you to choose  songs to play as comma (or  space) delimited numbers or
ranges of numbers. If the song  is already in the queue, the number is
highlighted in bold white text. Here, I decide to play song 8 then 0-3
by entering this at the prompt:

9 matches> 8, 0-3

The currently  playing song is  interrupted, and the chosen  songs are
added to  the head of the playback  queue. To see the  contents of the
queue, use the 'queue' command:

9 matches> queue
     (0)  Chromeo, She's In Control, 10: Ah Oui Comme Ca
     (1)  \"........................\"  1: My And My Man
     (2)  \"........................\"  2: Needy Girl
     (3)  \"........................\"  3: You're So Gangsta

Notice that  the prompt changed  from \"library>\" to  \"\9 matches>\"
after our  initial search.  Successive  searches refine the  result of
previous searches, and the prompt indicates the number of items you're
currently searching  within. If there  had been more than  50 matches,
they would  not be printed  by default, but  you could use  the 'show'
command at any time to print  them. Also note that the 'queue' command
doesn't disrupt the  current search results (this is  why numbering in
the  queue listing is  surrounded with  parentheses, to  indicate that
entering numbers for playback does not refer to them).

To  add songs  to the  queue  without interrupting  the current  song,
prefix the song list with \"+\" (to append) or \"pre\" (to prepend).

The  queue can be  cleared with  the 'clear'  command, and  the 'skip'
command skips  the current song and  advances to the next  song in the
queue.  The 'next'  command is  similar, but  differs when  looping is
enabled: 'next' retains the current song at the end of the queue so it
will play again, 'skip' does not.

The \"loop\" command toggles looping  mode. In looping mode, songs are
returned to  the end of  the queue when  they finish playing,  or when
they are bypassed using the 'next' command.

When you've  completed a  search, a single  blank line  moves backward
through the search history, eventually returning to the \"library>\"
prompt.

If you've  imported a large  library, the ID3  tags may not  have been
scanned.   In this case,  the program  will suggest  that you  run the
scanid3 command.   Scanning ID3  tags can be  very time  consuming, as
each file must be opened  and read from. Once scanned, ID3 information
is remembered by caching  it in the ~~/.shuffletron/id3-cache file, so
you only need to do this the first time you run the program.  ID3 tags
of new files are scanned  automatically when the program starts unless
there are more than 1,000 new files.

Additional help topics:
   help commands
   help examples
   help alarms

"))

(defun print-commands ()
  (format t "
Command list:

  /[query]       Search library for [query].
  show           Print search matches, highlighting songs in queue.
  back           Undo last search.
  [songs]        Play list of songs.
  all            Play all songs in selection (equivalent to \"0-\")
  +[songs]       Append list of songs to queue.
  pre[songs]     Prepend list of songs to queue.
  random         Play a random song from the current selection.
  random QUERY   Play a random song matching QUERY
  shuffle SONGS  Play songs in random order.

  queue          Print queue contents and current song playing.
  shuffle        Randomize order of songs in queue.
  clear          Clear the queue (current song continues playing)
  loop           Toggle loop mode (loop through songs in queue)
  qdrop          Remove last song from queue
  qdrop RANGES   Remove songs from queue
  qtag TAGS      Apply tags to all songs in queue
  fromqueue      Transfer queue to selection
  toqueue        Replace queue with selection

  now            Print name of song currently playing.
  play           Resume playing
  stop           Stop playing (current song pushed to head of queue)
  pause          Toggle paused/unpaused.
  skip           Skip currently playing song. If looping is enabled, this
                 song won't played again.
  next           Advance to next song. If looping is enabled, the current
                 song will be enqueued. 
  repeat N       Add N repetitions of currently playing song to head of queue.
  seek TIME      Seek to time (in [h:]m:ss format, or a number in seconds)
  seek +TIME     Seek forward
  seek -TIME     Seek backward
  startat TIME   Always start playback at a given time (to skip long intros)

  tag            List tags of currently playing song.
  tag TAGS       Add one or more textual tags to the current song.
  untag TAGS     Remove the given tags from the currently playing song.
  tagged TAGS    Search for files having any of specified tags.
  tags           List all tags (and # occurrences) within current query.
  killtag TAGS   Remove all occurances of the given tags
  tagall TAGS    Apply tags to all selected songs
  untagall TAGS  Remove given tags from all selected songs

  time           Print current time
  alarm          Set alarm (see \"help alarms\")

  scanid3        Scan new files for ID3 tags
  prescan        Toggle file prescanning (useful if file IO is slow)
  exit           Exit the program.

  help [topic]   Help
"))

(defun print-examples ()
  (format t "
How to find and play a song, then return to library mode:

library> /vampire sushi
f  t    0 Old Time Relijun, Witchcraft Rebellion,  3: Vampire Sushi

1 matches> 0
1 matches>
library>

How to refine search results:

library> /beatles
223 matches> /window
fa t    0 Beatles, The, Abbey Road, 13: She Came In Through The Bathroom Window
1 matches>

How to play your entire library in shuffle mode:

First, ensure you are at the \"library\" prompt. If the prompt reads
differently, hit enter until it reappears.

  library> shuffle 0-

"))

(defun print-alarm-help ()
  (format t "
The \"alarm\" command provides an  alarm clock feature which will play
music when  the scheduled  wakeup time is  reached. There is  a single
wakeup time, and  when it is reached the wakeup  time is cleared. When
the alarm is triggered, the music player will do one of the following:

  1) If playback is paused, unpause the player.
  2) Otherwise, prepend ten random songs to queue and play them.

With  no argument,  the \"alarm\"  command prints  the  current wakeup
time. An argument  to the command specifies the  wakeup time. This can
be done in a variety of formats:

  alarm at 7:45 am      # \"at\" is optional and doesn't change the meaning
  alarm 7:45 am
  alarm 9 pm
  alarm 7               # If AM/PM not specified, assumes AM
  alarm in 5 minutes    # Relative alarm times, in minutes or hours
  alarm in 10m          # minutes, minute, mins, min, , m are synonyms
  alarm in 7 hours      # hours, hour, hr, h are synonyms
  alarm in 8h
  alarm in 7:29         # h:mm format - seven hours, twenty-nine minutes
  alarm reset           # off/never/delete/disable/cancel/clear/reset

If the player is already playing when the alarm goes off, the song
already playing will be interrupted by the next song in the queue.
"))

;;; TODO: Somewhere, the help should mention the 'ignore' tag, but
;;; there's currently no prose dedicated to tagging where it would
;;; make sense to mention it.
