# MIDIUtil #

**Note: Version 0.89 Released 1 December 2013**

MIDIUtil is a pure Python library that allows one to write muti-track Musical Instrument Digital Interface (MIDI) files from within Python programs. It is object-oriented and allows one to create and write these files with a minimum of fuss.

MIDIUtil isn't a full implementation of the MIDI specification. The actual specification is a large, sprawling document which has organically grown over the course of decades. I have selectively implemented some of the more useful and common aspects of the specification. The choices have been somewhat idiosyncratic; I largely implemented what I needed.  When I decided that it could be of use to other people I fleshed it out a bit, but there are still things missing. Regardless, the code is fairly easy to understand and well structured. Additions can be made to the library by anyone with a good working knowledge of the MIDI file format and a good, working knowledge of Python. Documentation for extending the library is provided. If you _don't_ have a good understanding of MIDI (and don't want to acquire one), feel free to request an addition.

You can visit the project's website, where on-line documentation (including a class reference and instructions for updating the library at [EmergentMusics.org](http://www.emergentmusics.org/midiutil)

[**Note to Python 3 users:** An initial port to Python 3 had been made and committed to the development branch of the code (which is available via svn). The source file is called MidiFile3.py. To use it, change your import statement to read:

`from midiutil.MidiFile3 import MIDIFile`

This version will also work with Python 2.6.X, but is incompatible with Python 2.5.X.
This latest version should correct issues with 3.X, where X > 0.]


# Quick Start #

Using the software is easy:

  * The package must be imported into your namespace
  * A MIDIFile object is created
  * Events (notes, tempo-changes, etc.) are added to the object
  * The MIDI file is written to disk.

Detailed documentation is provided in the distribution; what follows is a simple example to get you going quickly. In this example we'll create a one track MIDI File, assign a name and tempo to the track, add a one beat middle-C to the track, and write it to disk.

```
#Import the library

from midiutil.MidiFile import MIDIFile

# Create the MIDIFile Object with 1 track
MyMIDI = MIDIFile(1)

# Tracks are numbered from zero. Times are measured in beats.

track = 0   
time = 0

# Add track name and tempo.
MyMIDI.addTrackName(track,time,"Sample Track")
MyMIDI.addTempo(track,time,120)

# Add a note. addNote expects the following information:
track = 0
channel = 0
pitch = 60
time = 0
duration = 1
volume = 100

# Now add the note.
MyMIDI.addNote(track,channel,pitch,time,duration,volume)

# And write it to disk.
binfile = open("output.mid", 'wb')
MyMIDI.writeFile(binfile)
binfile.close()
```

There are several additional event types that can be added and there are various options available for creating the MIDIFile object, but the above is sufficient to begin using the library and creating note sequences.