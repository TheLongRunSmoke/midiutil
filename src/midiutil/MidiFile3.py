# -----------------------------------------------------------------------------
# Name:        MidiFile.py
# Purpose:     MIDI file manipulation utilities
#
# Author:      Mark Conway Wirt <emergentmusics) at (gmail . com>
# Author:      Alexander Varakosov <thelongrunsmoke) at (gmail . com>
#
# Created:     2008/04/17
# Fixed:       2017/04/17
# Copyright:   (c) 2009 Mark Conway Wirt
# Copyright:   (c) 2017 thelongrunsmoke
# License:     Please see License.txt for the terms under which this
#              software is distributed.
# -----------------------------------------------------------------------------

import math
import struct
import sys

# TICKS_PER_BEAT is the number of "ticks" (time measurement in the MIDI file) that
# corresponds to one beat. This number is somewhat arbitrary, but should be chosen
# to provide adequate temporal resolution.

TICKS_PER_BEAT = 960

controllerEventTypes = {
    'pan': 0x0a
}


class MIDIEvent:
    """
    The class to contain the MIDI Event (placed on MIDIEventList.
    """

    def __init__(self):
        self.type = 'unknown'
        self.time = 0
        self.ord = 0

    def __lt__(self, other):
        """ Sorting function for events."""
        if self.time < other.time:
            return True
        elif self.time > other.time:
            return False
        else:
            if self.ord < other.ord:
                return True
            elif self.ord > other.ord:
                return False
            else:
                return False

    def __cmp__(self, other):
        """ Sorting function for events."""
        if self.time < other.time:
            return -1
        elif self.time > other.time:
            return 1
        else:
            if self.ord < other.ord:
                return -1
            elif self.ord > other.ord:
                return 1
            else:
                return 0


class GenericEvent:
    """The event class from which specific events are derived
    """

    def __init__(self, time):
        self.time = time
        self.type = 'Unknown'

    def __eq__(self, other):
        """
        Equality operator for Generic Events and derived classes.
        
        In the processing of the event list, we have need to remove duplicates. To do this
        we rely on the fact that the classes are hashable, and must therefore have an 
        equality operator (__hash__() and __eq__() must both be defined).
        
        This is the most embarrassing portion of the code, and anyone who knows about OO
        programming would find this almost unbelievable. Here we have a base class that
        knows specifics about derived classes, thus breaking the very spirit of 
        OO programming.
        
        I suppose I should go back and restructure the code, perhaps removing the derived
        classes altogether. At some point perhaps I will.
        """
        if self.time != other.time or self.type != other.type:
            return False

        # What follows is code that encodes the concept of equality for each derived 
        # class. Believe it f you dare.

        if self.type == 'note':
            if self.pitch != other.pitch or self.channel != other.channel:
                return False
        if self.type == 'tempo':
            if self.tempo != other.tempo:
                return False
        if self.type == 'programChange':
            if self.programNumber != other.programNumber or self.channel != other.channel:
                return False
        if self.type == 'trackName':
            if self.trackName != other.trackName:
                return False
        if self.type == 'controllerEvent':
            if self.parameter1 != other.parameter1 or \
                            self.channel != other.channel or \
                            self.eventType != other.eventType:
                return False

        if self.type == 'SysEx':
            if self.manID != other.manID:
                return False

        if self.type == 'UniversalSysEx':
            if self.code != other.code or \
                            self.subcode != other.subcode or \
                            self.sysExChannel != other.sysExChannel:
                return False

        return True

    def __hash__(self):
        """
        Return a hash code for the object.
        
        This is needed for the removal of duplicate objects from the event list. The only
        real requirement for the algorithm is that the hash of equal objects must be equal.
        There is probably great opportunity for improvements in the hashing function.
        """
        # Robert Jenkin's 32 bit hash.
        a = int(self.time)
        a = (a + 0x7ed55d16) + (a << 12)
        a = (a ^ 0xc761c23c) ^ (a >> 19)
        a = (a + 0x165667b1) + (a << 5)
        a = (a + 0xd3a2646c) ^ (a << 9)
        a = (a + 0xfd7046c5) + (a << 3)
        a = (a ^ 0xb55a4f09) ^ (a >> 16)
        return a


class MIDITrack:
    """A class that encapsulates a MIDI track
    """

    # Nested class definitions.

    class Note(GenericEvent):
        """A class that encapsulates a note
        """

        def __init__(self, channel, pitch, time, duration, volume, annotation=None):

            GenericEvent.__init__(self, time)
            self.pitch = pitch
            self.duration = duration
            self.volume = volume
            self.type = 'note'
            self.channel = channel
            self.annotation = annotation

        def compare(self, other):
            """Compare two notes for equality.
            """
            if self.pitch == other.pitch \
                    and self.time == other.time \
                    and self.duration == other.duration \
                    and self.volume == other.volume \
                    and self.type == other.type \
                    and self.channel == other.channel:
                return True
            else:
                return False

    class Tempo(GenericEvent):
        """A class that encapsulates a tempo meta-event
        """

        def __init__(self, time, tempo):
            GenericEvent.__init__(self, time)
            self.type = 'tempo'
            self.tempo = int(60000000 / tempo)

    class ProgramChange(GenericEvent):
        """A class that encapsulates a program change event.
        """

        def __init__(self, channel, time, program_number):
            GenericEvent.__init__(self, time, )
            self.type = 'programChange'
            self.programNumber = program_number
            self.channel = channel

    class SysExEvent(GenericEvent):
        """A class that encapsulates a System Exclusive  event.
        """

        def __init__(self, time, man_id, payload):
            GenericEvent.__init__(self, time, )
            self.type = 'SysEx'
            self.manID = man_id
            self.payload = payload

    class UniversalSysExEvent(GenericEvent):
        """A class that encapsulates a Universal System Exclusive  event.
        """

        def __init__(self, time, real_time, sys_ex_channel, code, subcode, payload):
            GenericEvent.__init__(self, time, )
            self.type = 'UniversalSysEx'
            self.realTime = real_time
            self.sysExChannel = sys_ex_channel
            self.code = code
            self.subcode = subcode
            self.payload = payload

    class ControllerEvent(GenericEvent):
        """A class that encapsulates a program change event.
        """

        def __init__(self, channel, time, event_type, parameter1, ):
            GenericEvent.__init__(self, time, )
            self.type = 'controllerEvent'
            self.parameter1 = parameter1
            self.channel = channel
            self.eventType = event_type

    class TrackName(GenericEvent):
        """A class that encapsulates a program change event.
        """

        def __init__(self, time, track_name):
            GenericEvent.__init__(self, time, )
            self.type = 'trackName'
            self.trackName = track_name

    def __init__(self, remove_duplicates, deinterleave):
        """Initialize the MIDITrack object.
        """
        self.headerString = struct.pack('cccc', b'M', b'T', b'r', b'k')
        self.dataLength = 0  # Is calculated after the data is in place
        self.MIDIdata = b""
        self.closed = False
        self.eventList = []
        self.MIDIEventList = []
        self.remdep = remove_duplicates
        self.deinterleave = deinterleave

    def add_note_by_number(self, channel, pitch, time, duration, volume, annotation=None):
        """Add a note by chromatic MIDI number
        """
        self.eventList.append(MIDITrack.Note(channel, pitch, time, duration, volume, annotation))

    def add_controller_event(self, channel, time, event_type, parameter1):
        """
        Add a controller event.
        """

        self.eventList.append(MIDITrack.ControllerEvent(channel, time, event_type, parameter1))

    def add_tempo(self, time, tempo):
        """
        Add a tempo change (or set) event.
        """
        self.eventList.append(MIDITrack.Tempo(time, tempo))

    def add_sys_ex(self, time, man_id, payload):
        """
        Add a SysEx event.
        """
        self.eventList.append(MIDITrack.SysExEvent(time, man_id, payload))

    def add_universal_sys_ex(self, time, code, subcode, payload, sys_ex_channel=0x7F, real_time=False):
        """
        Add a Universal SysEx event.
        """
        self.eventList.append(MIDITrack.UniversalSysExEvent(time, real_time, sys_ex_channel, code, subcode, payload))

    def add_program_change(self, channel, time, program):
        """
        Add a program change event.
        """
        self.eventList.append(MIDITrack.ProgramChange(channel, time, program))

    def add_track_name(self, time, track_name):
        """
        Add a track name event.
        """
        self.eventList.append(MIDITrack.TrackName(time, track_name))

    def change_note_tuning(self, tunings, sys_ex_channel=0x7F, real_time=False, tuning_program=0):
        """Change the tuning of MIDI notes
        """
        payload = struct.pack('>B', tuning_program)
        payload = payload + struct.pack('>B', len(tunings))
        for (noteNumber, frequency) in tunings:
            payload = payload + struct.pack('>B', noteNumber)
            midi_frequency = frequency_transform(frequency)
            for byte in midi_frequency:
                payload = payload + struct.pack('>B', byte)

        self.eventList.append(MIDITrack.UniversalSysExEvent(0, real_time, sys_ex_channel, 8, 2, payload))

    def process_event_list(self):
        """
        Process the event list, creating a MIDIEventList
        
        For each item in the event list, one or more events in the MIDIEvent
        list are created.
        """

        # Loop over all items in the eventList

        for thing in self.eventList:
            if thing.type == 'note':
                event = MIDIEvent()
                event.type = "NoteOn"
                event.time = thing.time * TICKS_PER_BEAT
                event.pitch = thing.pitch
                event.volume = thing.volume
                event.channel = thing.channel
                event.ord = 3
                self.MIDIEventList.append(event)

                event = MIDIEvent()
                event.type = "NoteOff"
                event.time = (thing.time + thing.duration) * TICKS_PER_BEAT
                event.pitch = thing.pitch
                event.volume = thing.volume
                event.channel = thing.channel
                event.ord = 2
                self.MIDIEventList.append(event)

            elif thing.type == 'tempo':
                event = MIDIEvent()
                event.type = "Tempo"
                event.time = thing.time * TICKS_PER_BEAT
                event.tempo = thing.tempo
                event.ord = 3
                self.MIDIEventList.append(event)

            elif thing.type == 'programChange':
                event = MIDIEvent()
                event.type = "ProgramChange"
                event.time = thing.time * TICKS_PER_BEAT
                event.programNumber = thing.programNumber
                event.channel = thing.channel
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'trackName':
                event = MIDIEvent()
                event.type = "TrackName"
                event.time = thing.time * TICKS_PER_BEAT
                event.trackName = thing.trackName
                event.ord = 0
                self.MIDIEventList.append(event)

            elif thing.type == 'controllerEvent':
                event = MIDIEvent()
                event.type = "ControllerEvent"
                event.time = thing.time * TICKS_PER_BEAT
                event.eventType = thing.eventType
                event.channel = thing.channel
                event.paramerter1 = thing.parameter1
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'SysEx':
                event = MIDIEvent()
                event.type = "SysEx"
                event.time = thing.time * TICKS_PER_BEAT
                event.manID = thing.manID
                event.payload = thing.payload
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'UniversalSysEx':
                event = MIDIEvent()
                event.type = "UniversalSysEx"
                event.realTime = thing.realTime
                event.sysExChannel = thing.sysExChannel
                event.time = thing.time * TICKS_PER_BEAT
                event.code = thing.code
                event.subcode = thing.subcode
                event.payload = thing.payload
                event.ord = 1
                self.MIDIEventList.append(event)

            else:
                print("Error in MIDITrack: Unknown event type")
                sys.exit(2)

        # Assumptions in the code expect the list to be time-sorted.
        # self.MIDIEventList.sort(lambda x, y: x.time - y.time)

        self.MIDIEventList.sort(key=lambda x: x.time)

        if self.deinterleave:
            self.de_interleave_notes()

    def remove_duplicates(self):
        """
        Remove duplicates from the eventList.
        
        This function will remove duplicates from the eventList. This is necessary
        because we the MIDI event stream can become confused otherwise.
        """

        # For this algorithm to work, the events in the eventList must be hashable 
        # (that is, they must have a __hash__() and __eq__() function defined).

        temp_dict = {}
        for item in self.eventList:
            temp_dict[item] = 1

        self.eventList = list(temp_dict.keys())

        # Sort on type, them on time. Necessary because keys() has no requirement to return
        # things in any order.

        self.eventList.sort(key=lambda x: x.type)
        self.eventList.sort(key=lambda x: x.time)  # A bit of a hack.

    def close_track(self):
        """Called to close a track before writing
        
        This function should be called to "close a track," that is to
        prepare the actual data stream for writing. Duplicate events are
        removed from the eventList, and the MIDIEventList is created.
        
        Called by the parent MIDIFile object.
        """

        if self.closed:
            return
        self.closed = True

        if self.remdep:
            self.remove_duplicates()

        self.process_event_list()

    def write_midi_stream(self):
        """
        Write the meta data and note data to the packed MIDI stream.
        """

        # Process the events in the eventList

        self.write_events_to_stream()

        # Write MIDI close event.

        self.MIDIdata = self.MIDIdata + struct.pack('BBBB', 0x00, 0xFF, 0x2F, 0x00)

        # Calculate the entire length of the data and write to the header

        self.dataLength = struct.pack('>L', len(self.MIDIdata))

    def write_events_to_stream(self):
        """
        Write the events in MIDIEvents to the MIDI stream.
        """
        precise_time = 0.0  # Actual time of event, ignoring round-off
        actual_time = 0.0  # Time as written to midi stream, include round-off
        for event in self.MIDIEventList:

            precise_time = precise_time + event.time

            # Convert the time to variable length and back, to see how much
            # error is introduced

            test_buffer = bytes()
            var_time = write_var_length(event.time)
            for timeByte in var_time:
                test_buffer = test_buffer + struct.pack('>B', timeByte)
            (roundedVal, discard) = read_var_length(0, test_buffer)
            rounded_time = actual_time + roundedVal
            # print "Rounded, Precise: %15.10f %15.10f" % (rounded_time, precise_time)

            # Calculate the delta between the two and apply it to the event time.

            delta = precise_time - rounded_time
            event.time = event.time + delta

            # Now update the actual_time value, using the updated event time.

            test_buffer = bytes()
            var_time = write_var_length(event.time)
            for timeByte in var_time:
                test_buffer = test_buffer + struct.pack('>B', timeByte)
            (roundedVal, discard) = read_var_length(0, test_buffer)
            actual_time = actual_time + roundedVal

        for event in self.MIDIEventList:
            if event.type == "NoteOn":
                code = 0x9 << 4 | event.channel
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.pitch)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.volume)
            elif event.type == "NoteOff":
                code = 0x8 << 4 | event.channel
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.pitch)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.volume)
            elif event.type == "Tempo":
                code = 0xFF
                subcode = 0x51
                fourbite = struct.pack('>L', event.tempo)
                threebite = fourbite[1:4]  # Just discard the MSB
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', subcode)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', 0x03)  # Data length: 3
                self.MIDIdata = self.MIDIdata + threebite
            elif event.type == 'ProgramChange':
                code = 0xC << 4 | event.channel
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.programNumber)
            elif event.type == 'TrackName':
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('B', 0xFF)  # Meta-event
                self.MIDIdata = self.MIDIdata + struct.pack('B', 0X03)  # Event Type
                data_length = len(event.trackName)
                data_lenght_var = write_var_length(data_length)
                for i in range(0, len(data_lenght_var)):
                    self.MIDIdata = self.MIDIdata + struct.pack("b", data_lenght_var[i])
                self.MIDIdata = self.MIDIdata + event.trackName.encode()
            elif event.type == "ControllerEvent":
                code = 0xB << 4 | event.channel
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.eventType)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.paramerter1)
            elif event.type == "SysEx":
                code = 0xF0
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)

                payload_length = write_var_length(len(event.payload) + 2)
                for lenByte in payload_length:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', lenByte)

                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.manID)
                self.MIDIdata = self.MIDIdata + event.payload
                self.MIDIdata = self.MIDIdata + struct.pack('>B', 0xF7)
            elif event.type == "UniversalSysEx":
                code = 0xF0
                var_time = write_var_length(event.time)
                for timeByte in var_time:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', timeByte)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', code)

                # Do we need to add a length?
                payload_length = write_var_length(len(event.payload) + 5)
                for lenByte in payload_length:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', lenByte)

                if event.realTime:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', 0x7F)
                else:
                    self.MIDIdata = self.MIDIdata + struct.pack('>B', 0x7E)

                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.sysExChannel)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.code)
                self.MIDIdata = self.MIDIdata + struct.pack('>B', event.subcode)
                self.MIDIdata = self.MIDIdata + event.payload
                self.MIDIdata = self.MIDIdata + struct.pack('>B', 0xF7)

    def de_interleave_notes(self):
        """Correct Interleaved notes.
        
        Because we are writing multiple notes in no particular order, we
        can have notes which are interleaved with respect to their start
        and stop times. This method will correct that. It expects that the
        MIDIEventList has been time-ordered.
        """

        temp_event_list = []
        stack = {}

        for event in self.MIDIEventList:

            if event.type == 'NoteOn':
                if str(event.pitch) + str(event.channel) in stack:
                    stack[str(event.pitch) + str(event.channel)].append(event.time)
                else:
                    stack[str(event.pitch) + str(event.channel)] = [event.time]
                temp_event_list.append(event)
            elif event.type == 'NoteOff':
                if len(stack[str(event.pitch) + str(event.channel)]) > 1:
                    event.time = stack[str(event.pitch) + str(event.channel)].pop()
                    temp_event_list.append(event)
                else:
                    stack[str(event.pitch) + str(event.channel)].pop()
                    temp_event_list.append(event)
            else:
                temp_event_list.append(event)

        self.MIDIEventList = temp_event_list

        # A little trickery here. We want to make sure that NoteOff events appear 
        # before NoteOn events, so we'll do two sorts -- on on type, one on time. 
        # This may have to be revisited, as it makes assumptions about how 
        # the internal sort works, and is in essence creating a sort on a primary 
        # and secondary key.

        self.MIDIEventList.sort(key=lambda x: x.type)
        self.MIDIEventList.sort(key=lambda x: x.time)

    def adjust_time(self, origin):
        """
        Adjust Times to be relative, and zero-origin
        """

        if len(self.MIDIEventList) == 0:
            return
        temp_event_list = []

        running_time = 0

        for event in self.MIDIEventList:
            adjusted_time = event.time - origin
            event.time = adjusted_time - running_time
            running_time = adjusted_time
            temp_event_list.append(event)

        self.MIDIEventList = temp_event_list

    def write_track(self, file_handle):
        """
        Write track to disk.
        """

        if not self.closed:
            self.close_track()

        file_handle.write(self.headerString)
        file_handle.write(self.dataLength)
        file_handle.write(self.MIDIdata)


class MIDIHeader:
    """
    Class to encapsulate the MIDI header structure.
    
    This class encapsulates a MIDI header structure. It isn't used for much,
    but it will create the appropriately packed identifier string that all
    MIDI files should contain. It is used by the MIDIFile class to create a
    complete and well formed MIDI pattern.
    
    """

    def __init__(self, num_tracks):
        """ Initialize the data structures
        """
        self.headerString = struct.pack('cccc', b'M', b'T', b'h', b'd')
        self.headerSize = struct.pack('>L', 6)
        # Format 1 = multi-track file
        self.format = struct.pack('>H', 1)
        self.numTracks = struct.pack('>H', num_tracks)
        self.ticksPerBeat = struct.pack('>H', TICKS_PER_BEAT)

    def write_file(self, file_handle):
        file_handle.write(self.headerString)
        file_handle.write(self.headerSize)
        file_handle.write(self.format)
        file_handle.write(self.numTracks)
        file_handle.write(self.ticksPerBeat)


class MIDIFile:
    """Class that represents a full, well-formed MIDI pattern.
    
    This is a container object that contains a header, one or more tracks,
    and the data associated with a proper and well-formed MIDI pattern.
    
    Calling:
    
        MyMIDI = MidiFile(tracks, removeDuplicates=True,  deinterleave=True)
        
        normally
        
        MyMIDI = MidiFile(tracks)
        
    Arguments:
    
        tracks: The number of tracks this object contains
            
        removeDuplicates: If true (the default), the software will remove duplicate
        events which have been added. For example, two notes at the same channel,
        time, pitch, and duration would be considered duplicate.
        
        deinterleave: If True (the default), overlapping notes (same pitch, same
        channel) will be modified so that they do not overlap. Otherwise the sequencing
        software will need to figure out how to interpret NoteOff events upon playback.
    """

    def __init__(self, num_tracks, remove_duplicates=True, deinterleave=True):
        """
        Initialize the class
        """
        self.header = MIDIHeader(num_tracks)

        self.tracks = list()
        self.numTracks = num_tracks
        self.closed = False

        for i in range(0, num_tracks):
            self.tracks.append(MIDITrack(remove_duplicates, deinterleave))

    # Public Functions. These (for the most part) wrap the MIDITrack functions, where most
    # Processing takes place.

    def add_note(self, track, channel, pitch, time, duration, volume, annotation=None):
        """
        Add notes to the MIDIFile object
        
        Use:
            MyMIDI.addNotes(track,channel,pitch,time, duration, volume)
            
        Arguments:
            track: The track to which the note is added.
            channel: the MIDI channel to assign to the note. [Integer, 0-15]
            pitch: the MIDI pitch number [Integer, 0-127].
            time: the time (in beats) at which the note sounds [Float].
            duration: the duration of the note (in beats) [Float].
            volume: the volume (velocity) of the note. [Integer, 0-127].
            annotation: 
        """
        self.tracks[track].addNoteByNumber(channel, pitch, time, duration, volume, annotation)

    def add_track_name(self, track, time, track_name):
        """
        Add a track name to a MIDI track.
        
        Use:
            MyMIDI.addTrackName(track,time,trackName)
            
        Argument:
            track: The track to which the name is added. [Integer, 0-127].
            time: The time at which the track name is added, in beats [Float].
            trackName: The track name. [String].
        """
        self.tracks[track].addTrackName(time, track_name)

    def add_tempo(self, track, time, tempo):
        """
        Add a tempo event.
        
        Use:
            MyMIDI.addTempo(track, time, tempo)
            
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            tempo: The tempo, in Beats per Minute. [Integer]
        """
        self.tracks[track].addTempo(time, tempo)

    def add_program_change(self, track, channel, time, program):
        """
        Add a MIDI program change event.
        
        Use:
            MyMIDI.addProgramChange(track,channel, time, program)
            
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            channel: The channel the event is assigned to. [Integer, 0-15].
            time: The time at which the event is added, in beats. [Float].
            program: the program number. [Integer, 0-127].
        """
        self.tracks[track].addProgramChange(channel, time, program)

    def add_controller_event(self, track, channel, time, event_type, parameter):
        """
        Add a MIDI controller event.
        
        Use:
            MyMIDI.addControllerEvent(track, channel, time, eventType, parameter1)
            
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            channel: The channel the event is assigned to. [Integer, 0-15].
            time: The time at which the event is added, in beats. [Float].
            event_type: the controller event type.
            parameter: The event's parameter. The meaning of which varies by event type.
        """
        self.tracks[track].addControllerEvent(channel, time, event_type, parameter)

    def change_note_tuning(self, track, tunings, sys_ex_channel=0x7F, real_time=False, tuning_program=0):
        """
        Change a note's tuning using SysEx change tuning program.
            
        Use:
            MyMIDI.changeNoteTuning(track,[tunings],realTime=False, tuningProgram=0)
            
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            tunings: A list of tuples in the form (pitchNumber, frequency). 
                     [[(Integer,Float]]
            real_time: Boolean which sets the real-time flag. Defaults to false.
            sys_ex_channel: do note use (see below).
            tuning_program: Tuning program to assign. Defaults to zero. [Integer, 0-127]
            
        In general the sysExChannel should not be changed (parameter will be depreciated).
        
        Also note that many software packages and hardware packages do not implement
        this standard!
        """
        self.tracks[track].changeNoteTuning(tunings, sys_ex_channel, real_time, tuning_program)

    def write_file(self, file_handle):
        """
        Write the MIDI File.
        
        Use:
            MyMIDI.writeFile(file_handle)
        
        Arguments:
            file_handle: a file handle that has been opened for binary writing.
        """

        self.header.write_file(file_handle)

        # Close the tracks and have them create the MIDI event data structures.
        self.close()

        # Write the MIDI Events to file.
        for i in range(0, self.numTracks):
            self.tracks[i].write_track(file_handle)

    def add_sys_ex(self, track, time, man_id, payload):
        """
        Add a SysEx event
        
        Use:
            MyMIDI.addSysEx(track,time,ID,payload)
            
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            ID: The SysEx ID number
            payload: the event payload.
            
        Note: This is a low-level MIDI function, so care must be used in
        constructing the payload. It is recommended that higher-level helper
        functions be written to wrap this function and construct the payload if
        a developer finds him or herself using the function heavily.
        """
        self.tracks[track].addSysEx(time, man_id, payload)

    def add_universal_sys_ex(self, track, time, code, subcode, payload, sys_ex_channel=0x7F, real_time=False):
        """
        Add a Universal SysEx event.
        
        Use:
            MyMIDI.addUniversalSysEx(track, time, code, subcode, payload,\
                                      sysExChannel=0x7f, real_time=False)
                    
        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            code: The even code. [Integer]
            subcode: The event sub-code [Integer]
            payload: The event payload. [Binary string]
            sys_ex_channel: The SysEx channel.
            real_time: Sets the real-time flag. Defaults to zero.
        
        Note: This is a low-level MIDI function, so care must be used in
        constructing the payload. It is recommended that higher-level helper
        functions be written to wrap this function and construct the payload if
        a developer finds him or herself using the function heavily. As an example
        of such a helper function, see the changeNoteTuning function, both here and
        """

        self.tracks[track].addUniversalSysEx(time, code, subcode, payload, sys_ex_channel, real_time)

    def shift_tracks(self, offset=0):
        """Shift tracks to be zero-origined, or origined at offset.
        
        Note that the shifting of the time in the tracks uses the MIDIEventList -- in other
        words it is assumed to be called in the stage where the MIDIEventList has been
        created. This function, however, it meant to operate on the eventList itself.
        """
        origin = 1000000  # A little silly, but we'll assume big enough

        for track in self.tracks:
            if len(track.eventList) > 0:
                for event in track.eventList:
                    if event.time < origin:
                        origin = event.time

        for track in self.tracks:
            temp_event_list = []
            # runningTime = 0

            for event in track.eventList:
                adjusted_time = event.time - origin
                # event.time = adjusted_time - runningTime + offset
                event.time = adjusted_time + offset
                # runningTime = adjusted_time
                temp_event_list.append(event)

            track.eventList = temp_event_list

    # End Public Functions ########################

    def close(self):
        """Close the MIDIFile for further writing.
        
        To close the File for events, we must close the tracks, adjust the time to be
        zero-origined, and have the tracks write to their MIDI Stream data structure.
        """

        if self.closed:
            return

        for i in range(0, self.numTracks):
            self.tracks[i].close_track()
            # We want things like program changes to come before notes when they are at the
            # same time, so we sort the MIDI events by their ordinality
            self.tracks[i].MIDIEventList.sort()

        origin = self.find_origin()

        for i in range(0, self.numTracks):
            self.tracks[i].adjust_time(origin)
            self.tracks[i].write_midi_stream()

        self.closed = True

    def find_origin(self):
        """Find the earliest time in the file's tracks.append.
        """
        origin = 1000000  # A little silly, but we'll assume big enough

        # Note: This code assumes that the MIDIEventList has been sorted, so this should be insured
        # before it is called. It is probably a poor design to do this.
        # TODO: -- Consider making this less efficient but more robust by not assuming the list to be sorted.

        for track in self.tracks:
            if len(track.MIDIEventList) > 0:
                if track.MIDIEventList[0].time < origin:
                    origin = track.MIDIEventList[0].time

        return origin


def write_var_length(i):
    """Accept an input, and write a MIDI-compatible variable length stream
    
    The MIDI format is a little strange, and makes use of so-called variable
    length quantities. These quantities are a stream of bytes. If the most
    significant bit is 1, then more bytes follow. If it is zero, then the
    byte in question is the last in the stream
    """
    input_length = int(i + 0.5)
    output = [0, 0, 0, 0]
    revers = [0, 0, 0, 0]
    count = 0
    result = input_length & 0x7F
    output[count] = result
    count = count + 1
    input_length = input_length >> 7
    while input_length > 0:
        result = input_length & 0x7F
        result = result | 0x80
        output[count] = result
        count = count + 1
        input_length = input_length >> 7

    revers[0] = output[3]
    revers[1] = output[2]
    revers[2] = output[1]
    revers[3] = output[0]
    return revers[4 - count:4]


def read_var_length(offset, buffer):
    """A function to read a MIDI variable length variable.

    It returns a tuple of the value read and the number of bytes processed. The
    input is an offset into the buffer, and the buffer itself.
    """
    toffset = offset
    output = 0
    bytes_read = 0
    while True:
        output = output << 7
        byte = struct.unpack_from('>B', buffer, toffset)[0]
        toffset = toffset + 1
        bytes_read = bytes_read + 1
        output = output + (byte & 127)
        if (byte & 128) == 0:
            break
    return output, bytes_read


def frequency_transform(freq):
    """Returns a three-byte transform of a frequencyTransform
    """
    resolution = 16384
    freq = float(freq)
    dollars = 69 + 12 * math.log(freq / (float(440)), 2)
    first_byte = int(dollars)
    lower_freq = 440 * pow(2.0, ((float(first_byte) - 69.0) / 12.0))
    if freq != lower_freq:
        cent_dif = 1200 * math.log((freq / lower_freq), 2)
    else:
        cent_dif = 0
    cents = round(cent_dif / 100 * resolution)  # round?
    second_byte = min([int(cents) >> 7, 0x7F])
    third_byte = cents - (second_byte << 7)
    third_byte = min([third_byte, 0x7f])
    if third_byte == 0x7f and second_byte == 0x7F and first_byte == 0x7F:
        third_byte = 0x7e

    third_byte = int(third_byte)
    return [first_byte, second_byte, third_byte]


def return_frequency(freq_bytes):
    """The reverse of frequencyTransform. Given a byte stream, return a frequency.
    """
    resolution = 16384.0
    base_frequency = 440 * pow(2.0, (float(freq_bytes[0] - 69.0) / 12.0))
    frac = (float((int(freq_bytes[1]) << 7) + int(freq_bytes[2])) * 100.0) / resolution
    frequency = base_frequency * pow(2.0, frac / 1200.0)
    return frequency
