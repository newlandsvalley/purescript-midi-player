purescript-midi-player
======================

WORK IN PROGRESS

This is a purescript-pux module that is a player for [MIDI recordings](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs).  It follows the standard TEA/Pux guidelines for an embedded module.  It implements a view of a player widget which operates autonomously from the calling application and responds to stop/pause/start button presses which in turn produce the equivalent stop/pause/start events.  This is achieved by ensuring that the calling program delegates all such player event messages to the player itself within the main event loop.  

The state of the player is initialised whenever the calling program invokes the SetRecording event which tells the player the recording that it should play.

to build
--------
   
   bower install
   ./build.sh
   
   and then navigate to /dist/index.html