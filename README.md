purescript-midi-player
======================


This is a purescript-pux module that is a player for [MIDI recordings](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs) or for [ABC tunes](https://github.com/newlandsvalley/purescript-abc-parser/blob/master/src/Data/Abc.purs).


It follows the standard TEA/Pux guidelines for an embedded module.  It implements a view of a player widget which operates autonomously from the calling application and responds to stop/pause/start button presses which in turn produce the equivalent stop/pause/start events.  This is achieved by ensuring that the calling program delegates all such player event messages to the player itself within the main event loop.  

The player does not attempt to re-render the view after each MIDI message because updates happen too thick and fast and the tune does
not play at tempo.  Instead, phrases of notes are accumulated from the MIDI into a __Melody__ data structure by the HybridPerformance module where each phrase is
of relatively short duration and the view is re-rendered after each phrase.  This gives a reasonable compromise between playing
the tune at the appropriate tempo and allowing the stop/start buttons to be responsive.


The calling progran can use __SetMidi__ to re-initialise the player with a MIDI recording, __SetABC__ to do so with an ABC tune or __SetMelody__ to do so directly with a Melody itself (needing no translation).  However, in the first two cases, the Melody is only created when the 'play' button is depressed for the first time. 

to build the module
-------------------

   bower install

   pulp build


to build the example
--------------------

   bower install

   ./buildExample.sh

   and then navigate to /dist/index.html   
   
dependencies
------------

| Module                  | Reference                                                              |
| ----------------------- | ---------------------------------------------------------------------- |
| purescript-midi         | https://github.com/newlandsvalley/purescript-midi.git                  |
| purescript-soundfonts   | https://github.com/newlandsvalley/purescript-polyphonic-soundfonts.git |
| purescript-abc-parser   | https://github.com/newlandsvalley/purescript-abc-parser.git            |
| purescript-pux          | 9.1.0                                                                  |
