purescript-midi-player
======================


This is a purescript-pux module that is a player for [MIDI recordings](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs).

It is a specialisation for playing general MIDI of the basic [soundfont-player](https://github.com/newlandsvalley/purescript-soundfont-player). As with the base player, it is designed as a semi-autonomous Pux module, delegating all play messages to the base player. However, it intercepts the very first Play message in order to convert a MIDI recording to the _Melody_ format that is required. This means the Melody is only created if it is actually needed. 

The calling progran can use __SetRecording__ to re-initialise the player with a MIDI recording.

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

| Module                     | Reference                                                              |
| -------------------------- | ---------------------------------------------------------------------- |
| purescript-midi            | https://github.com/newlandsvalley/purescript-midi.git                  |
| purescript-soundfonts      | https://github.com/newlandsvalley/purescript-polyphonic-soundfonts.git |
| purescript-soundont_player | https://github.com/newlandsvalley/purescript-soundfont_player.git      |
| purescript-pux             | 9.1.0                                                                  |
