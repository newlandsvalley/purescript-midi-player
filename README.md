purescript-midi-player
======================


This is a purescript-pux module that is a player for [MIDI recordings](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs) and also for [ABC tunes](https://github.com/newlandsvalley/purescript-abc-parser).

It is a specialisation of the basic [soundfont-player](https://github.com/newlandsvalley/purescript-soundfont-player) for playing tunes in one or other of these formats. As with the base player, it is designed as a semi-autonomous Pux module, delegating all play messages to the base player. However, it intercepts the very first Play message in order to convert a MIDI recording or ABC tune to the _Melody_ format that is required. This means the Melody is only created if it is actually needed. 

The calling program can use __SetRecording__ to re-initialise the player with a MIDI recording or __SetAbc__ to do so with ABC.  The reason that both formats are supported in a single player is largely because the intended use is for ABC which uses MIDI as a transitional format.  However, it would be straightforward to produce a variant that was dedicated to MIDI if this were required.

to build the module
-------------------

   bower install

   pulp build


to build the (MIDI) example
---------------------------

   bower install

   ./buildExample.sh

   and then navigate to /dist/index.html   
   
dependencies
------------

| Module                     | Reference                                                              |
| -------------------------- | ---------------------------------------------------------------------- |
| purescript-midi            | 1.2.0                                                                  |
| purescript-soundfonts      | 2.0.0                                                                  |
| purescript-abc-parser      | https://github.com/newlandsvalley/purescript-abc-parser.git            |
| purescript-soundont_player | https://github.com/newlandsvalley/purescript-soundfont_player.git      |
| purescript-pux             | 9.1.0                                                                  |
