Synths
======

is a haskell framework for music programming.

...at least it will be. I hope :)

You can play the raw PCM output with gstreamer:

    <binary> | gst-launch fdsrc fd=0 ! audio/x-raw-int, rate=44100, channels=1,\
    endianness=1234, width=16, depth=16, signed=false ! audioconvert ! autoaudiosink

or with sox:

    <binary> | play -r 44100 -b 16 -c 1 --endian little -t raw -e unsigned-integer -

