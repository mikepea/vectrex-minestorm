vectrex-minestorm
=================

Vectrex Mine Storm game, via
http://roadsidethoughts.com/vectrex/mine-storm.htm, with a Kingswood as09 build
system.

I'm currently learnin 6809 assembler and Vectrex programming, and having the
*commented source code* for Mine Storm appear on the interwebs was an amazing treat.
John Hall has become one of my favourite people :)

Reading the code though suggested that it needed the AVOCET 'ASM09' assembler,
which I couldn't find anywhere. I took it upon myself to port it over to the
Kingwood as09 assembler that the homebrew community seem to prefer, and this is
the result.

*It's alive!!!*

I've taken the liberty of splitting out the code into some digestable chunks,
and will over time be pulling bits of it apart and adding more comments as
I go. I'm using this as a learning exercise, so it's maybe not the best place
to look if you want a stock build of Mine Storm RevC.

If you're interesting in making Mine Storm 3 however, maybe with multiplayer
action, I'm all ears ;)


Building
--------

On Linux this should be as simple as:

    make

On MacOS, you'll need Vagrant - https://www.vagrantup.com/, then:

    vagrant up
    vagrant ssh
    cd /vagrant
    make

Windows users, I'm sure there's enough hints in the Makefile, but I've not used
Windows in an age. Sorry -- pull requests are very welcome :)

Thanks
------

Huge thanks to John Hall for making this source code available, alongside all
the other wonderful things under http://roadsidethoughts.com/vectrex. I dearly
hope I haven't caused offence by making this available via my github, happy to
receive any guidance as to what you'd prefer to see or not see happen to it.

Also many thanks to Frank A. Kingswood for providing the as09 assember in
a form that I'm comfortable including here (license and size-wise). It's also
available from http://www.kingswood-consulting.co.uk/assemblers/. See
as09_142/readme.txt and as09_142/as09.man for more details.
