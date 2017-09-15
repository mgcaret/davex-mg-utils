# MG's DaveX Utilities

This is a collection utilities for [davex](https://sourceforge.net/p/davex/home/Home/), mostly centered around AppleTalk, but also a few others including fixed or enhanced versions of a few existing davex.

## Utilities

See the source or generated help files for each utility, for further information/options/etc.

### AppleTalk

#### afp.sessions

Displays AFP sessions.

#### afp.userprefix

Print or set the AFP user prefix.

#### at.boot

Boots over Appletalk.

#### at.info

Display some AppleTalk info

#### at.zones

Display AppleTalk zone information.

#### nbp.lookup

Look up NBP names registered on the network.

#### nbp.parse

Parse an NBP name and display the parts.  Mainly used for testing the parsing routine.

#### tardis

Get time from TimeLord server.


### Fixed/Enhanced

#### alias

This is a fixed version of the alias command included in davex 1.30p.

#### deschw

This has been enhanced to identify the CPU and to detect emulation.  If an emulator supporting EMUBYTE is found, displays its name and version if known.

See idemu for more comprehensive emulator detection.

There are also a few other enhancements around SmartPort device info.

### Misc

#### dmem

Displays info on davex "dynamic memory."  Mainly for testing purposes.

#### idemu

Identifies emulators.  If a positive ID on EMUBYTE can be used, uses it.  Otherwise, attempts to use the various emulators' idiosynchracies regarding emulator ROM and I/O space to make the identification.  It does not interact with any slot hardware.

#### iie.card

Display or change the speed of the 65C02 on the Apple //e Card for Macintosh LC.

Optionally, use undocumented features to display the user-selected startup slot.

#### mig.insp

View the contents of the Apple IIc Plus MIG RAM.

## Building

Building the utilities requires a Unix-like environment with GNU Make and the usual set of Unix utilities such as sed, awk, egrep, etc.

A working installtion of cc65 must be present and in the PATH (for ca65, ld65, od65).

The davex source code (link at top) must be present somewhere, and the DAVEX variable in the Makefile should point to it.

In order to build the default target (a ShrinkIt file), nulib2 must be present and in the PATH.

In order to build a disk image, you must have AppleCommander CLI version set up and must modify the Makefile.

The utilities are developed on macOS X.

### Interesting make targets

  * ``all``: build shrinkit file DAVEX.MG.SHK
  * ``disk``: build davex.mg.po
  * ``emulate``: builds disk and attempts to open with Virtual ][.  Set BOOTDSK in Makefile first.

## Developing

Fork it, send pull requests, etc.

I created a custom set of macros and tools for doing davex external commands.  These are in ``davex-mg.inc`` and in the ``utils`` directory.

In particular, the macros along with ``utils/auto_origin.sh`` allow automatically meeting the davex external command loading recommendations, which are "load on a page boundary so that the end of the file loads between $AF00 and $AFFF."

Additionaly, ``utils/gen_help.sh`` is used to extract the help file contents from the source code of each utility.


