# Ada Industrial Control Widget Library (AICWL)

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/aicwl.json)](https://alire.ada.dev/crates/aicwl.html)

### Foreword

This is, in essence, a mirror of Dmitry A. Kazakov's
[Ada Industrial Control Widget Library](http://www.dmitry-kazakov.de/ada/aicwl.htm).

I am not the developer of this library. This repository serves as a mirror of the
original releases of AICWL to ease integration with
[Alire](https://github.com/alire-project/alire).

For documentation refer to the [AICWL website](http://www.dmitry-kazakov.de/ada/aicwl.htm).

Dmitry also already provides pre-packaged binary and source archives for most supported
platforms, if that is all you need, you can download and install these and ignore this
repository.

## Introduction

AICWL is a library for designing high-quality industrial control
widgets for Ada applications. The software is based on
[GtkAda](https://docs.adacore.com/live/wave/gtkada/html/gtkada_rm/index.html),
Ada bindings to [Gtk+](https://www.gtk.org/), and
[cairo](https://www.cairographics.org/manual/index.html).

The key features of the library are:

* Widgets composed of transparent layers drawn by cairo
* Fully scalable graphics
* Support of time controlled refresh policy for real-time and heavy-duty applications
* Caching graphical operations
* Stream I/O support for serialization and deserialization
* Ready-to-use gauge, meter, oscilloscope widgets
* Editor widget for WYSIWYG design of complex dashboards

For further information, visit the
[AICWL website](http://www.dmitry-kazakov.de/ada/aicwl.htm).

### Changelog

Refer to the original software's
[Change log](http://www.dmitry-kazakov.de/ada/aicwl.htm#changes_log).

## Maintainer's note

The Alire crate is packaged rather minimalistic to keep dependencies on
external libraries at a minimum. The crate covers the core functionality of AICWL,
though, so it should be sufficient for most needs.

For example, the original distribution has references to
[Simple Components](http://www.dmitry-kazakov.de/ada/components.htm) which are
not strictly necessary for the core functionality of the library.

Maybe I'll package them one day and add them as dependency to the Alire crate.

Also disabled is the source view widget, which seems to require
libgtksourceview-2, another external dependency. Again, not strictly necessary
to cover the core functionality.

### Rules and Conventions

* The first (and only) rule is that I will not change the original sources (i.e.
  the subdirectory named `sources` which contains everything provided by the
  software's author. Sources are provided as is.
  * I may deviate from that rule if the need arises, but this will then be
    clearly stated in the release notes, and will likely require extra-ordinary
    circumstances.
* Version numbers and associated tage follow those of the original author
  * For tagging and the creation of Alire creates, I do add a patch level,
    though.
    Example:
    AICWL3.24.0 denotes the (unchanged) AICWL3.24 release. If the patch level
    increases that means I repackaged the software, updated the alire manifest
    or otherwise improved upon the release.

### Repository Structure

Whenever a new version is released by the author, I will integrate this version
as is on the `main` in the `sources` directory branch and tag this import as
such (i.e. the latest tag is `aicwl3.24`).

Released crates will branch from the initial tag under a branch names
`releases/version-number` (e.g. `releases/3.24`). Whenever I package a new
version for Alire to be integrated with the Alire index this will happen
under this branch. The released version will be tagged as
rel-version-number-patchlevel (e.g. `rel-3.24-0`).

That means, contrary to most github projects the latest (packaged) version is
not on the `main` branch, but under the appropriate `releases` branch, the
latest copy of the software-as-is remains on the top of the main branch, though.

Considering that the main purpose of this repository is to provide Alire crates,
this shouldn't concern you, if you use Alire, your interaction with this
repository itself should be rather limited.

### Releases

Currently the only and newest release is AICWL 3.24, for obvious reasons I
started the integration from there. For historical purposed most AICWL releases
can be found in the repository, down to version AICWL 3.9, but they are not
packaged for Alire and I doubt they ever will.
