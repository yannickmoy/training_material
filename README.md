![Training Material Build](https://github.com/adacore/training_material/workflows/CI/badge.svg)

# Overview

This repository is a collection of courses for teaching Ada (or SPARK) plus some
support files to convert the courses into various presentation formats. The
courses are written in ReStructured Text (RST) format, making them easy to
convert into word processing documents, slides, or any other formats as needed.
The most common conversion tool is Pandoc, and there is a folder that contains
some artifacts that can be used with the tool. (Artifacts for other tools
may be added in the future). Each of the folders at this level are described
below.

# Download

You can download the PDF by going to [the actions page](http://github.com/adacore/training_material/actions).

**NB: You need to be connected to GitHub.**

Click on the build you want. You probably want to take one from the **master** branch.

![Click build](images/github/artifacts_1.png)

Scroll at the bottom

![Bottom scroll](images/github/artifacts_2.png)

![Artifact examples](images/github/artifacts_3.png)

## Courses

Each folder contains a collection of RST files that make up modules within
the course. The filenames for the modules should use a numbering scheme so
users can quickly understand the typical order of presentation. 

### Module Labs

If the course is designed with laboratory exercises / examples, these RST
files should be in a subfolder, where the name of the file is the same as the
course name with ".lab" at the end. The benefit of keeping labs separate from
their enclosing courses is to be able to generate "Lab Manuals" as separate
course materials. You can also create subfolders here to store lab answers
and supporting files.

### Using the Labs

Some labs will work right away, but for `solar_system` labs you will need to install
the SDL library and wrappers first. See [`extern/README.md`](extern/README.md).

## Images

All images used in any of the courses should be stored here. This allows
sharing of images across courses without duplication. The filename should be
as descriptive as possible.

## Pandoc

This folder contains artifacts useful in running Pandoc to generate any output
format.

## Support Files

This folder contains files that may be useful for multiple presentation formats.

## Contrib

This folder contains scripts that are used for generating the docs or packages.

## Extern

This folder contains sources that are used in support of labs (as frameworks), and are
standalone.
