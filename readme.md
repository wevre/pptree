# pptree

A utility for creating a tree diagram (_à la_ unix `tree` command) for arbitrary
lists of filenames. By "arbitrary", I mean that, unlike the `tree` command, you
don't have to list out all the files in a directory, you can print a tree for
the two or three files you care about, they can be from completely different
sub-directories, and they don't even need to be _real_ files, the input just
needs to be strings with separators that _look_ like files.

# How to install

Clone the repository to your computer, change to its directory, and run the
following

```
mkdir classes
clj -e "(compile 'wevre.pptree.main)"
clj -A:uberjar
```

to make an executable jar file in 'target' directory of the project. If you
want, make an alias to that jar file so it can be invoked with `pptree` from the
command line. For example, in `~/.bashrc` place the following line

```
alias pptree='java -jar <path to project>/target/pptree.jar'
```

# How to use

Assuming you created the executable jar file and alias as above, you could then
run the command

```
pptree
~/Documents/Important Files/file2.txt
~/Documents/Important Files/file10.txt
~/Documents/Important Files/file1.txt
^D
```

to get the following output

```
~/Documents/Important Files/
├── file1.txt
├── file2.txt
└── file10.txt
```

Also, try running `pptree` using the contents of the sample test input and
trying out the various command line options, `-h`, `-l`, `-F`, `-f`, `-I`, for
example:

```
pptree -h < resources/test1.txt
```

To come full circle, you could _re-create_ the results of the tree command
(meaning you could get a tree printout of the current directory and recursively
all its sub-directories -- the same thing that the `tree` command already does)
with something like this:

```
find "$PWD" -type f | pptree
```

which of course is completely pointless, except that when I did this in the
project directory, and scrolled through the (very long) output, I was happy to
see that the natural sorting algorithm kicked in. For example, under the
`classes/` directory that Clojure creates, there are many files with a number at
the end of their name, and using
[natural-compare](https://github.com/wevre/natural-compare) they were ordered
properly.

# Why did I create this?

At work I often send file locations to team members, and instead of just a list,
I wanted to be able to send something more visual that captured the hierarchy,
something like an outline, ...something like the `tree` command! But I didn't
want an entire directory like the `tree` command would give me, I wanted it to
work on a list of _arbitrary_ files (see opening paragraph), plus I was on
Windows and I didn't know if such a thing as the `tree` command even existed
(I've since learned that yes it does, but, again, _arbitrary_) so I decided to
write my own. My first version, which I still use at work, was in Python, and
then I later rewrote it in Clojure, which is what you are looking at now.

# How does it work?

Section to be written. Maybe I'll do a blog post or something, though I don't
currently have a blog. There are lots of problem spaces to explore with this
little project: parsing command-line options, recursion, using Clojure zippers
and tree walkers, lots of interesting stuff.

# License

Copyright © 2020 Mike Weaver

Licensed under the terms of the Eclipse Public License 2.0, see
[license.txt](https://github.com/wevre/pptree/blob/master/license.txt).