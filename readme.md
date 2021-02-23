# pptree

A utility for creating a tree diagram (_à la_ POSIX `tree` tool) for arbitrary
lists of filenames. Unlike the `tree` command, you don't have to list out all
the files in a directory, you can just print a tree of the two or three files
you care about.

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

Also, try running `pptree` using the contents of this file as input

```
pptree < resources/test2.txt
```

and checking out the command line options: `-h`, `-l`, `-F`, `-f`, `-I`.

# Why did I create this?

At work I often send file locations to team members, and instead of just a list,
I wanted to be able to send something more visual that captured the hierarchy,
something like an outline, ... something like the `tree` command output. But I
didn't want an entire directory like the `tree` command would give me (plus I
was on Windows and I didn't even know if such a thing as the `tree` command even
existed) so I decided to write my own. My first version, which I still use at
work, was in Python, and then I later rewrote it in Clojure, which is what you
are looking at now.

# How does it work?

Section to be written.

# License

Copyright © Mike Weaver

Licensed under the terms of the Eclipse Public License 2.0, see license.txt.
