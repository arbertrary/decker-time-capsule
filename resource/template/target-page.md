---
decker [options] [target]
---

Main Targets:
    help            Prints a help document to stdout in Markdown syntax. 
    html            Builds HTML versions of all available documents.
    server          Builds HTML versions of all documents and then watches for document changes. Creates a local server.
    example         Writes a few example files to the current directory. 
    tutorial        Writes extended example files to the current directory
    clean           Recursively removes all generated files from the project directory.
    open            Opens the `index.html` file in a browser
    presentation    Creates a local server without building files.    
    pdf-decks       Builds PDF versions of all slide decks.

Other Targets:
    watch           Builds HTML versions of all documents and then watches for document changes.
    publish         Publishes the generated files to a remote location using `rsync` if the location is specified in the meta data. 
    check           Checks the availability of the external dependencies. Decker still workswithout these, but some functionality will be unavailable.
    index           Builds only the index file
    decks           Builds only the slide decks (ending in `*-deck.md`)
    info            Prints information about directories and target files
    publish-annotations     Publish 
    fast


Options

Decker supports most common `make` options:

    -B, --always-make           Unconditionally make all targets.
    -d[=FILE], --debug[=FILE]   Print lots of debugging information.
    -h, --help                  Print this message and exit.
    -j[=N], --jobs[=N]          Allow N jobs/threads at once [default CPUs].
    -k, --keep-going            Keep going when some targets can't be made.
    -l, --lint                  Perform limited validation after the run.
    --live[=FILE]               List the files that are live [to live.txt].
    --numeric-version           Print just the version number and exit.
    -o FILE, --old-file=FILE, --assume-old=FILE
                                Consider FILE to be very old and don't remake it.
    --old-all                   Don't remake any files.
    -r[=FILE], --report[=FILE], --profile[=FILE]
                                Write out profiling information [to report.html].
    -s, --silent                Don't print anything.
    --sleep                     Sleep for a second before building.
    -q, --quiet                 Don't print much.
    --no-time                   Don't print build time.
    --touch                     Assume targets are clean.
    -V, --verbose, --trace      Print tracing information.
    -v, --version               Print the version number and exit.
    -w, --print-directory       Print the current directory.
    -W FILE, --what-if=FILE, --new-file=FILE, --assume-new=FILE
                                Consider FILE to be infinitely new.
