My Emacs configuration
======================

This is a set of config files I used to customize Emacs according to my needs. You can use it loading the file `init.el` in your Emacs init-file or specifying an alternate init-file via command line options: `emacs -q -l /path/to/repo/emacs-config/init.el`

In `/patches` folder you got modifications that cannot be applied via init-files. To apply those patches, go to `/usr/share/emacs/your.version/` and run `patch -p0 -i /path/to/patch/file`. For now there are the following:

1. `server.patch`:
        As far as I looked over the documentation and forums there is no standard way to force `emacsclient` to open an X window for a new frame. Here's a little hack, that ensures that X windows will always be used for `emacsclient` frames if the `--display` option is specified.
