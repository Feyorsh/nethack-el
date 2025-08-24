# `nethack-el`

What happens when you combine the most advanced, self-documenting, customizable,
extensible real-time display editor, Emacs, with nethack, the most elaborate
role-playing environment ever invented?

You get the most advanced, elaborate, self-documenting, customizable,
extensible, role-playing environment in the world!

All of the Elisp is GPLv2, and the patches are under the modified 3-clause BSD.

![](./images/screenshot.png)

## Features

* Customizable keys
* Customizable colors
* Macros
* Event hooks
* All the beauty that comes with Emacs

## Project Status
I am semi-actively adding new features and fixing bugs as of August 2025.
I mostly work on things that affect how I play NetHack (e.g. my `nethackrc`), so please open an issue/PR if something doesn't work---it's likely I just don't use that feature and never ran into that particular issue.

Some things on my to-do list are upstreaming my Nix derivation for nethack-el/NetHack lisp port into nixpkgs and potentially creating a package on MELPA.
Also, I need to prevent the game from getting into a bad state when sending input to an "idle" window, e.g. opening your inventory and then trying to move your character on the map.

Some things that are _not_ maintained and likely never will be (unless someone opens a PR):
- [nh-bot.el](./nh-bot.el)
- [nethack-tiles.el](./nethack-tiles.el)/[slashem-tiles.el](./slashem-tiles.el) (X11-like interface with XPMs)
- [nethack-nhlaunch.el](./nethack-nhlaunch.el) (use nethack-el on a server)
- [nethack-keys-dvorak.el](./nethack-keys-dvorak.el)

> [!IMPORTANT]
> nethack-el only supports versions 3.6.7 and 3.7.0-WIP of NetHack; older patches are kept in HEAD for convenience but you will likely have difficulty getting them to work.
> Please do not open issues with using nethack-el with versions of NetHack older than 3.6.7.


Previous repo: <https://github.com/be11ng/nethack-el>

Old website: <http://savannah.nongnu.org/projects/nethack-el>

## Build and run

*The following section is nearly copied verbatim from the old INSTALL file.*

These instructions are known to work on \*NIX systems (sorry Windows users).

### Easy installation

* Install the Elisp sources

  * Add the `nethack-el` folder to your Emacs load-path.

  * Make sure you have all of your dependencies installed.  You'll need `make`,
    `gcc`, `bison` or `yacc`, `flex` or `lex`, and the ncurses-dev library for
    your system.

  * Add the following line somewhere inside your `init.el`:

    ```elisp
    (nethack-install)
    ```

    This will download, patch, and build a NetHack lisp patch, and also sets the
    variables for `nethack-el` to detect the executable.

  * Play with `M-x nethack RET`.

### Manual Build

These instructions are known to work on \*NIX systems (sorry Windows users).

* Download either `nethack-366.tgz` from <https://nethack.org>.

  * 331, 340, 341, 342, 343, and SLASH'EM 007e0 and 007e3 are not really
    functional any more due to a major charge to the way statuses are passed
    between the C and Lisp halves of `nethack-el`.

* Untar the package and apply the respective patch

  For example, if you were installing 3.6.6, you would do something like:

  ```
  $ tar xzf nethack-366-src.tgz
  $ cp enh-366.patch NetHack-NetHack-3.6.6_Released
  $ cd NetHack-NetHack-3.6.6_Released
  $ patch -p 1 < enh-366.patch
  ```

* Follow the instructions in `sys/*/Install.*` or `NewInstall.*`

  * This may be as simple as running `sys/unix/setup.sh` to copy the Makefiles.

  * For the 3.6.6 patch, included is a hints file under
    `sys/unix/hints/linux-lisp` based under the default Linux one, which builds
    into wherever the `PREFIX` environment variable points to at runtime.  You
    may want to edit the `linux-lisp` hints file before running something along
    the lines of:

    ```
    $ cd sys/unix
    $ vi hints/linux-lisp
    $ sh setup.sh hints/linux-lisp
    ```

* Compile (from the toplevel)

  * Following the instructions as outlined so far, that would be:

    ```
    $ cd ../..
    $ make all
    $ make install
    ```

* Install the Elisp sources

  * Edit the Makefile in the `nethack-el` directory to set the location of your
    Emacs.

  * Byte compile (not strictly necessary).

    ```
    $ cd .. # Or wherever nethack-el is located
    $ make all
    ```

  * Place the `*.elc` or `*.el` files in your load-path.

  * Add the following lines somewhere inside your `init.el`:

    ```elisp
    (autoload 'nethack "nethack" "Play Nethack." t)
    (setq nethack-program "/PATH/TO/PATCHED/nethack")
    ```

  * Play with `M-x nethack RET`.

## Play NetHack over a network

Because `nethack-el` communicates with nethack over a text stream, that
stream can be either a subprocess or a TCP stream. Follow these steps
to play `nethack-el` over the internet:

* On the server

  Edit the config values `USER_FILE`, `CHROOT_DIR`, `SHED_UID`, and `SHED_GID`
  in `nhlaunch.c`. Compile `nhlaunch.c` (instructions are at the top of the
  file).

  `USER_FILE` is the file that will store the users and passwords

  `CHROOT_DIR` is the root directory that contains a fakeroot installation of
  nethack and any libraries required.

  `SHED_UID` is the UID that `nhlaunch` will change to when loaded.

  `SHED_GID` is the GID that `nhlaunch` with change to when loaded.

* On the client (in Emacs)

  `M-x nethack-connect-to-server RET`

  Type the server and the port. Enjoy! Now everyone can play on the same server
  using their own Emacs.
