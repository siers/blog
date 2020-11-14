---
title: Adding context to window managers keybinds
---
I currently use 8 workspaces. They're accessible through holding the WM hotkey — `Mod4`, the windows key and one of `qwerasdf`. Why `qwerasdf`? Because these are accessible with one hand and
form a rectangular shape that makes a lot of sense in my head.

Still, 8 workspaces are often too little and although
there's some structure such as the four corners `qraf`,
I have a hard time remembering what's in the inner four spaces.

So how about we address workspaces like real programmers — in a two dimensional array?
I made a tool that would let us do just that!

It lets me address workspaces by two numbers — *primary* and *secondary*, each from 1 to 4.
When you change the secondary number, the script, on the surface of it, doesn't do much interesting.
When you change the primary number, the last selected secondary window is remembered.
The WM does not really know much it though, just about windows of name `/[1-4][1-4]/`.

The script also doesn't change WM's state,
it just translates the changes of numbers into the name of workspace,
only asking about the current workspace's name.
Let me give you an example that I obtained by adding some debug prints to the script:

    select-workspace primary 4
    41
    select-workspace secondary 2
    42
    select-workspace primary 2
    21
    select-workspace primary 4
    42
    select-workspace secondary 1
    41
    select-workspace primary 2
    21
    select-workspace primary 4
    41

To remember the choices made number pairs are stored in `/tmp/.workspaces`.

    2 2
    4 1

For primaries `1234` I use keys `erdf`, for secondaries `1234` I use keys `qwas`. To make sense of that visually, I've drawn this:

    e   |  r
    ____|_____
    q w |  q w
    a s |  a s
        |
    d   |  f
    ____|_____
    q w |  q w
    a s |  a s

So each of `erdf`s have their own `qwas`. That gives me 16 workspaces and a nice way to think about them.
I do have to mention that it took me some time to adjust this functionality,
but after about a month it's more or less second nature.

Since the script is run whenever a keybind requests its presence,
running a couple of them at the same time is very doable if your computer is doing mad swapping,
which is the inevitable result of having too little RAM and 16 workspaces with a window in each one.
And since there's a shared, mutable variable without a lock
it does get a little corrupted from time to time; `rm /tmp/.workspaces` helps here.

This script can be used with [herbstluftwm](http://www.herbstluftwm.org/) or *theoretically* other WMs
that offer an API to change workspaces and reveal the name of one being used.

I call the script *herbstiles*, and source is available on [here](https://github.com/siers/herbstiles), keybinds included.
