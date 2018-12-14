========
hexagons
========

.. image:: https://img.shields.io/circleci/project/github/buhman/hexagons.svg?style=flat-square
   :target: https://circleci.com/gh/buhman/hexagons

overview
--------

hexagons is a networked multiplayer abstract virtual tabletop and semantic tile
system written in `CHICKEN <https://www.call-cc.org/>`_ scheme.

.. image:: https://ptpb.pw/AIVQt_NHRAbCdhqT2r9qXaJDsBj6.gif
   :target: https://ptpb.pw/AIVQt_NHRAbCdhqT2r9qXaJDsBj6.gif

usage
-----

hexagons depends on CHICKEN 4.13.0. `notes
<https://ptpb.pw/ZjBD/sh>`_ on how to do this in OSX.

Install dependencies and compile the hexagons client and server with::

  chicken-install -s -n

Start the client and server::

  ./hexagons
  ./server

default client bindings
-----------------------

The client has three modes; current mode is shown in the top-center of the
screen.

modes and sub-modes
^^^^^^^^^^^^^^^^^^^
================= ==============
cycle mode        ``ctrl-space``
toggle pathable   ``ctrl-p``
toggle visible    ``ctrl-v``
cycle token color ``ctrl-c``
================= ==============

camera
^^^^^^
================= ================
grip scale        ``mouse wheel``
grip translate    ``middle mouse``
================= ================

``tile`` mode
^^^^^^^^^^^^^
================= ================
create tile       ``left mouse``
delete tile       ``right mouse``
================= ================

``token-edit`` mode
^^^^^^^^^^^^^^^^^^^
================= ================
create token      ``left mouse``
delete token      ``right mouse``
================= ================

``tile`` mode
^^^^^^^^^^^^^
================= ================
select token      ``left mouse``
move token        ``right mouse``
================= ================

chat
^^^^
================= ===============
compose message   ``<all keys>``
send message      ``return``
================= ===============

roll expressions
----------------

If `levo-client` is connected, it will respond in chat to roll expressions that
are prefixed with ``comma``, as in ``,10ld20``.

See `levo <https://github.com/buhman/levo>`_ for more examples.
