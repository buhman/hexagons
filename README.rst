========
hexagons
========

.. image:: https://img.shields.io/circleci/project/github/buhman/hexagons.svg?style=flat-square
   :target: https://circleci.com/gh/buhman/hexagons

overview
--------

hexagons is a networked multiplayer abstract virtual tabletop and semantic tile system.

usage
-----

hexagons depends on `CHICKEN <https://www.call-cc.org/>`_ 4.13.0. `notes
<https://ptpb.pw/ZjBD/sh>`_ on how to do this in OSX.

Install dependencies and compile the hexagons client and server with::

  chicken-install -s -n

Start the client and server::

  ./hexagons
  ./server
