# Report missing on used and undefined markdown links

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Build Status][build-badge]][build-link]

This library report on missing on used and undefined links in [markdown]
formatted files.  It finds the following issues:

* Bracketed references without links are provided without link definitions.
* Unused link definitions where links are defined but never used.
* Invalid link URLs.

When the report is run with `markdown-link-report`, a compilation buffer is
created with each line an error/issue with the markdown document.


## License

Copyright (c) 2022 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[melpa-link]: https://melpa.org/#/markdown-link
[melpa-stable-link]: https://stable.melpa.org/#/markdown-link
[melpa-badge]: https://melpa.org/packages/markdown-link-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/markdown-link-badge.svg
[build-badge]: https://github.com/plandes/markdown-link/workflows/CI/badge.svg
[build-link]: https://github.com/plandes/markdown-link/actions

[markdown]: https://en.wikipedia.org/wiki/Markdown
