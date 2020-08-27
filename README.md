
# asciiart

A very quick, very dirty, very simple and very nasty attempt at making
ASCII art in R. Please forgive me.

This is ascii art when plotted on a uniform grid. When shown as text in
a text editor it will look vertically stretched, as the aspect ratio of
the character glyphs is not one\! This needs to be changed and worked
on.

``` r
library(asciiart)
asciiart(asciiart::hadley)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
