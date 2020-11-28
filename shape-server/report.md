---
papersize: a4
title: CSU44012
subtitle: Assignment 1
urlcolor: blue
author: Jack O'Sullivan - \#17331147 - [osullj19@tcd.ie](mailto:osullj19@tcd.ie)
---

# Introduction

The DSL designed for this assignment was relatively similar to the `Shape` from
week 5. It was expanded to add colouring options (handled similarly to the
`Shape`). The `Drawing` type now represents a shape, its colouring and a
transform.

The rendering code was made more generic, including exposing functions to render
a single pixel, an RGBA image or a fully-encoded PNG.

# Completion of deliverables

Overall, most of the deliverables were fulfilled. All of the required shapes are
implemented; `Mandelbrot` was left over from week 5 and `Polygon` needed a
function to determine if a `Point` was within a `Polygon` represented by a
`[Point]`.

All of the required transformations (along with `Shear`) were implemented. By
providing a method to collapse the transformations into a single affine
transformation matrix, the transformation of `Point`s is optimized.

Provided colouring options are quite limited, although there is a custom mapping
option (gradient is given as an example). Alpha blending is provided, which
neatly solves the masking requirement.

The web-UI, while very simplistic, is nicely interactive: `Hint`, a wrapper
around the GHC API is used to interpret expressions and render the image.
Hotlinking / embedding of images is easy, since the expression is embedded into
a URL of the form `/render/:expr/png`, where `expr` is a Base64 string.

# DSL design

More high-level constructs (rather than functions building on primitives) makes
rendering more flexible. While optimisations like affine transforms are good for
performance, they might not "translate" to other rendering targets, such as the
high-level vector markup language SVG. By keeping the high-level composed
transformations around, alternative mapping to such targets remains possible.
The same applies to colouring, constructs like a `Gradient` might map to some
sort of styling option in another target. Keeping this in mind led to a few
tweaks to the language to keep it flexible.
