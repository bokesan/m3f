# m3f ![Continuous Integration](https://github.com/bokesan/m3f/actions/workflows/ci.yml/badge.svg)

List metadata of Hasselblad raw files.

## Install / Build

The installation consists of a single executable, `m3f`. Download the version for your
OS from releases and unzip it in a folder in your path.

Building from source requires a Common Lisp implementation. `m3f` is tested with and
known to work with [SBCL](https://www.sbcl.org/) and
[CCL](https://ccl.clozure.com/),
but should also work with other implementations.

## Usage

`m3f` is a command-line application. By default, it displays the
metadata in a format as close as possible to what the "Capture Info"
tab in Phocus shows. For example:

```
$ m3f test/resources/raw1.3FR
        Device: CFV 100C/907X
       Created: 2025:01:16 15:46:52
    Dimensions: 8742x11656, crop mode 65:24 (XPan): 4302x11656
          Lens: XCD 28P, serial number: 8QHI15094 (year: 2024)
     Converter:
     Extension: 9 mm
           HTS: ?
           ISO: 400
       Shutter: 1/160
      Aperture: f/4.0
   Light Meter: Centre W
 Exposure Mode: Aperture
    Focus Mode: Single
 Serial Number: JT63001117
GPS Coordinate: ?
   Orientation: Rotate 90 CW
       Quality: 16 bit
 White Balance: Daylight
    Drive Mode: Single Shot
```

The lines up to and including "GPS Coordinate" should be identical to
the information shown by Phocus, except that `m3f` will sometimes
display more details, such as the lens serial number.

A "?" means that I have not yet been able to decode the information.
I need help with that; see [the wiki](https://github.com/bokesan/m3f/wiki).

Running `m3f` without arguments will display detailed usage
information.

