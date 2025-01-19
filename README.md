# m3f

List metadata of Hasselblad raw files.

## Building

You need a Common Lisp implementation to build `m3f`. It is tested and
known to work with [SBCL](https://www.sbcl.org/) and
[CCL](https://ccl.clozure.com/),
but should also work with other implementations.

## Usage

`m3f` is a command-line application. By default, it displays the
metadata in a format as close as possible to what the "Capture Info"
tab in Phocus shows. For example:

```
$ m3f B0001234.3FR
        Device: CFV 100C/907X
       Created: 2025:01:12 08:37:56
    Dimensions: 11656x8742
          Lens: XCD 30, serial number: 2WVU10787 (year: 2017)
     Converter: ?
     Extension: ?
           HTS: ?
           ISO: 400
       Shutter: 1/100
      Aperture: f/3.5
   Light Meter: Centre W
 Exposure Mode: Full Auto
    Focus Mode: Single
 Serial Number: JT63001117
GPS Coordinate: ?
       Quality: 16 bit
 White Balance: Auto
```

The lines up to and including "GPS Coordinate" should be identical to
the information that is shown by Phocus, except that `m3f` will sometimes
display more details, such as the lens serial number.

A "?" means that I have not yet been able to decode the information.

Running `m3f` without arguments will display detailed usage
information.

