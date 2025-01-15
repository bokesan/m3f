# m3f

List metadata of Hasselblad raw files.

Raw files from Hasselblad camera are not currently (as of 2024)
well-supported by [ExifTool](https://exiftool.org).
Hence, I started to develop a simple stand-alone tool collect
my discoveries.

## Usage

`m3f` is a command-line application. By default, it displays the
metadata in a format as close as possible to what then "Capture Info"
tab in Phocus shows. For example:

```
$ m3f B0001234.3FR
        Device: CFV 100C/907X
       Created: 2025:01:12 08:37:56
    Dimensions: 11656x8742
          Lens: XCD 30, serial number: 2WVU10787 (year: 2017, product code: 2W)
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
     Crop Mode: No Crop (645) [11656x8742 +128,96]
```

The lines up to and inclusing "GPS Coordinate" should be identical to
the information shown in Phocus, exept that `m3f` will sometimes
display more details, for example the lens serial number.

A "?" means that I have not yet been able to decode the information.
