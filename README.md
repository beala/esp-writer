# Compliation

```
git clone git@github.com:beala/esp-writer.git
cd esp-writer
stack install
```

# Usage

```
abeal$ esp-writer --help
Usage: esp-writer (-d|--device DEVICE) (-f|--file FILE) (-n|--name NAME)
                  [--exec]
  Uploads a file to an esp8266 board running nodemcu.

Available options:
  -h,--help                Show this help text
  -d,--device DEVICE       Serial device.
  -f,--file FILE           File to send.
  -n,--name NAME           Name on board.
  --exec                   Immediately execute the script and watch output.
  ```