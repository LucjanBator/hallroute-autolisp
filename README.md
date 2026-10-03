# Hallroute AutoLISP bridge for AutoCAD

Move machines between your AutoCAD drawing and the free [Hallroute](https://hallroute.com) factory layout optimizer, without retyping coordinates.

Hallroute places every machine in a hall for the shortest real forklift routes: distances around machines, aisle widths, operator areas, pallet space and escape routes. This script is the bridge between your drawing and the optimizer.

## What the script does

| Command | What it does |
|---|---|
| `HREXPORT` | Writes the machines you select (blocks or closed polylines) to a CSV file that Hallroute can import. |
| `HRIMPORT` | Reads `hallroute-layout.csv` (from the **Download for AutoCAD** package in Hallroute) and moves and rotates the machines in your drawing. |
| `HRLANG` | Switches the script messages between English, German and Polish. |

You can also skip the script and use DXF only: save your hall as DXF, import it in Hallroute, and insert the DXF result back into your drawing.

## Install

1. Download `hallroute.lsp`.
2. In AutoCAD type `APPLOAD`, select the file and click **Load**. To load it every time, add it to the **Startup Suite** in the same window.
3. Type `HREXPORT` to start.

Written for AutoCAD and its toolsets (Mechanical, Architecture and others). AutoCAD LT 2024 and later also run AutoLISP, but the script uses Visual LISP functions that have not been tested there.

## Status

Beta. The script is new and has not been tested on every AutoCAD version. If something does not work, please open an issue with your AutoCAD version and a small sample drawing.

## Links

- Optimizer (free for halls with up to 10 machines): https://hallroute.com/app
- Forklift aisle width calculator (ASR A1.8): https://hallroute.com/aisle-width-calculator
