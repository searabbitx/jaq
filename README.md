# jaq
![build](https://github.com/searabbitx/jaq/actions/workflows/build-test-release.yml/badge.svg?branch=main)

jq-like json processing tool with saner filter language

## Filter examples

**colors.json**
```json
{"colors":[{"color":"black","category":"hue","type":"primary","code":{"rgba":[255,255,255,1],"hex":"#000"}},{"color":"white","category":"value","code":{"rgba":[0,0,0,1],"hex":"#FFF"}},{"color":"red","category":"hue","type":"primary","code":{"rgba":[255,0,0,1],"hex":"#FF0"}}]}
```

Just format
```bash
$ cat colors.json | jaq
```
```json
{
  "colors": [
    {
      "color": "black",
      "category": "hue",
      "type": "primary",
      "code": { "rgba": [ 255, 255, 255, 1 ], "hex": "#000" }
    },
    {
      "color": "white",
      "category": "value",
      "code": { "rgba": [ 0, 0, 0, 1 ], "hex": "#FFF" }
    },
    {
      "color": "red",
      "category": "hue",
      "type": "primary",
      "code": { "rgba": [ 255, 0, 0, 1 ], "hex": "#FF0" }
    }
  ]
}
```

Extract field
```bash
$ cat colors.json | jaq "colors.code.hex"
```
```json
[ "#000", "#FFF", "#FF0" ]
```

Select multiple fields
```bash
$ cat colors.json | jaq "colors.select(color, category)"
```
```json
[
  { "color": "black", "category": "hue" },
  { "color": "white", "category": "value" },
  { "color": "red", "category": "hue" }
]
```

You can also access subfields in `select(...)`
```bash
$ cat colors.json | jaq "colors.select(color, code.hex)"
```
```json
[
  { "color": "black", "hex": "#000" },
  { "color": "white", "hex": "#FFF" },
  { "color": "red", "hex": "#FF0" },
]
```

Use aliases for fields
```bash
$ cat colors.json | jaq "colors.select(color as name, code.hex as code)"
```
```json
[
  { "name": "black", "code": "#000" },
  { "name": "white", "code": "#FFF" },
  { "name": "red", "code": "#FF0" },
  { "name": "blue", "code": "#00F" },
  { "name": "yellow", "code": "#FF0" },
  { "name": "green", "code": "#0F0" }
]
```

Extract index
```bash
$ cat colors.json | jaq "colors[2]"
```
```json
{
  "color": "red",
  "category": "hue",
  "type": "primary",
  "code": { "rgba": [ 255, 0, 0, 1 ], "hex": "#FF0" }
}
```

Filter results (available operators: `==`, `!=`, `<`, `<=`, `>`, `>=`, logical operators: `&&`, `||`)
```bash
$ cat colors.json | jaq "colors.filter(code.hex == '#FFF')"
```
```json
[
  {
    "color": "white",
    "category": "value",
    "code": { "rgba": [ 0, 0, 0, 1 ], "hex": "#FFF" }
  }
]
```

```bash
$ cat colors.json | jaq "colors.filter(color == 'white' || code.rgba[1] > 0)"
```
```json
[
  {
    "color": "black",
    "category": "hue",
    "type": "primary",
    "code": { "rgba": [ 255, 255, 255, 1 ], "hex": "#000" }
  },
  {
    "color": "white",
    "category": "value",
    "code": { "rgba": [ 0, 0, 0, 1 ], "hex": "#FFF" }
  }
]
```

Regex match
```bash
$ cat colors.json | jaq "colors.filter(color ~ /black|white/)"
```
```json
[
  {
    "color": "black",
    "category": "hue",
    "type": "primary",
    "code": { "rgba": [ 255, 255, 255, 1 ], "hex": "#000" }
  },
  {
    "color": "white",
    "category": "value",
    "code": { "rgba": [ 0, 0, 0, 1 ], "hex": "#FFF" }
  }
]
```

String helper functions (available functions: uppercase, lowercase, capitalize, replace
```bash
$ cat colors.json | jaq "colors.select(color.uppercase())"
```
```json
[
  { "color": "BLACK" },
  { "color": "WHITE" },
  { "color": "RED" },
  { "color": "BLUE" },
  { "color": "YELLOW" },
  { "color": "GREEN" }
]
```

```bash
$ cat color.json | "colors.select(color.replace(\"e\",\"X\"))"
```
```json
[
  { "color": "black" },
  { "color": "whitX" },
  { "color": "rXd" },
  { "color": "bluX" },
  { "color": "yXllow" },
  { "color": "grXXn" }
]
```

Read json from file (note: empty filter returns json as is)
```bash
$ jaq "" colors.json
```
```json
{
  "colors": [
    {
      "color": "black",
      "category": "hue",
      "type": "primary",
      "code": { "rgba": [ 255, 255, 255, 1 ], "hex": "#000" }
    },
    {
      "color": "white",
      "category": "value",
      "code": { "rgba": [ 0, 0, 0, 1 ], "hex": "#FFF" }
    },
    {
      "color": "red",
      "category": "hue",
      "type": "primary",
      "code": { "rgba": [ 255, 0, 0, 1 ], "hex": "#FF0" }
    }
  ]
}
```
