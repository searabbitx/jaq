# jaq
![build](https://github.com/searabbitx/jaq/actions/workflows/build-test-release.yml/badge.svg?branch=main)

jq-like json processing tool with saner filter language

## Examples

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
$ cat colors.json | jaq "colors.select(color as name, code.hex as code)" test_data/colors.json
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
