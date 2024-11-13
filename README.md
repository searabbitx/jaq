# jaq

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
  { "color": "red", "category": "hue" },
  { "color": "blue", "category": "hue" },
  { "color": "yellow", "category": "hue" },
  { "color": "green", "category": "hue" }
]
```
