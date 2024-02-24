import std/options, std/strutils, std/bitops, std/math, std/unicode

type
  JsonValueKind = enum
    StringValue, NumberValue, BooleanValue, NullValue, ObjectValue, ArrayValue

  JsonValue = object
    start, len: int
    case kind: JsonValueKind
    of StringValue, NumberValue, NullValue:
      discard
    of BooleanValue:
      b: bool
    of ObjectValue:
      o: seq[(string, JsonValue)]
    of ArrayValue:
      a: seq[JsonValue]

let
  t = "true"
  f = "false"
  n = "null"

template error(msg: string) =
  raise newException(CatchableError, msg)

when defined(release):
  {.push checks: off.}

proc getu4(input: string, start: int): int32 =
  for i in 0 ..< 4:
    let c = input[start + i]
    case c:
    of '0' .. '9':
      result = result shl 4 or ord(c).int32 - ord('0')
    of 'a' .. 'f':
      result = result shl 4 or ord(c).int32 - ord('a') + 10
    of 'A' .. 'F':
      result = result shl 4 or ord(c).int32 - ord('A') + 10
    else:
      # Not possible, pre-validated
      error("Invalid hex digit at " & $(start + i))

proc copy(dst: var string, src: string, start, len: int) {.inline.} =
  when nimvm:
    for i in start ..< start + len:
      dst.add src[i]
  else:
    if len > 0:
      let tmp = dst.len
      dst.setLen(tmp + len)
      copyMem(dst[tmp].addr, src[start].unsafeAddr, len)

proc unescapeString(input: string, start, len: int): string =
  if len == 0:
    return

  var
    i = start
    copyStart = i
  while i < start + len:
    let c = input[i]
    if c == '\\':
      copy(result, input, copyStart, i - copyStart)
      # We can blindly index ahead since this is checked in parseString
      let e = input[i + 1]
      case e:
      of '"', '\\', '/': result.add(e)
      of 'b': result.add '\b'
      of 'f': result.add '\f'
      of 'n': result.add '\n'
      of 'r': result.add '\r'
      of 't': result.add '\t'
      of 'u':
        # Wrong but avoiding non-std deps
        let r1 = getu4(input, i + 2)
        result.add Rune(r1)
        i += 4
      else:
        error("Invalid escaped character at " & $i) # Not possible
      i += 2
      copyStart = i
    else:
      inc i

  copy(result, input, copyStart, start + len - copyStart)

proc parseString(input: string, i: var int): int =
  let start = i

  inc i

  while i < input.len:
    if input[i] == '"':
      inc i
      break
    elif input[i] == '\\':
      if i + 1 == input.len:
        error("Unexpected end of JSON input")
      case input[i + 1]:
      of '"', '\\', '/', 'b', 'f', 'n', 'r', 't':
        i += 2
      of 'u':
        if i + 6 > input.len:
          error("Unexpected end of JSON input")
        else:
          const valid = {'0' .. '9', 'A' .. 'F', 'a' .. 'f'}
          if input[i + 2] notin valid or input[i + 3] notin valid or
            input[i + 4] notin valid or input[i + 5] notin valid:
            error("Invalid escaped unicode hex sequence at " & $i)
          i += 6
      else:
        error("Invalid escaped character at " & $i)
    else:
      inc i

  let len = i - start

  return len

proc parseNumber(input: string, i: var int): int =
  let
    start = i
    after = input.find({' ', '\n', '\r', '\n', ',', '}', ']'}, start = i + 1)
  var len: int
  if after == -1:
    len = input.len - i
    i = input.len
  else:
    len = after - i
    i = after

  var ni = start

  if input[start] == '-':
    if len == 1:
      error("Invalid number at " & $start)
    inc ni

  if input[ni] == '0':
    if ni == start + len - 1:
      return len
    # Check there is no leading zero unless followed by a '.'
    if ni + 1 == start + len or input[ni + 1] != '.':
      error("Invalid number at " & $start)

  var d, e: bool
  while ni < start + len:
    case input[ni]:
    of '.':
      if d or e or ni + 1 == start + len:
        error("Invalid number at " & $start)
      if input[ni + 1] notin '0' .. '9':
        error("Invalid number at " & $start)
      ni += 2
      d = true
    of 'e', 'E':
      if e or ni + 1 == start + len:
        error("Invalid number at " & $start)
      if input[ni + 1] == '+' or input[ni + 1] == '-':
        if ni + 2 == start + len:
          error("Invalid number at " & $start)
        ni += 2
      else:
        inc ni
      e = true
    of '0' .. '9':
      inc ni
    else:
      error("Invalid number at " & $start)

  return len

proc parseBoolean(input: string, i: var int): bool {.inline.} =
  if i + 3 < input.len and equalMem(input[i].unsafeAddr, t.cstring, 4):
    i += 4
    return true
  elif i + 4 < input.len and equalMem(input[i].unsafeAddr, f.cstring, 5):
    i += 5
    return false
  else:
    error("Expected true or false at " & $i)

proc parseNull(input: string, i: var int) {.inline.} =
  if i + 3 < input.len and equalMem(input[i].unsafeAddr, n.cstring, 4):
    i += 4
  else:
    error("Expected null at " & $i)

proc skipWhitespace(input: string, i: var int) =
  while i < input.len:
    if input[i] in {' ', '\n', '\r', '\t'}:
      inc i
    else:
      break

proc parseJson*(input: string): JsonValue =
  var
    i: int
    stack: seq[(string, JsonValue)]
    root: Option[JsonValue]

  while true:
    skipWhitespace(input, i)

    if i == input.len:
      break

    if root.isSome:
      error("Unexpected non-whitespace character at " & $i)

    var key: string
    if stack.len > 0:
      case stack[^1][1].kind:
      of ObjectValue:
        if input[i] == '}':
          inc i
          var popped = stack.pop()
          popped[1].len = i - popped[1].start
          if stack.len > 0:
            case stack[^1][1].kind
            of ObjectValue:
              stack[^1][1].o.add(move popped)
            of ArrayValue:
              stack[^1][1].a.add(move popped[1])
            else:
              error("Unexpected JsonValue kind, should not happen")
          else:
            root = some(move popped[1])
          continue

        if stack[^1][1].o.len > 0:
          if input[i] != ',':
            error("Expected , at " & $i)
          inc i
          skipWhitespace(input, i)

        if input[i] == '"':
          let
            start = i
            keyLen = parseString(input, i)
          key = unescapeString(input, start + 1, keyLen - 2)
          skipWhitespace(input, i)
          if i == input.len:
            error("Unexpected end of JSON input")
          if input[i] != ':':
            error("Expected : at " & $i)
          inc i
          skipWhitespace(input, i)
        else:
          error("Unexpected " & input[i] & " at " & $i)

      of ArrayValue:
        if input[i] == ']':
          inc i
          var popped = stack.pop()
          popped[1].len = i - popped[1].start
          if stack.len > 0:
            case stack[^1][1].kind
            of ObjectValue:
              stack[^1][1].o.add(move popped)
            of ArrayValue:
              stack[^1][1].a.add(move popped[1])
            else:
              error("Unexpected JsonValue kind, should not happen")
          else:
            root = some(move popped[1])
          continue

        if stack[^1][1].a.len > 0:
          if input[i] != ',':
            error("Expected , at " & $i)
          inc i
          skipWhitespace(input, i)

      else:
        error("Unexpected JsonValue kind, should not happen")

    let start = i
    var value: JsonValue
    case input[i]:
    of '[':
      value = JsonValue(kind: ArrayValue)
      inc i
    of '{':
      value = JsonValue(kind: ObjectValue)
      inc i
    of '"':
      value = JsonValue(kind: StringValue)
      value.len = parseString(input, i)
    of 't', 'f':
      value = JsonValue(kind: BooleanValue, b: parseBoolean(input, i))
      value.len = i - start
    of 'n':
      parseNull(input, i)
      value = JsonValue(kind: NullValue)
      value.len = i - start
    of '-', '0' .. '9':
      value = JsonValue(kind: NumberValue)
      value.len = parseNumber(input, i)
    else:
      error("Unexpected " & input[i] & " at " & $i)

    value.start = start

    if value.kind in {ArrayValue, ObjectValue}:
      stack.add((move key, move value))
    elif stack.len > 0:
      case stack[^1][1].kind:
      of ArrayValue:
        stack[^1][1].a.add(value)
      of ObjectValue:
        for j in 0 ..< stack[^1][1].o.len:
          if stack[^1][1].o[j][0] == key:
            error("Duplicate object key: " & key)
        stack[^1][1].o.add((move key, value))
      else:
        error("Unexpected JsonValue kind, should not happen")
    else:
      root = some(value)

  if stack.len > 0 or not root.isSome:
    error("Unexpected end of JSON input")

  return move root.get

when defined(release):
  {.pop.}
