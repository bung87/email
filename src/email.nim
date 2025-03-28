import std/[strutils, base64, tables, encodings]

type
  EmailPart* = ref object
    headers*: Table[string, string]
    content*: string
    parts*: seq[EmailPart]
    filename*: string
    charset*: string


proc decodeQuotedPrintable(s: string): string =
  ## Decodes a quoted-printable encoded string
  var i = 0
  var output = newSeq[byte]()
  
  while i < s.len:
    if s[i] == '=':
      if i + 1 >= s.len:
        output.add('='.byte)
        inc i
        continue
      
      # Handle soft line breaks
      if s[i+1] == '\r' and i + 2 < s.len and s[i+2] == '\n':
        i += 3  # Skip =, CR, LF
      elif s[i+1] == '\n':
        i += 2  # Skip = and LF
      else:
        if i + 2 >= s.len:
          output.add('='.byte)
          output.add(s[i+1].byte)
          i += 2
          continue
        
        let hexStr = s.substr(i+1, i+2)
        try:
          let val = parseHexInt(hexStr).byte
          output.add(val)
          i += 3
        except ValueError:
          # Invalid hex sequence, preserve original
          output.add('='.byte)
          output.add(s[i+1].byte)
          output.add(s[i+2].byte)
          i += 3
    else:
      output.add(s[i].byte)
      inc i
  
  # Convert byte sequence to string
  result = newString(output.len)
  for j in 0..<output.len:
    result[j] = output[j].char

proc decodeHeader(header: string): string =
  var
    buffer = ""
    currentPos = 0
    headerLen = header.len

  while currentPos < headerLen:
    # Look for encoded word start
    if header[currentPos] == '=' and currentPos+1 < headerLen and header[currentPos+1] == '?':
      # Parse encoded word
      let
        charsetStart = currentPos + 2
        charsetEnd = header.find('?', charsetStart)
      
      if charsetEnd == -1:
        buffer.add header[currentPos..currentPos+1]
        currentPos += 2
        continue

      let
        encodingStart = charsetEnd + 1
        encodingEnd = header.find('?', encodingStart)
      
      if encodingEnd == -1:
        buffer.add header[currentPos..charsetEnd]
        currentPos = charsetEnd + 1
        continue

      let
        payloadStart = encodingEnd + 1
        payloadEnd = header.find("?=", payloadStart)
      
      if payloadEnd == -1:
        buffer.add header[currentPos..encodingEnd]
        currentPos = encodingEnd + 1
        continue

      let
        charset = header[charsetStart..<charsetEnd]
        encoding = header[encodingStart..<encodingEnd].toUpperAscii
        payload = header[payloadStart..<payloadEnd]

      # Decode payload
      let decodedPayload = 
        try:
          case encoding[0]
          of 'B':
            decode(payload)
          of 'Q':
            decodeQuotedPrintable(payload.replace("_", " "))
          else:
            payload
        except:
          payload
      
      # Convert charset to UTF-8
      try:
        buffer.add convert(decodedPayload, "UTF-8", charset)
      except:
        buffer.add decodedPayload
      
      currentPos = payloadEnd + 2
    else:
      # Add normal character
      buffer.add header[currentPos]
      currentPos += 1

  result = buffer

proc parsePartContent(part: EmailPart, lines: var seq[string], i: var int, boundary: string) =
  var contentBuffer: string
  while i < lines.len:
    if lines[i].startsWith("--" & boundary):
      break
    contentBuffer.add lines[i] & "\n"
    i += 1

  # Default to UTF-8 if charset not specified
  let contentType = part.headers.getOrDefault("Content-Type", "")
  var charset = "UTF-8"
  if "charset=" in contentType:
    charset = contentType.split("charset=")[1].split(';')[0].strip(chars={'"'})

  # Handle encoding
  let encoding = part.headers.getOrDefault("Content-Transfer-Encoding", "7bit").toLowerAscii()
  case encoding:
  of "base64":
    try:
      part.content = decode(contentBuffer.convert("UTF-8", charset))
    except:
      part.content = contentBuffer
  of "quoted-printable":
    part.content = decodeQuotedPrintable(contentBuffer.convert("UTF-8", charset))
  else:
    part.content = contentBuffer.convert("UTF-8", charset)


proc parseEmail*(content: string): EmailPart =
  result = EmailPart(headers: initTable[string, string](), parts: @[])
  let normalized = content.replace("\r\n", "\n").replace("\r", "\n")
  var lines = normalized.split('\n')
  var i = 0
  var currentKey = ""
  var rawValue = ""

  # Parse headers
  while i < lines.len:
    let line = lines[i].strip(leading = false, trailing = true)
    
    if line.len == 0:
      # End of headers
      if currentKey != "":
        result.headers[currentKey] = decodeHeader(rawValue).strip(leading = true, trailing = false)
      break
    
    if line[0] in {' ', '\t'}:
      # Continuation line
      if currentKey != "":
        # Remove leading whitespace and prepend with space
        rawValue &= "" & line.strip(leading = true, trailing = false)
    else:
      # New header line - save previous header
      if currentKey != "":
        result.headers[currentKey] = decodeHeader(rawValue).strip(leading = true, trailing = false)
      
      # Parse new header
      let colonPos = line.find(':')
      if colonPos != -1:
        currentKey = line[0..<colonPos].strip()
        rawValue = line[colonPos+1..^1].strip(leading = false, trailing = true)
      else:
        # Handle invalid header line
        currentKey = ""
        rawValue = ""
    
    i += 1

  # Save the last header if any
  if currentKey != "":
    result.headers[currentKey] = decodeHeader(rawValue).strip(leading = true, trailing = false)

  # Parse body
  let contentType = result.headers.getOrDefault("Content-Type", "")
  if "multipart/" in contentType:
    let boundary = contentType.split("boundary=")[1].split(';')[0].strip(chars={' ', '"'})
    var currentPart: EmailPart
    
    while i < lines.len:
      if lines[i].startsWith("--" & boundary):
        if currentPart != nil:
          parsePartContent(currentPart, lines, i, boundary)
          result.parts.add(currentPart)
        
        if lines[i].endsWith("--"):
          break
        
        currentPart = EmailPart(headers: initTable[string, string](), parts: @[])
        i += 1
        
        # Parse part headers
        while i < lines.len and lines[i].len > 0:
          let line = lines[i].strip(leading=false, trailing=true)
          let colonPos = line.find(':')
          if colonPos != -1:
            let key = line[0..<colonPos].strip()
            let value = line[colonPos+1..^1].strip()
            currentPart.headers[key] = decodeHeader(value).strip
          i += 1
        i += 1
      else:
        i += 1
  else:
    result.content = lines[i..^1].join("\n")
    let encoding = result.headers.getOrDefault("Content-Transfer-Encoding", "7bit").toLowerAscii()
    let charset = result.headers.getOrDefault("Content-Type", "").split("charset=")[1].split(';')[0].strip(chars={'"'}).normalize

    case encoding:
    of "base64":
      result.content = decode(result.content.convert("UTF-8", charset))
    of "quoted-printable":
      result.content = decodeQuotedPrintable(result.content.convert("UTF-8", charset))
    else:
      result.content = result.content.convert("UTF-8", charset)

when isMainModule:
  let emailContent = readFile("a.eml")

  let email = parseEmail(emailContent)
  
  echo "From: ", email.headers.getOrDefault("From", "")
  echo "To: ", email.headers.getOrDefault("To", "")
  echo "Subject: ", email.headers.getOrDefault("Subject", "")
  echo email.content
  for part in email.parts:
    echo "\nPart Content-Type: ", part.headers.getOrDefault("Content-Type", "")
    if "attachment" in part.headers.getOrDefault("Content-Disposition", ""):
      echo "Attachment: ", part.filename
      echo "File content: ", part.content[0..min(20, part.content.high)]
    else:
      echo "Body: ", part.content