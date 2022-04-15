from flask import Flask, json, request, jsonify
import base64
import array

secret = b'a'
shift_n = 3

api = Flask(__name__)

@api.route('/parser', methods=['POST'])
def parser():
  body = request.get_json(silent=True, force=True)
  print("== received:", body)
  try:
    data = base64.b64decode(bytes(body['payload'], encoding='utf8'))
    return parse(body['type'], body['schema_name'], body['parser_opts'], data)
  except Exception as err:
    print("== parse failed for:", str(err))
    return json.dumps({'code': 2, 'result': "parse failed:"+ str(err)})


def parse(dtype, schema_name, parser_opts, data):
  result = b'decode failed'
  code = 2
  if parser_opts == 'xor':
    result = xor(data)
    code = 1
  elif parser_opts == 'subst':
    result = subst(dtype, data, shift_n)
    code = 1
  else:
    result = b'algorithm not supported'
    code = 2
  return json.dumps({'code': code, 'result': str(base64.b64encode(result), encoding='utf8')})

def xor(data):
  """
  >>> xor(xor(b'abc'))
  b'abc'
  >>> xor(xor(b'!}~*'))
  b'!}~*'
  """
  length = len(data)
  bdata = bytearray(data)
  bsecret = bytearray(secret * length)
  result = bytearray(length)
  for i in range(length):
    result[i] = bdata[i] ^ bsecret[i]
  return bytes(result)

def subst(dtype, data, n):
  """
  >>> subst('decode', b'abc', 3)
  b'def'
  >>> subst('decode', b'ab~', 1)
  b'bc!'
  >>> subst('encode', b'def', 3)
  b'abc'
  >>> subst('encode', b'bc!', 1)
  b'ab~'
  """
  adata = array.array('B', data)
  for i in range(len(adata)):
    if dtype == 'decode':
      adata[i] = shift(adata[i], n)
    elif dtype == 'encode':
      adata[i] = shift(adata[i], -n)
  return bytes(adata)

def shift(char, n):
  ## shift the char by shift_n
  enc = char + n
  ## there are 94 printable chars [33, 126]
  return 33 + (enc - 33) % 94

if __name__ == '__main__':
  import doctest
  doctest.testmod()
  api.run(port=9003)

