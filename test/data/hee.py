import sys
import serial

def calc_size(payload):
  size = len(payload)
  size_high = hex((size >> 8) & 0xFF)
  size_low = hex(size & 0xFF)
  return {'size_high': size_high, 'size_low': size_low}

def calc_checksum(data):
  ck_a = 0
  ck_b = 0
  for i in range(len(data)):
    ck_a = ck_a + ord(data[i])
    ck_b = ck_b + ck_a
  return {'ck_a': hex(ck_a & 0xFF), 'ck_b': hex(ck_b & 0xFF)}

def send_to_serial(data):
  ser = serial.Serial('/dev/ttyUSB1', 9600)
  ser.write(data)

payload = sys.argv[1]
payload_size = calc_size(payload)

command = "He" # sync characters 1 and 2
command += chr(int("0x10", 16)) # command type, character 1
command += chr(int("0x03", 16)) # command type, character 2
command += chr(int(payload_size['size_high'], 16))
command += chr(int(payload_size['size_low'], 16))

header_checksum = calc_checksum(command[2:]) # checksum is calculated without "He"

command += chr(int(header_checksum['ck_a'], 16))
command += chr(int(header_checksum['ck_b'], 16))

command += payload

payload_checksum = calc_checksum(command[2:]) # checksum is calculated without "He"

command += chr(int(payload_checksum['ck_a'], 16))
command += chr(int(payload_checksum['ck_b'], 16))

send_to_serial(command)

print ' '.join(x.encode('hex') for x in command) # for Python 3.x: ':'.join(hex(ord(x))[2:] for x in 'Hello World!')




