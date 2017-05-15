#!/usr/bin/env python
import socket
import struct
import json
def send(s, message):
	length = len(message)
        # network (= big-endian)
	lenBin = struct.pack("!H", length)
	s.send(lenBin)
	s.send(message)

def pack(method, raw):
        methodLen = len(method)
        methodLenBin = struct.pack("B", methodLen)
        data = methodLenBin+method+raw
        return data

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("localhost", 5555))

send(s, pack("auth",json.dumps({'user':'abcd', 'password':"abcd"})))
send(s, pack("move",json.dumps({'x':100})))
send(s, pack("move",json.dumps({'x':100})))

data = s.recv(1024)
print data
s.close()
