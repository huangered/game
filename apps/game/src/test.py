#!/usr/bin/env python
import socket
import struct

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
send(s, pack("t","test"))
send(s, pack("ggg","abcdefghijklmn"))
data = s.recv(1024)
print data
s.close()
