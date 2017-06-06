#!/usr/bin/env python
import socket
import struct
import json
from datetime import datetime

class GameClient:
	def __init__(self):
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.s.connect(("localhost", 5555))

	def send(self, message):
		print "send: %s" % (message)
		length = len(message)
        # network (= big-endian)
		lenBin = struct.pack("!H", length)
		self.s.send(lenBin)
		self.s.send(message)

	def pack(self, method, raw):
		methodLen = len(method)
		methodLenBin = struct.pack("B", methodLen)
		data = methodLenBin+method+raw
		return data

	def recv(self):
		data = self.s.recv(2)
		leng1 = struct.unpack("!H", data)[0]
		data = self.s.recv(1)
		leng2 = struct.unpack("B", data)[0]
		data = self.s.recv(leng2)
		data2 = self.s.recv(leng1 - 1)
		print "Datetime: %s\nTotal len: %d, method len: %d\nMethod: %s\nMsg:\n============\n%s\n============\n" % (str(datetime.now()), leng1, leng2, data, data2)

	def auth(self, user, password):
		self.send(self.pack("auth",json.dumps({'user':user, 'password':password})))
		self.recv()
	def show(self):
		self.send(self.pack("show", json.dumps({})))
		self.recv()
	def listPlayer(self):
		self.send(self.pack("list_player", json.dumps({})))
		self.recv()
	def enterInGame(self, playerId):
		self.send(self.pack("enter_in_game", json.dumps({'playerId':playerId})))
	def move(self, x, y):
		self.send(self.pack("move",json.dumps({'x':x, 'y':y})))
	def worlds(self):
		self.send(self.pack("worlds",json.dumps({})))
	def friends(self):
		self.send(self.pack("friends", json.dumps({})))
	def talk(self, userId, msg):
		self.send(self.pack("talk", json.dumps({"userId":1, "msg":"hello world"})))
		self.recv(),
	def logout(self):
		self.send(self.pack("logout", json.dumps({})))
	def close(self):
		self.s.close()
if __name__ == '__main__':
	client = GameClient()
	client.auth("abcd","abcd")
	client.show()
	client.listPlayer()
	client.enterInGame(1)
	client.move(100,2)
	client.move(200,3)
	client.worlds()
	client.friends()
	client.talk(1, "hell world!")
	client.talk(1, "hell world!")
	client.talk(1, "hell world!")
	client.talk(1, "hell world!")
	client.close()