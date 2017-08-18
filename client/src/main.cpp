#include "client.h"


int main(int argc, char** argv){
  Client* c = new Client("abcd", 12345);
  c->print();
  c->connect();
  delete c;
  return 0;
}
