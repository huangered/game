#include <iostream>

#include "client.h"

using namespace std;

int main(int argc, char** argv){
  std::cout<<"hello world"<<std::endl;
  
  Client* c = new Client("abcd",123);
  c->print();
  delete c;
  return 0;
}
