#include <string>
#include <iostream>

class Client {
 private:
  std::string _host;
  int _port;
 public:
 Client(std::string host, int port):_host(host),_port(port){
  }

  int connect(){return 0;}
  int send(char* data, int len){return 0;}
  int receive(char* buf){return 0;}
  void print(){
    std::cout<<"print"<<std::endl;
  }
};
