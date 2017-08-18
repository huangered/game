#include <string>


class Client {
 private:
  std::string _host;
  int _port;
 public:
 Client(std::string host, int port):_host(host),_port(port){
  }

  int connect(){}
  int send(char* data, int len){}
  int receive(char* buf){}
}
