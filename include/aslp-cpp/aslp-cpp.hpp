#pragma once

#include <string>
#include <memory>


namespace httplib {
class Client;
} // namespace httplib;

class aslp_connection
{
  std::unique_ptr<httplib::Client> client {nullptr};

public:
  aslp_connection(const std::string& server_addr, int server_port);
  auto get_opcode(int opcode) -> std::string;
  void wait_active();
};



class aslp_client {
private:
  const std::string server_addr;
  pid_t server_pid;
  int server_port;
  void shutdown();
public:
  aslp_client(const aslp_client&) = delete;
  aslp_client(aslp_client&&) = delete;
  auto operator=(const aslp_client&) -> aslp_client& = delete;
  auto operator=(aslp_client&&) -> aslp_client& = delete;

  aslp_client(pid_t pid, std::string addr, int port)
      : server_pid(pid)
      , server_port(port)
      , server_addr(std::move(addr))
  {
  }

  std::unique_ptr<aslp_client> static start() {
    return start("127.0.0.1", 8000);
  }

  auto static start(const std::string& addr, int server_port) -> std::unique_ptr<aslp_client>;

  auto get_opcode(int opcode) -> std::string;


  virtual ~aslp_client() {
    shutdown();
  };

};

