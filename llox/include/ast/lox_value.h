#include <string>

enum class value_type {
  NIL,
  BOOLEAN,
  NUMBER,
  STRING,
};

inline const char* value_type_to_string(value_type type) {
  switch(type) {
    case value_type::NIL: return "NIL";
    case value_type::BOOLEAN: return "BOOLEAN";
    case value_type::NUMBER: return "NUMBER";
    case value_type::STRING: return "STRING";
    default: return "UNKNOWN";
  }
}

struct LoxValue {
  value_type type_tag;
  union {
    double number;
    bool boolean;
    std::string *str_data;
  };

  LoxValue() : type_tag(value_type::NIL) {}
  LoxValue(bool b) : type_tag(value_type::BOOLEAN), boolean(b) {}
  LoxValue(double num) : type_tag(value_type::NUMBER), number(num) {}
  LoxValue(std::string &&s)
      : type_tag(value_type::STRING), str_data(new std::string(std::move(s))) {}

  LoxValue(LoxValue &&other) noexcept {
    type_tag = other.type_tag;
    switch (type_tag) {
    case value_type::NIL:
      break;
    case value_type::NUMBER:
      number = other.number;
      other.number = 0;
      break;
    case value_type::BOOLEAN:
      boolean = other.boolean;
      other.boolean = false;
      break;
    case value_type::STRING:
      str_data = other.str_data;
      other.str_data = nullptr;
      break;
    }
    other.type_tag = value_type::NIL;
  }
  LoxValue &operator=(LoxValue &&rhs) noexcept {
    if (this != &rhs) {
      release_resources();
      type_tag = rhs.type_tag;
      switch (type_tag) {
      case value_type::NUMBER:
        number = rhs.number;
        break;
      case value_type::BOOLEAN:
        boolean = rhs.boolean;
        break;
      case value_type::STRING:
        str_data = rhs.str_data;
        rhs.str_data = nullptr;
        break;
      case value_type::NIL:
        break;
      }
      rhs.type_tag = value_type::NIL;
    }
    return *this;
  }
  ~LoxValue() { release_resources(); }

private:
  void release_resources() {
    if (type_tag == value_type::STRING) {
      if (str_data) {
        delete str_data;
        str_data = nullptr;
      }
    }
    type_tag = value_type::NIL;
  }
  LoxValue(const LoxValue &) = delete;
  LoxValue &operator=(const LoxValue &) = delete;
};
