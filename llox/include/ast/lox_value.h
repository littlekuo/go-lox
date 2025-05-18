#include <string>

enum class value_type {
  NIL,
  BOOLEAN,
  NUMBER,
  STRING,
  OBJECT,
};

class LoxObject {
public:
  virtual ~LoxObject() = default;
  virtual value_type get_type() const = 0;
};

class LoxString : public LoxObject {
  std::string data;

public:
  explicit LoxString(const std::string &&s) : data(std::move(s)) {}
  value_type get_type() const override { return value_type::STRING; }
  std::string_view value() const { return data; }
};

struct LoxValue {
  value_type type_tag;
  union {
    double number;
    bool boolean;
    LoxObject *object;
  } data;

  LoxValue() : type_tag(value_type::NIL), data{.object = nullptr} {}
  LoxValue(bool b) : type_tag(value_type::BOOLEAN), data{.boolean = b} {}
  LoxValue(double num) : type_tag(value_type::NUMBER), data{.number = num} {}
  LoxValue(LoxObject *obj) : type_tag(obj->get_type()), data{.object = obj} {}

  LoxValue(const LoxValue &) = delete;
  LoxValue &operator=(const LoxValue &) = delete;
  LoxValue(LoxValue &&other) noexcept : type_tag(other.type_tag) {
    memcpy(&data, &other.data, sizeof(data));
    other.reset();
  }
  LoxValue &operator=(LoxValue &&rhs) noexcept {
    if (this != &rhs) {
      release_resources();

      type_tag = rhs.type_tag;
      memcpy(&data, &rhs.data, sizeof(data));

      rhs.reset();
    }
    return *this;
  }
  ~LoxValue() { release_resources(); }

private:
  void release_resources() {
    switch (type_tag) {
    case value_type::STRING:
      delete data.object;
      break;
    case value_type::OBJECT:
      delete data.object;
      break;
    default:
      break;
    }
  }

  void reset() {
    switch (type_tag) {
    case value_type::NUMBER:
      data.number = 0.0;
      break;
    case value_type::BOOLEAN:
      data.boolean = false;
      break;
    case value_type::STRING:
      data.object = nullptr;
      break;
    case value_type::OBJECT:
      data.object = nullptr;
      break;
    case value_type::NIL:
      break;
    }
    type_tag = value_type::NIL;
  }
};
