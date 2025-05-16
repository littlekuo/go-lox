CXX := clang++
CXXFLAGS := -std=c++17 -Wall -Wextra -g -Iinclude/ \
            -isystem$(shell llvm-config --includedir) \
            -Wno-unused-parameter \
            $(shell llvm-config --cxxflags)
			
LDFLAGS := $(shell llvm-config --ldflags)
LIBS := $(shell llvm-config --libs core)

SRC_DIR := src
BUILD_DIR := build
BIN_DIR := bin
TARGET := $(BIN_DIR)/llox

SRCS := $(shell find $(SRC_DIR) -name '*.cpp')
OBJS := $(patsubst $(SRC_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(SRCS))
DEPS := $(OBJS:.o=.d)


all: $(TARGET)

$(TARGET): $(OBJS) | $(BIN_DIR)
	$(CXX) $(LDFLAGS) $^ -o $@ $(LIBS)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp | $(BUILD_DIR)
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MMD -MP -c $< -o $@

$(BUILD_DIR) $(BIN_DIR):
	@mkdir -p $@

-include $(DEPS)

clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)

rebuild: clean all

.PHONY: all clean rebuild
