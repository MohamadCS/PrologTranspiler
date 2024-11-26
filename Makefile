# Copyright (c) 2024, Emily Dror

# Compiler and Linker Options
export CXX := clang++
export CXXFLAGS := -Wall -std=c++17
export ANTLR_CMD := antlr -Dlanguage=Cpp

export LD := $(CXX)
export LD_FLAGS = -L $(LIB_DIR) -l $(DYLIB_NAME) -Wl,-rpath,@loader_path/$(LIB_DIR)

################################################################################
### Project Paramaters
TARGET := prolog_parser
DYLIB_NAME := antlr4-runtime
GRAMMAR_FILE = $(GRAMMAR_DIR)/prolog.g4

################################################################################
### Process command line options to the makefile

# VERBOSE:
#   When not specified, suppress normal makefile output for a more terse version
#   of the output.  But, allow a user to see the full makefile output when this
#   option is specified.
ifneq ($(VERBOSE), 1)
    brief = @echo "[$(patsubst $(OBJDIR)/%,%,$(@))]";
    verb = @
else
    brief = @echo "";
    verb =
endif

# CCACHE:
#   If CCACHE is specified on the command line, do the following
#   - Modify CC to include "ccache" as a prefix
#   - Setup NOCCACHE_CC to always be a version of CC without ccache
#   - Don't use CCACHE for HOST_CXX as it is used for linking, which cannot be cached
ifdef CCACHE
CCACHE_AVAILABLE := $(shell which ccache > /dev/null; echo $$?)
ifeq ($(CCACHE_AVAILABLE),0)
$(info Enabling ccache for build)
export CXX := ccache $(CXX)
else
$(info CCACHE requested, but no ccache found)
endif
endif

################################################################################
### Directory Structure
ROOT := .

SRC_DIR := $(ROOT)/src
LIB_DIR := $(ROOT)/lib
TEST_DIR := $(ROOT)/tests
BUILD_DIR := $(ROOT)/build
GRAMMAR_DIR := $(ROOT)/grammar
INCLUDE_DIRS := \
    -I $(ROOT)/include \
    -I $(ROOT)/include/antlr4-runtime \
	-I $(BUILD_DIR)/include

# Build Directory, Don't Change!!
OBJDIR := build/obj
OBJ_DIR := $(BUILD_DIR)/obj
BUILD_INCLUDE_DIR := $(BUILD_DIR)/include
SRC_FILES := $(wildcard $(SRC_DIR)/*.cpp)
OBJ_FILES := $(SRC_FILES:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)

# Lexer and Parser, Don't Change!!
LEXER_PATH := $(BUILD_DIR)/$(patsubst %.g4,%Lexer.cpp,$(GRAMMAR_DIR)/prolog.g4)
PARSER_PATH := $(BUILD_DIR)/$(patsubst %.g4,%Parser.cpp,$(GRAMMAR_DIR)/prolog.g4)
LEXER_OBJ := $(OBJ_DIR)/$(subst .cpp,.o,$(notdir $(LEXER_PATH)))
PARSER_OBJ := $(OBJ_DIR)/$(subst .cpp,.o,$(notdir $(PARSER_PATH)))

################################################################################
### Rules

.PHONY: all
all: $(TARGET)

$(TARGET): BUILD_DIRS $(OBJ_FILES) $(LEXER_OBJ) $(PARSER_OBJ)
	$(brief)
	$(verb) $(LD) $(OBJ_FILES) $(LEXER_OBJ) $(PARSER_OBJ) $(LD_FLAGS) -o $(TARGET)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp ANTLR_HEADERS
	$(brief)
	$(verb) $(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $< -o $@

$(LEXER_OBJ): ANTLR_HEADERS
	$(brief)
	$(verb) $(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $(LEXER_PATH) -o $@

$(PARSER_OBJ): ANTLR_HEADERS
	$(brief)
	$(verb) $(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $(PARSER_PATH) -o $@

ANTLR_HEADERS: BUILD_DIRS
	$(brief)
	@ # Generate ANTLR4 lexer and parser
	$(verb) $(ANTLR_CMD) -o $(BUILD_DIR) $(GRAMMAR_FILE)

	@ # Move header files into build/include directory
	$(verb) mv $(BUILD_DIR)/$(GRAMMAR_DIR)/*.h $(BUILD_INCLUDE_DIR)

BUILD_DIRS:
	@echo "\nUsing Development Build Flow\n"
	$(verb) mkdir -p $(OBJ_DIR)
	$(verb) mkdir -p $(BUILD_INCLUDE_DIR)

.PHONY: clean
clean:
	$(brief)
	$(verb) rm -rf $(BUILD_DIR)/* $(TARGET)
	$(verb) rm -rf $(TEST_DIR)/output*

.PHONY: test
test:
	$(verb) rm -rf $(TEST_DIR)/output*
	$(verb) echo "Running tests..."
	$(verb) python3 run_tests.py
