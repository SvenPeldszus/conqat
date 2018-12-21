CXXFLAGS=-O0 --coverage -Iinclude

.PHONY: all clean coverage

BUILD_DIR=build
COVERAGE_DIR=coverage

SOURCES=$(wildcard *.cpp */*.cpp)
OBJS=$(patsubst %.cpp,$(BUILD_DIR)/%.o,$(SOURCES))
GCOV_FILES=$(patsubst %.cpp,%.gcov,$(SOURCES))

all: coverage

test: $(OBJS)
	$(CXX) -o test $(CXXFLAGS) $(OBJS)

clean:
	rm -rf $(BUILD_DIR) *~ */*~ *.gcov */*.gcov test $(COVERAGE_DIR)

run: test
	./test

coverage: run $(GCOV_FILES)
	mkdir -p $(COVERAGE_DIR)
	mv *.gcov $(COVERAGE_DIR)

build/%.o: %.cpp
	mkdir -p $(dir $@)
	$(CXX) -c $(CXXFLAGS) -o $@ $<

%.gcov: %.cpp
	gcov -o $(BUILD_DIR)/$(dir $<) -l -p $<