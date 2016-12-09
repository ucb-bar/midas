
default: simulator

SRC_DIR ?= src/main/cc/pyrite
CC = g++
CFLAGS = -Wall -std=c++11 -g -fsanitize=address

simulator: $(SRC_DIR)/modela.cc
	$(CC) $(CFLAGS) -o $@ $^


clean:
	rm -rf simulator

.PHONY: clean
