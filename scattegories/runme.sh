#!/bin/bash

# Function to compile the rebar3 directory
compile_rebar3_directory() {
  echo "Compiling rebar3 directory..."
  rebar3 compile
}

start_erlang_interpreter_with_network() {
  local name=$1
  echo "Starting Erlang interpreter with module name $name and the network"
  
  erl -pa _build/default/lib/*/ebin -sname "$name" -eval 'p_network:start().'
}
start_erlang_interpreter() {
  local name=$1
  echo "Starting Erlang interpreter with module name $name..."
  erl -pa _build/default/lib/*/ebin -sname "$name"
}

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null
then
  echo "rebar3 is not installed. Please install rebar3 and try again."
  exit 1
fi

# Prompt the user for input
echo "Enter the name of your username (or press Enter to skip):"
read name
DEFAULT_NETWORK="scattegories_network"
# Compile the rebar3 directory
compile_rebar3_directory

# Start the Erlang interpreter with the compiled modules in the code path and the specified module name
if [ "$name" == "scattegories_network" ]; then
  start_erlang_interpreter_with_network "$name"
elif [[ -n "$name" ]]; then
  start_erlang_interpreter "$name"
else
  start_erlang_interpreter "no_name"
fi