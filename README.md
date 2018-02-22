# can-lite-vhdl
A lightweight Controller Area Network (CAN) controller in VHDL

This code provides a simple FIFO interface to a CAN controller. It was developed for using in conjunction with a CANopen controller on an FPGA with limited resources, hence the minimal interface and selected status flags.  As CANopen primarily uses standard CAN frames, extended frames are unsupported (trasmit or receive).

`src/CanBus_pkg.vhd` defines some data types to abstract bit ordering.

`src/CanLite.vhd` contains the top-level VHDL module (CanLite), as well as the submodules it depends on.

`test/CanLite_tb.vhd` is a simple testbench that connects two nodes on a virtual CAN bus.
