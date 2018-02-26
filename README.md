# can-lite-vhdl
A lightweight Controller Area Network (CAN) controller in VHDL

This code provides a simple FIFO interface to a CAN controller. It was developed for using in conjunction with a CANopen controller on an FPGA with limited resources, hence the minimal interface and selected status flags.  As CANopen primarily uses standard CAN frames, extended frames are unsupported (trasmit or receive).  While it is typical to use this module with an asynchronous (dual clock) FIFO, it is not required.  However, it may be difficult to achieve the appropriate CAN timing with some application clocks, and FIFOs help prevent data loss (overflow) if status bits are not continuously checked.

`src/CanBus_pkg.vhd` defines some data types to abstract bit ordering.

`src/CanLite.vhd` contains the top-level VHDL module (CanLite), as well as the submodules it depends on.

`test/CanLite_tb.vhd` is a simple testbench that uses Xilinx asynchronous FIFOs and connects two nodes on a virtual CAN bus.

Please excuse the mixed coding styles, as this code was adapted multiple times (see `src/CanLite.vhd`).  The top-level file uses the coding style of GRC-PLC-CDD, which is an internal NASA document.
