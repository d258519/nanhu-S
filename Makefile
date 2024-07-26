
idea:
	mill -i mill.idea.GenIdea/idea

clean:
	rm -rf ./build

wave:
	gtkwave ./build/workdir-default/trace.vcd