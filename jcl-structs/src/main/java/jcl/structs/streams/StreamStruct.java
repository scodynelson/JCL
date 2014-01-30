package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.streams.Stream;

public class StreamStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Stream.INSTANCE;
	}
}
