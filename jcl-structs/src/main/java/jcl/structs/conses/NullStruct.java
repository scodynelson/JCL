package jcl.structs.conses;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.conses.Null;

import java.util.Collections;
import java.util.List;

public class NullStruct extends ListStruct {

	public static final NullStruct INSTANCE = new NullStruct();

	private NullStruct() {
	}

	@Override
	public LispType getType() {
		return Null.INSTANCE;
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public LispStruct getFirst() {
		return INSTANCE;
	}

	@Override
	public List<LispStruct> getRest() {
		return Collections.emptyList();
	}

	@Override
	public LispStruct getElement(final int index) {
		return INSTANCE;
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		return Collections.emptyList();
	}

	@Override
	public boolean isDotted() {
		return false;
	}
}
