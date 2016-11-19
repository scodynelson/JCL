package jcl.lang.internal.array;

import java.util.List;

import jcl.lang.CharacterStruct;
import jcl.lang.SimpleStringStruct;
import jcl.type.CharacterType;

public class SimpleStringStructImpl extends StringStructImpl implements SimpleStringStruct {

	private SimpleStringStructImpl(final String stringValue) {
		super(stringValue);
	}

	private SimpleStringStructImpl(final int size, final List<CharacterStruct> contents, final CharacterType elementType, final boolean isAdjustable, final Integer fillPointer) {
		super(size, contents, elementType, isAdjustable, fillPointer);
	}
}
