/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.sequence;

import java.util.stream.Stream;

import jcl.lang.LispStruct;

/**
 * The {@link SequenceStruct} is the object representation of a Lisp 'sequence' type.
 */
public interface SequenceStruct extends LispStruct {

	default Stream<LispStruct> stream() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default Stream<LispStruct> parallelStream() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default LispStruct[] toArray() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default Long length() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default LispStruct elt(final long index) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default void setElt(final long index, final LispStruct newValue) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default SequenceStruct reverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default SequenceStruct nReverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}
}
