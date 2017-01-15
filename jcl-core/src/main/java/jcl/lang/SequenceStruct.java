/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.stream.Stream;

/**
 * The {@link SequenceStruct} is the object representation of a Lisp 'sequence' type.
 */
public interface SequenceStruct extends LispStruct, Iterable<LispStruct> {

	default Stream<LispStruct> stream() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default Stream<LispStruct> parallelStream() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default LispStruct[] toArray() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	IntegerStruct length();

	LispStruct elt(final IntegerStruct index);

	LispStruct setfElt(final LispStruct newElement, final IntegerStruct index);

	SequenceStruct reverse();

	SequenceStruct nReverse();
}
