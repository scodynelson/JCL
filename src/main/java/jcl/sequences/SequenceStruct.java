/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences;

import java.util.List;

import jcl.LispStruct;

/**
 * The {@link SequenceStruct} is the object representation of a Lisp 'sequence' type.
 */
public interface SequenceStruct extends LispStruct {

	/**
	 * Returns the Lisp sequence as a Java list.
	 *
	 * @return the Lisp sequence as a Java list
	 */
	@Deprecated
	List<LispStruct> getAsJavaList();

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
