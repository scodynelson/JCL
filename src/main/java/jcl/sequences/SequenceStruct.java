/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.numbers.IntegerStruct;

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

	default IntegerStruct length() {
		// TODO: Do this right later...

		final List<LispStruct> asJavaList = getAsJavaList();
		final int size = asJavaList.size();
		return new IntegerStruct(BigInteger.valueOf(size));
	}

	default SequenceStruct reverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	default SequenceStruct nReverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}
}
