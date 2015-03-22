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
	List<LispStruct> getAsJavaList();
}
