/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences;

import jcl.LispStruct;

import java.util.List;

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
