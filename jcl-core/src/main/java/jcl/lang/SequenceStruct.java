/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * The {@link SequenceStruct} is the object representation of a Lisp 'sequence' type.
 */
public interface SequenceStruct extends LispStruct, Iterable<LispStruct> {

	/**
	 * Returns the sequence as a {@link Stream} of elements.
	 *
	 * @return a {@link Stream} of elements
	 */
	default Stream<LispStruct> stream() {
		return StreamSupport.stream(spliterator(), false);
	}

	/**
	 * Returns the sequence as a {@link Stream} of elements, supporting parallel processing.
	 *
	 * @return a {@link Stream} of elements
	 */
	default Stream<LispStruct> parallelStream() {
		return StreamSupport.stream(spliterator(), true);
	}

	/**
	 * Returns the sequence as an array of elements.
	 *
	 * @return an array of elements
	 */
	default LispStruct[] toArray() {
		// TODO fill in subclass implementations
		throw new UnsupportedOperationException("Not supported yet.");
	}

	/**
	 * Returns the number of elements in the sequence.
	 * <p>
	 * If sequence is a vector with a fill pointer, the active length as specified by the fill pointer is returned.
	 *
	 * @return the number of elements in the sequence
	 */
	IntegerStruct length();

	/**
	 * Accesses the element of sequence specified by index.
	 *
	 * @param index
	 * 		the index of the element to access
	 *
	 * @return the element
	 */
	LispStruct elt(final IntegerStruct index);

	/**
	 * Sets the element of sequence specified by index to a new value.
	 *
	 * @param newElement
	 * 		new element value
	 * @param index
	 * 		the index of the element to set
	 *
	 * @return the new element
	 */
	LispStruct setfElt(final LispStruct newElement, final IntegerStruct index);

	/**
	 * Return a new sequence of the same kind as sequence, containing the same elements, but in reverse order. Never
	 * modifies the given sequence.
	 *
	 * @return a new sequence in reverse order
	 */
	SequenceStruct reverse();

	/**
	 * Return a new sequence of the same kind as sequence, containing the same elements, but in reverse order. Might
	 * modify and return the given sequence.
	 *
	 * @return a new sequence in reverse order
	 */
	SequenceStruct nReverse();
}
