package jcl.lang.internal;

import java.util.Iterator;
import java.util.NoSuchElementException;

import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.condition.exception.TypeErrorException;

/**
 * Iterator for {@link ConsStruct} structures. Conses are "linked-list" structures.
 */
final class ConsIterator implements Iterator<LispStruct> {

	/**
	 * The current node in the linked cons structure.
	 */
	private ListStruct current;

	/**
	 * Constructor for building the iterator.
	 *
	 * @param cons
	 * 		the {@link ConsStruct} to be iterated over
	 */
	ConsIterator(final ConsStruct cons) {
		current = cons;
	}

	@Override
	public boolean hasNext() {
		return current instanceof ConsStruct;
	}

	@Override
	public LispStruct next() {
		if (!hasNext()) {
			throw new NoSuchElementException("No elements left in the Cons.");
		}
		final ConsStruct currentAsCons = (ConsStruct) current;

		final LispStruct currentCdr = currentAsCons.cdr();
		if (!(currentCdr instanceof ListStruct)) {
			throw new TypeErrorException("Not a proper list.");
		}
		current = (ListStruct) currentCdr;
		return currentAsCons.car();
	}
}
