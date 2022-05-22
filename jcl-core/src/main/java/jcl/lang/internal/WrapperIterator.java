package jcl.lang.internal;

import java.util.Iterator;
import java.util.function.Consumer;

import jcl.lang.LispStruct;

/**
 * Iterator for wrapping a more specific {@link LispStruct} type with a generic {@link Iterator}.
 *
 * @param <TYPE>
 * 		the wrapped type of iterable contents
 */
final class WrapperIterator<TYPE extends LispStruct> implements Iterator<LispStruct> {

	/**
	 * The wrapped {@link Iterator} for the type.
	 */
	private final Iterator<TYPE> iterator;

	/**
	 * Constructor for building the iterator.
	 *
	 * @param iterator
	 * 		the {@link Iterator} to wrap and delegate to
	 */
	WrapperIterator(final Iterator<TYPE> iterator) {
		this.iterator = iterator;
	}

	@Override
	public boolean hasNext() {
		return iterator.hasNext();
	}

	@Override
	public LispStruct next() {
		return iterator.next();
	}

	@Override
	public void forEachRemaining(final Consumer<? super LispStruct> action) {
		iterator.forEachRemaining(action);
	}
}
