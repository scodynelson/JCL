package jcl.lang.internal;

import java.util.Iterator;
import java.util.function.Consumer;

import jcl.lang.LispStruct;

final class WrapperIterator<TYPE extends LispStruct> implements Iterator<LispStruct> {

	private final Iterator<TYPE> iterator;

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
