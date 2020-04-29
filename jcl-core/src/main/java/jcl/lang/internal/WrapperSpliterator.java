package jcl.lang.internal;

import java.util.Spliterator;
import java.util.function.Consumer;

import jcl.lang.LispStruct;

class WrapperSpliterator<TYPE extends LispStruct> implements Spliterator<LispStruct> {

	private Spliterator<TYPE> spliterator;

	WrapperSpliterator(final Spliterator<TYPE> spliterator) {
		this.spliterator = spliterator;
	}

	@Override
	public boolean tryAdvance(final Consumer<? super LispStruct> action) {
		return spliterator.tryAdvance(action);
	}

	@Override
	public void forEachRemaining(final Consumer<? super LispStruct> action) {
		spliterator.forEachRemaining(action);
	}

	@Override
	public Spliterator<LispStruct> trySplit() {
		return new WrapperSpliterator<>(spliterator.trySplit());
	}

	@Override
	public long estimateSize() {
		return spliterator.estimateSize();
	}

	@Override
	public int characteristics() {
		return spliterator.characteristics();
	}
}
