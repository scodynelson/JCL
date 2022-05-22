package jcl.lang.internal;

import java.util.Spliterator;
import java.util.function.Consumer;

import jcl.lang.LispStruct;

/**
 * Spliterator for wrapping a more specific {@link LispStruct} type with a generic {@link Spliterator}.
 *
 * @param <TYPE>
 * 		the wrapped type of iterable contents
 */
class WrapperSpliterator<TYPE extends LispStruct> implements Spliterator<LispStruct> {

	/**
	 * The wrapped {@link Spliterator} for the type.
	 */
	private final Spliterator<TYPE> spliterator;

	/**
	 * Constructor for building the spliterator.
	 *
	 * @param spliterator
	 * 		the {@link Spliterator} to wrap and delegate to
	 */
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
