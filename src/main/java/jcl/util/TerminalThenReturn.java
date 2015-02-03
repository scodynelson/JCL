package jcl.util;

import java.util.Optional;
import java.util.function.Function;

class TerminalThenReturn<S, R> extends ThenReturn<S, R> {

	private final R result;

	TerminalThenReturn(final S object, final R result) {
		super(object);
		this.result = result;
	}

	@Override
	public <T> IsReturn<S, T, R> isInstanceOf(final Class<T> clazz) {
		return new TerminalIsReturn<>(object, result);
	}

	@Override
	public Optional<R> get() {
		return Optional.ofNullable(result);
	}

	@Override
	public R otherwise(final R result) {
		return this.result;
	}
	
	@Override
	public R otherwise(final Function<S, R> resultFunction) {
	    return result;
	}
}
