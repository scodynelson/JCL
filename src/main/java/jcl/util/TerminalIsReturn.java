package jcl.util;

import java.util.function.Function;

class TerminalIsReturn<S, T, R> extends IsReturn<S, T, R> {

	private final R result;

	TerminalIsReturn(final S object, final R result) {
		super(object, null);
		this.result = result;
	}

	@Override
	public ThenReturn<S, R> thenReturn(final Function<T, R> resultFunction) {
		return new TerminalThenReturn<>(object, result);
	}

	@Override
	public ThenReturn<S, R> thenReturn(final R result) {
		return new TerminalThenReturn<>(object, this.result);
	}
}
