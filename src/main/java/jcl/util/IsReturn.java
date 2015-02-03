package jcl.util;

import java.util.function.Function;

public class IsReturn<S, T, R> {

	final S object;
	private final Class<T> clazz;

	public IsReturn(final S object, final Class<T> clazz) {
		this.object = object;
		this.clazz = clazz;
	}

	public ThenReturn<S, R> thenReturn(final Function<T, R> resultFunction) {
		if (isSameClass()) {
			return new TerminalThenReturn<>(object, resultFunction.apply(cast()));
		}
		return new ThenReturn<>(object);
	}

	public ThenReturn<S, R> thenReturn(final R result) {
		if (isSameClass()) {
			return new TerminalThenReturn<>(object, result);
		}
		return new ThenReturn<>(object);
	}

	@SuppressWarnings("unchecked")
	private T cast() {
		return (T) object;
	}

	private boolean isSameClass() {
		return (object != null) && clazz.isAssignableFrom(object.getClass());
	}
}
