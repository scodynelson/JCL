package jcl.util;

import java.util.function.Consumer;
import java.util.function.Function;

public class Is<S, T> {

	private final Then<S> parent;
	private final S object;
	private final Class<T> clazz;

	Is(final Then<S> parent, final S object, final Class<T> clazz) {
		this.parent = parent;
		this.object = object;
		this.clazz = clazz;
	}

	public Then<S> then(final Consumer<T> thenBlock) {
		if (isSameClass()) {
			thenBlock.accept(cast());
			return new TerminalThen<>();
		}
		return parent;
	}

	public <R> ThenReturn<S, R> thenReturn(final Function<T, R> resultFunction) {
		if (isSameClass()) {
			return new TerminalThenReturn<>(object, resultFunction.apply(cast()));
		}
		return new ThenReturn<>(object);
	}

	public <R> ThenReturn<S, R> thenReturn(final R result) {
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
