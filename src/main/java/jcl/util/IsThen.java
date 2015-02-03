package jcl.util;

import java.util.function.Consumer;

public class IsThen<S, T> {

	final Then<S> parent;
	private final S object;
	private final Class<T> clazz;

	IsThen(final Then<S> parent, final S object, final Class<T> clazz) {
		this.parent = parent;
		this.object = object;
		this.clazz = clazz;
	}

	public Then<S> then(final Consumer<T> thenConsumer) {
		if (isSameClass()) {
			thenConsumer.accept(cast());
			return new TerminalThen<>();
		}
		return parent;
	}

	@SuppressWarnings("unchecked")
	private T cast() {
		return (T) object;
	}

	private boolean isSameClass() {
		return (object != null) && clazz.isAssignableFrom(object.getClass());
	}
}
