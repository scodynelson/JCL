package jcl.util;

import java.util.function.Consumer;

class TerminalIsThen<S, T> extends IsThen<S, T> {

	TerminalIsThen(final Then<S> parent, final S object, final Class<T> clazz) {
		super(parent, object, clazz);
	}

	@Override
	public Then<S> then(final Consumer<T> thenConsumer) {
		return parent;
	}
}
