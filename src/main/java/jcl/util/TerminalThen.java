package jcl.util;

import java.util.function.Consumer;

public class TerminalThen<S> extends Then<S> {

	public TerminalThen() {
		super(null);
	}

	@Override
	public <T> IsThen<S, T> isInstanceOf(final Class<T> clazz) {
		return new TerminalIsThen<>(this, null, null);
	}

	@Override
	public void otherwise(final Consumer<S> otherwiseConsumer) {
	}
}
