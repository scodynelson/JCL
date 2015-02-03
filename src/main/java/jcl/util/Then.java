package jcl.util;

import java.util.function.Consumer;

public class Then<S> {

	private final S object;

	Then(final S object) {
		this.object = object;
	}

	public <T> IsThen<S, T> isInstanceOf(final Class<T> clazz) {
		return new IsThen<>(this, object, clazz);
	}

	public void otherwise(final Consumer<S> otherwiseConsumer) {
		otherwiseConsumer.accept(object);
	}
}
