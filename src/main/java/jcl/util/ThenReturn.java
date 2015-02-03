package jcl.util;

import java.util.Optional;
import java.util.function.Function;

public class ThenReturn<S, R> {

	final S object;

	public ThenReturn(final S object) {
		this.object = object;
	}

	public <T> IsReturn<S, T, R> isInstanceOf(final Class<T> clazz) {
		return new IsReturn<>(object, clazz);
	}

	public Optional<R> get() {
		return Optional.<R>empty();
	}

	public R otherwise(final Function<S, R> resultFunction) {
		return resultFunction.apply(object);
	}

	public R otherwise(final R result) {
		return result;
	}
}
