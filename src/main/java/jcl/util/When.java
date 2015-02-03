package jcl.util;

public class When<S> {

	private final S object;

	When(final S object) {
		this.object = object;
	}

	public <T> Is<S, T> isInstanceOf(final Class<T> clazz) {
		return new Is<>(new Then<>(object), object, clazz);
	}
}
