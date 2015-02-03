package jcl.util;

public final class InstanceOf {

	private InstanceOf() {
	}

	public static <S> When<S> when(final S object) {
		return new When<>(object);
	}
}
