package jcl.util;

import java.util.function.BiFunction;
import java.util.function.Consumer;

public final class ClassMatcher {

	private final BiFunction<Object, Consumer<Object>, Boolean> binder;

	private ClassMatcher(final BiFunction<Object, Consumer<Object>, Boolean> next) {
		binder = next;
	}

	public void exec(final Object o) {
		binder.apply(o, null);
	}

	@SuppressWarnings("unchecked")
	public <Y> ClassMatcher with(final Class<Y> targetClass, final Consumer<Y> consumer) {
		return new ClassMatcher((obj, next) -> {
			if (binder.apply(obj, next)) {
				return true;
			}

			if (targetClass.isAssignableFrom(obj.getClass())) {
				final Y as = (Y) obj;
				consumer.accept(as);
				return true;
			}

			return false;
		});
	}

	public ClassMatcher fallthrough(final Consumer<Object> consumer) {
		return new ClassMatcher((obj, next) -> {
			if (binder.apply(obj, next)) {
				return true;
			}

			consumer.accept(obj);

			return true;
		});
	}

	public static ClassMatcher match() {
		return new ClassMatcher((x, y) -> false);
	}

/*
match().with(Payload.class, this::handlePayload)
           .with(ResizeWorkLoadMessage.class, this::processResize)
           .with(HeartBeat.class, this::heartBeat)
           .fallthrough(i -> logger.with(i).warn("Unknown type called to actor, cannot route"))
           .exec(message);
 */
}
