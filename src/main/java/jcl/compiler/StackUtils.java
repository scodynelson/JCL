package jcl.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Stack;

import lombok.experimental.UtilityClass;

@UtilityClass
public final class StackUtils {

	public static <T> Iterable<T> pushAll(final Stack<T> stack, final Iterable<T> items) {
		for (final T item : items) {
			stack.push(item);
		}
		return items;
	}

	public static <T> Collection<T> popX(final Stack<T> stack, final int numberOfItems) {

		final Collection<T> items = new ArrayList<>(numberOfItems);
		for (int i = 0; i < numberOfItems; i++) {
			final T item = stack.pop();
			items.add(item);
		}
		return items;
	}
}
