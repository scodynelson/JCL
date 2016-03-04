package jcl.lists;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.sequences.SequenceStruct;
import jcl.types.ListType;
import org.apache.commons.collections4.CollectionUtils;

/**
 * The {@link ListStruct} is the object representation of a Lisp 'list' type.
 */
public abstract class ListStruct extends BuiltInClassStruct implements SequenceStruct, Iterable<LispStruct> {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ListStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(ListType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the list object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ListStruct(final ListType type,
	                     final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	/**
	 * Returns the size of the list.
	 *
	 * @return the size of the list
	 */
	public abstract int size();

	public LispStruct getCar() {
		return getFirst();
	}

	public LispStruct getCdr() {
		return getRest();
	}

	/**
	 * Returns the first element in the list.
	 *
	 * @return the first element in the list
	 */
	public abstract LispStruct getFirst();

	/**
	 * Returns all but the first element in the list.
	 *
	 * @return all but the first element in the list
	 */
	public abstract ListStruct getRest();

	/**
	 * Returns the last element in the list.
	 *
	 * @return the last element in the list
	 */
	public abstract ListStruct getLast();

	/**
	 * Returns all but the last element in the list.
	 *
	 * @return all but the last element in the list
	 */
	public abstract ListStruct getAllButLast();

	/**
	 * Returns the element at the provided {@code index} location in the list.
	 *
	 * @param index
	 * 		the location of the element to retrieve
	 *
	 * @return the element at the provided {@code index} location in the list.
	 */
	public abstract LispStruct getElement(int index);

	/**
	 * Sets the value of the element at the provided {@code index} location in the list to the provided {@code
	 * newValue}
	 * object.
	 *
	 * @param index
	 * 		the location of the element to set
	 * @param newValue
	 * 		the new value to set
	 */
	public abstract void setElement(int index, LispStruct newValue);

	/**
	 * Determines if the list is a dotted list.
	 *
	 * @return if the list is a dotted list
	 */
	public abstract boolean isDotted();

	/**
	 * Determines if the list is a circular list.
	 *
	 * @return if the list is a circular list
	 */
	public abstract boolean isCircular();

	/**
	 * Determines if the list is a proper list.
	 *
	 * @return if the list is a proper list
	 */
	public boolean isProper() {
		return !isDotted() && !isCircular();
	}

	public abstract Stream<LispStruct> stream();

	public abstract Stream<LispStruct> parallelStream();

	public abstract LispStruct[] toArray();

	public abstract ListStruct copyTree();

	public abstract ListStruct copyList();

	public abstract Long listLength();

	// BUILDERS

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildProperList(final LispStruct... lispStructs) {
		return (lispStructs.length == 0) ? NullStruct.INSTANCE : getProperList(Arrays.asList(lispStructs));
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildProperList(final List<? extends LispStruct> lispStructs) {
		return CollectionUtils.isEmpty(lispStructs) ? NullStruct.INSTANCE : getProperList(lispStructs);
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getProperList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = CollectionUtils.isEmpty(rest) ? NullStruct.INSTANCE : getProperList(rest);
		return new ConsStruct(car, cdr);
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildDottedList(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NullStruct.INSTANCE;
		} else if (lispStructs.length == 1) {
			final LispStruct firstElement = lispStructs[0];
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return new ConsStruct(firstElement, NullStruct.INSTANCE);
		} else {
			return getDottedList(Arrays.asList(lispStructs));
		}
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildDottedList(final List<? extends LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NullStruct.INSTANCE;
		} else if (lispStructs.size() == 1) {
			final LispStruct firstElement = lispStructs.get(0);
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return new ConsStruct(firstElement, NullStruct.INSTANCE);
		} else {
			return getDottedList(lispStructs);
		}
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getDottedList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = (rest.size() == 1) ? lispStructs.get(1) : getDottedList(rest);
		return new ConsStruct(car, cdr);
	}
}
