package jcl.structs.conses;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.sequences.SequenceStruct;
import jcl.types.conses.List;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Arrays;

/**
 * The {@code ListStruct} is the object representation of a Lisp 'list' type.
 */
public abstract class ListStruct extends BuiltInClassStruct implements SequenceStruct {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected ListStruct(final java.util.List<Class<LispStruct>> directSuperClasses, final java.util.List<Class<LispStruct>> subClasses) {
		this(List.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the list object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected ListStruct(final List type,
						 final java.util.List<Class<LispStruct>> directSuperClasses, final java.util.List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	/**
	 * This method returns the size of the list.
	 *
	 * @return the size of the list
	 */
	public abstract int size();

	/**
	 * This method returns the first element in the list.
	 *
	 * @return the first element in the list
	 */
	public abstract LispStruct getFirst();

	/**
	 * This method returns all but the first element in the list.
	 *
	 * @return all but the first element in the list
	 */
	public abstract ListStruct getRest();

	/**
	 * This method returns the element at the provided {@code index} location in the list.
	 *
	 * @param index the location of the element to retrieve
	 * @return the element at the provided {@code index} location in the list.
	 */
	public abstract LispStruct getElement(int index);

	/**
	 * This method set the value of the element at the provided {@code index} location in the list to the provided
	 * {@code newValue} object.
	 *
	 * @param index    the location of the element to set
	 * @param newValue the new value to set
	 */
	public abstract void setElement(int index, LispStruct newValue);

	/**
	 * This method returns the Lisp list as a Java list.
	 *
	 * @return the Lisp list as a Java list
	 */
	public abstract java.util.List<LispStruct> getAsJavaList();

	/**
	 * This method determines if the list is a dotted list.
	 *
	 * @return if the list is a dotted list
	 */
	public abstract boolean isDotted();

	/**
	 * This method determines if the list is a circular list.
	 *
	 * @return if the list is a circular list
	 */
	public abstract boolean isCircular();

	/**
	 * This method determines if the list is a proper list.
	 *
	 * @return if the list is a proper list
	 */
	public boolean isProper() {
		return !isDotted() && !isCircular();
	}

	@Override
	public String toString() {
		return "ListStruct{}";
	}

	// BUILDERS

	/**
	 * This factory method builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildProperList(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NullStruct.INSTANCE;
		} else {
			return getProperList(Arrays.asList(lispStructs));
		}
	}

	/**
	 * This factory method builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildProperList(final java.util.List<LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NullStruct.INSTANCE;
		} else {
			return getProperList(lispStructs);
		}
	}

	/**
	 * This factory method builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getProperList(final java.util.List<LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final java.util.List<LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr;
		if (CollectionUtils.isEmpty(rest)) {
			cdr = NullStruct.INSTANCE;
		} else {
			cdr = getProperList(rest);
		}
		return new ConsStruct(car, cdr);
	}

	/**
	 * This factory method builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildDottedList(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NullStruct.INSTANCE;
		} else if (lispStructs.length == 1) {
			return new ConsStruct(lispStructs[0], NullStruct.INSTANCE);
		} else {
			return getDottedList(Arrays.asList(lispStructs));
		}
	}

	/**
	 * This factory method builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct buildDottedList(final java.util.List<LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NullStruct.INSTANCE;
		} else if (lispStructs.size() == 1) {
			return new ConsStruct(lispStructs.get(0), NullStruct.INSTANCE);
		} else {
			return getDottedList(lispStructs);
		}
	}

	/**
	 * This factory method builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs the list elements
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getDottedList(final java.util.List<LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final java.util.List<LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr;
		if (rest.size() == 1) {
			cdr = lispStructs.get(1);
		} else {
			cdr = getDottedList(rest);
		}

		return new ConsStruct(car, cdr);
	}
}
