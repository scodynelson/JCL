package jcl.lists;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.sequences.SequenceStruct;
import jcl.symbols.NILStruct;
import org.apache.commons.collections4.CollectionUtils;

/**
 * The {@link ListStruct} is the object representation of a Lisp 'list' type.
 */
public interface ListStruct extends SequenceStruct, Iterable<LispStruct> {

	/**
	 * Returns the size of the list.
	 *
	 * @return the size of the list
	 */
	int size();

	LispStruct getCar();

	LispStruct getCdr();

	/**
	 * Returns the first element in the list.
	 *
	 * @return the first element in the list
	 */
	LispStruct getFirst();

	/**
	 * Returns all but the first element in the list.
	 *
	 * @return all but the first element in the list
	 */
	ListStruct getRest();

	/**
	 * Returns the last element in the list.
	 *
	 * @return the last element in the list
	 */
	ListStruct getLast();

	/**
	 * Returns all but the last element in the list.
	 *
	 * @return all but the last element in the list
	 */
	ListStruct getAllButLast();

	/**
	 * Returns the element at the provided {@code index} location in the list.
	 *
	 * @param index
	 * 		the location of the element to retrieve
	 *
	 * @return the element at the provided {@code index} location in the list.
	 */
	LispStruct getElement(int index);

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
	void setElement(int index, LispStruct newValue);

	/**
	 * Determines if the list is a dotted list.
	 *
	 * @return if the list is a dotted list
	 */
	boolean isDotted();

	/**
	 * Determines if the list is a circular list.
	 *
	 * @return if the list is a circular list
	 */
	boolean isCircular();

	/**
	 * Determines if the list is a proper list.
	 *
	 * @return if the list is a proper list
	 */
	default boolean isProper() {
		return !isDotted() && !isCircular();
	}

	Stream<LispStruct> stream();

	Stream<LispStruct> parallelStream();

	LispStruct[] toArray();

	ListStruct copyTree();

	ListStruct copyList();

	ListStruct copyAlist();

	Long listLength();

	boolean tailp(LispStruct object);

	ListStruct ldiff(LispStruct object);

	ListStruct nthCdr(long n);

	LispStruct getProperty(LispStruct indicator, LispStruct defaultValue);

	ListStruct setProperty(LispStruct indicator, LispStruct newValue);

	boolean removeProperty(LispStruct indicator);

	ValuesStruct getProperties(ListStruct indicators);

	// BUILDERS

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	static ListStruct buildProperList(final LispStruct... lispStructs) {
		return (lispStructs.length == 0) ? NILStruct.INSTANCE : getProperList(Arrays.asList(lispStructs));
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	static ListStruct buildProperList(final List<? extends LispStruct> lispStructs) {
		return CollectionUtils.isEmpty(lispStructs) ? NILStruct.INSTANCE : getProperList(lispStructs);
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	static ListStruct getProperList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = CollectionUtils.isEmpty(rest) ? NILStruct.INSTANCE : getProperList(rest);
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
	static ListStruct buildDottedList(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NILStruct.INSTANCE;
		} else if (lispStructs.length == 1) {
			final LispStruct firstElement = lispStructs[0];
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return new ConsStruct(firstElement, NILStruct.INSTANCE);
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
	static ListStruct buildDottedList(final List<? extends LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NILStruct.INSTANCE;
		} else if (lispStructs.size() == 1) {
			final LispStruct firstElement = lispStructs.get(0);
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return new ConsStruct(firstElement, NILStruct.INSTANCE);
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
	static ListStruct getDottedList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = (rest.size() == 1) ? lispStructs.get(1) : getDottedList(rest);
		return new ConsStruct(car, cdr);
	}
}
