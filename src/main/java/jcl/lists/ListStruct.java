package jcl.lists;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.sequences.SequenceStruct;
import jcl.symbols.NILStruct;
import org.apache.commons.collections4.CollectionUtils;

/**
 * The {@link ListStruct} is the object representation of a Lisp 'list' type.
 */
public interface ListStruct extends SequenceStruct, Iterable<LispStruct> {

	LispStruct getCar();

	void setCar(LispStruct car);

	LispStruct getCdr();

	void setCdr(LispStruct cdr);

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

	ListStruct copyTree();

	ListStruct copyList();

	ListStruct copyAlist();

	Long listLength();

	boolean tailp(LispStruct object);

	ListStruct ldiff(LispStruct object);

	LispStruct nth(long index);

	void setNth(long index, LispStruct newValue);

	ListStruct nthCdr(long n);

	LispStruct getProperty(LispStruct indicator, LispStruct defaultValue);

	ListStruct setProperty(LispStruct indicator, LispStruct newValue);

	boolean removeProperty(LispStruct indicator);

	ValuesStruct getProperties(ListStruct indicators);

	default LispStruct last() {
		return last(1);
	}

	LispStruct last(long n);

	default ListStruct butLast() {
		return butLast(1);
	}

	ListStruct butLast(long n);

	default ListStruct nButLast() {
		return nButLast(1);
	}

	ListStruct nButLast(long n);

	static LispStruct append() {
		return NILStruct.INSTANCE;
	}

	static LispStruct append(final LispStruct object) {
		return object;
	}

	static LispStruct append(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.equals(list)) {
				continue;
			}

			final ListStruct copyList = list.copyList();
			if (NILStruct.INSTANCE.equals(result)) {
				result = copyList;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ListStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ListStruct lastOfResult = (ListStruct) last;
			lastOfResult.setCdr(copyList);
		}

		if (NILStruct.INSTANCE.equals(result)) {
			return object;
		}

		final LispStruct last = result.last();
		if (!(last instanceof ListStruct)) {
			throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
		}
		final ListStruct lastOfResult = (ListStruct) last;
		lastOfResult.setCdr(object);

		return result;
	}

	static LispStruct nConc() {
		return NILStruct.INSTANCE;
	}

	static LispStruct nConc(final LispStruct object) {
		return object;
	}

	static LispStruct nConc(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.equals(list)) {
				continue;
			}

			if (NILStruct.INSTANCE.equals(result)) {
				result = list;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ListStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ListStruct lastOfResult = (ListStruct) last;
			lastOfResult.setCdr(list);
		}

		if (NILStruct.INSTANCE.equals(result)) {
			return object;
		}

		final LispStruct last = result.last();
		if (!(last instanceof ListStruct)) {
			throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
		}
		final ListStruct lastOfResult = (ListStruct) last;
		lastOfResult.setCdr(object);

		return result;
	}

	default LispStruct revAppend(final LispStruct tail) {
		final ListStruct reverse = reverse();
		return nConc(Collections.singletonList(reverse), tail);
	}

	default LispStruct nReconc(final LispStruct tail) {
		final ListStruct nReverse = nReverse();
		return nConc(Collections.singletonList(nReverse), tail);
	}

	static ListStruct makeList(final Long size, final LispStruct initialElement) {
		ListStruct result = NILStruct.INSTANCE;
		for (long l = 0; l < size; l++) {
			result = new ConsStruct(initialElement, result);
		}
		return result;
	}

	@Override
	ListStruct reverse();

	@Override
	ListStruct nReverse();

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
