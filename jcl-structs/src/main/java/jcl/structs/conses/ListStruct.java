package jcl.structs.conses;

import jcl.structs.LispStruct;
import jcl.structs.sequences.SequenceStruct;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Arrays;
import java.util.List;

public abstract class ListStruct implements SequenceStruct {

	public abstract int size();

	public abstract LispStruct getFirst();

	public abstract List<LispStruct> getRest();

	public abstract LispStruct getElement(int index);

	public abstract List<LispStruct> getAsJavaList();

	public abstract boolean isDotted();

	// BUILDERS

	public static ListStruct getStruct() {
		return NullStruct.INSTANCE;
	}

	public static ListStruct getStruct(final LispStruct lispStruct) {
		return ConsStruct.getStruct(lispStruct, NullStruct.INSTANCE);
	}

	public static ListStruct getStruct(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NullStruct.INSTANCE;
		} else {
			return getCons(Arrays.asList(lispStructs));
		}
	}

	public static ListStruct getStruct(final List<? extends LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NullStruct.INSTANCE;
		} else {
			return getCons(lispStructs);
		}
	}

	public static ListStruct getStruct(final boolean isDotted, final List<? extends LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NullStruct.INSTANCE;
		} else {
			if (isDotted) {
				return getDottedCons(lispStructs);
			} else {
				return getCons(lispStructs);
			}
		}
	}

	// TODO: do these contructors better
	private static ListStruct getDottedCons(final List<? extends LispStruct> lispStructs) {

		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr;
		if (CollectionUtils.size(rest) > 1) {
			cdr = getDottedCons(rest);
		} else if (CollectionUtils.isNotEmpty(rest)) {
			cdr = lispStructs.get(1);
		} else {
			// TODO: handle exception here...
			cdr = null;
		}

		return ConsStruct.getStruct(car, cdr);
	}

	private static ListStruct getCons(final List<? extends LispStruct> lispStructs) {

		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr;
		if (CollectionUtils.isEmpty(rest)) {
			cdr = NullStruct.INSTANCE;
		} else {
			cdr = getCons(rest);
		}

		return ConsStruct.getStruct(car, cdr);
	}
}
