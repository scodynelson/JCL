package jcl.structs.conses;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.conses.Cons;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ConsStruct extends ListStruct {

	private final LispStruct car;
	private final LispStruct cdr;

	private ConsStruct(final LispStruct car, final LispStruct cdr) {
		this.car = car;
		this.cdr = cdr;
	}

	public LispStruct getCar() {
		return car;
	}

	public LispStruct getCdr() {
		return cdr;
	}

	@Override
	public LispType getType() {
		return Cons.INSTANCE;
	}

	@Override
	public int size() {
		if (cdr == NullStruct.INSTANCE) {
			return 1;
		} else if (cdr instanceof ListStruct) {
			return 1 + ((ListStruct) cdr).size();
		} else {
			return 2;
		}
	}

	@Override
	public LispStruct getFirst() {
		return car;
	}

	@Override
	public List<LispStruct> getRest() {
		// TODO: do this right...
		if (cdr instanceof ListStruct) {
			return ((ListStruct) cdr).getAsJavaList();
		} else {
			return Collections.singletonList(cdr);
		}
	}

	@Override
	public LispStruct getElement(final int index) {
		// TODO: do this right...
		if (index == 0) {
			return car;
		} else if (cdr instanceof ListStruct) {
			return ((ListStruct) cdr).getElement(index - 1);
		} else {
			return cdr;
		}
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		if (cdr == NullStruct.INSTANCE) {
			final List<LispStruct> javaList = new ArrayList<>();
			javaList.add(car);
			return javaList;
		} else if (cdr instanceof ListStruct) {
			final List<LispStruct> javaList = new ArrayList<>();
			javaList.add(car);
			javaList.addAll(((ListStruct) cdr).getAsJavaList());
			return javaList;
		} else {
			final List<LispStruct> javaList = new ArrayList<>();
			javaList.add(car);
			javaList.add(cdr);
			return javaList;
		}
	}

	@Override
	public boolean isDotted() {
		return (cdr != NullStruct.INSTANCE) && (!(cdr instanceof ListStruct) || ((ListStruct) cdr).isDotted());
	}

	@Override
	public String toString() {
		return "ConsStruct{"
				+ "car=" + car
				+ ", cdr=" + cdr
				+ '}';
	}

	// BUILDERS

	public static ConsStruct getStruct(final LispStruct car, final LispStruct cdr) {
		return new ConsStruct(car, cdr);
	}
}
