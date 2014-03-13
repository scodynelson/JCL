package jcl.structs;

import jcl.types.Cons;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The {@code ConsStruct} is the object representation of a Lisp 'cons' type.
 */
public class ConsStruct extends ListStruct {

	private LispStruct car;
	private LispStruct cdr;

	/**
	 * Public constructor.
	 *
	 * @param car the car of the binary cons structure
	 */
	public ConsStruct(final LispStruct car) {
		this(car, NullStruct.INSTANCE);
	}

	/**
	 * Public constructor.
	 *
	 * @param car the car of the binary cons structure
	 * @param cdr the cdr of the binary cons structure
	 */
	public ConsStruct(final LispStruct car, final LispStruct cdr) {
		super(Cons.INSTANCE, null, null);
		this.car = car;
		this.cdr = cdr;
	}

	/**
	 * Getter for cons car property.
	 *
	 * @return cons car property
	 */
	public LispStruct getCar() {
		return car;
	}

	/**
	 * Setter for cons car property.
	 *
	 * @param car new cons car property value
	 */
	public void setCar(final LispStruct car) {
		this.car = car;
	}

	/**
	 * Getter for cons cdr property.
	 *
	 * @return cons cdr property
	 */
	public LispStruct getCdr() {
		return cdr;
	}

	/**
	 * Setter for cons cdr property.
	 *
	 * @param cdr new cons cdr property value
	 */
	public void setCdr(final LispStruct cdr) {
		this.cdr = cdr;
	}

	@Override
	public int size() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return 1 + cdrAsList.size();
		} else {
			return 2;
		}
	}

	@Override
	public LispStruct getElement(final int index) {
		if (index == 0) {
			return car;
		} else if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.getElement(index - 1);
		} else {
			return cdr;
		}
	}

	@Override
	public void setElement(final int index, final LispStruct newValue) {
		if (index == 0) {
			car = newValue;
		} else if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			cdrAsList.setElement(index - 1, newValue);
		} else {
			cdr = newValue;
		}
	}

	@Override
	public LispStruct getFirst() {
		return car;
	}

	@Override
	public ListStruct getRest() {
		if (cdr instanceof ListStruct) {
			return (ListStruct) cdr;
		} else {
			return new ConsStruct(car);
		}
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		final List<LispStruct> javaList = new ArrayList<>();
		javaList.add(car);
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			javaList.addAll(cdrAsList.getAsJavaList());
		} else {
			javaList.add(cdr);
		}
		return javaList;
	}

	@Override
	public boolean isDotted() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.isDotted();
		}
		return false;
	}

	@Override
	public boolean isCircular() {
		final Set<ConsStruct> conses = new HashSet<>();
		conses.add(this);

		return innerIsCircular(this, conses);
	}

	/**
	 * This private method tests the provided {@code consStruct} for circularity. If the consStruct itself, or any of its
	 * cons nodes are located in the provided conses list, the cons is circular. This method is recursive and will constantly
	 * populate the {@code conses} set with node values for testing.
	 *
	 * @param consStruct the cons structure to test for circularity
	 * @param conses     the set of cons nodes currently found in the tree
	 * @return whether or not the provided {@code consStruct} is circular
	 */
	private static boolean innerIsCircular(final ConsStruct consStruct, final Set<ConsStruct> conses) {

		boolean isCarCircular = false;
		final LispStruct car = consStruct.car;

		if (car instanceof ConsStruct) {
			final ConsStruct carAsCons = (ConsStruct) car;
			if (conses.contains(carAsCons)) {
				return true;
			}

			conses.add(carAsCons);
			isCarCircular = innerIsCircular(carAsCons, conses);
		}

		boolean isCdrCircular = false;
		final LispStruct cdr = consStruct.cdr;

		if (cdr instanceof ConsStruct) {
			final ConsStruct cdrAsCons = (ConsStruct) cdr;
			if (conses.contains(cdrAsCons)) {
				return true;
			}

			conses.add(cdrAsCons);
			isCdrCircular = innerIsCircular(cdrAsCons, conses);
		}

		return isCarCircular || isCdrCircular;
	}

	@Override
	public String toString() {
		if (isCircular()) {
			return "ConsStruct{'circular'}";
		} else {
			return "ConsStruct{"
					+ "car=" + car
					+ ", cdr=" + cdr
					+ '}';
		}
	}
}
