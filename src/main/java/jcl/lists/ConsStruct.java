package jcl.lists;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.symbols.NILStruct;
import jcl.types.ConsType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link ConsStruct} is the object representation of a Lisp 'cons' type.
 */
public class ConsStruct extends ListStruct {

	private static final long serialVersionUID = 4871665353563364787L;

	private LispStruct car;

	private LispStruct cdr;

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 */
	public ConsStruct(final LispStruct car) {
		this(car, NullStruct.INSTANCE);
	}

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 * @param cdr
	 * 		the cdr of the binary cons structure
	 */
	public ConsStruct(final LispStruct car, final LispStruct cdr) {
		super(ConsType.INSTANCE, null, null);
		this.car = car;
		if (NILStruct.INSTANCE.equals(cdr)) {
			this.cdr = NullStruct.INSTANCE;
		} else {
			this.cdr = cdr;
		}
	}

	/**
	 * Getter for cons {@link #car} property.
	 *
	 * @return cons {@link #car} property
	 */
	public LispStruct getCar() {
		return car;
	}

	/**
	 * Setter for cons {@link #car} property.
	 *
	 * @param car
	 * 		new cons {@link #car} property value
	 */
	public void setCar(final LispStruct car) {
		this.car = car;
	}

	/**
	 * Getter for cons {@link #cdr} property.
	 *
	 * @return cons {@link #cdr} property
	 */
	public LispStruct getCdr() {
		return cdr;
	}

	/**
	 * Setter for cons {@link #cdr} property.
	 *
	 * @param cdr
	 * 		new cons {@link #cdr} property value
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
	public LispStruct getFirst() {
		return car;
	}

	@Override
	public ListStruct getRest() {
		return (cdr instanceof ListStruct) ? (ListStruct) cdr : new ConsStruct(car);
	}

	@Override
	public ListStruct getLast() {
		return (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).getLast() : this;
	}

	@Override
	public ListStruct getAllButLast() {
		if (!(cdr instanceof ConsStruct)) {
			return NullStruct.INSTANCE;
		}

		final ConsStruct cdrAsCons = (ConsStruct) cdr;
		final ListStruct newCdr = cdrAsCons.getAllButLast();

		return new ConsStruct(car, newCdr);
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
	public boolean isDotted() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.isDotted();
		}
		return true;
	}

	@Override
	public boolean isCircular() {
		final Set<ConsStruct> conses = new HashSet<>();
		conses.add(this);

		return innerIsCircular(this, conses);
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

	/**
	 * Tests the provided {@code consStruct} for circularity. If the consStruct itself, or any of its cons nodes are
	 * located in the provided conses list, the cons is circular. This method is recursive and will constantly populate
	 * the {@code conses} set with node values for testing.
	 *
	 * @param consStruct
	 * 		the cons structure to test for circularity
	 * @param conses
	 * 		the set of cons nodes currently found in the tree
	 *
	 * @return whether or not the provided {@code consStruct} is circular
	 */
	private static boolean innerIsCircular(final ConsStruct consStruct, final Set<ConsStruct> conses) {
		return isElementCircular(consStruct.car, conses) || isElementCircular(consStruct.cdr, conses);
	}

	/**
	 * Tests the provided {@code element} for circularity. If the element is a consStruct, it tests it for circularity
	 * appropriately.
	 *
	 * @param element
	 * 		the element structure to test for circularity
	 * @param conses
	 * 		the set of cons nodes currently found in the tree
	 *
	 * @return whether or not the provided {@code element} is circular
	 */
	private static boolean isElementCircular(final LispStruct element, final Set<ConsStruct> conses) {
		final boolean isElementCircular;
		if (element instanceof ConsStruct) {
			final ConsStruct elementAsCons = (ConsStruct) element;
			if (conses.contains(elementAsCons)) {
				return true;
			}

			conses.add(elementAsCons);
			isElementCircular = innerIsCircular(elementAsCons, conses);
			conses.remove(elementAsCons);
		} else {
			isElementCircular = false;
		}
		return isElementCircular;
	}

	@Override
	public int hashCode() {
//		if (isCircular()) {
//		    // TODO: we should figure out how to handle circularities here... or should we???
//		    return new HashCodeBuilder().appendSuper(super.hashCode())
//		                                .toHashCode();
//		} else {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(car)
		                            .append(cdr)
		                            .toHashCode();
//		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ConsStruct rhs = (ConsStruct) obj;

//		if (isCircular() || rhs.isCircular()) {
//			// TODO: we should figure out how to handle circularities here... or should we???
//			return false;
//		} else {
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(car, rhs.car)
		                          .append(cdr, rhs.cdr)
		                          .isEquals();
//		}
	}

	@Override
	public String toString() {
		if (isCircular()) {
			return "ConsStruct{'circular'}";
		} else {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(car)
			                                                                .append(cdr)
			                                                                .toString();
		}
	}
}
