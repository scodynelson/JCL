package jcl.classes;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link ClassStruct} is the object representation of a Lisp 'class' type.
 */
public abstract class ClassStruct extends StandardObjectStruct {

	private static final long serialVersionUID = 8395096559216207722L;

	private final LispType type;

	private final List<Class<LispStruct>> directSuperClasses;

	private final List<Class<LispStruct>> subClasses;

	/**
	 * Protected constructor.
	 */
	protected ClassStruct() {
		this(null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 */
	protected ClassStruct(final String documentation) {
		this(documentation, jcl.types.Class.INSTANCE, null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ClassStruct(final LispType type,
	                      final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(null, type, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ClassStruct(final String documentation, final LispType type,
	                      final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(documentation);
		this.type = type;
		this.directSuperClasses = (directSuperClasses == null) ? Collections.emptyList() : directSuperClasses;
		this.subClasses = (subClasses == null) ? Collections.emptyList() : subClasses;
	}

	@Override
	public LispType getType() {
		return type;
	}

	/**
	 * Getter for class {@link #directSuperClasses} property.
	 *
	 * @return class {@link #directSuperClasses} property
	 */
	public List<Class<LispStruct>> getDirectSuperClasses() {
		return directSuperClasses;
	}

	/**
	 * Getter for standard object {@link #subClasses} property.
	 *
	 * @return standard object {@link #subClasses} property
	 */
	public List<Class<LispStruct>> getSubClasses() {
		return subClasses;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(type)
		                            .append(directSuperClasses)
		                            .append(subClasses)
		                            .toHashCode();
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
		final ClassStruct rhs = (ClassStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(type, rhs.type)
		                          .append(directSuperClasses, rhs.directSuperClasses)
		                          .append(subClasses, rhs.subClasses)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(type)
		                                                                .append(directSuperClasses)
		                                                                .append(subClasses)
		                                                                .toString();
	}
}
