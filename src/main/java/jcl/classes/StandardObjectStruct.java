package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StandardObject;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct implements LispStruct {

	private static final long serialVersionUID = -6010971799794147777L;

	protected String documentation;

	/**
	 * Protected constructor.
	 */
	protected StandardObjectStruct() {
		documentation = null;
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 */
	protected StandardObjectStruct(final String documentation) {
		this.documentation = documentation;
	}

	/**
	 * Getter for standard object {@link #documentation} property.
	 *
	 * @return standard object {@link #documentation} property
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Setter for standard object {@link #documentation} property.
	 *
	 * @param documentation
	 * 		new standard object {@link #documentation} property value
	 */
	public void setDocumentation(final String documentation) {
		this.documentation = documentation;
	}

	@Override
	public LispType getType() {
		return StandardObject.INSTANCE;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(documentation)
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
		final StandardObjectStruct rhs = (StandardObjectStruct) obj;
		return new EqualsBuilder().append(documentation, rhs.documentation)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(documentation)
		                                                                .toString();
	}
}
