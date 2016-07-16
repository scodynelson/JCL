package jcl.lang;

import jcl.type.LispType;
import jcl.type.StandardObjectType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct implements LispStruct {

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
		return StandardObjectType.INSTANCE;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(documentation)
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
		final String typeClassName = getClass().getSimpleName();
		return "#<" + typeClassName + '>';
	}
}
