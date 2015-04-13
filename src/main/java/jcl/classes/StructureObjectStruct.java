package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.functions.FunctionStruct;
import jcl.types.StructureObjectType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	private static final long serialVersionUID = 5766790087319221572L;

	protected String documentation;

	protected final FunctionStruct defaultConstructor;

	protected final FunctionStruct printer;

	public StructureObjectStruct() {
		documentation = null;
		defaultConstructor = null;
		printer = null;
	}

	public StructureObjectStruct(final FunctionStruct defaultConstructor) {
		documentation = null;
		this.defaultConstructor = defaultConstructor;
		printer = null;
	}

	public StructureObjectStruct(final String documentation, final FunctionStruct defaultConstructor, final FunctionStruct printer) {
		this.documentation = documentation;
		this.defaultConstructor = defaultConstructor;
		this.printer = printer;
	}

	/**
	 * Getter for structure object {@link #documentation} property.
	 *
	 * @return structure object {@link #documentation} property
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Setter for structure object {@link #documentation} property.
	 *
	 * @param documentation
	 * 		new structure object {@link #documentation} property value
	 */
	public void setDocumentation(final String documentation) {
		this.documentation = documentation;
	}

	public FunctionStruct getDefaultConstructor() {
		return defaultConstructor;
	}

	public FunctionStruct getPrinter() {
		return printer;
	}

	@Override
	public LispType getType() {
		return StructureObjectType.INSTANCE;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(documentation)
		                            .append(defaultConstructor)
		                            .append(printer)
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
		final StructureObjectStruct rhs = (StructureObjectStruct) obj;
		return new EqualsBuilder().append(documentation, rhs.documentation)
		                          .append(defaultConstructor, rhs.defaultConstructor)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(documentation)
		                                                                .append(defaultConstructor)
		                                                                .append(printer)
		                                                                .toString();
	}
}
