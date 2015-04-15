package jcl.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.ClassStruct;
import jcl.functions.FunctionStruct;
import jcl.types.StructureClassType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StructureClassStruct} is the object representation of a Lisp 'structure-class' type.
 */
public abstract class StructureClassStruct extends ClassStruct {

	private static final long serialVersionUID = 8418743690243529133L;

	protected final FunctionStruct defaultConstructor;

	/**
	 * Protected constructor.
	 *
	 * @param defaultConstructor
	 * 		the default constructor function for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final FunctionStruct defaultConstructor,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(StructureClassType.INSTANCE, defaultConstructor, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the structure class object
	 * @param defaultConstructor
	 * 		the default constructor function for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final LispType type, final FunctionStruct defaultConstructor,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
		this.defaultConstructor = defaultConstructor;
	}

	public FunctionStruct getDefaultConstructor() {
		return defaultConstructor;
	}

	public abstract StructureObjectStruct newInstance();

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(defaultConstructor)
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
		final StructureClassStruct rhs = (StructureClassStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(defaultConstructor, rhs.defaultConstructor)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(defaultConstructor)
		                                                                .toString();
	}
}
