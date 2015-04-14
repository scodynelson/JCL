package jcl.classes;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.functions.FunctionStruct;
import jcl.symbols.SymbolStruct;
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

	protected final FunctionStruct printer;

	/**
	 * Protected constructor.
	 *
	 * @param defaultConstructor
	 * 		the default constructor function for this structure class
	 * @param printer
	 * 		the printer function for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(StructureClassType.INSTANCE, defaultConstructor, printer, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the structure class object
	 * @param defaultConstructor
	 * 		the default constructor function for this structure class
	 * @param printer
	 * 		the printer function for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final LispType type, final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
		this.defaultConstructor = defaultConstructor;
		this.printer = printer;
	}

	public FunctionStruct getDefaultConstructor() {
		return defaultConstructor;
	}

	public FunctionStruct getPrinter() {
		return printer;
	}

	// TODO: validation on slots? are they actually stored on the instance? Probably not...
	public abstract StructureObjectStruct newInstance(final Map<SymbolStruct<?>, LispStruct> slots);

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
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
		final StructureClassStruct rhs = (StructureClassStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(defaultConstructor, rhs.defaultConstructor)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(defaultConstructor)
		                                                                .append(printer)
		                                                                .toString();
	}
}
