package jcl.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.ClassStruct;
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

	protected final SymbolStruct defaultConstructorSymbol;

	protected final SymbolStruct printerSymbol;

	/**
	 * Protected constructor.
	 *
	 * @param defaultConstructorSymbol
	 * 		the default constructor function symbol for this structure class
	 * @param printerSymbol
	 * 		the printer function symbol for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(StructureClassType.INSTANCE, defaultConstructorSymbol, printerSymbol, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the structure class object
	 * @param defaultConstructorSymbol
	 * 		the default constructor function symbol for this structure class
	 * @param printerSymbol
	 * 		the printer function symbol for this structure class
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StructureClassStruct(final LispType type, final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
	}

	public SymbolStruct getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public abstract StructureObjectStruct newInstance();

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(defaultConstructorSymbol)
		                            .append(printerSymbol)
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
		                          .append(defaultConstructorSymbol, rhs.defaultConstructorSymbol)
		                          .append(printerSymbol, rhs.printerSymbol)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(defaultConstructorSymbol)
		                                                                .append(printerSymbol)
		                                                                .toString();
	}
}
