package jcl.lang;

import java.util.List;

import jcl.type.LispType;
import jcl.type.StructureClassType;

/**
 * The {@link StructureClassStruct} is the object representation of a Lisp 'structure-class' type.
 */
public abstract class StructureClassStruct extends ClassStruct {

	protected final SymbolStructImpl defaultConstructorSymbol;

	protected final SymbolStructImpl printerSymbol;

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
	protected StructureClassStruct(final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
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
	protected StructureClassStruct(final LispType type, final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                               final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
	}

	public SymbolStructImpl getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public abstract StructureObjectStruct newInstance();
}
