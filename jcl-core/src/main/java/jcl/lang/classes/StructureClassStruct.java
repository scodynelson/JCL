package jcl.lang.classes;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StructureClassStruct} is the object representation of a Lisp 'structure-class' type.
 */
public abstract class StructureClassStruct extends ClassStruct {

	protected final SymbolStruct defaultConstructorSymbol;

	protected final SymbolStruct printerSymbol;

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the structure class object
	 * @param defaultConstructorSymbol
	 * 		the default constructor function symbol for this structure class
	 * @param printerSymbol
	 * 		the printer function symbol for this structure class
	 */
	protected StructureClassStruct(final SymbolStruct name, final SymbolStruct defaultConstructorSymbol,
	                               final SymbolStruct printerSymbol) {
		super(name);
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
	}

	public SymbolStruct getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public abstract StructureObjectStruct newInstance();

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.STRUCTURE_CLASS;
	}

	@Override
	public ClassStruct classOf() {
		return ClassStruct.findClass(CommonLispSymbols.STRUCTURE_CLASS);
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.STRUCTURE_CLASS) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == ClassStruct.findClass(CommonLispSymbols.STRUCTURE_CLASS)) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
