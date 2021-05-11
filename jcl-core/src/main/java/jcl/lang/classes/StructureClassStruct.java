package jcl.lang.classes;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StructureClassStruct} is the object representation of a Lisp 'structure-class' type.
 */
public abstract class StructureClassStruct extends ClassStruct {

	private static final SymbolStruct STRUCTURE_CLASS_DEFINITION = SymbolStruct.toLispSymbol("STRUCTURE-CLASS-DEFINITION");

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

	public static StructureClassStruct getStructureClass(final SymbolStruct symbol) {
		return getStructureClass(symbol, true);
	}

	public static StructureClassStruct getStructureClass(final SymbolStruct symbol, final boolean errorP) {
		final LispStruct definition = symbol.getProperty(STRUCTURE_CLASS_DEFINITION, NILStruct.INSTANCE);
		if (NILStruct.INSTANCE.eq(definition)) {
			if (errorP) {
				throw new ProgramErrorException(
						"Provided symbol '" + symbol + "' does not have a defined structure-class.");
			}
			return null;
		}
		if (!(definition instanceof StructureClassStruct)) {
			throw new IllegalStateException("Invalid structure-class: " + definition);
		}
		return (StructureClassStruct) definition;
	}

	public static void setStructureClass(final SymbolStruct symbol, final StructureClassStruct structureClass) {
		symbol.setProperty(STRUCTURE_CLASS_DEFINITION, structureClass);
	}

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
