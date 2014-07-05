package jcl.compiler.old.symbol;

import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;

public class KeywordOld {

	/* Keywords for compiler environment */
	public static final SymbolStruct LoadTimeValue = new KeywordSymbolStruct("LOAD-TIME-VALUE");
	/**
	 * An instance of the Keyword FIELD_NAME
	 */
	public static final SymbolStruct FieldName = new KeywordSymbolStruct("FIELD_NAME");
	/**
	 * An instance of the Keyword Binding
	 */
	public static final SymbolStruct Binding = new KeywordSymbolStruct("BINDING");
	/**
	 * An instance of the Keyword Bindings
	 */
	public static final SymbolStruct Bindings = new KeywordSymbolStruct("BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final SymbolStruct FletBinding = new KeywordSymbolStruct("FLET-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final SymbolStruct FletBindings = new KeywordSymbolStruct("FLET-BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final SymbolStruct LabelsBinding = new KeywordSymbolStruct("LABELS-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final SymbolStruct LabelsBindings = new KeywordSymbolStruct("LABELS-BINDINGS");
	/**
	 * An instance of the Keyword SymbolTable
	 */
	public static final SymbolStruct SymbolTable = new KeywordSymbolStruct("SYMBOL-TABLE");
	/**
	 * An instance of the Keyword Closure
	 */
	public static final SymbolStruct Closure = new KeywordSymbolStruct("CLOSURE");
	/**
	 * An instance of the Keyword Parent
	 */
	public static final SymbolStruct Parent = new KeywordSymbolStruct("PARENT");
	/**
	 * An instance of the Keyword Type
	 */
	public static final SymbolStruct Type = new KeywordSymbolStruct("TYPE");
	/**
	 * An instance of the Keyword Allocation
	 */
	public static final SymbolStruct Allocation = new KeywordSymbolStruct("ALLOCATION");
	/**
	 * An instance of the Keyword Scope
	 */
	public static final SymbolStruct Scope = new KeywordSymbolStruct("SCOPE");
	/**
	 * An instance of the Keyword InitForm
	 */
	public static final SymbolStruct InitForm = new KeywordSymbolStruct("INIT-FORM");
	/**
	 * An instance of the Keyword UnboundValue
	 */
	public static final SymbolStruct UnboundValue = new KeywordSymbolStruct("*UNBOUND-VALUE*");
	/**
	 * An instance of the Keyword Lexical
	 */
	public static final SymbolStruct Lexical = new KeywordSymbolStruct("LEXICAL");
	/**
	 * An instance of the Keyword Dynamic
	 */
	public static final SymbolStruct Dynamic = new KeywordSymbolStruct("DYNAMIC");
	/**
	 * An instance of the Keyword Parameter
	 */
	public static final SymbolStruct Parameter = new KeywordSymbolStruct("PARAMETER");
	/**
	 * An instance of the Keyword Local
	 */
	public static final SymbolStruct Local = new KeywordSymbolStruct("LOCAL");
	/**
	 * An instance of the Keyword References
	 */
	public static final SymbolStruct References = new KeywordSymbolStruct("REFERENCES");
	/**
	 * An instance of the Keyword Required
	 */
	public static final SymbolStruct Required = new KeywordSymbolStruct("REQUIRED");
	/**
	 * An instance of the Keyword Free
	 */
	public static final SymbolStruct Free = new KeywordSymbolStruct("FREE");
	/**
	 * An instance of the Keyword Position
	 */
	public static final SymbolStruct Position = new KeywordSymbolStruct("POSITION");
	/**
	 * An instance of the Keyword Depth
	 */
	public static final SymbolStruct Depth = new KeywordSymbolStruct("DEPTH");
	/**
	 * An instance of the SymbolStruct Name.
	 */
	public static final SymbolStruct Name = new KeywordSymbolStruct("NAME");

	public static final SymbolStruct CompileToplevel = new KeywordSymbolStruct("COMPILE-TOPLEVEL");
	public static final SymbolStruct LoadToplevel = new KeywordSymbolStruct("LOAD-TOPLEVEL");
	public static final SymbolStruct Execute = new KeywordSymbolStruct("EXECUTE");
}
