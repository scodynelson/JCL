package jcl.symbols;

public final class SpecialOperator extends SymbolStruct<SpecialOperator> {

	public static final SpecialOperator BLOCK = new SpecialOperator("BLOCK");
	public static final SpecialOperator CATCH = new SpecialOperator("CATCH");
	public static final SpecialOperator EVAL_WHEN = new SpecialOperator("EVAL_WHEN");
	public static final SpecialOperator FLET = new SpecialOperator("FLET");
	public static final SpecialOperator FUNCTION = new SpecialOperator("FUNCTION");
	public static final SpecialOperator GO = new SpecialOperator("GO");
	public static final SpecialOperator IF = new SpecialOperator("IF");
	public static final SpecialOperator LABELS = new SpecialOperator("LABELS");
	public static final SpecialOperator LET = new SpecialOperator("LET");
	public static final SpecialOperator LET_STAR = new SpecialOperator("LET_STAR");
	public static final SpecialOperator LOAD_TIME_VALUE = new SpecialOperator("LOAD_TIME_VALUE");
	public static final SpecialOperator LOCALLY = new SpecialOperator("LOCALLY");
	public static final SpecialOperator MACROLET = new SpecialOperator("MACROLET");
	public static final SpecialOperator MULTIPLE_VALUE_CALL = new SpecialOperator("MULTIPLE_VALUE_CALL");
	public static final SpecialOperator MULTIPLE_VALUE_PROG1 = new SpecialOperator("MULTIPLE_VALUE_PROG1");
	public static final SpecialOperator PROGN = new SpecialOperator("PROGN");
	public static final SpecialOperator PROGV = new SpecialOperator("PROGV");
	public static final SpecialOperator QUOTE = new SpecialOperator("QUOTE");
	public static final SpecialOperator RETURN_FROM = new SpecialOperator("RETURN_FROM");
	public static final SpecialOperator SETQ = new SpecialOperator("SETQ");
	public static final SpecialOperator SYMBOL_MACROLET = new SpecialOperator("SYMBOL_MACROLET");
	public static final SpecialOperator TAGBODY = new SpecialOperator("TAGBODY");
	public static final SpecialOperator THE = new SpecialOperator("THE");
	public static final SpecialOperator THROW = new SpecialOperator("THROW");
	public static final SpecialOperator UNWIND_PROTECT = new SpecialOperator("UNWIND_PROTECT");

	private SpecialOperator(final String name) {
		super(name);
	}
}
