package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.functions.GensymFunction;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

public class SpecialOperatorAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final Analyzer<LispStruct, ListStruct> INSTANCE = new SpecialOperatorAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		final SpecialOperator specialOperator = (SpecialOperator) input.getFirst();
		LispStruct result = null;

		// Determine the special form and generate its code.
		if (specialOperator.equals(SpecialOperator.BLOCK)) {
			result = SemanticAnalyzer.saBlock(input);
		} else if (specialOperator.equals(SpecialOperator.CATCH)) {
			result = SemanticAnalyzer.saCatch(input);
		} else if (specialOperator.equals(SpecialOperator.EVAL_WHEN)) {
			result = SemanticAnalyzer.saEvalWhen(input);
		} else if (specialOperator.equals(SpecialOperator.FLET)) {
			result = SemanticAnalyzer.saFlet(input);
		} else if (specialOperator.equals(SpecialOperator.FUNCTION)) {
			result = SemanticAnalyzer.saFunction(input);
		} else if (specialOperator.equals(SpecialOperator.GO)) {
			result = SemanticAnalyzer.saGo(input);
		} else if (specialOperator.equals(SpecialOperator.IF)) {
			result = SemanticAnalyzer.saIf(input);
		} else if (specialOperator.equals(SpecialOperator.LABELS)) {
			result = SemanticAnalyzer.saLabels(input);
		} else if (specialOperator.equals(SpecialOperator.LET)) {
			result = SemanticAnalyzer.saLet(input);
		} else if (specialOperator.equals(SpecialOperator.LET_STAR)) {
			result = SemanticAnalyzer.saLetStar(input);
		} else if (specialOperator.equals(SpecialOperator.LOAD_TIME_VALUE)) {
			result = SemanticAnalyzer.saLoadTimeValue(input, "LOAD_TIME_VALUE_", GensymFunction.funcall(String.valueOf(System.currentTimeMillis()) + '_').toString());
		} else if (specialOperator.equals(SpecialOperator.LOCALLY)) {
			result = SemanticAnalyzer.saLocally(input);
		} else if (specialOperator.equals(SpecialOperator.MACROLET)) {
			result = SemanticAnalyzer.saMacrolet(input);
		} else if (specialOperator.equals(SpecialOperator.MULTIPLE_VALUE_CALL)) {
			result = SemanticAnalyzer.saMultipleValueCall(input);
		} else if (specialOperator.equals(SpecialOperator.MULTIPLE_VALUE_PROG1)) {
			result = SemanticAnalyzer.saMultipleValueProg1(input);
		} else if (specialOperator.equals(SpecialOperator.PROGN)) {
			result = SemanticAnalyzer.saProgn(input);
		} else if (specialOperator.equals(SpecialOperator.PROGV)) {
			result = SemanticAnalyzer.saProgv(input);
		} else if (specialOperator.equals(SpecialOperator.QUOTE)) {
			result = SemanticAnalyzer.saQuote(input, null);
		} else if (specialOperator.equals(SpecialOperator.RETURN_FROM)) {
			result = SemanticAnalyzer.saReturnFrom(input);
		} else if (specialOperator.equals(SpecialOperator.SETQ)) {
			result = SemanticAnalyzer.saSetq(input);
		} else if (specialOperator.equals(SpecialOperator.SYMBOL_MACROLET)) {
			result = SemanticAnalyzer.saSymbolMacrolet(input);
		} else if (specialOperator.equals(SpecialOperator.TAGBODY)) {
			result = SemanticAnalyzer.saTagbody(input);
		} else if (specialOperator.equals(SpecialOperator.THE)) {
			result = SemanticAnalyzer.saThe(input);
		} else if (specialOperator.equals(SpecialOperator.THROW)) {
			result = SemanticAnalyzer.saThrow(input);
		} else if (specialOperator.equals(SpecialOperator.UNWIND_PROTECT)) {
			result = SemanticAnalyzer.saUnwindProtect(input);

			// Special, Special Operators
		} else if (specialOperator.equals(SpecialOperator.DECLARE)) {
			result = SemanticAnalyzer.saDeclare(input);
		} else if (specialOperator.equals(SpecialOperator.LAMBDA)) {
			result = SemanticAnalyzer.saLambda(input);
		} else if (specialOperator.equals(SpecialOperator.MACRO_LAMBDA)) {
			result = SemanticAnalyzer.saMacroLambda(input);

			// Compiler, Special Operators
		} else if (specialOperator.equals(SpecialOperator.DEFSTRUCT)) {
			result = SemanticAnalyzer.saDefstruct(input);
		} else if (specialOperator.equals(SpecialOperator.FUNCTION_MARKER)) {
			result = SemanticAnalyzer.saFunctionCall(input);
		} else if (specialOperator.equals(SpecialOperator.STATIC_FIELD)) {
			result = SemanticAnalyzer.saStaticField(input);
		}
		return result;
	}
}
