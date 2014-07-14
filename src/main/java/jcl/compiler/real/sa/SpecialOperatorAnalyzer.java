package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.sa.specialoperator.BlockAnalyzer;
import jcl.compiler.real.sa.specialoperator.CatchAnalyzer;
import jcl.compiler.real.sa.specialoperator.EvalWhenAnalyzer;
import jcl.compiler.real.sa.specialoperator.FletAnalyzer;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.compiler.real.sa.specialoperator.GoAnalyzer;
import jcl.compiler.real.sa.specialoperator.IfAnalyzer;
import jcl.compiler.real.sa.specialoperator.LabelsAnalyzer;
import jcl.compiler.real.sa.specialoperator.LetAnalyzer;
import jcl.compiler.real.sa.specialoperator.LetStarAnalyzer;
import jcl.compiler.real.sa.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.specialoperator.LocallyAnalyzer;
import jcl.compiler.real.sa.specialoperator.MacroletAnalyzer;
import jcl.compiler.real.sa.specialoperator.MultipleValueCallAnalyzer;
import jcl.compiler.real.sa.specialoperator.MultipleValueProg1Analyzer;
import jcl.compiler.real.sa.specialoperator.PrognAnalyzer;
import jcl.compiler.real.sa.specialoperator.ProgvAnalyzer;
import jcl.compiler.real.sa.specialoperator.QuoteAnalyzer;
import jcl.compiler.real.sa.specialoperator.ReturnFromAnalyzer;
import jcl.compiler.real.sa.specialoperator.SetqAnalyzer;
import jcl.compiler.real.sa.specialoperator.SymbolMacroletAnalyzer;
import jcl.compiler.real.sa.specialoperator.TagbodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.TheAnalyzer;
import jcl.compiler.real.sa.specialoperator.ThrowAnalyzer;
import jcl.compiler.real.sa.specialoperator.UnwindProtectAnalyzer;
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
			result = BlockAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.CATCH)) {
			result = CatchAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.EVAL_WHEN)) {
			result = EvalWhenAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.FLET)) {
			result = FletAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.FUNCTION)) {
			result = FunctionAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.GO)) {
			result = GoAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.IF)) {
			result = IfAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.LABELS)) {
			result = LabelsAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.LET)) {
			result = LetAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.LET_STAR)) {
			result = LetStarAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.LOAD_TIME_VALUE)) {
			result = LoadTimeValueAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.LOCALLY)) {
			result = LocallyAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.MACROLET)) {
			result = MacroletAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.MULTIPLE_VALUE_CALL)) {
			result = MultipleValueCallAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.MULTIPLE_VALUE_PROG1)) {
			result = MultipleValueProg1Analyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.PROGN)) {
			result = PrognAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.PROGV)) {
			result = ProgvAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.QUOTE)) {
			result = QuoteAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.RETURN_FROM)) {
			result = ReturnFromAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.SETQ)) {
			result = SetqAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.SYMBOL_MACROLET)) {
			result = SymbolMacroletAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.TAGBODY)) {
			result = TagbodyAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.THE)) {
			result = TheAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.THROW)) {
			result = ThrowAnalyzer.INSTANCE.analyze(input);
		} else if (specialOperator.equals(SpecialOperator.UNWIND_PROTECT)) {
			result = UnwindProtectAnalyzer.INSTANCE.analyze(input);

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
