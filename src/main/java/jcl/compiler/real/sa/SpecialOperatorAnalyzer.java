package jcl.compiler.real.sa;

import jcl.LispStruct;
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
import jcl.compiler.real.sa.specialoperator.compiler.DefstructAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.MacroLambdaAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.HashMap;
import java.util.Map;

public class SpecialOperatorAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final Analyzer<LispStruct, ListStruct> INSTANCE = new SpecialOperatorAnalyzer();

	private static final Map<SpecialOperator, Analyzer<LispStruct, ListStruct>> STRATEGIES = new HashMap<>();

	static {
		STRATEGIES.put(SpecialOperator.BLOCK, BlockAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.CATCH, CatchAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.EVAL_WHEN, EvalWhenAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.FLET, FletAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.FUNCTION, FunctionAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.GO, GoAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.IF, IfAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LABELS, LabelsAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LET, LetAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LET_STAR, LetStarAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LOAD_TIME_VALUE, LoadTimeValueAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LOCALLY, LocallyAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.MACROLET, MacroletAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_CALL, MultipleValueCallAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_PROG1, MultipleValueProg1Analyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.PROGN, PrognAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.PROGV, ProgvAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.QUOTE, QuoteAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.RETURN_FROM, ReturnFromAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.SETQ, SetqAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.SYMBOL_MACROLET, SymbolMacroletAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.TAGBODY, TagbodyAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.THE, TheAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.THROW, ThrowAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.UNWIND_PROTECT, UnwindProtectAnalyzer.INSTANCE);

//		STRATEGIES.put(SpecialOperator.DECLARE, DeclareAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LAMBDA, LambdaAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.MACRO_LAMBDA, MacroLambdaAnalyzer.INSTANCE);

		STRATEGIES.put(SpecialOperator.DEFSTRUCT, DefstructAnalyzer.INSTANCE);
	}

	@Override
	public LispStruct analyze(final ListStruct input) {

		final SpecialOperator specialOperator = (SpecialOperator) input.getFirst();

		final Analyzer<LispStruct, ListStruct> strategy = STRATEGIES.get(specialOperator);
		if (strategy == null) {
			throw new ProgramErrorException("SpecialOperator symbol supplied is not supported.");
		}
		return strategy.analyze(input);
	}
}
