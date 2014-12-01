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
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

@Component
public class SpecialOperatorAnalyzer implements Analyzer<LispStruct, ListStruct> {

	private static final Map<SpecialOperator, Class<? extends Analyzer<? extends LispStruct, ListStruct>>> STRATEGIES = new HashMap<>();

	static {
		STRATEGIES.put(SpecialOperator.BLOCK, BlockAnalyzer.class);
		STRATEGIES.put(SpecialOperator.CATCH, CatchAnalyzer.class);
		STRATEGIES.put(SpecialOperator.EVAL_WHEN, EvalWhenAnalyzer.class);
		STRATEGIES.put(SpecialOperator.FLET, FletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.FUNCTION, FunctionAnalyzer.class);
		STRATEGIES.put(SpecialOperator.GO, GoAnalyzer.class);
		STRATEGIES.put(SpecialOperator.IF, IfAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LABELS, LabelsAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LET, LetAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LET_STAR, LetStarAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LOAD_TIME_VALUE, LoadTimeValueAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LOCALLY, LocallyAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MACROLET, MacroletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_CALL, MultipleValueCallAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_PROG1, MultipleValueProg1Analyzer.class);
		STRATEGIES.put(SpecialOperator.PROGN, PrognAnalyzer.class);
		STRATEGIES.put(SpecialOperator.PROGV, ProgvAnalyzer.class);
		STRATEGIES.put(SpecialOperator.QUOTE, QuoteAnalyzer.class);
		STRATEGIES.put(SpecialOperator.RETURN_FROM, ReturnFromAnalyzer.class);
		STRATEGIES.put(SpecialOperator.SETQ, SetqAnalyzer.class);
		STRATEGIES.put(SpecialOperator.SYMBOL_MACROLET, SymbolMacroletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.TAGBODY, TagbodyAnalyzer.class);
		STRATEGIES.put(SpecialOperator.THE, TheAnalyzer.class);
		STRATEGIES.put(SpecialOperator.THROW, ThrowAnalyzer.class);
		STRATEGIES.put(SpecialOperator.UNWIND_PROTECT, UnwindProtectAnalyzer.class);

//		STRATEGIES.put(SpecialOperator.DECLARE, DeclareAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LAMBDA, LambdaAnalyzer.class);

		STRATEGIES.put(SpecialOperator.DEFSTRUCT, DefstructAnalyzer.class);
	}

	@Autowired
	private ApplicationContext context;

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final SpecialOperator specialOperator = (SpecialOperator) input.getFirst();

		final Class<? extends Analyzer<? extends LispStruct, ListStruct>> strategy = STRATEGIES.get(specialOperator);
		final Analyzer<? extends LispStruct, ListStruct> strategyBean = context.getBean(strategy);

		if (strategy == null) {
			throw new ProgramErrorException("SpecialOperator symbol supplied is not supported.");
		}
		return strategyBean.analyze(input, analyzer);
	}
}
