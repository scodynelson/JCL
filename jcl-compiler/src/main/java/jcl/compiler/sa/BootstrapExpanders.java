package jcl.compiler.sa;

import jcl.compiler.sa.analyzer.LambdaExpander;
import jcl.compiler.sa.analyzer.MacroLambdaExpander;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.sa.analyzer.defstruct.DefstructExpander;
import jcl.compiler.sa.analyzer.specialoperator.BlockExpander;
import jcl.compiler.sa.analyzer.specialoperator.CatchExpander;
import jcl.compiler.sa.analyzer.specialoperator.EvalWhenExpander;
import jcl.compiler.sa.analyzer.specialoperator.FletExpander;
import jcl.compiler.sa.analyzer.specialoperator.FunctionExpander;
import jcl.compiler.sa.analyzer.specialoperator.GoExpander;
import jcl.compiler.sa.analyzer.specialoperator.IfExpander;
import jcl.compiler.sa.analyzer.specialoperator.LabelsExpander;
import jcl.compiler.sa.analyzer.specialoperator.LetExpander;
import jcl.compiler.sa.analyzer.specialoperator.LetStarExpander;
import jcl.compiler.sa.analyzer.specialoperator.LoadTimeValueExpander;
import jcl.compiler.sa.analyzer.specialoperator.LocallyExpander;
import jcl.compiler.sa.analyzer.specialoperator.MacroletExpander;
import jcl.compiler.sa.analyzer.specialoperator.MultipleValueCallExpander;
import jcl.compiler.sa.analyzer.specialoperator.MultipleValueProg1Expander;
import jcl.compiler.sa.analyzer.specialoperator.PrognExpander;
import jcl.compiler.sa.analyzer.specialoperator.ProgvExpander;
import jcl.compiler.sa.analyzer.specialoperator.QuoteExpander;
import jcl.compiler.sa.analyzer.specialoperator.ReturnFromExpander;
import jcl.compiler.sa.analyzer.specialoperator.SetqExpander;
import jcl.compiler.sa.analyzer.specialoperator.SymbolMacroletExpander;
import jcl.compiler.sa.analyzer.specialoperator.TagbodyExpander;
import jcl.compiler.sa.analyzer.specialoperator.TheExpander;
import jcl.compiler.sa.analyzer.specialoperator.ThrowExpander;
import jcl.compiler.sa.analyzer.specialoperator.UnwindProtectExpander;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapExpanders {

	public static void bootstrap() {
		CommonLispSymbols.LAMBDA.setMacroFunctionExpander(LambdaExpander.INSTANCE);
		CommonLispSymbols.MACRO_LAMBDA.setMacroFunctionExpander(MacroLambdaExpander.INSTANCE);

		CommonLispSymbols.DECLARE.setMacroFunctionExpander(DeclareExpander.INSTANCE);

		CommonLispSymbols.DEFSTRUCT_SO.setMacroFunctionExpander(DefstructExpander.INSTANCE);

		CommonLispSymbols.BLOCK.setMacroFunctionExpander(BlockExpander.INSTANCE);
		CommonLispSymbols.CATCH.setMacroFunctionExpander(CatchExpander.INSTANCE);
		CommonLispSymbols.EVAL_WHEN.setMacroFunctionExpander(EvalWhenExpander.INSTANCE);
		CommonLispSymbols.FLET.setMacroFunctionExpander(FletExpander.INSTANCE);
		CommonLispSymbols.FUNCTION.setMacroFunctionExpander(FunctionExpander.INSTANCE);
		CommonLispSymbols.GO.setMacroFunctionExpander(GoExpander.INSTANCE);
		CommonLispSymbols.IF.setMacroFunctionExpander(IfExpander.INSTANCE);
		CommonLispSymbols.LABELS.setMacroFunctionExpander(LabelsExpander.INSTANCE);
		CommonLispSymbols.LET.setMacroFunctionExpander(LetExpander.INSTANCE);
		CommonLispSymbols.LET_STAR.setMacroFunctionExpander(LetStarExpander.INSTANCE);
		CommonLispSymbols.LOAD_TIME_VALUE.setMacroFunctionExpander(LoadTimeValueExpander.INSTANCE);
		CommonLispSymbols.LOCALLY.setMacroFunctionExpander(LocallyExpander.INSTANCE);
		CommonLispSymbols.MACROLET.setMacroFunctionExpander(MacroletExpander.INSTANCE);
		CommonLispSymbols.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(MultipleValueCallExpander.INSTANCE);
		CommonLispSymbols.MULTIPLE_VALUE_PROG1.setMacroFunctionExpander(MultipleValueProg1Expander.INSTANCE);
		CommonLispSymbols.PROGN.setMacroFunctionExpander(PrognExpander.INSTANCE);
		CommonLispSymbols.PROGV.setMacroFunctionExpander(ProgvExpander.INSTANCE);
		CommonLispSymbols.QUOTE.setMacroFunctionExpander(QuoteExpander.INSTANCE);
		CommonLispSymbols.RETURN_FROM.setMacroFunctionExpander(ReturnFromExpander.INSTANCE);
		CommonLispSymbols.SETQ.setMacroFunctionExpander(SetqExpander.INSTANCE);
		CommonLispSymbols.SYMBOL_MACROLET.setMacroFunctionExpander(SymbolMacroletExpander.INSTANCE);
		CommonLispSymbols.TAGBODY.setMacroFunctionExpander(TagbodyExpander.INSTANCE);
		CommonLispSymbols.THE.setMacroFunctionExpander(TheExpander.INSTANCE);
		CommonLispSymbols.THROW.setMacroFunctionExpander(ThrowExpander.INSTANCE);
		CommonLispSymbols.UNWIND_PROTECT.setMacroFunctionExpander(UnwindProtectExpander.INSTANCE);
	}
}
