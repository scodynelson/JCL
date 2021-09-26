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
		CommonLispSymbols.LAMBDA.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LambdaExpander.INSTANCE);
		CommonLispSymbols.MACRO_LAMBDA.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, MacroLambdaExpander.INSTANCE);

		CommonLispSymbols.DECLARE.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, DeclareExpander.INSTANCE);

		CommonLispSymbols.DEFSTRUCT_SO.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, DefstructExpander.INSTANCE);

		CommonLispSymbols.BLOCK.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, BlockExpander.INSTANCE);
		CommonLispSymbols.CATCH.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, CatchExpander.INSTANCE);
		CommonLispSymbols.EVAL_WHEN.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, EvalWhenExpander.INSTANCE);
		CommonLispSymbols.FLET.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, FletExpander.INSTANCE);
		CommonLispSymbols.FUNCTION.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, FunctionExpander.INSTANCE);
		CommonLispSymbols.GO.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, GoExpander.INSTANCE);
		CommonLispSymbols.IF.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, IfExpander.INSTANCE);
		CommonLispSymbols.LABELS.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LabelsExpander.INSTANCE);
		CommonLispSymbols.LET.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LetExpander.INSTANCE);
		CommonLispSymbols.LET_STAR.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LetStarExpander.INSTANCE);
		CommonLispSymbols.LOAD_TIME_VALUE.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LoadTimeValueExpander.INSTANCE);
		CommonLispSymbols.LOCALLY.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, LocallyExpander.INSTANCE);
		CommonLispSymbols.MACROLET.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, MacroletExpander.INSTANCE);
		CommonLispSymbols.MULTIPLE_VALUE_CALL.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, MultipleValueCallExpander.INSTANCE);
		CommonLispSymbols.MULTIPLE_VALUE_PROG1.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, MultipleValueProg1Expander.INSTANCE);
		CommonLispSymbols.PROGN.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, PrognExpander.INSTANCE);
		CommonLispSymbols.PROGV.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, ProgvExpander.INSTANCE);
		CommonLispSymbols.QUOTE.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, QuoteExpander.INSTANCE);
		CommonLispSymbols.RETURN_FROM.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, ReturnFromExpander.INSTANCE);
		CommonLispSymbols.SETQ.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, SetqExpander.INSTANCE);
		CommonLispSymbols.SYMBOL_MACROLET.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, SymbolMacroletExpander.INSTANCE);
		CommonLispSymbols.TAGBODY.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, TagbodyExpander.INSTANCE);
		CommonLispSymbols.THE.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, TheExpander.INSTANCE);
		CommonLispSymbols.THROW.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, ThrowExpander.INSTANCE);
		CommonLispSymbols.UNWIND_PROTECT.setProp(CommonLispSymbols.MACRO_FUNCTION_DEFINITION, UnwindProtectExpander.INSTANCE);
	}
}
