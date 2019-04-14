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
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapExpanders {

	public static void bootstrap() {
		SpecialOperatorStructImpl.LAMBDA.setMacroFunctionExpander(LambdaExpander.INSTANCE);
		SpecialOperatorStructImpl.MACRO_LAMBDA.setMacroFunctionExpander(MacroLambdaExpander.INSTANCE);

		SpecialOperatorStructImpl.DECLARE.setMacroFunctionExpander(DeclareExpander.INSTANCE);

		SpecialOperatorStructImpl.DEFSTRUCT.setMacroFunctionExpander(DefstructExpander.INSTANCE);

		SpecialOperatorStructImpl.BLOCK.setMacroFunctionExpander(BlockExpander.INSTANCE);
		SpecialOperatorStructImpl.CATCH.setMacroFunctionExpander(CatchExpander.INSTANCE);
		SpecialOperatorStructImpl.EVAL_WHEN.setMacroFunctionExpander(EvalWhenExpander.INSTANCE);
		SpecialOperatorStructImpl.FLET.setMacroFunctionExpander(FletExpander.INSTANCE);
		SpecialOperatorStructImpl.FUNCTION.setMacroFunctionExpander(FunctionExpander.INSTANCE);
		SpecialOperatorStructImpl.GO.setMacroFunctionExpander(GoExpander.INSTANCE);
		SpecialOperatorStructImpl.IF.setMacroFunctionExpander(IfExpander.INSTANCE);
		SpecialOperatorStructImpl.LABELS.setMacroFunctionExpander(LabelsExpander.INSTANCE);
		SpecialOperatorStructImpl.LET.setMacroFunctionExpander(LetExpander.INSTANCE);
		SpecialOperatorStructImpl.LET_STAR.setMacroFunctionExpander(LetStarExpander.INSTANCE);
		SpecialOperatorStructImpl.LOAD_TIME_VALUE.setMacroFunctionExpander(LoadTimeValueExpander.INSTANCE);
		SpecialOperatorStructImpl.LOCALLY.setMacroFunctionExpander(LocallyExpander.INSTANCE);
		SpecialOperatorStructImpl.MACROLET.setMacroFunctionExpander(MacroletExpander.INSTANCE);
		SpecialOperatorStructImpl.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(MultipleValueCallExpander.INSTANCE);
		SpecialOperatorStructImpl.MULTIPLE_VALUE_PROG1.setMacroFunctionExpander(MultipleValueProg1Expander.INSTANCE);
		SpecialOperatorStructImpl.PROGN.setMacroFunctionExpander(PrognExpander.INSTANCE);
		SpecialOperatorStructImpl.PROGV.setMacroFunctionExpander(ProgvExpander.INSTANCE);
		SpecialOperatorStructImpl.QUOTE.setMacroFunctionExpander(QuoteExpander.INSTANCE);
		SpecialOperatorStructImpl.RETURN_FROM.setMacroFunctionExpander(ReturnFromExpander.INSTANCE);
		SpecialOperatorStructImpl.SETQ.setMacroFunctionExpander(SetqExpander.INSTANCE);
		SpecialOperatorStructImpl.SYMBOL_MACROLET.setMacroFunctionExpander(SymbolMacroletExpander.INSTANCE);
		SpecialOperatorStructImpl.TAGBODY.setMacroFunctionExpander(TagbodyExpander.INSTANCE);
		SpecialOperatorStructImpl.THE.setMacroFunctionExpander(TheExpander.INSTANCE);
		SpecialOperatorStructImpl.THROW.setMacroFunctionExpander(ThrowExpander.INSTANCE);
		SpecialOperatorStructImpl.UNWIND_PROTECT.setMacroFunctionExpander(UnwindProtectExpander.INSTANCE);
	}
}
