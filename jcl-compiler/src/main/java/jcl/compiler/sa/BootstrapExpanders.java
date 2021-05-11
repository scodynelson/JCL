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
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapExpanders {

	public static void bootstrap() {
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LAMBDA, LambdaExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.MACRO_LAMBDA, MacroLambdaExpander.INSTANCE);

		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.DECLARE, DeclareExpander.INSTANCE);

		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.DEFSTRUCT_SO, DefstructExpander.INSTANCE);

		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.BLOCK, BlockExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.CATCH, CatchExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.EVAL_WHEN, EvalWhenExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.FLET, FletExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.FUNCTION, FunctionExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.GO, GoExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.IF, IfExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LABELS, LabelsExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LET, LetExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LET_STAR, LetStarExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LOAD_TIME_VALUE, LoadTimeValueExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.LOCALLY, LocallyExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.MACROLET, MacroletExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.MULTIPLE_VALUE_CALL, MultipleValueCallExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.MULTIPLE_VALUE_PROG1, MultipleValueProg1Expander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.PROGN, PrognExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.PROGV, ProgvExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.QUOTE, QuoteExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.RETURN_FROM, ReturnFromExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.SETQ, SetqExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.SYMBOL_MACROLET, SymbolMacroletExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.TAGBODY, TagbodyExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.THE, TheExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.THROW, ThrowExpander.INSTANCE);
		SymbolStruct.setMacroFunctionDefinition(CommonLispSymbols.UNWIND_PROTECT, UnwindProtectExpander.INSTANCE);
	}
}
